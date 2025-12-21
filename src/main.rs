use crate::formatter::{DocFormatter, Formatter};
use crate::visitor::{Visitor, Walkable};
use clap::Parser;
use miette::{Diagnostic, LabeledSpan, Report, SourceCode};
use rayon::prelude::*;
use std::cmp::PartialEq;
use std::error::Error;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::{fs, process};

mod ast;
mod formatter;
mod lexer;
mod parser;
mod pretty_print;
mod visitor;

#[derive(Parser)]
#[command(version, about = "Efficient code formatter for ColdFusion")]
struct Cli {
    // First argument is default the file name / directory to run formatter on
    format_path: String,

    #[arg(
        short,
        long,
        default_value_t = false,
        help = "Write changes back to file"
    )]
    write: bool,

    #[arg(
        alias = "max-length",
        value_name = "max-length",
        visible_aliases = &["max-length"],
        short,
        long,
        help = "Set maximum line length for formatting",
        default_value = "120"
    )]
    max_line_length: Option<usize>,

    #[arg(
        long,
        help = "Optional file to output formatted content to (Instead of overwriting original file). Only works if formatting single file"
    )]
    output: Option<String>,

    #[arg(
        long,
        default_value_t = false,
        help = "Print the AST structure instead of formatting"
    )]
    print_ast: bool,

    #[arg(
        long,
        help = "Maximum depth to display when printing AST (unlimited if not specified)"
    )]
    ast_depth: Option<usize>,

    #[arg(
        long,
        default_value_t = false,
        help = "Use ANSI colors when printing AST"
    )]
    ast_colors: bool,

    #[arg(
        long,
        default_value_t = false,
        help = "Show token positions (line:col) in AST output"
    )]
    ast_show_tokens: bool,
}

fn main() -> miette::Result<()> {
    let cli = Cli::parse();

    // Parse the provided path
    let path = PathBuf::from(&cli.format_path);
    if !path.exists() {
        eprintln!("Error: Path '{}' does not exist.", cli.format_path);
        process::exit(0);
    }

    // If print_ast is enabled, handle AST printing separately
    if cli.print_ast {
        return print_ast_for_path(&path, &cli);
    }

    let output_path = if let Some(output_path) = &cli.output {
        Some(PathBuf::from(output_path))
    } else {
        None
    };
    let output_path_opt = if let Some(output_path) = &output_path {
        Some(output_path.as_path())
    } else {
        None
    };

    // If path is a directory, list all files and collect into vec to process
    let files_to_process = if path.is_dir() {
        get_files_with_extension(&path, "cfc")
    } else {
        if path.extension().and_then(|s| s.to_str()) != Some("cfc") {
            eprintln!("Error: File '{}' is not a .cfc file.", cli.format_path);
            process::exit(0);
        }
        vec![path]
    };

    // Thread-safe error collection
    let errors = Arc::new(Mutex::new(Vec::new()));

    // Start timing the entire processing
    let total_start = std::time::Instant::now();

    // Process files in parallel using rayon
    files_to_process.par_iter().for_each(|file_path| {
        if cli.write {
            // Format and write to each file, print diagnostics
            // let format_start = std::time::Instant::now();
            if let Err(e) = process_file(
                file_path,
                cli.max_line_length.unwrap_or(80),
                output_path_opt,
            ) {
                errors.lock().unwrap().push(e);
            }
            // let format_time = format_start.elapsed().as_micros();
            // println!("Formatted {} in {}μs", file_path.display(), format_time);
        } else {
            // Compare file content with formatted content and see if matches
            // let format_start = std::time::Instant::now();
            let source = std::fs::read_to_string(file_path).unwrap();

            let formatted_source = format_content(
                &source,
                file_path.to_str().unwrap(),
                cli.max_line_length.unwrap_or(80),
            );
            match formatted_source {
                Ok(formatted_source) => {
                    // TODO: Report as error that needs formatting
                    if formatted_source == source {
                        // println!("File {} is already formatted.", file_path.display());
                    } else {
                        println!("File {} needs formatting.", file_path.display());
                    }
                }
                Err(e) => errors.lock().unwrap().push(e),
            }

            // let format_time = format_start.elapsed().as_micros();
            // println!("Checked {} in {}μs", file_path.display(), format_time);
        }
    });

    // Calculate total time
    let total_time = total_start.elapsed().as_millis();

    // Extract errors from Arc<Mutex<>> after parallel processing
    let error_reports = Arc::try_unwrap(errors).unwrap().into_inner().unwrap();
    if error_reports.len() > 0 {
        let errors = Reports::from(error_reports);
        errors
            .reports
            .into_iter()
            .for_each(|e| eprintln!("{:?}", e));
    }

    // Print summary
    let action = if cli.write { "Formatted" } else { "Checked" };
    let file_count = files_to_process.len();
    let file_word = if file_count == 1 { "file" } else { "files" };
    println!(
        "\n{} {} {} in {}ms",
        action, file_count, file_word, total_time
    );

    Ok(())
}

/// Print the AST for a given path
fn print_ast_for_path(path: &PathBuf, cli: &Cli) -> miette::Result<()> {
    // Only support single files for AST printing
    if path.is_dir() {
        eprintln!("Error: AST printing only supports single files, not directories.");
        process::exit(1);
    }

    if path.extension().and_then(|s| s.to_str()) != Some("cfc") {
        eprintln!("Error: File '{}' is not a .cfc file.", path.display());
        process::exit(1);
    }

    // Read the file
    let source = std::fs::read_to_string(path)
        .map_err(|e| miette::miette!("Failed to read file: {}", e))?;

    // Parse into AST
    let mut parser = parser::Parser::new(&source, path.to_str().unwrap())?;
    let ast = parser.parse().map_err(Reports::from)?;

    // Configure pretty print settings
    let config = pretty_print::PrettyPrintConfig {
        show_tokens: cli.ast_show_tokens,
        show_token_values: true,
        max_depth: cli.ast_depth,
        use_colors: cli.ast_colors,
        compact_mode: false,
    };

    // Print the AST
    let ast_output = ast.pretty_print_with_config(config);
    println!("{}", ast_output);

    Ok(())
}

fn get_files_with_extension<P: AsRef<Path>>(path: P, ext: &str) -> Vec<PathBuf> {
    let mut files = Vec::new();

    if let Ok(entries) = fs::read_dir(path) {
        for entry in entries.filter_map(Result::ok) {
            let path = entry.path();

            if path.is_file() {
                if let Some(extension) = path.extension() {
                    if extension == ext {
                        files.push(path);
                    }
                }
            } else if path.is_dir() {
                files.extend(get_files_with_extension(path, ext));
            }
        }
    }

    files
}

#[derive(Debug)]
pub struct Reports {
    reports: Vec<Report>,
}
impl From<Vec<Report>> for Reports {
    fn from(value: Vec<Report>) -> Self {
        Self { reports: value }
    }
}

impl Error for Reports {}

impl Display for Reports {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            _f,
            "{} error{} occurred during processing.",
            self.reports.len(),
            if self.reports.len() > 1 { "s" } else { "" }
        )
    }
}

impl Diagnostic for Reports {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new("Error occurred while formatting file(s)"))
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new(format!(
            "See individual error{} for more details",
            if self.reports.len() > 1 { "s" } else { "" }
        )))
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        None
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        None
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        Some(Box::new(self.reports.iter().map(|report| report.as_ref())))
    }
}

/// Process a single file through the formatting pipeline
fn process_file(
    file_path: &Path,
    max_line_length: usize,
    write_path: Option<&Path>,
) -> miette::Result<()> {
    println!("Formatting: {}...", file_path.display());

    let start_total = std::time::Instant::now();

    // Read file
    let start_file = std::time::Instant::now();
    let source = std::fs::read_to_string(file_path);
    if let Ok(source) = &source {
        let read_time = start_file.elapsed().as_micros();

        // Parse
        let mut parser = parser::Parser::new(&source, file_path.to_str().unwrap())?;
        let parse_start = std::time::Instant::now();
        let ast = parser.parse().map_err(Reports::from)?;
        let parse_total_time = parse_start.elapsed().as_micros();
        let lex_time = parser.lex_time;
        let parse_time = parse_total_time - lex_time;

        // Format to Doc tree
        let doc_build_start = std::time::Instant::now();
        let mut formatter = Formatter::new();
        let doc = ast.walk(&mut formatter);
        let doc_build_time = doc_build_start.elapsed().as_micros();

        // Render to string
        let render_start = std::time::Instant::now();
        let mut doc_formatter = DocFormatter::new(max_line_length, 4);
        let formatted_result = doc_formatter.format(&doc);
        let render_time = render_start.elapsed().as_micros();

        // Write back to file (using temporary file for safety)
        let write_start = std::time::Instant::now();
        if let Some(write_path) = write_path {
            write_file_safely(write_path, &formatted_result);
        } else {
            write_file_safely(file_path, &formatted_result);
        }
        let write_time = write_start.elapsed().as_micros();

        let total_time = start_total.elapsed().as_micros();

        // Print timing information
        // TODO: Put behind debug flag
        // println!("  Read: {}μs", read_time);
        // println!("  Lex: {}μs", lex_time);
        // println!("  Parse: {}μs", parse_time);
        // println!("  Doc Build: {}μs", doc_build_time);
        // println!("  Render: {}μs", render_time);
        // println!("  Write: {}μs", write_time);
        // println!("  Total: {}μs", total_time);
    }

    Ok(())
}

fn format_content(input: &str, file_name: &str, max_line_length: usize) -> miette::Result<String> {
    // Parse into AST (parser internally handles lexing)
    let mut parser = parser::Parser::new(input, file_name)?;
    let ast = parser.parse().map_err(Reports::from)?;

    // Format the AST using the visitor pattern
    let mut formatter = Formatter::new();
    let doc = ast.walk(&mut formatter);

    // Render the document
    let mut doc_formatter = DocFormatter::new(max_line_length, 4); // 80 char width, 4 space indent
    Ok(doc_formatter.format(&doc))
}

/// Write content to file safely using a temporary file
fn write_file_safely(file_path: &Path, content: &str) {
    let temp_path = file_path.with_extension("cfc.tmp");

    // Write to temporary file first
    let _ = std::fs::write(&temp_path, content);

    // Atomically replace the original file
    let _ = std::fs::rename(&temp_path, file_path);
}
