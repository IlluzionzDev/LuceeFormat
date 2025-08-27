#![feature(associated_type_defaults)]

use crate::formatter::{DocFormatter, Formatter};
use crate::visitor::{Visitor, Walkable};
use clap::Parser;
use std::cmp::PartialEq;
use std::path::{Path, PathBuf};
use std::{fs, process};

mod ast;
mod formatter;
mod lexer;
mod parser;
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
        visible_aliases = &["max_length"],
        short,
        long,
        help = "Set maximum line length for formatting",
        default_value = "80"
    )]
    max_line_length: Option<usize>,

    #[arg(
        long,
        help = "Optional file to output formatted content to (Instead of overwriting original file)"
    )]
    output: Option<String>,
}

fn main() {
    let cli = Cli::parse();

    // Parse the provided path
    let path = PathBuf::from(&cli.format_path);
    if !path.exists() {
        eprintln!("Error: Path '{}' does not exist.", cli.format_path);
        process::exit(0);
    }

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

    files_to_process.iter().for_each(|file_path| {
        if cli.write {
            // Format and write to each file, print diagnostics
            let format_start = std::time::Instant::now();
            if let Err(e) = process_file(file_path, cli.max_line_length.unwrap_or(80)) {
                eprintln!("Error processing file {}: {}", file_path.display(), e);
            }
            let format_time = format_start.elapsed().as_micros();
            println!("Formatted {} in {}μs", file_path.display(), format_time);
        } else {
            // Compare file content with formatted content and see if matches
            let format_start = std::time::Instant::now();
            let source = std::fs::read_to_string(file_path).unwrap();
            let formatted_source = format_content(&source, cli.max_line_length.unwrap_or(80));
            let format_time = format_start.elapsed().as_micros();
            println!("Checked {} in {}μs", file_path.display(), format_time);

            if formatted_source == source {
                println!("File {} is already formatted.", file_path.display());
            } else {
                println!("File {} needs formatting.", file_path.display());
            }
        }
    });
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

/// Process a single file through the formatting pipeline
fn process_file(
    file_path: &Path,
    max_line_length: usize,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("Formatting: {}...", file_path.display());

    let start_total = std::time::Instant::now();

    // Read file
    let start_file = std::time::Instant::now();
    let source = std::fs::read_to_string(file_path)?;
    let read_time = start_file.elapsed().as_micros();

    // Parse
    let mut parser = parser::Parser::new(&source);
    let parse_start = std::time::Instant::now();
    let ast = parser.parse();
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
    write_file_safely(file_path, &formatted_result)?;
    let write_time = write_start.elapsed().as_micros();

    let total_time = start_total.elapsed().as_micros();

    // Print timing information
    // println!("  Read: {}μs", read_time);
    // println!("  Lex: {}μs", lex_time);
    // println!("  Parse: {}μs", parse_time);
    // println!("  Doc Build: {}μs", doc_build_time);
    // println!("  Render: {}μs", render_time);
    // println!("  Write: {}μs", write_time);
    // println!("  Total: {}μs", total_time);

    Ok(())
}

fn format_content(input: &str, max_line_length: usize) -> String {
    // Parse into AST (parser internally handles lexing)
    let mut parser = parser::Parser::new(input);
    let ast = parser.parse();

    // Format the AST using the visitor pattern
    let mut formatter = Formatter::new();
    let doc = ast.walk(&mut formatter);

    // Render the document
    let mut doc_formatter = DocFormatter::new(max_line_length, 4); // 80 char width, 4 space indent
    doc_formatter.format(&doc)
}

/// Write content to file safely using a temporary file
fn write_file_safely(file_path: &Path, content: &str) -> Result<(), Box<dyn std::error::Error>> {
    let temp_path = file_path.with_extension("cfc.tmp");

    // Write to temporary file first
    std::fs::write(&temp_path, content)?;

    // Atomically replace the original file
    std::fs::rename(&temp_path, file_path)?;

    Ok(())
}
