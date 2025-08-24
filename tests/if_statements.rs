mod common;

use common::{assert_formats_to, assert_parses_successfully};

#[test]
fn test_simple_if_statement() {
    let input = r#"if (true) { return "hello"; }"#;
    let expected = r#"if (true) { return "hello" }"#;

    assert_formats_to(input, expected);
}

#[test]
fn test_if_statement_with_multiline_body() {
    let input = r#"if (condition) {
        var x = 1;
        return x;
    }"#;
    let expected = r#"if (condition) {
    var x = 1
    return x
}"#;

    assert_formats_to(input, expected);
}

#[test]
fn test_if_else_statement() {
    let input = r#"if (true) { return "yes"; } else { return "no"; }"#;
    let expected = r#"if (true) { return "yes" } else { return "no" }"#;

    assert_formats_to(input, expected);
}

#[test]
fn test_if_else_with_multiline_bodies() {
    let input = r#"if (condition) {
        var x = 1;
        return x;
    } else {
        var y = 2;
        return y;
    }"#;
    let expected = r#"
if (condition) {
    var x = 1
    return x
} else {
    var y = 2
    return y
}"#;

    assert_formats_to(input, expected);
}

#[test]
fn test_if_else_if_chain() {
    let input = r#"if (x == 1) {
        return "one";
    } else if (x == 2) {
        return "two";
    } else {
        return "other";
    }"#;
    let expected = r#"if (x == 1) {
    return "one"
} else if (x == 2) {
    return "two"
} else {
    return "other"
}"#;

    assert_formats_to(input, expected);
}

#[test]
fn test_nested_if_statements() {
    let input = r#"if (outer) {
        if (inner) {
            return "nested";
        }
    }"#;
    let expected = r#"if (outer) { if (inner) { return "nested" } }"#;

    assert_formats_to(input, expected);
}

#[test]
fn test_if_parsing_only() {
    // Test various valid if statement syntaxes parse correctly
    assert_parses_successfully(r#"if (true) { return; }"#);
    assert_parses_successfully(r#"if (x EQ y) { func(); }"#);
    assert_parses_successfully(r#"if (array[index] GT 5) { process(); }"#);
    assert_parses_successfully(r#"if (!condition) { handle(); }"#);
}
