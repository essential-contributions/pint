use std::{fs, io::Write, path::PathBuf};

fn remove_ansi_codes(input: &str) -> String {
    let re = regex::Regex::new(r"\x1B\[([0-9;]*[A-Za-z])").unwrap();
    re.replace_all(input, "").to_string()
}

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

struct Output {
    stdout: String,
    stderr: String,
}

#[cfg(test)]
fn pintc_command(args: &str) -> Output {
    let output = test_bin::get_test_bin("pintc")
        .args(args.split_ascii_whitespace().collect::<Vec<_>>())
        .output()
        .expect("failed to start pintc");

    Output {
        stdout: remove_ansi_codes(&String::from_utf8_lossy(&output.stdout)),
        stderr: remove_ansi_codes(&String::from_utf8_lossy(&output.stderr)),
    }
}

#[test]
fn err() {
    // Missing input_file file
    check(
        &pintc_command("").stderr,
        expect_test::expect![[r#"
            error: the following required arguments were not provided:
              <FILEPATH>

            Usage: pintc <FILEPATH>

            For more information, try '--help'.
        "#]],
    );

    // input_file file not found
    check(
        &pintc_command("missing.pnt").stderr,
        expect_test::expect![[r#"
            Error: couldn't read missing.pnt: No such file or directory (os error 2)
            Error: could not compile `missing.pnt` due to previous error
        "#]],
    );

    // Missing output file after `-o`
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    write!(input_file.as_file_mut(), "solve satisfy;").unwrap();

    let output = pintc_command(&format!("{} -o", input_file.path().to_str().unwrap(),));
    check(
        &output.stderr,
        expect_test::expect![[r#"
        error: a value is required for '--output <OUTPUT>' but none was supplied

        For more information, try '--help'.
    "#]],
    );
}

#[test]
fn compile_errors() {
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    let code = r#"var t: {} = {}; var a = a[]; solve satisfy;"#;
    write!(input_file.as_file_mut(), "{code}").unwrap();

    let output = pintc_command(input_file.path().to_str().unwrap());

    // No output file generated due to parse errors
    assert!(!input_file.path().with_extension("json").exists());

    // Check multi-error outputs
    check(
        &output
            .stderr
            .replace(input_file.path().to_str().unwrap(), "filepath"),
        expect_test::expect![[r#"
            Error: empty tuple types are not allowed
               ╭─[filepath:1:8]
               │
             1 │ var t: {} = {}; var a = a[]; solve satisfy;
               │        ─┬  
               │         ╰── empty tuple type found
            ───╯
            Error: empty tuple expressions are not allowed
               ╭─[filepath:1:13]
               │
             1 │ var t: {} = {}; var a = a[]; solve satisfy;
               │             ─┬  
               │              ╰── empty tuple expression found
            ───╯
            Error: missing array or map index
               ╭─[filepath:1:25]
               │
             1 │ var t: {} = {}; var a = a[]; solve satisfy;
               │                         ─┬─  
               │                          ╰─── missing array or map element index
            ───╯
            Error: could not compile `filepath` due to 3 previous errors
        "#]],
    );

    check(&output.stdout, expect_test::expect![""]);
}

#[test]
fn default_output() {
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    write!(input_file.as_file_mut(), "solve satisfy;").unwrap();

    let output = pintc_command(input_file.path().to_str().unwrap());

    assert!(input_file.path().with_extension("json").exists());
    let _ = fs::remove_file(input_file.path().with_extension("json"));

    check(&output.stderr, expect_test::expect![""]);
    check(&output.stdout, expect_test::expect![""]);
}

#[test]
fn explicit_output() {
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    write!(input_file.as_file_mut(), "solve satisfy;").unwrap();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let mut output_file = PathBuf::from(temp_dir.path());
    output_file.push("path/to/file.json");
    let output = pintc_command(&format!(
        "{} -o {}",
        input_file.path().to_str().unwrap(),
        output_file.to_str().unwrap(),
    ));

    assert!(output_file.exists());
    let _ = fs::remove_file(output_file);

    check(&output.stderr, expect_test::expect![""]);
    check(&output.stdout, expect_test::expect![""]);
}

#[cfg(feature = "solver-scip")]
#[test]
fn solve() {
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    write!(input_file.as_file_mut(), "var x: int = 5; solve satisfy;").unwrap();

    let output = pintc_command(&format!("{} --solve", input_file.path().to_str().unwrap(),));

    // No output file generated since this is a --solve flow
    assert!(!input_file.path().with_extension("json").exists());

    println!("{}", output.stdout);
    check(&output.stderr, expect_test::expect![""]);

    // `check(..)` is not working well here for some reason, so I'm using `assert_eq` instead.
    assert_eq!(
        &output.stdout,
        r#"   Problem is satisfiable
    Solution:
     ::x: 5
"#
    );
}
