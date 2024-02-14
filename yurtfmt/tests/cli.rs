use std::{fs, io::Write};

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
fn yurtfmt_command(args: &str) -> Output {
    let output = test_bin::get_test_bin("yurtfmt")
        .args(args.split_ascii_whitespace().collect::<Vec<_>>())
        .output()
        .expect("failed to start yurtfmt");

    Output {
        stdout: remove_ansi_codes(&String::from_utf8_lossy(&output.stdout)),
        stderr: remove_ansi_codes(&String::from_utf8_lossy(&output.stderr)),
    }
}

#[test]
fn err() {
    // Missing input_file file
    check(
        &yurtfmt_command("").stderr,
        expect_test::expect![[r#"
            error: the following required arguments were not provided:
              <FILEPATHS>...

            Usage: yurtfmt <FILEPATHS>...

            For more information, try '--help'.
        "#]],
    );

    // input_file file not found
    check(
        &yurtfmt_command("missing.yrt").stderr,
        expect_test::expect![[r#"
            Error: No such file or directory (os error 2)
        "#]],
    );
}

#[test]
fn compile_errors() {
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    write!(input_file.as_file_mut(), "let t").unwrap();

    let output = yurtfmt_command(input_file.path().to_str().unwrap());

    // Check error output
    check(
        &output
            .stderr
            .replace(input_file.path().to_str().unwrap(), "filepath"),
        expect_test::expect![[r#"
            Error: 
               ╭─[filepath:1:6]
               │
             1 │ let t
               │      │ 
               │      ╰─ Error formatting starting at location 5 and ending at location 5
            ───╯
            Error: could not format "filepath" due to previous error
        "#]],
    );

    check(&output.stdout, expect_test::expect![""]);
}

#[test]
fn single_file() {
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    let code = r#"let t = 
        5; 
    solve    satisfy  ;"#;
    write!(input_file.as_file_mut(), "{code}").unwrap();

    let output = yurtfmt_command(input_file.path().to_str().unwrap());

    check(&output.stderr, expect_test::expect![""]);
    check(&output.stdout, expect_test::expect![""]);
    check(
        &fs::read_to_string(input_file.path()).expect("temp input file must exist"),
        expect_test::expect![[r#"
        let t = 5;
        solve satisfy;
    "#]],
    );
}

#[test]
fn multiple_file() {
    let mut input_file_1 = tempfile::NamedTempFile::new().unwrap();
    let code = r#"let t = 
        5; 
    solve    satisfy  ;"#;
    write!(input_file_1.as_file_mut(), "{code}").unwrap();

    let mut input_file_2 = tempfile::NamedTempFile::new().unwrap();
    let code = r#"let x = 5;
    constraint x 
    > 3 +
            9;

    solve    satisfy  ;"#;
    write!(input_file_2.as_file_mut(), "{code}").unwrap();

    let output = yurtfmt_command(&format!(
        "{} {}",
        input_file_1.path().to_str().unwrap(),
        input_file_2.path().to_str().unwrap(),
    ));

    check(&output.stderr, expect_test::expect![""]);
    check(&output.stdout, expect_test::expect![""]);
    check(
        &fs::read_to_string(input_file_1.path()).expect("temp input file must exist"),
        expect_test::expect![[r#"
            let t = 5;
            solve satisfy;
        "#]],
    );
    check(
        &fs::read_to_string(input_file_2.path()).expect("temp input file must exist"),
        expect_test::expect![[r#"
            let x = 5;
            constraint x > 3 + 9;
            solve satisfy;
        "#]],
    );
}
