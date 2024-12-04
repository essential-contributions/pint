use std::{fs, io::Write, path::PathBuf};

use tempfile::Builder;

fn remove_ansi_codes(input: &str) -> String {
    let re = regex::Regex::new(r"\x1B\[([0-9;]*[A-Za-z])").unwrap();
    re.replace_all(input, "").to_string()
}

#[cfg(test)]
fn check(actual: &str, expect: expect_test::Expect) {
    expect.assert_eq(actual);
}

#[cfg(test)]
fn check_file(actual: &str, expect: expect_test::ExpectFile) {
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
    let input_file = tempfile::NamedTempFile::new().unwrap();

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
    let code = r#"predicate test(t: {}, a: int) { constraint t == {}; constraint a == a[]; }"#;
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
               ╭─[filepath:1:19]
               │
             1 │ predicate test(t: {}, a: int) { constraint t == {}; constraint a == a[]; }
               │                   ─┬  
               │                    ╰── empty tuple type found
            ───╯
            Error: empty tuple expressions are not allowed
               ╭─[filepath:1:49]
               │
             1 │ predicate test(t: {}, a: int) { constraint t == {}; constraint a == a[]; }
               │                                                 ─┬  
               │                                                  ╰── empty tuple expression found
            ───╯
            Error: missing array or map index
               ╭─[filepath:1:69]
               │
             1 │ predicate test(t: {}, a: int) { constraint t == {}; constraint a == a[]; }
               │                                                                     ─┬─  
               │                                                                      ╰─── missing array or map element index
            ───╯
            Error: could not compile `filepath` due to 3 previous errors
        "#]],
    );

    check(&output.stdout, expect_test::expect![""]);
}

#[test]
fn default_output() {
    let mut input_file = Builder::new()
        .prefix(".tmpOm3IwU")
        .rand_bytes(0)
        .tempfile()
        .unwrap();
    write!(input_file.as_file_mut(), "predicate test() {{}}").unwrap();

    let output = pintc_command(input_file.path().to_str().unwrap());

    assert!(input_file.path().with_extension("json").exists());
    let _ = fs::remove_file(input_file.path().with_extension("json"));

    let mut expected_output_file = tempfile::NamedTempFile::new().unwrap();
    write!(expected_output_file.as_file_mut(), 
"    contract .tmpOm3IwU       EB87FCE275A9AB10996D212F39221A56B90E01C37FA9D16EE04A3FE8E17DEED9
         └── .tmpOm3IwU::test BA6595C5C75346E6C82BED0CE770D0758ADD1712163FCE45E38E5E8EAC6AA153
").unwrap();

    check(&output.stderr, expect_test::expect![""]);
    check_file(
        &output.stdout,
        expect_test::expect_file![expected_output_file.path()],
    );
}

#[test]
fn explicit_output() {
    let mut input_file = Builder::new()
        .prefix(".tmpulGrH8")
        .rand_bytes(0)
        .tempfile()
        .unwrap();
    write!(input_file.as_file_mut(), "predicate test() {{}}").unwrap();

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

    let mut expected_output_file = tempfile::NamedTempFile::new().unwrap();
    write!(expected_output_file.as_file_mut(), 
"    contract .tmpulGrH8       EB87FCE275A9AB10996D212F39221A56B90E01C37FA9D16EE04A3FE8E17DEED9
         └── .tmpulGrH8::test BA6595C5C75346E6C82BED0CE770D0758ADD1712163FCE45E38E5E8EAC6AA153
").unwrap();

    check(&output.stderr, expect_test::expect![""]);
    check_file(
        &output.stdout,
        expect_test::expect_file![expected_output_file.path()],
    );
}

#[test]
fn explicit_salt() {
    let mut input_file = Builder::new()
        .prefix(".dmXkll")
        .rand_bytes(0)
        .tempfile()
        .unwrap();
    write!(input_file.as_file_mut(), "predicate test() {{}}").unwrap();

    // Salt has 64 digits
    let output = pintc_command(&format!(
        "{} --salt 0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF",
        input_file.path().to_str().unwrap(),
    ));

    let mut expected_output_file = tempfile::NamedTempFile::new().unwrap();
    write!(expected_output_file.as_file_mut(), 
"    contract .dmXkll       4337DBAA25DD2434C4C96F4D3EF1C57B06366875BBACB51768D8FFB01027980B
         └── .dmXkll::test BA6595C5C75346E6C82BED0CE770D0758ADD1712163FCE45E38E5E8EAC6AA153
").unwrap();

    check(&output.stderr, expect_test::expect![""]);
    check_file(
        &output.stdout,
        expect_test::expect_file![expected_output_file.path()],
    );    

    // Salt has less than 64 digits
    let output = pintc_command(&format!(
        "{} --salt 123456789ABCDEF",
        input_file.path().to_str().unwrap(),
    ));

    let mut expected_output_file = tempfile::NamedTempFile::new().unwrap();
    write!(expected_output_file.as_file_mut(), 
"    contract .dmXkll       AB92EA32F6DC2E304C5385A28A9BAB1DE90C7B441362F98E7556B3AD796D0EBA
         └── .dmXkll::test BA6595C5C75346E6C82BED0CE770D0758ADD1712163FCE45E38E5E8EAC6AA153
").unwrap();

    check(&output.stderr, expect_test::expect![""]);
    check_file(
        &output.stdout,
        expect_test::expect_file![expected_output_file.path()],
    );
}

#[test]
fn explicit_salt_err() {
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    write!(input_file.as_file_mut(), "predicate test() {{}}").unwrap();

    // Salt too long
    let output = pintc_command(&format!(
        "{} --salt 3456789ABCDEF0123456789ABCDEF0123456789ABC0123456789ABCDEF0123456789ABCDEF01234",
        input_file.path().to_str().unwrap(),
    ));
    check(
        &output.stderr,
        expect_test::expect![[r#"
            error: invalid value '3456789ABCDEF0123456789ABCDEF0123456789ABC0123456789ABCDEF0123456789ABCDEF01234' for '--salt <SALT>': Salt must be a hexadecimal number with up to 64 digts (256 bits)

            For more information, try '--help'.
        "#]],
    );
    check(&output.stdout, expect_test::expect![""]);

    // Salt illegal hex characters
    let output = pintc_command(&format!(
        "{} --salt YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",
        input_file.path().to_str().unwrap(),
    ));
    check(
        &output.stderr,
        expect_test::expect![[r#"
            error: invalid value 'YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY' for '--salt <SALT>': Salt must be a hexadecimal number with up to 64 digts (256 bits)

            For more information, try '--help'.
        "#]],
    );
    check(&output.stdout, expect_test::expect![""]);

    // Missing salt with --salt flag
    let output = pintc_command(&format!("{} --salt", input_file.path().to_str().unwrap(),));

    check(
        &output.stderr,
        expect_test::expect![[r#"
        error: a value is required for '--salt <SALT>' but none was supplied

        For more information, try '--help'.
    "#]],
    );
    check(&output.stdout, expect_test::expect![""]);
}
