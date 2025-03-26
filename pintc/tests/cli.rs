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
               ╭─[ filepath:1:19 ]
               │
             1 │ predicate test(t: {}, a: int) { constraint t == {}; constraint a == a[]; }
               │                   ─┬  
               │                    ╰── empty tuple type found
            ───╯
            Error: empty tuple expressions are not allowed
               ╭─[ filepath:1:49 ]
               │
             1 │ predicate test(t: {}, a: int) { constraint t == {}; constraint a == a[]; }
               │                                                 ─┬  
               │                                                  ╰── empty tuple expression found
            ───╯
            Error: missing array or map index
               ╭─[ filepath:1:69 ]
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
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    write!(input_file.as_file_mut(), "predicate test() {{}}").unwrap();

    let output = pintc_command(input_file.path().to_str().unwrap());

    assert!(
        std::env::var("PINTC_NO_OUTPUT").is_ok()
            || input_file.path().with_extension("json").exists()
    );
    let _ = fs::remove_file(input_file.path().with_extension("json"));

    let stdout = output.stdout.replace(
        input_file.path().file_stem().unwrap().to_str().unwrap(),
        "<filename>",
    );
    let stdout = stdout.trim();

    check(&output.stderr, expect_test::expect![""]);
    check(
        stdout,
        expect_test::expect![[r#"
            contract <filename>       37EC28A102C79540AF05182F1B3C0113FFC8920A1075E2C9155AE2C9FF0213BE
                     └── <filename>::test DF3F619804A92FDB4057192DC43DD748EA778ADC52BC498CE80524C014B81119"#]],
    );
}

#[test]
fn explicit_output() {
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    write!(input_file.as_file_mut(), "predicate test() {{}}").unwrap();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let mut output_file = PathBuf::from(temp_dir.path());
    output_file.push("path/to/file.json");
    let output = pintc_command(&format!(
        "{} -o {}",
        input_file.path().to_str().unwrap(),
        output_file.to_str().unwrap(),
    ));

    assert!(std::env::var("PINTC_NO_OUTPUT").is_ok() || output_file.exists());
    let _ = fs::remove_file(output_file);

    let stdout = output.stdout.replace(
        input_file.path().file_stem().unwrap().to_str().unwrap(),
        "<filename>",
    );
    let stdout = stdout.trim();

    check(&output.stderr, expect_test::expect![""]);
    check(
        stdout,
        expect_test::expect![[r#"
            contract <filename>       37EC28A102C79540AF05182F1B3C0113FFC8920A1075E2C9155AE2C9FF0213BE
                     └── <filename>::test DF3F619804A92FDB4057192DC43DD748EA778ADC52BC498CE80524C014B81119"#]],
    );
}

#[test]
fn explicit_salt() {
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    write!(input_file.as_file_mut(), "predicate test() {{}}").unwrap();

    // Salt has 64 digits
    let output = pintc_command(&format!(
        "{} --salt 0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF",
        input_file.path().to_str().unwrap(),
    ));

    let stdout = output.stdout.replace(
        input_file.path().file_stem().unwrap().to_str().unwrap(),
        "<filename>",
    );
    let stdout = stdout.trim();

    check(&output.stderr, expect_test::expect![""]);
    check(
        stdout,
        expect_test::expect![[r#"
            contract <filename>       46D7681856634E6CEB8E7B9DCE72B583B2E37D8DD4B75CFFFE7A2BE1908D3034
                     └── <filename>::test DF3F619804A92FDB4057192DC43DD748EA778ADC52BC498CE80524C014B81119"#]],
    );

    // Salt has less than 64 digits
    let output = pintc_command(&format!(
        "{} --salt 123456789ABCDEF",
        input_file.path().to_str().unwrap(),
    ));

    let stdout = output.stdout.replace(
        input_file.path().file_stem().unwrap().to_str().unwrap(),
        "<filename>",
    );
    let stdout = stdout.trim();

    check(&output.stderr, expect_test::expect![""]);
    check(
        stdout,
        expect_test::expect![[r#"
            contract <filename>       A17DE0E7C1379B78EFCBAF47277C1D90D480065063F428B98EE533638B9E1415
                     └── <filename>::test DF3F619804A92FDB4057192DC43DD748EA778ADC52BC498CE80524C014B81119"#]],
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

#[test]
fn multiple_predicate_output() {
    let mut input_file = tempfile::NamedTempFile::new().unwrap();
    write!(
        input_file.as_file_mut(),
        "predicate test() {{}}
        predicate check(x : int) {{constraint x == 0;}}
        predicate verify(x : int) {{constraint x == 1;}}
        predicate validate(x : int) {{constraint x == 2;}}
        predicate assess(x : int) {{constraint x == 3;}}
        predicate examine(x : int) {{constraint x == 4;}}
        predicate inspect(x : int) {{constraint x == 5;}}
        predicate evaluate(x : int) {{constraint x == 6;}}"
    )
    .unwrap();

    let output = pintc_command(input_file.path().to_str().unwrap());

    assert!(
        std::env::var("PINTC_NO_OUTPUT").is_ok()
            || input_file.path().with_extension("json").exists()
    );
    let _ = fs::remove_file(input_file.path().with_extension("json"));

    let stdout = output.stdout.replace(
        input_file.path().file_stem().unwrap().to_str().unwrap(),
        "<filename>",
    );
    let stdout = stdout.trim();

    check(&output.stderr, expect_test::expect![""]);
    check(
        stdout,
        expect_test::expect![[r#"
            contract <filename>           8978A1F25C643931E1C957D7ACA3E063580084305E8B029C78C750ABD2C4DDFE
                     ├── <filename>::test     DF3F619804A92FDB4057192DC43DD748EA778ADC52BC498CE80524C014B81119
                     ├── <filename>::check    6B0442260EE3080F9A6D53E4A24A1C7D7820411752425D150EC6668ECACC789B
                     ├── <filename>::verify   2E433887AF721D4F3F5F4A7650C1FBF4A1776DC278CB7F226AFEE692995AC390
                     ├── <filename>::validate 7D1C819AA673D086A17B950EA079CCE104ADF149039F43F07AF3331A4CC67A17
                     ├── <filename>::assess   ECAD8CAAF128F83D22A5D57647888A49BADF71CA93CB4EA64F31D43589170550
                     ├── <filename>::examine  7116DA61ED695C689D81DF2C99D0F3857702B8C7F82C1856296969576BA8F236
                     ├── <filename>::inspect  EEB22071C9AA4A19EEC0E97CF05888B1496BBA0E2DBB2E52460EAF68C9A4BA03
                     └── <filename>::evaluate 0E0E8BD8660B7BFBA846D460CA159775A94503219A101FFC576EFE064D26BF43"#]],
    );
}
