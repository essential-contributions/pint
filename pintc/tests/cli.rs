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
            contract <filename>       960C48AD596D02C6E1E5464FD76D48E7F1E5BBA86C86E2811D866B4720F8D7CD
                     └── <filename>::test 25CD8A3AE1357171AA5CC657A8F373ADBEAA959DC107907213F60D81DBACDD9B"#]],
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
            contract <filename>       960C48AD596D02C6E1E5464FD76D48E7F1E5BBA86C86E2811D866B4720F8D7CD
                     └── <filename>::test 25CD8A3AE1357171AA5CC657A8F373ADBEAA959DC107907213F60D81DBACDD9B"#]],
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
            contract <filename>       57BA3E410AD0243DA2B0B8A8249DA6EC20F5D5D843C811A37092448DA01A27C7
                     └── <filename>::test 25CD8A3AE1357171AA5CC657A8F373ADBEAA959DC107907213F60D81DBACDD9B"#]],
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
            contract <filename>       AA3081FDBD6AADBFA6498D6CC6888F0AD948DAF9347BACEC9ABAE08B638C2E3D
                     └── <filename>::test 25CD8A3AE1357171AA5CC657A8F373ADBEAA959DC107907213F60D81DBACDD9B"#]],
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
            contract <filename>           44AB9A67290D6FE534F9F2332CB9D73B8F40F741B7A5545F8F115BE5A16509DC
                     ├── <filename>::test     25CD8A3AE1357171AA5CC657A8F373ADBEAA959DC107907213F60D81DBACDD9B
                     ├── <filename>::check    548AD409019586303D8CBFA5D2AE1A4526393E45354F01DB827EC8B83B3E4356
                     ├── <filename>::verify   0F39815C68106D04F66D62AAE15C9CB7AB917182075D8A183970CDB6397ED227
                     ├── <filename>::validate C89890A919484591D418986326F4438B2D4B6F9484DEE05907E5C8190939A50D
                     ├── <filename>::assess   0798D60493412894631092FAF4B6D0258833FFEF357593DB591912A37C527051
                     ├── <filename>::examine  77A0B788C3B5F29F1E3F22205A9AD0038B83C4C8D42295C7DEFEC9FC52D9205B
                     ├── <filename>::inspect  A428BDD0B8B3B78E3094226F54D738895C409E3DFD68631238ACC1F4CDC66F9F
                     └── <filename>::evaluate 7DAA4DEF41350FB6E921FA062CCD3C74232AFA299E0738119BB2C4C798ECF747"#]],
    );
}
