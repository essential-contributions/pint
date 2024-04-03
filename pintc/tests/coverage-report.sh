#!/usr/bin/env bash

# -e: exit on any failure, -u: fail on undefined references, -o pipefail: fail if any part of
# pipeline fails.
set -euo pipefail

# Check for installed utilities.
if [[ -z "$(type -p cargo-profdata)" ]] ; then
    echo 'Error: cannot find `cargo profdata` utility.'
    echo 'Please install the `llvm-tools` package via:'
    echo '    $ rustup component add llvm-tools'
    echo '    $ cargo install cargo-binutils'
    exit 1
fi
if [[ -z "$(type -p rustfilt)" ]] ; then
    echo 'Error: cannot find `rustfilt` utility.'
    echo 'Please install the `rustfilt` package via:'
    echo '    $ cargo install rustfilt'
    exit 1
fi
if [[ -z "$(type -p jq)" ]] ; then
    echo 'Error: cannot find `jq` utility.'
    echo 'Please install the `jq` package via:'
    echo '    $ brew install jq # If on a Mac, or'
    echo '    $ apt install jq  # If on Linux, probably.  You work it out.'
    exit 1
fi

show_summary="yes"
while getopts 's' OPT ; do
    case ${OPT} in
        s)
            show_summary="no"
            shift
            ;;
        \?)
            echo 'use: $0 [-s [func-name-regex]]'
            echo '  where:'
            echo '    no args will report a summary of all source files,'
            echo '    -s will show a summary of source level coverage for all functions,'
            echo '    and "func-name-regex" restricts the summary to matching functions.'
            exit
            ;;
    esac
done

# Create a working directory for temp files.
working_dir=$(mktemp --tmpdir -d "pintc_test_cov_XXXX")

# Function for cleaning up which will run regardless of success or failure.
cleanup() {
    # Remove the working directory and its temp files.
    rm -r ${working_dir}
}
trap cleanup EXIT

# Run the test suite with profiling flag set.  This dumps a bunch of .profraw files in the CWD.
RUSTFLAGS="-C instrument-coverage" \
    LLVM_PROFILE_FILE="${working_dir}/%p_%m.profraw" \
    cargo test --tests

# Merge all the .profraw files into a single .profdata file.
profdata_file="${working_dir}/pintc_tests.profdata"
cargo profdata -- merge -sparse ${working_dir}/*.profraw -o ${profdata_file}

# Get the paths to the test binaries by parsing JSON output from a dry run.
object_list=$( \
    RUSTFLAGS="-C instrument-coverage" \
    cargo test --tests --no-run --message-format=json | \
    jq -r 'select(.profile.test == true) | .filenames[] | "--object \(.)"' | \
    grep -v 'dSYM' \
)

if [[ ${show_summary} == "yes" ]] ; then
    # Generate the report.
    report_file=$(date "+pintc_test_coverage_summary_%y%m%d-%H%M.txt")
    cargo cov -- report \
        > ${report_file} \
        --use-color \
        --ignore-filename-regex='/.cargo/registry|/rustc' \
        --instr-profile=${profdata_file} \
        --summary-only \
        ${object_list}

    echo
    echo "====================================================================="
    echo "  Summary saved to file ${report_file}"
    echo "====================================================================="
    echo
else
    # Generate a summary.
    func_regex_opt=""
    if [[ ${#} != 0 ]] ; then
        # Restrict to matching a function name by regex.
        func_regex_opt="--name-regex=${1}"
    fi

    cargo cov -- show \
        --Xdemangler=rustfilt \
        --instr-profile=${profdata_file} \
        --show-line-counts-or-regions \
        --ignore-filename-regex='/.cargo/registry|/rustc' \
        ${func_regex_opt} \
        ${object_list}
fi
