#!/usr/bin/env bash

# Formats a lalrpop file. Overwrites the input file.

set -euo pipefail

if [[ "${#}" == 0 ]] ; then
    echo 'Please specify LALRPOP file to format.'
    exit 1
fi

# Create a temporary Rust file.
tmp_file=$(mktemp -t 'popfmt_XXX.rs')

# Comment out everything which isn't Rust and put it in a `mod { fn() { ... }, ... }` block.
awk < ${1} > ${tmp_file} '
BEGIN {
    print("mod format_me_XXX {")
}
END {
    print("} // format_me_XXX")
}
/{/ {
    if (inside) {
        s = $0 ; gsub(/[^{]+/, "", s)
        nest += length(s)
    }
}
/}/ {
    if (inside) {
        s = $0 ; gsub(/[^}]+/, "", s)
        nest -= length(s)
        if (nest == 0) {
            print("} // format_me_XXX")
            inside = 0
        }
    }
}
{
    if (!inside) {
        print("//XXX", $0)
    } else {
        print($0)
    }
}
/ <.*> .*=>\?? {$/ {
    print("fn format_me_XXX() {")
    inside = 1
    nest = 1
}
'

# Format the temporary Rust file.
rustfmt ${tmp_file}

# Remove all the comment markers, restoring the non-Rust parts.
sed < ${tmp_file} -E -e 's,^ *//XXX ?,,' | grep -v format_me_XXX > "${1}"

# Remove the temporary file.
rm -f ${tmp_file}
