#!/usr/bin/env python3
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
# Generate synthetic abixml test data for the quadratic symbol
# comparison regression test.
#
# v0 has 10 function symbols, v1 has 20000.  The diff produces ~19990
# added unreferenced function symbols, exercising both the hash-based
# set-difference in corpus diff computation and the hash-based alias
# lookup in show_linkage_name_and_aliases.

import os

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
OUT_DIR = os.path.join(SCRIPT_DIR, "data", "test-abidiff-exit")

V0_COUNT = 10
V1_COUNT = 20000


def write_abi(path, num_symbols):
    with open(path, "w") as f:
        f.write("<abi-corpus path='test.o' architecture='elf-amd-x86_64'>\n")
        f.write("  <elf-function-symbols>\n")
        for i in range(num_symbols):
            f.write(
                f"    <elf-symbol name='fn_{i:05d}' type='func-type'"
                f" binding='global-binding' visibility='default-visibility'"
                f" is-defined='yes'/>\n"
            )
        f.write("  </elf-function-symbols>\n")
        f.write("</abi-corpus>\n")


if __name__ == "__main__":
    write_abi(
        os.path.join(OUT_DIR, "test-many-unreferenced-syms-v0.abi"),
        V0_COUNT,
    )
    write_abi(
        os.path.join(OUT_DIR, "test-many-unreferenced-syms-v1.abi"),
        V1_COUNT,
    )
    print(f"Generated v0 ({V0_COUNT} symbols) and v1 ({V1_COUNT} symbols)")
