#!/usr/bin/env bash
set -euo pipefail
export CPU_TARGET=rp2350
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "${SCRIPT_DIR}"

SIGNATURE="pow_int integer, integer, integer"

./build.sh pow_int_cf.c "$@"

BLOCK=$(./emit_cfunction_block.sh pow_int_cf.bin "${SIGNATURE}")

python3 - "$BLOCK" <<'PY'
import sys
import re
from pathlib import Path

block = sys.argv[1]
block = block.rstrip() + "\n"
pattern = re.compile(r"^(csub\s+pow_int\b.*?end\s+csub)\s*$", re.IGNORECASE | re.MULTILINE | re.DOTALL)

path = Path("test_pow.bas")
text = path.read_text()
new_text, count = pattern.subn(block, text, count=1)
if count == 0:
    raise SystemExit("error: could not find CSUB pow_int block in test_pow.bas")
path.write_text(new_text)
print("updated test_pow.bas")
PY
