#!/usr/bin/env bash
set -euo pipefail
export CPU_TARGET=rp2350
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "${SCRIPT_DIR}"

SIGNATURE="mandelbrot integer, integer, integer, integer, integer, integer"

./build.sh mandelbrot_cf.c "$@"

BLOCK=$(./emit_cfunction_block.sh mandelbrot_cf.bin "${SIGNATURE}")

python3 - "$BLOCK" <<'PY'
import sys
import re
from pathlib import Path

block = sys.argv[1]
block = block.rstrip() + "\n"
pattern = re.compile(r"^(csub\s+mandelbrot\b.*?end\s+csub)\s*$", re.IGNORECASE | re.MULTILINE | re.DOTALL)

def replace_block(path):
    text = Path(path).read_text()
    new_text, count = pattern.subn(block, text, count=1)
    if count == 0:
        raise SystemExit(f"error: could not find CSUB mandelbrot block in {path}")
    Path(path).write_text(new_text)

for target in ("mand.bas", "test_mandel.bas"):
    replace_block(Path(".") / target)
    print(f"updated {target}")
PY
