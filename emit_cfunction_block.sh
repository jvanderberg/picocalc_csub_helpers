#!/usr/bin/env bash
set -euo pipefail

usage() {
  echo "Usage: $0 <binary> <signature> [keyword]" >&2
  echo "  Example: $0 pow_int_cf.bin \"pow_int% (base%, exp%)\"" >&2
  exit 1
}

BIN=${1:-}
SIG=${2:-}
[[ -n ${BIN} && -n ${SIG} ]] || usage
[[ -f ${BIN} ]] || { echo "error: binary '${BIN}' not found" >&2; exit 1; }

KEYWORD=${3:-CSUB}

printf '%s %s\n' "${KEYWORD}" "${SIG}"
printf ' 00000000\n'

python3 - "$BIN" <<'PY'
import sys
path = sys.argv[1]
data = open(path, 'rb').read()
if len(data) % 4:
    data += b'\x00' * (4 - len(data) % 4)
for i in range(0, len(data), 16):
    chunk = data[i:i+16]
    words = [chunk[j:j+4][::-1].hex().upper() for j in range(0, len(chunk), 4)]
    print(' ' + ' '.join(words))
PY

echo 'End CSUB'
