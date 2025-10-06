#!/usr/bin/env bash
set -euo pipefail

if ! command -v arm-none-eabi-gcc >/dev/null 2>&1; then
  echo "error: arm-none-eabi-gcc not found. Install the ARM GNU toolchain or add it to PATH." >&2
  exit 1
fi
if ! command -v arm-none-eabi-objcopy >/dev/null 2>&1; then
  echo "error: arm-none-eabi-objcopy not found. Install the ARM GNU toolchain or add it to PATH." >&2
  exit 1
fi

SRC=${1:-pow_int_cf.c}
BASENAME=$(basename "${SRC}" .c)
OBJ="${BASENAME}.elf"
BIN="${BASENAME}.bin"
HEX="${BASENAME}.hex"

ARM_CFLAGS=(
  -Os
  -mcpu=cortex-m33
  -mthumb
  -fno-function-sections
  -fno-data-sections
  -nostdlib
  -nostartfiles
  -I../PicoMiteAllVersions
)

arm-none-eabi-gcc "${ARM_CFLAGS[@]}" -c "${SRC}" -o "${OBJ}"
arm-none-eabi-objcopy -O binary "${OBJ}" "${BIN}"
xxd -ps -c16 "${BIN}" > "${HEX}"

cat <<MSG
Built CFunction payloads:
  ELF    : ${OBJ}
  Binary : ${BIN}
  Hex    : ${HEX} (16-byte chunks, handy for pasting into AMRCFGEN)

To generate the BASIC CSUB block, run (adjust the signature to match the routine), e.g.:
  ./emit_cfunction_block.sh ${BIN} "pow_int integer, integer, integer"

If you prefer using AMRCFGEN, point it at ${OBJ} (the relocatable ELF).
MSG
