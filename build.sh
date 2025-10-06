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

CPU_TARGET=${CPU_TARGET:-rp2350}

cpu_lower=$(printf '%s' "${CPU_TARGET}" | tr 'A-Z' 'a-z')

case "${cpu_lower}" in
  rp2040)
    MCU_FLAG=-mcpu=cortex-m0plus
    ;;
  rp2350)
    MCU_FLAG=-mcpu=cortex-m33
    ;;
  *)
    echo "error: unknown CPU_TARGET '${CPU_TARGET}' (use rp2040 or rp2350)" >&2
    exit 1
    ;;
esac

ARM_CFLAGS=(
  -Os
  "${MCU_FLAG}"
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
