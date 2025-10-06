#include "../PicoMiteAllVersions/PicoCFunctions.h"

long long pow_int_cf(void *arg0, void *arg1,
                     void *arg2, void *arg3, void *arg4,
                     void *arg5, void *arg6, void *arg7,
                     void *arg8, void *arg9)
{
    long long *out = (long long *)arg0;
    long long base = *(long long *)arg1;
    long long exp = *(long long *)arg2;

    if (exp < 0)
    {
        *out = 0; // negative exponents unsupported for integer result
        return 0;
    }

    long long result = 1;
    long long b = base;
    long long e = exp;

    while (e > 0)
    {
        if (e & 1)
            result *= b;
        e >>= 1;
        if (e)
            b *= b;
    }

    *out = result;
    return 0;
}
