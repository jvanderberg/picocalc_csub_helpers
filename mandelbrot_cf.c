#include "../PicoMiteAllVersions/PicoCFunctions.h"

long long mandelbrot_cf(void *arg0, void *arg1,
                        void *arg2, void *arg3, void *arg4,
                        void *arg5, void *arg6, void *arg7,
                        void *arg8, void *arg9)
{
    long long *out = (long long *)arg0;      // result array (INTEGER())
    long long start_x = *(long long *)arg1;  // Q2.30
    long long start_y = *(long long *)arg2;  // Q2.30
    long long step = *(long long *)arg3;     // Q2.30
    long long n_points = *(long long *)arg4; // number of samples
    long long max_iter = *(long long *)arg5; // max iterations

    if (n_points <= 0 || max_iter <= 0)
        return 0;

    const long long THRESH = 4LL << 30; // |z|^2 > 4  (Q2.30)

    for (long long idx = 0; idx < n_points; idx++)
    {
        long long cx = start_x + idx * step;
        long long cy = start_y;
        long long zx = 0, zy = 0;
        long long zx2 = 0, zy2 = 0;
        long long iter = 0;

        while (iter < max_iter)
        {
            // escape test on previous z
            if (zx2 + zy2 > THRESH)
                break;

            // z = z^2 + c   (single rounding per component, 64-bit only)
            long long xx = zx * zx; // up to ~4.61e18 before shift
            long long yy = zy * zy;
            long long xy = zx * zy;

            // real:  (zx*zx - zy*zy) >> 30  + cx
            long long zx_new = ((xx - yy) >> 30) + cx;

            // imag:  (2*zx*zy) >> 30  == (zx*zy) >> 29  (avoids an extra *2 overflow)
            long long zy_new = (xy >> 29) + cy;

            zx = zx_new;
            zy = zy_new;

            // recompute squares for next loop's escape check
            zx2 = (zx * zx) >> 30;
            zy2 = (zy * zy) >> 30;

            iter++;
        }
        out[idx] = iter;
    }
    return 0;
}
