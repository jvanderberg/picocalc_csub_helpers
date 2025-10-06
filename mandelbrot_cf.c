#include "../PicoMiteAllVersions/PicoCFunctions.h"

long long mandelbrot_cf(void *arg0, void *arg1,
                        void *arg2, void *arg3, void *arg4,
                        void *arg5, void *arg6, void *arg7,
                        void *arg8, void *arg9)
{
    long long *out = (long long *)arg0;      // result array (integer())
    long long start_x = *(long long *)arg1;  // starting real coordinate (scaled integer)
    long long start_y = *(long long *)arg2;  // starting imaginary coordinate (scaled integer)
    long long step = *(long long *)arg3;     // increment applied to real part each point (scaled)
    long long n_points = *(long long *)arg4; // number of samples
    long long max_iter = *(long long *)arg5; // maximum iterations per point

    if (n_points <= 0 || max_iter <= 0)
    {
        return 0;
    }

    for (long long idx = 0; idx < n_points; idx++)
    {
        long long cx = start_x + idx * step;
        long long cy = start_y;
        long long zx = 0;
        long long zy = 0;
        long long zx2 = 0;
        long long zy2 = 0;
        long long threshold = 4LL << 28; // 4 in Q3.28
        long long iter = 0;

        while (iter < max_iter)
        {
            long long sum = zx2 + zy2;
            if (sum > threshold)
                break;

            long long zx_twice = zx << 1;
            zy = ((zx_twice * zy) >> 28) + cy;
            zx = ((zx2 - zy2) + cx);
            zx2 = (zx * zx) >> 28;
            zy2 = (zy * zy) >> 28;
            iter++;
        }
        out[idx] = iter;
    }
    // out[0] = 42;
    return 0;
}
