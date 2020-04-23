#include <stdio.h>

/* putchard - putchar that takes a double and returns 0 */
extern double putchard(double x) {
    putchar((char) x);
    return 0.0;
}