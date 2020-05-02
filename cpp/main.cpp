#include <iostream>

extern "C" {
    double mandel(double, double, double, double);

    // library functions
    double putchard(double x) {
        fputc((char) x, stderr);
        return 0.0;
    }
    double printd(double x) {
        fprintf(stderr, "%f\n", x);
        return 0;
    }
}

int main() {
    std::cout << "Mandelbrot:\n" << mandel(-2.3, -1.3, 0.05, 0.07) << std::endl;
}