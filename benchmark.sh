#!/bin/sh
time ./cxc quadratic.cxc < quadratic_benchmark_input
time python quadratic_benchmark.py < quadratic_benchmark_input
