# project-cpath

### R package for Bayesian modelling using stan with GPU support

This is a R package that uses Stan (cmdstan) for the back-end estimation. It is tailored specifically to be used on [C-Path](https://c-path.org/) disease data. The modifications to stan include explicit derivation of the model derivative implemented in C++ and OpenCL to achieve maximum performance. 

### Installation

Make sure that you have R package **devtools** installed.
You can install this package by executing the following command in R:

```r
install_github("bstatcomp/project-cpath")
```

If you have a GPU available in your system and the drivers properly installed, the package will automatically detect the GPU and try to use to maximum effect.

The package currently supports Window and Ubuntu OS.
