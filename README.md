
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phyopt

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ricardo-semiao/phyopt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ricardo-semiao/phyopt/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/phyopt)](https://CRAN.R-project.org/package=phyopt)
<!-- badges: end -->

This packages implements a two-step, population-based, hybrid
optimization algorithm, for solving problems of the form:

$$
  \max_{x, \tilde{x}} f(x, \tilde{x}) ~~s.t.~~ g(x, \tilde{x}) \geq 0,~ \tilde{g}(\tilde{x}) \geq 0\\
  x \in X,~ \tilde{x} \in \tilde{X}
$$

The paper fully describing the motivation, theoretical framework, and
method design will be made public soon.

For now, see the viggnette `example.Rmd`, which contains a simple
example but a thorough explanation of how the package works.
Additionally, see the documentation for the `optimize_phy()` function,
which is the main function of the package

## Installation

The package is not public yet.
