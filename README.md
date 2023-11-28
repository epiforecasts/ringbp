
# *ringbp*: Simulate infectious disease transmission with contact tracing

<!-- badges: start -->

![GitHub R package version](https://img.shields.io/github/r-package/v/)
[![R-CMD-check](https://github.com//actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com//actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh//branch/main/graph/badge.svg)](https://app.codecov.io/gh/?branch=main)
![GitHub contributors](https://img.shields.io/github/contributors/)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

*ringbp* is an R package that provides methods to simulate infectious
disease transmission in the presence of contact tracing. It was
initially developed to support a paper written in early 2020 to assess
the [feasibility of controlling
COVID-19](https://github.com/cmmid/ringbp.ncov). For more details on the
methods implemented here, see the associated
[paper](https://doi.org/10.1016/S2214-109X(20)30074-7).

## Installation

The current development version of *ringbp* can be installed from
[GitHub](https://github.com/) using the `pak` package.

``` r
if(!require("pak")) install.packages("pak")
pak::pak("")
```

## Quick start

The main functionality of the package is in the `scenario_sim()`
function. Here is an example for running 10 simulations of a given
scenario:

``` r
library("ringbp")
library("ggplot2")

res <- scenario_sim(
  n.sim = 10, ## 10 simulations
  num.initial.cases = 1, ## one initial case in each of the simulations
  prop.asym = 0, ## no asymptomatic infections
  prop.ascertain = 0.2, ## 20% probability of ascertainment by contact tracing
  cap_cases = 4500, ## don't simulate beyond 4500 infections
  cap_max_days = 350, ## don't simulate beyond 350 days
  r0isolated = 0, ## isolated individuals have R0 of 0
  r0community = 2.5, ## non-isolated individuals have R0 of 2.5
  disp.com = 0.16, ## dispersion parameter in the community
  disp.iso = 1, ## dispersion  parameter of those isolated
  delay_shape = 1.651524, ## shape parameter of time from onset to isolation
  delay_scale = 4.287786, ## scale parameter of time from onset to isolation
  k = 0, ## skew of generation interval to be beyond onset of symptoms
  quarantine = FALSE ## whether quarantine is in effect
)

# Plot of weekly cases
ggplot(data=res, ggplot2::aes(x = week, y = cumulative, col = as.factor(sim))) +
  geom_line(show.legend = FALSE, alpha = 0.3) +
  scale_y_continuous(name = "Number of cases") + 
  theme_bw()

## estimate extinction probability
extinct_prob(res, cap_cases = 4500)
```
