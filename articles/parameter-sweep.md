# Scenario parameter sweep

This vignette demonstrates how to run an analysis using the {ringbp}
simulation across multiple scenarios.

The functionality included in this vignette was previously in the
`parameter_sweep()` function in {ringbp} \<= v0.1.2. This function has
been removed as it is preferred to demonstrate the functionality but
allow users the flexibility and control to run scenarios in whichever
way they choose.

``` r
library(ringbp)
library(data.table)
```

The
[`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
function is the core function to run outbreak simulations using
{ringbp}, because it allows running the
[`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md)
over multiple replicates.

## Set up scenario parameter space

The first step to construct a `data.frame` (in this example we will use
a `data.table`) with a grid of parameters, i.e. a parameter space to
explore. We use
[`expand.grid()`](https://rdrr.io/r/base/expand.grid.html) to create all
the combinations of parameters.

Parameters that are grouped are put into a nested `data.table` to make
sure they are coupled and combinations of these parameters are not
mixed. See `delay_group` below for an example of this.

``` r
# Put parameters that are grouped by disease into this data.table
scenarios <- data.table(
  expand.grid(
   delay_group = list(data.table(
     delay = c("SARS", "Wuhan"),
     onset_to_isolation = c(
       \(n) rweibull(n = n, shape = 1.651524, scale = 4.287786),
       \(n) rweibull(n = n, shape = 2.305172, scale = 9.483875)
     )
   )),
   r0_community = c(1.1, 1.5),
   r0_isolated = 0,
   disp_community = 0.16,
   disp_isolated = 1,
   prop_presymptomatic = c(0.01, 0.15),
   prop_asymptomatic = c(0, 0.1),
   prop_ascertain = seq(0, 1, 0.25),
   initial_cases = c(5, 10),
   quarantine = FALSE,
   cap_max_days = 365,
   cap_cases = 5000
 )
)
```

To unnest the `data.table` in order to sweep across the scenarios, a few
data manipulation steps are required:

``` r
list_cols <- grep("_group", colnames(scenarios), value = TRUE)
non_list_cols <- setdiff(colnames(scenarios), list_cols)
scenarios <- scenarios[, rbindlist(delay_group), by = c(non_list_cols)]
```

Lastly in setting up the parameter grid we add a column called
`scenario` which is numbered 1 to the total number of scenarios; and we
add the incubation period to the parameter grid.

``` r
scenarios[, scenario :=  1:.N]
incub <- \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
scenarios[, incubation_period := rep(list(incub), .N)]
```

## Parameter sweep across scenarios

Before we run
[`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
across each parameter set, we split the scenarios into a list. This
makes it easy to loop over the resulting list using R functions like
[`lapply()`](https://rdrr.io/r/base/lapply.html).

``` r
scenario_sims <- scenarios[, list(data = list(.SD)), by = scenario]
```

For this example we will run 3 replicates for each parameter set. The
simulation model is stochastic so by running multiple replicates for
each, we can understand the variance within scenarios. For a
scientifically robust analysis, the number of replicates should be much
higher (e.g. 100-1000).

``` r
n <- 3
res <- lapply(scenario_sims$data, \(x, n) {
  scenario_sim(
    n = n,
    initial_cases = x$initial_cases,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = x$r0_community, size = x$disp_community),
      isolated = \(n) rnbinom(n = n, mu = x$r0_isolated, size = x$disp_isolated)
    ),
    delays = delay_opts(
      incubation_period = x$incubation_period[[1]],
      onset_to_isolation = x$onset_to_isolation[[1]]
    ),
    event_probs = event_prob_opts(
      asymptomatic = x$prop_asymptomatic,
      presymptomatic_transmission = x$prop_presymptomatic,
      symptomatic_ascertained = x$prop_ascertain
    ),
    interventions = intervention_opts(quarantine = x$quarantine),
    sim = sim_opts(cap_max_days = x$cap_max_days, cap_cases = x$cap_cases)
  )
  },
  n = n
)
```

The output of this parameter sweep is a list of `data.table`s, so to
give an idea of what the output looks like we show the first 3 scenarios
(there are 160 in total). See
[`?scenario_sim`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
for more information on the contents of these `data.table`s.

``` r
head(res, 3)
#> [[1]]
#>        sim  week weekly_cases cumulative effective_r0 cases_per_gen
#>      <int> <num>        <num>      <num>        <num>        <list>
#>   1:     1     0            5          5            0             0
#>   2:     1     1            0          5            0             0
#>   3:     1     2            0          5            0             0
#>   4:     1     3            0          5            0             0
#>   5:     1     4            0          5            0             0
#>  ---                                                               
#> 155:     3    48            0          5            0             0
#> 156:     3    49            0          5            0             0
#> 157:     3    50            0          5            0             0
#> 158:     3    51            0          5            0             0
#> 159:     3    52            0          5            0             0
#> 
#> [[2]]
#>        sim  week weekly_cases cumulative effective_r0
#>      <int> <num>        <num>      <num>        <num>
#>   1:     1     0            5          5     1.394015
#>   2:     1     1           20         25     1.394015
#>   3:     1     2           45         70     1.394015
#>   4:     1     3          116        186     1.394015
#>   5:     1     4          188        374     1.394015
#>  ---                                                 
#> 155:     3    48            0          7     0.200000
#> 156:     3    49            0          7     0.200000
#> 157:     3    50            0          7     0.200000
#> 158:     3    51            0          7     0.200000
#> 159:     3    52            0          7     0.200000
#>                        cases_per_gen
#>                               <list>
#>   1:  11, 41, 70, 99,145,185,...[18]
#>   2:  11, 41, 70, 99,145,185,...[18]
#>   3:  11, 41, 70, 99,145,185,...[18]
#>   4:  11, 41, 70, 99,145,185,...[18]
#>   5:  11, 41, 70, 99,145,185,...[18]
#>  ---                                
#> 155:                             2,0
#> 156:                             2,0
#> 157:                             2,0
#> 158:                             2,0
#> 159:                             2,0
#> 
#> [[3]]
#>        sim  week weekly_cases cumulative effective_r0
#>      <int> <num>        <num>      <num>        <num>
#>   1:     1     0           13         13     1.409416
#>   2:     1     1           27         40     1.409416
#>   3:     1     2           75        115     1.409416
#>   4:     1     3          128        243     1.409416
#>   5:     1     4          168        411     1.409416
#>  ---                                                 
#> 155:     3    48            0          7     0.200000
#> 156:     3    49            0          7     0.200000
#> 157:     3    50            0          7     0.200000
#> 158:     3    51            0          7     0.200000
#> 159:     3    52            0          7     0.200000
#>                        cases_per_gen
#>                               <list>
#>   1:  13, 21, 26, 72,127,125,...[18]
#>   2:  13, 21, 26, 72,127,125,...[18]
#>   3:  13, 21, 26, 72,127,125,...[18]
#>   4:  13, 21, 26, 72,127,125,...[18]
#>   5:  13, 21, 26, 72,127,125,...[18]
#>  ---                                
#> 155:                             2,0
#> 156:                             2,0
#> 157:                             2,0
#> 158:                             2,0
#> 159:                             2,0
```

## Storing simulations with scenarios

In the previous example we constructed the parameter grid and then ran
the parameter sweep storing the simulated outbreaks in a separate
object. It can be useful to keep the simulated output together with the
parameter grid to be able to check which parameters correspond to which
simulated outbreak.

In this example we run the same scenario parameter sweep as before
(again with 3 replicates), but store the simulated outbreak
`data.table`s in the same object as the list of scenarios. We append the
simulation results to the `scenario_sims` `data.table` and assign them
to the `sims` column.

``` r
scenario_sims[, sims := lapply(data, \(x, n) {
  scenario_sim(
    n = n,
    initial_cases = x$initial_cases,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = x$r0_community, size = x$disp_community),
      isolated = \(n) rnbinom(n = n, mu = x$r0_isolated, size = x$disp_isolated)
    ),
    delays = delay_opts(
      incubation_period = x$incubation_period[[1]],
      onset_to_isolation = x$onset_to_isolation[[1]]
    ),
    event_probs = event_prob_opts(
      asymptomatic = x$prop_asymptomatic,
      presymptomatic_transmission = x$prop_presymptomatic,
      symptomatic_ascertained = x$prop_ascertain
    ),
    interventions = intervention_opts(quarantine = x$quarantine),
    sim = sim_opts(cap_max_days = x$cap_max_days, cap_cases = x$cap_cases)
  )
  },
  n = n
)]
scenario_sims
#>      scenario               data                sims
#>         <int>             <list>              <list>
#>   1:        1 <data.table[1x14]> <data.table[159x6]>
#>   2:        2 <data.table[1x14]> <data.table[159x6]>
#>   3:        3 <data.table[1x14]> <data.table[159x6]>
#>   4:        4 <data.table[1x14]> <data.table[159x6]>
#>   5:        5 <data.table[1x14]> <data.table[159x6]>
#>  ---                                                
#> 156:      156 <data.table[1x14]> <data.table[159x6]>
#> 157:      157 <data.table[1x14]> <data.table[159x6]>
#> 158:      158 <data.table[1x14]> <data.table[159x6]>
#> 159:      159 <data.table[1x14]> <data.table[159x6]>
#> 160:      160 <data.table[1x14]> <data.table[159x6]>
head(scenario_sims$data, 3)
#> [[1]]
#>    r0_community r0_isolated disp_community disp_isolated prop_presymptomatic
#>           <num>       <num>          <num>         <num>               <num>
#> 1:          1.1           0           0.16             1                0.01
#>    prop_asymptomatic prop_ascertain initial_cases quarantine cap_max_days
#>                <num>          <num>         <num>     <lgcl>        <num>
#> 1:                 0              0             5      FALSE          365
#>    cap_cases  delay onset_to_isolation incubation_period
#>        <num> <char>             <list>            <list>
#> 1:      5000   SARS      <function[1]>     <function[1]>
#> 
#> [[2]]
#>    r0_community r0_isolated disp_community disp_isolated prop_presymptomatic
#>           <num>       <num>          <num>         <num>               <num>
#> 1:          1.1           0           0.16             1                0.01
#>    prop_asymptomatic prop_ascertain initial_cases quarantine cap_max_days
#>                <num>          <num>         <num>     <lgcl>        <num>
#> 1:                 0              0             5      FALSE          365
#>    cap_cases  delay onset_to_isolation incubation_period
#>        <num> <char>             <list>            <list>
#> 1:      5000  Wuhan      <function[1]>     <function[1]>
#> 
#> [[3]]
#>    r0_community r0_isolated disp_community disp_isolated prop_presymptomatic
#>           <num>       <num>          <num>         <num>               <num>
#> 1:          1.5           0           0.16             1                0.01
#>    prop_asymptomatic prop_ascertain initial_cases quarantine cap_max_days
#>                <num>          <num>         <num>     <lgcl>        <num>
#> 1:                 0              0             5      FALSE          365
#>    cap_cases  delay onset_to_isolation incubation_period
#>        <num> <char>             <list>            <list>
#> 1:      5000   SARS      <function[1]>     <function[1]>
head(scenario_sims$sims, 3)
#> [[1]]
#>        sim  week weekly_cases cumulative effective_r0 cases_per_gen
#>      <int> <num>        <num>      <num>        <num>        <list>
#>   1:     1     0            5          5          0.2           2,0
#>   2:     1     1            2          7          0.2           2,0
#>   3:     1     2            0          7          0.2           2,0
#>   4:     1     3            0          7          0.2           2,0
#>   5:     1     4            0          7          0.2           2,0
#>  ---                                                               
#> 155:     3    48            0          8          0.3           3,0
#> 156:     3    49            0          8          0.3           3,0
#> 157:     3    50            0          8          0.3           3,0
#> 158:     3    51            0          8          0.3           3,0
#> 159:     3    52            0          8          0.3           3,0
#> 
#> [[2]]
#>        sim  week weekly_cases cumulative effective_r0
#>      <int> <num>        <num>      <num>        <num>
#>   1:     1     0            5          5     0.100000
#>   2:     1     1            1          6     0.100000
#>   3:     1     2            0          6     0.100000
#>   4:     1     3            0          6     0.100000
#>   5:     1     4            0          6     0.100000
#>  ---                                                 
#> 155:     3    48            0        914     1.082393
#> 156:     3    49            0        914     1.082393
#> 157:     3    50            0        914     1.082393
#> 158:     3    51            0        914     1.082393
#> 159:     3    52            0        914     1.082393
#>                        cases_per_gen
#>                               <list>
#>   1:                             1,0
#>   2:                             1,0
#>   3:                             1,0
#>   4:                             1,0
#>   5:                             1,0
#>  ---                                
#> 155:  19, 45, 61, 72,103,121,...[18]
#> 156:  19, 45, 61, 72,103,121,...[18]
#> 157:  19, 45, 61, 72,103,121,...[18]
#> 158:  19, 45, 61, 72,103,121,...[18]
#> 159:  19, 45, 61, 72,103,121,...[18]
#> 
#> [[3]]
#>        sim  week weekly_cases cumulative effective_r0             cases_per_gen
#>      <int> <num>        <num>      <num>        <num>                    <list>
#>   1:     1     0            5          5     0.000000                         0
#>   2:     1     1            0          5     0.000000                         0
#>   3:     1     2            0          5     0.000000                         0
#>   4:     1     3            0          5     0.000000                         0
#>   5:     1     4            0          5     0.000000                         0
#>  ---                                                                           
#> 155:     3    48            0       5939     1.482541 16,17,11,10,26,80,...[18]
#> 156:     3    49            0       5939     1.482541 16,17,11,10,26,80,...[18]
#> 157:     3    50            0       5939     1.482541 16,17,11,10,26,80,...[18]
#> 158:     3    51            0       5939     1.482541 16,17,11,10,26,80,...[18]
#> 159:     3    52            0       5939     1.482541 16,17,11,10,26,80,...[18]
```

## Running scenarios in parallel

Finally, we demonstrate how the above example can be easily run in
parallel using the [{future} framework](https://www.futureverse.org/).
We load the {future} and {future.apply} R packages for this.

``` r
library(future)
library(future.apply)
```

We can replace the [`lapply()`](https://rdrr.io/r/base/lapply.html) with
the
[`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)
function and then choose how to run the analysis in parallel. See
[`?future::plan`](https://future.futureverse.org/reference/plan.html)
for details.

Here we show how to run the analysis on 2 separate R sessions running in
the background.

``` r
future::plan("multisession", workers = 2)
```

Now we re-run the parameter sweep with parallelisation.

``` r
scenario_sims[, sims := future_lapply(data, \(x, n) {
  scenario_sim(
    n = n,
    initial_cases = x$initial_cases,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = x$r0_community, size = x$disp_community),
      isolated = \(n) rnbinom(n = n, mu = x$r0_isolated, size = x$disp_isolated)
    ),
    delays = delay_opts(
      incubation_period = x$incubation_period[[1]],
      onset_to_isolation = x$onset_to_isolation[[1]]
    ),
    event_probs = event_prob_opts(
      asymptomatic = x$prop_asymptomatic,
      presymptomatic_transmission = x$prop_presymptomatic,
      symptomatic_ascertained = x$prop_ascertain
    ),
    interventions = intervention_opts(quarantine = x$quarantine),
    sim = sim_opts(cap_max_days = x$cap_max_days, cap_cases = x$cap_cases)
  )
  },
  n = n,
  future.seed = TRUE
)]
scenario_sims
```

Note, we set the `future.seed = TRUE` to ensure that parallel-safe
random numbers are produced.
