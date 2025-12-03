# outbreak_setup works as expected

    Code
      outbreak_setup(initial_cases = 5, delays = delay_opts(incubation_period = function(
        n) stats::rweibull(n = n, shape = 2.32, scale = 6.49), onset_to_isolation = function(
        n) stats::rweibull(n = n, shape = 2, scale = 4)), event_probs = event_prob_opts(
        asymptomatic = 0.2, presymptomatic_transmission = 0.5,
        symptomatic_ascertained = 0.8))
    Output
      Index: <asymptomatic>
         exposure asymptomatic caseid infector missed    onset new_cases
            <num>       <lgcl>  <int>    <num> <lgcl>    <num>     <int>
      1:        0        FALSE      1        0   TRUE 3.696253        NA
      2:        0        FALSE      2        0   TRUE 6.837342        NA
      3:        0        FALSE      3        0   TRUE 4.369195        NA
      4:        0        FALSE      4        0   TRUE 6.009202        NA
      5:        0        FALSE      5        0   TRUE 4.157019        NA
         isolated_time sampled
                 <num>  <lgcl>
      1:      5.984245   FALSE
      2:     12.350205   FALSE
      3:      9.761168   FALSE
      4:      7.443792   FALSE
      5:      7.509917   FALSE

