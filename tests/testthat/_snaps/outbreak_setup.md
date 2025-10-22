# outbreak_setup works as expected

    Code
      outbreak_setup(initial_cases = 5, delays = delay_opts(incubation_period = function(
        n) stats::rweibull(n = n, shape = 2.32, scale = 6.49), onset_to_isolation = function(
        n) stats::rweibull(n = n, shape = 2, scale = 4)), event_probs = event_prob_opts(
        asymptomatic = 0.2, presymptomatic_transmission = 0.5,
        symptomatic_ascertained = 0.8))
    Output
      Index: <asymptomatic>
         exposure asymptomatic caseid infector isolated missed    onset new_cases
            <num>       <lgcl>  <int>    <num>   <lgcl> <lgcl>    <num>    <lgcl>
      1:        0        FALSE      1        0    FALSE   TRUE 3.696253        NA
      2:        0        FALSE      2        0    FALSE   TRUE 6.837342        NA
      3:        0        FALSE      3        0    FALSE   TRUE 4.369195        NA
      4:        0        FALSE      4        0    FALSE   TRUE 6.009202        NA
      5:        0        FALSE      5        0    FALSE   TRUE 4.157019        NA
         isolated_time
                 <num>
      1:      5.984245
      2:     12.350205
      3:      9.761168
      4:      7.443792
      5:      7.509917

