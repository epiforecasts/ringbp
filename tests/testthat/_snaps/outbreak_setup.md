# outbreak_setup works as expected

    Code
      outbreak_setup(initial_cases = 5, delays = delay_opts(incubation_period = function(
        n) stats::rweibull(n = n, shape = 2.32, scale = 6.49), onset_to_isolation = function(
        n) stats::rweibull(n = n, shape = 2, scale = 4)), event_probs = event_prob_opts(
        asymptomatic = 0.2, presymptomatic_transmission = 0.5, symptomatic_traced = 0.8),
      interventions = intervention_opts())
    Output
      Index: <self_isolate__asymptomatic>
         exposure asymptomatic caseid infector traced    onset new_cases self_isolate
            <num>       <lgcl>  <int>    <num> <lgcl>    <num>     <int>       <lgcl>
      1:        0        FALSE      1        0  FALSE 3.696253        NA        FALSE
      2:        0        FALSE      2        0  FALSE 6.837342        NA        FALSE
      3:        0        FALSE      3        0  FALSE 4.369195        NA        FALSE
      4:        0        FALSE      4        0  FALSE 6.009202        NA        FALSE
      5:        0        FALSE      5        0  FALSE 4.157019        NA        FALSE
         isolated_time sampled
                 <num>  <lgcl>
      1:      5.940244   FALSE
      2:     11.030355   FALSE
      3:      7.331763   FALSE
      4:      8.002524   FALSE
      5:      6.409544   FALSE

