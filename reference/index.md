# Package index

## Model

- [`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md)
  : Run a single instance of the branching process model
- [`outbreak_setup()`](https://epiforecasts.io/ringbp/reference/outbreak_setup.md)
  : Set up initial cases for branching process
- [`outbreak_step()`](https://epiforecasts.io/ringbp/reference/outbreak_step.md)
  : Move forward one generation in the branching process

## Helper functions

### Probability distribution manipulation

- [`incubation_to_generation_time()`](https://epiforecasts.io/ringbp/reference/incubation_to_generation_time.md)
  : Convert symptom onset times to generation times

### Loop wrappers for scenario modelling

- [`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
  : Run a specified number of simulations with identical parameters

### Model parameters and control options

- [`delay_opts()`](https://epiforecasts.io/ringbp/reference/delay_opts.md)
  :

  Create a list of delay distributions to run the ringbp model

- [`event_prob_opts()`](https://epiforecasts.io/ringbp/reference/event_prob_opts.md)
  :

  Create a list of event probabilities to run the ringbp model

- [`intervention_opts()`](https://epiforecasts.io/ringbp/reference/intervention_opts.md)
  :

  Create a list of intervention settings to run the ringbp model

- [`offspring_opts()`](https://epiforecasts.io/ringbp/reference/offspring_opts.md)
  :

  Create a list of offspring distributions to run the ringbp model

- [`sim_opts()`](https://epiforecasts.io/ringbp/reference/sim_opts.md) :

  Create a list of simulation control options for the ringbp model

## Post-processing

- [`extinct_prob()`](https://epiforecasts.io/ringbp/reference/extinction.md)
  [`detect_extinct()`](https://epiforecasts.io/ringbp/reference/extinction.md)
  : Outbreak extinction functions
