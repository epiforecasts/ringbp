# Feasibility of controlling 2019-nCoV outbreaks by isolation of cases and contacts


## Abstract


**Aim:** To assess the viability of using isolation of infected cases combined with tracing the contacts of infected cases as a method of controlling an outbreak of the 2019 novel coronavirus.

**Methods:** We developed a branching process that models the effectiveness of contact tracing and isolation of infected cases at controlling a 2019 nCoV-like pathogen. Where possible the epidemiological properties of the 2019 nCoV outbreak in Wuhan were taken from data available from the current outbreak. If there is currently no data then we ran the branching process for a range of scenarios that the properties of 2019 nCoV are likely to fall within. To assess the probability of an outbreak being controlled within 12 weeks, we ran 1,000 simulations for different combinations of the size at which the outbreak is detected, the basic reproduction number R0, the delay from symptom onset to isolation, the probability that a contact was followed up by contact tracing, and the proportion of transmission that occurred before symptom onset. We measured the feasibility of using isolation and contact tracing to control 2019 nCoV by calculating the proportion of model runs where the outbreak was controlled within 12 weeks of exposure of the initial cases.

**Results:** While simulated outbreaks starting with only 5 people and R0 of 1.5 could be well controlled even with only minor contract tracing efforts, the prospects of controlling an outbreak dramatically dropped with the size of that outbreak at the time of its detection and with a higher R0. Across different initial outbreak sizes, the majority of scenarios with an R0 of 1.5 were controllable with under 50% of contacts successfully traced. For R0 of 2.5 (3.5) more than 70% (90%) of contacts had to be traced to control the majority of outbreaks. The delay between symptom onset and isolation played the largest role for lower values of R0. For higher numbers of R0 and large initial outbreak sizes, contact tracing was only potentially feasible when the proportion of infections before symptom onset was smaller than 1%.

**Conclusions:** We found that in most scenarios, even under perfect isolation conditions, contact tracing and case isolation is unlikely to sufficiently control a new outbreak of 2019 nCov within three months. The probability of control decreases with longer delays to isolation, fewer cases ascertained by contact tracing, and increasing proportion of cases that become infectious before the onset of symptoms. The size of a potential outbreak that is brought under control in each scenario is sensitive to the same parameters, and must be taken into consideration, as the efficacy of outbreak response interventions will be limited by available resources. This model can be modified to reflect updated transmission characteristics and more specific definitions of outbreak control to assess the potential success of local response efforts.

## Usage

### Set up

Set your working directory to the home directory of this project (or use the provided Rstudio project). Install the analysis and all dependencies with: 

```r
remotes::install_github("epiforecasts/ringbp", dependencies = TRUE)
```

### Run a single scenario

Run a single scenario for a 100 simulations.

```r
library(ringbp)
library(ggplot2)

res <- ringbp::wuhan_sim(n.sim = 100,num.initial.cases = 1,num.initial.clusters = 10,
                 prop.ascertain = 0.2, cap_cases = 4500, cap_max_days = 350,
                 r0isolated = 0, r0community = 2.5, disp.com = 0.16, disp.iso = 1,
                 mu_ip = 5.8, sd_ip = 2.6, mu_si = 7.5, sd_si = 3.4, delay_shape = 1.651524,
                 delay_scale = 4.287786,k = 0)

# Plot of weekly cases
ggplot2::ggplot(data=res, ggplot2::aes(x=week, y=cumulative, col=as.factor(sim))) +
  ggplot2::geom_line(show.legend = FALSE, alpha=0.3) +
  ggplot2::scale_y_continuous(name="Number of cases") + 
  ggplot2::theme_bw()

ringbp::extinct_prob(res,cap_cases = 4500)
```

### Run the full analysis

Run the analysis with the following:

```bash
Rscript inst/scripts/generate_results.R
```

### Generate figures

Render figures with the following:

```bash
Rscript inst/scripts/generate_figures.R
```

## Docker 

This analysis was developed in a docker container based on the tidyverse docker image. 

To build the docker image run (from the `ringbp` directory):

```bash
docker build . -t ringbp
```

To run the docker image run:

```bash
docker run -d -p 8787:8787 --name ringbp -e USER=ringbp -e PASSWORD=ringbp ringbp
```

The rstudio client can be found on port :8787 at your local machines ip. The default username:password is ringbp:ringbp, set the user with -e USER=username, and the password with - e PASSWORD=newpasswordhere. The default is to save the analysis files into the user directory.

To mount a folder (from your current working directory - here assumed to be `tmp`) in the docker container to your local system use the following in the above docker run command (as given mounts the whole `ringbp` directory to `tmp`).

```{bash, eval = FALSE}
--mount type=bind,source=$(pwd)/tmp,target=/home/ringbp
```

To access the command line run the following:

```{bash, eval = FALSE}
docker exec -ti ringbp bash
```

