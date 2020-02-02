# Wuhan novel coronavirus analysis

## Master to-do list
- [ ] Work out how changes in the value of `k`, the correlation between serial interval and incubation period, change model runs.
- [ ] Produce new graphs, descriptions of each in comments in the google doc
- [ ] Write up results
- [ ] Send round nCov working group
- [ ] Pre-print first draft

This outbreak contains code sent over by Adam and Alicia for branching process simulation from *Effectiveness of Ring Vaccination as a Control Strategy for Ebola Virus Disease* by Kucharski et al (2016):
https://wwwnc.cdc.gov/eid/article/22/1/15-1410_article

## Summary report
https://docs.google.com/document/d/1JXZ2hG8YQEWC7CufQuIyr5WE4ymz3io4_n_CQdfyRaQ/edit?invite=CJKy66IK&ts=5e2b3ea3

### Set up
* Installing the package is annoying because this is a private repository. You will need to set up a github personal access token (PAT), there is a step by step guide here: https://happygitwithr.com/github-pat.html
* You can then run the command `devtools::install_github("epiforecasts/ringbp")`
* Alternatively, if you are hoping to develop the package, clone this github repository to your own computer and open the R project file.

### Run an analysis

To check your package is working correctly, try to run some simulations with the following:

```r
# Run 100 simulations
res <- wuhan_sim(n.cores = 6,n.sim = 100,wvaccYN = 0,define_6m = 20*7,initial.cases.pcluster = 6,
          initial.clusters = 10, prop.ascertain = 0.2, cap_cases = 4500, cap_max_days = 350,
          r0within = 0.3, r0Am = 2.5, overkkmiss = 2, overkk = 0.16, vefficacy = 0.975,
          vuptake = 0.90, ring.size = 150, time_to_protection = 0, incub_mean = 7, 
          incub_var = 5, inf_mean = 7, inf_var = 5, delay_var = 2, 
          delay_mean = 7,time_to_isolation=0,outbreak_df_out = TRUE)


# plot(x=seq(0,10,0.1),y=dgamma(seq(0,10,0.1),shape = 1.4114166,rate=0.3261129),type="l")

# Plot of daily cases
# ggplot(data=res$outbreak_df, aes(x=day, y=number, col=as.factor(n.sim)))+
#   geom_line(show.legend = FALSE, alpha=0.1)+
#   scale_y_continuous(name="Number of cases")+ theme_bw()+
#   geom_line(aes(x=day, y=mean.number), col="black")

# Plot of weekly cases
ggplot(data=res$outbreak_df_week, aes(x=week, y=number, col=as.factor(n.sim)))+
  geom_line(show.legend = FALSE, alpha=0.3)+
  scale_y_continuous(name="Number of cases")+ theme_bw()+
  geom_line(aes(x=week, y=mean.number), col="black")

res$outbreak_df_week %>% arrange(n.sim) %>% group_by(n.sim) %>% mutate(cs=c(cumsum(number))) %>%
  ggplot(aes(x=week,y=cs,col=as.factor(n.sim))) + geom_line(show.legend = FALSE,alpha=0.3) + theme_bw() + ylab("Cumulate cases") + xlab("weeks since outbreak start") +
  scale_x_continuous(breaks=0:20)

# Proportion of runs that have 0 weekly cases in weeks 10-12 after outbreak
extinct_prob(res$outbreak_df_week)
```

## Table of parameters from branching process

Parameters passed to `wuhan_sim` function, **bold = unsure** :

| Parameter name | Parameter explanation | Canonical value |
| -------------- | --------------------- | ------------------------- |
| `wvaccYN` | Yes/no vaccination in model | `TRUE` |
| `define_6m` | "Period when weekly average is below 10" | 140 (20 weeks) |
| `initial.cases.pcluster` | Initial cases per ring (in ring = treatment) | Yes |
| `initial.clusters` | Initial number of different case clusters | Yes |
| `prop.ascertain` | proportion of cases identified | Varies |
| `cap_cases` | Limits the number of cases in simulation | 5000 |
| `cap_max_days` | Limits days since start of outbreak in simulation | 350 |
| `r0isolated` | R0 for isolated cases | 0 |
| `r0community` | R0 for missed (i.e. index cases) | Varies |
| `overkkmiss` | Dispersion of negative binomial for missed cases | 0.16 |
| `overkk` | Dispersion of negative binomial for isolated cases | 0.01 |
| `mu_ip` | mean of multivariate normal distribution for incubation period | **7** |
| `sd_ip` | standard deviation of multivariate normal distribution for incubation period | **5** |
| `inf_mean` | mean of multivariate normal distribution for pre-infectious period | Varies |
| `inf_var` | standard deviation of multivariate normal distribution for pre-infectious period | **5** |
| `delay_mean` | mean of distribution for delay from symptom onset to isolation | Varies |
| `delay_var` | variance of distribution for delay from symptom onset to isolation | 1.5 |



## Table of parameters that are changed across scenarios

### Theta varies scenario

| param | SARS | middle | flu |
| ------- | ------- | ------ | ---- |
| `inf_mean` | 9 | 5 | 2 |
| `overkkmiss` | 0.16 | 0.5 | 1 |

### R0 varies scenario
| param | lower | middle | upper |
| ------- | ------- | ------ | ---- |
| `r0community` | 1.5 | 2.5 | 3.5 |

### Delay distribution mean scenario
| param | lower | middle | upper |
| ------- | ------- | ------ | ---- |
| `delay_mean` | 3 | 5 | 7 |

### Contact tracing effectiveness scenario
| param | lower | middle | upper |
| ------- | ------- | ------ | ---- |
| `prop_ascertain` | 0 | to | 1 |


## Docker 

This analysis was developed in a docker container based on the tidyverse docker image. 

To build the docker image run (from the `wuhan_ncov` directory):

```bash
docker build . -t wuhan_ncov
```

To run the docker image run:

```bash
docker run -d -p 8787:8787 --name wuhan_ncov -e USER=wuhan_ncov -e PASSWORD=wuhan_ncov wuhan_ncov
```

The rstudio client can be found on port :8787 at your local machines ip. The default username:password is wuhan_ncov:wuhan_ncov, set the user with -e USER=username, and the password with - e PASSWORD=newpasswordhere. The default is to save the analysis files into the user directory.

To mount a folder (from your current working directory - here assumed to be `tmp`) in the docker container to your local system use the following in the above docker run command (as given mounts the whole `wuhan_ncov` directory to `tmp`).

```{bash, eval = FALSE}
--mount type=bind,source=$(pwd)/tmp,target=/home/wuhan_ncov
```

To access the command line run the following:

```{bash, eval = FALSE}
docker exec -ti wuhan_ncov bash
```

