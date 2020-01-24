# Wuhan novel coronavirus analysis

## Master to-do list
- [ ] Put together/acquire time series of cases for Wuhan outbreak (Take from alreafdy collated time series (Sunky/Eurosurveillance paper?))
- [ ] Source new parameter values for branching process model based on SARS/flu (see parameters in table below)
- [ ] Source new parameter values for nCov
- [ ] Find nice way to link theta value to chosen incubation period estimation
- [ ] Find nice way to summarise how controllable outbreak with given parameters is (prob of extinction within certain period?)
- [ ] Run simulations
- [ ] Write up
- [ ] Publish somewhere (need to decide where, blog? letter to journal? paper?)

This outbreak contains code sent over by Adam and Alicia for branching process simulation from *Effectiveness of Ring Vaccination as a Control Strategy for Ebola Virus Disease* by Kucharski et al (2016):
https://wwwnc.cdc.gov/eid/article/22/1/15-1410_article

The analysis as I understand it so far is this:
* Using the theoretical understanding of parameter that determines percentage of transmission that occurs before symptoms occur (theta) to understand the effectiveness of responding to outbreaks by isolating cases or both isolating cases and tracing and quarantining their contacts. This is fleshed out in *Factors that make an infectious disease outbreak controllable* by Fraser et al 2004 (https://www.pnas.org/content/101/16/6146)
* Use priors for the value of Rm (Reproduction number for 'missed cases'), Rw (Reproduction number for detected and isolated cases), and theta that are similar to SARS, increasing them to be similar to flu. Simulate outbreaks that are close to Wuhan-ncov in nature.
* Determine what values of theta and Rm will allow the Wuhan outbreak to be controlled by isolating cases (and tracing contacts) 
* Compare these values to current estimates of reproduction number for Wuhan ncov outbreak from other sources


## Parameters

### Parameter overview from Fraser et al. 

* Parameter estimates from SARs: 
	* R0: 2-4
	* Incubation period of 4.25 days with a variance of 14.25
	* Infectiousness over time: "can be inferred from viral shedding data (5), which peaks 5–10 days after onset of symptoms. To maximize , we chose a low variance distribution (var = 0.1 * mean^2) and a peak at 9.25 days after infection, yielding theta < 11%"

### Parameter overview from Campbell et al.

* *Bayesian inference of transmission chains using timing of symptoms, pathogen genomes and contact data* by Campbell et al
	* Table 2 has references for many epidemiological parameters of SARS-cov
	* https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1006930#sec006

### Wuhan data

* http://virological.org/t/epidemiological-data-from-the-ncov-2019-outbreak-early-descriptions-from-publicly-available-data/337
* https://docs.google.com/spreadsheets/d/1itaohdPiAeniCXNlntNztZ_oRvjh0HsGuJXUJWET008/edit#gid=0

### Other ideas
* Robin has data describing the proportion of cases for Wuhan with specific symptoms (fever, cough, shortness of breath, breathing difficulties (requires oxygen), chest pain, low white blood cell count). By comparing these with SARS, influenza etc we could potentially get a (very rough) idea of the level of symptoms for Wuhan vs other diseases, which could in turn (with some assumptions) inform the percentage of transmissions occurring from missed cases/in isolation (since that is already known for other pathogens)? Would need some thought...
	
## Usage

### Set up



### Run an analysis

To check your package is working correctly, try to run some simulations with the following:

```r
# Run 100 simulations
res <- wuhan_sim(n.cores = 4,n.sim = 100,wvaccYN = 0,define_6m = 239,initial.cases.pcluster = 5,
          initial.clusters = 5, prop.ascertain = 0.9, cap_cases = 4500, cap_max_days = 350,
          r0within = 0.5, r0Am = 2, overkkmiss = 1, overkk = 0.19, vefficacy = 1,
          vuptake = 0.90, ring.size = 100, time_to_protection = 2, incub_mean = 5, 
          incub_var = 1.5, inf_mean = 5, inf_var = 1.5, delay_shape = 2.4114166, 
          delay_rate = 0.3261129,time_to_isolation=1)

# Plot of daily cases
ggplot(data=res$outbreak_df, aes(x=day, y=number, col=as.factor(n.sim)))+
  geom_line(show.legend = FALSE, alpha=0.1)+
  scale_y_continuous(name="Number of cases")+ theme_bw()+
  geom_line(aes(x=day, y=mean.number), col="black")

# Plot of weekly cases
ggplot(data=res$outbreak_df_week, aes(x=week, y=number, col=as.factor(n.sim)))+
  geom_line(show.legend = FALSE, alpha=0.3)+
  scale_y_continuous(name="Number of cases")+ theme_bw()+
  geom_line(aes(x=week, y=mean.number), col="black")

# Proportion of runs that have 0 weekly cases in weeks 10-12 after outbreak
extinct_prob(res$outbreak_df_week)
```


## Table of parameters from branching process

Parameters passed to `wuhan_sim` function:

| Parameter name | Parameter explanation | Will we need to alter it? |
| -------------- | --------------------- | ------------------------- |
| `wvaccYN` | Yes/no vaccination in model | set to TRUE |
| `define_6m` | "Period when weekly average is below 10" | Not sure |
| `initial.cases.pcluster` | Initial cases per ring (in ring = treatment) | Yes |
| `initial.clusters` | Initial number of different case clusters | Yes |
| `prop.ascertain` | proportion of cases identified | Yes |
| `cap_cases` | Limits the number of cases in simulation | Yes |
| `cap_max_days` | Limits days since start of outbreak in simulation | Yes |
| `r0within` | R0 for cases within ring | Yes |
| `r0Am` | R0 for missed (i.e. index cases) | Yes |
| `overkkmiss` | Dispersion of negative binomial for missed cases | Yes? |
| `overkk` | Dispersion of negative binomial for cases within ring | Yes? |
| `vefficacy` | vaccine efficacy | Set to 100%? |
| `vuptake` | vaccine uptake | Set to 100%? |
| `ring.size` | Limits maximum ring size | Yes |
| `time_to_protection` | Time from intervention until its effectiveness | Yes |
| `time_to_isolation` | Time from ascertainment until isolation | Yes |
| `incub_mean` | mean of gamma distribution for incubation period | Yes |
| `incub_var` | variance of gamma distribution for incubation period | Yes |
| `inf_mean` | mean of gamma distribution for infectiousness | Yes |
| `inf_var` | variance of gamma distribution for infectiousness | Yes |
| `delay_shape` | shape parameter of gamma distribution for delay from symptom onset to isolation | Yes |
| `delay_rate` | rate parameter of gamma distribution for delay from symptom onset to isolation | Yes |

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

