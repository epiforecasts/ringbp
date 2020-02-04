# Wuhan novel coronavirus analysis

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
res <- wuhan_sim(n.sim = 100,num.initial.cases = 1,num.initial.clusters = 10,
                 prop.ascertain = 0.2, cap_cases = 4500, cap_max_days = 350,
                 r0isolated = 0, r0community = 2.5, disp.com = 0.16, disp.iso = 1,
                 mu_ip = 5.8, sd_ip = 2.6, mu_si = 7.5, sd_si = 3.4, delay_shape = 1.651524,
                 delay_scale = 4.287786,k = 0)

# Plot of weekly cases
ggplot(data=res, aes(x=week, y=cumulative, col=as.factor(sim)))+
  geom_line(show.legend = FALSE, alpha=0.3)+
  scale_y_continuous(name="Number of cases")+ theme_bw()

extinct_prob(res,cap_cases = 4500)

```

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

