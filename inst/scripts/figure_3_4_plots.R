res <- sweep_results %>% group_by(scenario) %>%
  mutate(pext=extinct_prob(sims[[1]],cap_cases = 5000))# %>%
  # mutate(num.initial.clusters = factor(num.initial.clusters,levels=c(5,20,40),
  #                                      labels = c("5 cases","20 cases","40 cases"))) %>%
  # mutate(index_R0 = factor(index_R0,levels = c(1.5,2.5,3.5),
  #                          labels = c("R0 = 1.5","R0 = 2.5","R0 = 3.5")))
  # mutate(theta = factor(theta,levels=c("<1%","15%","30%"),
  #                       labels = c("<1% transmission before symptoms",
  #                                  "15% transmission before symptoms",
  #                                  "30% transmission before symptoms")))

############
# FIGURE 3 #
############

res %>%
  filter(theta=="15%") %>%
  mutate(num.initial.clusters = factor(num.initial.clusters,levels=c(5,20,40),
                                       labels = c("5 cases","20 cases","40 cases"))) %>%
  mutate(index_R0 = factor(index_R0,levels = c(1.5,2.5,3.5),
                           labels = c("R0 = 1.5","R0 = 2.5","R0 = 3.5"))) %>%
  ggplot(aes(x=control_effectiveness,y=pext,col=as.factor(delay))) +
  geom_line() + geom_point() + facet_grid(num.initial.clusters~ index_R0) +
  scale_color_brewer(palette = "Set1",name="Delay from onset\n to hospitalisation") + theme_bw() +
  ylab("Proportion of outbreak simulations that were extinct \n within 3 months of initial exposure") +
  xlab("Proportion of reported contacts ascertained through contact tracing") +
  scale_x_continuous(breaks=seq(0,1,0.2),labels=paste0(seq(0,100,20),"%")) +
  scale_y_continuous(breaks=seq(0,1,0.2),labels=paste0(seq(0,100,20),"%"))

############
# FIGURE 4 #
############

res %>%
  filter(num.initial.clusters == 40) %>%
  mutate(delay = factor(delay,levels=c("Wuhan","SARS"),
                                       labels = c("Wuhan delay","SARS delay"))) %>%
  mutate(theta = factor(theta,levels = c("<1%","15%","30%"),
                           labels = c("<1% transmission \n  before symptoms",
                                      "15% transmission \n  before symptoms",
                                      "30% transmission \n  before symptoms"))) %>%
  ggplot(aes(x=control_effectiveness,y=pext,col=as.factor(index_R0))) +
  geom_line() + geom_point() + facet_grid(delay ~ theta) +
  scale_color_brewer(palette = "Dark2",name="Reproduction number") + theme_bw() +
  ylab("Proportion of outbreak simulations that were extinct \n within 3 months of initial exposure") +
  xlab("Proportion of reported contacts ascertained through contact tracing") +
  scale_x_continuous(breaks=seq(0,1,0.2),labels=paste0(seq(0,100,20),"%")) +
  scale_y_continuous(breaks=seq(0,1,0.2),labels=paste0(seq(0,100,20),"%"))

