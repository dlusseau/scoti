################################################################################
#                        visualize_bias_and_precision                          #
################################################################################
#
# This script loads model output from Scoti (CV and bias estimates) and plots
# figures for different coverages.
# 

# required packages
require(tidyverse)

# load data
monitor_estimate <- read.csv("Output/CV_and_bias_est.csv")

# calculate some metrics needed for plotting
monitor_estimate <- monitor_estimate %>% mutate(tot_cov=round(p_monitor_boat*pmonitor, 3),
                                                Bias=(BPUE_est-BPUE_real)/BPUE_real)

# check names
mon_names <- unique(names(monitor_estimate))
mon_names <- mon_names %>% str_subset("BPUE",negate=TRUE)
mon_names <- mon_names %>% str_subset("Bias",negate=TRUE)

# check scenario combinations
unique(monitor_estimate %>% select(all_of(mon_names)))

# summarize bias and CV data(means across year)
monitor_estimate_mean <- monitor_estimate %>% 
                         group_by(tot_cov, pmonitor, p_monitor_boat) %>%
                         summarize(Bias=mean(Bias),
                                   CV=mean(BPUE_est_CV)) %>%
                         mutate(tot_cov=paste0("cov = ",tot_cov)) %>% 
                         ungroup()

# Make bias plot
ggplot(monitor_estimate_mean, aes(y=pmonitor,x=p_monitor_boat)) +
  geom_smooth(color="black",size=0.1,linetype="dashed") +
  geom_point(aes(color=Bias,size=Bias)) +
  scale_colour_gradient2(midpoint=0, mid="#eee8d5", high="#dc322f", low="#268bd2") +
  xlab("Proportion of vessels monitored")+
  ylab("Proportion of fishing events monitored") +
  guides(size = "none") +
  facet_wrap(vars(tot_cov))

# Make CV plot
ggplot(monitor_estimate_mean, aes(y=pmonitor,x=p_monitor_boat)) +
  geom_smooth(color="black",size=0.1,linetype="dashed") +
  geom_point(aes(color=CV, size=CV)) +
  scale_colour_gradient(limits=c(0,NA), low="#eee8d5", high="#dc322f") +
  xlab("Proportion of vessels monitored")+
  ylab("Proportion of fishing events monitored") +
  guides(size = "none") +
  facet_wrap(vars(tot_cov))
