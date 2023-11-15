################################################################################
#                         estimate_bias_and_precision                          #
################################################################################
#
# This script takes input parameters required for simulating bycatches 
# from a fishery and monitors these using different monitoring schemes (using Scoti).
# 
# Input : fishery_params - a list (n_scenarios*n_fishing_params) with parameters
#                          used for simulating different bycatch fishery scenarios.
#         monitoring_params - a list (n_scenarios*n_monitoring_params) with 
#                             monitoring parameters for different monitoring 
#                             scenarios.
#
# Output: monitor_estimate - a data frame with monitoring estimates for different
#                            fishery and monitoring scenarios. Each row corresponds
#                            to a unique combination of a fishery and monitoring 
#                            scheme. If sampling is stratified on metiers BPUE and
#                            CV are given for each strata. 

# source fishing and monitor functions
source("make_fishing_year_metier.R")
source("monitor_BPUE_metier.R")

# required packages
require(tidyverse)

#--------#
# Input  #
#--------#

#
# fishery and bycatch
#

fishery_params <- list() # list with all fishery bycatch scenarios

fishery_params[[1]] <- list(mean.bycatch.event=1, # mean number of individuals caught(normal bycatch event)
  mean.bycatch.large.event=20, # mean number of individuals caught(large bycatch event)
  p.large.event=0.01, # probability of large bycatch event
  nboat=100, # number of boats to sample
  mean.fishing.event.boat.day=2, # mean number of fishing events per boat and day
  p.bycatch=c(0.1,.01), # probability of bycatch in metier 1 and metier 2
  p.metier=c(.2,.8), # proportion of metier 1 and metier 2 in the fleet
  stochastic=TRUE) # are mean number of fishing events per boat random across boats?

#
# monitoring
#

monitoring_params <- list() # list with all monitoring scenarios

comb_monitoring <- read.csv("monitor_effort_combination.csv",sep=";") # load monitoring effort combinations

for(i in 1:nrow(comb_monitoring)){
monitoring_params[[i]] <- pairlist(
  p_monitor_metier=cbind(0.4,0.6), # how monitoring is distributed between metiers
  nsample=1000, # number of monitoring samples for estimating bias and precision
  boat_samp=TRUE,  # sample a proportion of vessels
  p_haul_obs=1, # 
  pmonitor=comb_monitoring$pmonitor[i],
  p_monitor_boat=comb_monitoring$p_monitor_boat[i],
  detect_prob=1,
  refusal_rate=0, 
  misclassification=0, 
  bymetier=FALSE # should sampling be stratify by metier
) 
}

# end of inputs
#---------------#
#---------------#


#--------------------------------------------------#
# Simulate fishery bycatch and monitor the fishery #
#--------------------------------------------------#

# inits
nmetier <- length(fishery_params[[1]]$p.metier) # number of metiers in the fishery
monitor_estimate <- data.frame() # monitoring estimate data frame

for (y in 1:100) { # loop over years    
  for(i in 1:length(fishery_params)){  # loop over different fishery and bycatch scenarios
    
    # simulate one year of fishing
    fishing <- make_fishing_year_metier(mean.bycatch.event=fishery_params[[i]]$mean.bycatch.event, # mean number of individuals caught(normal bycatch event)
                                       mean.bycatch.large.event=fishery_params[[i]]$mean.bycatch.large.event, # mean number of individuals caught(large bycatch event)
                                       p.large.event=fishery_params[[i]]$p.large.event, # probability of large bycatch event
                                       nboat=fishery_params[[i]]$nboat, # number of boats to sample
                                       mean.fishing.event.boat.day=fishery_params[[i]]$mean.fishing.event.boat.day, # mean number of fishing events per boat and day
                                       p.bycatch=fishery_params[[i]]$p.bycatch, # probability of bycatch in metier 1 and metier 2
                                       p.metier=fishery_params[[i]]$p.metier, # proportion of metier 1 and metier 2 in the fleet
                                       stochastic=fishery_params[[i]]$stochastic) # are mean number of fishing events per boat random across boats?
    
    #------------------------------------#
    # Apply different monitoring schemes #
    #------------------------------------#
    
    # inits (these are needed in order to save each scenario in one row)
    p_bycatch <- fishery_params[[i]]$p.bycatch # probability of bycatch for different metiers
    names(p_bycatch) <-  paste0("p_bycatch_",1:nmetier)
    p_bycatch <- t(p_bycatch)
    
    p_metier <- fishery_params[[i]]$p.metier # proportion of different metiers in the fishery
    names(p_metier) <-  paste0("p_metier_",1:nmetier)
    p_metier <- t(p_metier)
    
    for(j in 1:length(monitoring_params)){ # loop over monitoring combinations
    
    if(monitoring_params[[j]]$bymetier){ # if monitoring is stratified by metier
    
      # temporary df for saving scenario
      temp<-data.frame(year=y,
                       p_bycatch,
                       p_metier,
                       pmonitor=monitoring_params[[j]]$pmonitor,
                       p_monitor_boat=monitoring_params[[j]]$p_monitor_boat,
                       boat_samp=monitoring_params[[j]]$boat_samp,
                       bymetier=monitoring_params[[j]]$bymetier,
                       p_monitor_metier=monitoring_params[[j]]$p_monitor_metier)
      
      # monitor the fishery
      monitoring<-monitor_BPUE_metier(pmonitor=monitoring_params[[j]]$pmonitor,
                                      nsample=1000,
                                      BPUE_real=BPUE_real,
                                      fishing=fishing, 
                                      p_monitor_boat=monitoring_params[[j]]$p_monitor_boat,
                                      boat_samp=monitoring_params[[j]]$boat_samp,
                                      p_haul_obs=monitoring_params[[j]]$p_haul_obs,
                                      detect_prob=monitoring_params[[j]]$detect_prob,
                                      refusal_rate=monitoring_params[[j]]$refusal_rate, 
                                      misclassification=monitoring_params[[j]]$missclassification, 
                                      bymetier=monitoring_params[[j]]$bymetier, 
                                      p_monitor_metier=monitoring_params[[j]]$p_monitor_metier)
      
      # Append BPUE and CV estimates in one row
      BPUE_real <- fishing %>% group_by(metiers) %>% summarize(BPUE=sum(nbycatch)/n())
      BPUE_real <- BPUE_real$BPUE
      names(BPUE_real) <-  paste0("BPUE_real_",1:length(BPUE_real))
      BPUE_real <- t(BPUE_real)
	  
	  effort_real <- fishing %>% group_by(metiers) %>% summarize(effort=n())
      effort_real <- effort_real$effort
      names(effort_real) <-  paste0("fishingeffort_real_",1:length(effort_real))
      effort_real <- t(effort_real)
      
      BPUE_est <- monitoring$BPUE_est
      names(BPUE_est) <-  paste0("BPUE_est_",1:length(BPUE_est))
      BPUE_est <- t(BPUE_est)
      
      CV<-monitoring$CV
      names(CV) <- paste0("CV_",1:length(CV))
      CV <- t(CV)
    
      temp <- cbind(temp,BPUE_real,BPUE_est,CV,effort_real) # append estimates to scenario description
      
      monitor_estimate<-rbind(monitor_estimate,temp) # monitoring scenario done         
        
    }else{ # non-stratified sampling
      
      # temporary df for saving scenario
      temp<-data.frame(year=y,
                       p_bycatch,
                       p_metier,
                       pmonitor=monitoring_params[[j]]$pmonitor,
                       p_monitor_boat=monitoring_params[[j]]$p_monitor_boat,
                       boat_samp=monitoring_params[[j]]$boat_samp,
                       bymetier=monitoring_params[[j]]$bymetier)
      
      # monitor the fishery
      monitoring<-monitor_BPUE_metier(pmonitor=monitoring_params[[j]]$pmonitor,
                                      nsample=1000,
                                      BPUE_real=BPUE_real,
                                      fishing=fishing, 
                                      p_monitor_boat=monitoring_params[[j]]$p_monitor_boat,
                                      boat_samp=monitoring_params[[j]]$boat_samp,
                                      p_haul_obs=monitoring_params[[j]]$p_haul_obs,
                                      detect_prob=monitoring_params[[j]]$detect_prob,
                                      refusal_rate=monitoring_params[[j]]$refusal_rate, 
                                      misclassification=monitoring_params[[j]]$missclassification, 
                                      bymetier=monitoring_params[[j]]$bymetier, 
                                      p_monitor_metier=monitoring_params[[j]]$p_monitor_metier)
      
      temp$BPUE_real=sum(fishing$nbycatch)/nrow(fishing)
      temp$BPUE_est<-monitoring$BPUE_est
      temp$BPUE_est_CV<-monitoring$CV
	  temp$effort_real<-nrow(fishing)
      monitor_estimate<-rbind(monitor_estimate,temp)   
    }
    } # end of monitoring scenario
  } # end of fishery scenario
} # end of year

# save monitoring results
write.csv(monitor_estimate,"Output/CV_and_bias_est.csv")
