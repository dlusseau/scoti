#####
##### this function monitors the simulated fishing year
####################################################################################################



# simulate sampling in a bycatch monitoring program  

# sampling effort and allocation
# - vessels
# - hauls

# sampling design
# - stratify by metier
# - first sample vessels, then sample hauls

# properties of vessel / captain
# - refusal to accept onboard observer


# properties of monitoring event
# - observe haul or not
# - detection of bycatch
# - correct species identification




monitor_BPUE_metier <- function(
    
  # note: sorted arguments on topic 
  
  # data frame with a simulated fishing year (see make_fishing_year_metier)
  fishing = NA,
  
  # overall monitoring params
  # nsample: number of samples (monitoring events) to be taken from the fishing data
  nsample = 1000,
  
  
  # sampling effort
  # p_monitor_boat: = pvessel??. Proportion of vessels monitored. In MS both terms are used.
  p_monitor_boat = 0.1,
  
  # pmonitor: proportion of hauls monitored for each vessel
  pmonitor = 0.5,
  
  # p_monitor_metier: proportion of monitoring allocated to (each?) métier
  # Note: what is valid format? what if length > 1 (see also below when p_monitor_boat is multiplied by p_monitor_metier)
  p_monitor_metier = 0.5,
  
  
  
  # sampling design
  # bymetier: is sampling stratified by metier?
  bymetier = FALSE,
  
  # boat_samp: ?? Guess: whether boat is sampled first, then events??
  boat_samp = TRUE,
  
  
  # refusal_rate: the probability that a vessel selected for monitoring refuses to engage
  # refusal is replaced to introduce selective pressures (unclear)
  # option 1 random, option 2 associated with nbycatch
  refusal_rate = 0,
  
  # quality of monitoring
  # p_haul_obs: probability that a haul was observed by the observer
  p_haul_obs = 1,
  
  # detect_prob: detection probability of each individual in the bycatch event
  # while at first, detect_prob is an independent draw on each individual in the haul, we can implement a decrease in detect_prob, with nbycatch increasing (on a log scale or similar)
  detect_prob = 1,
  
  
  # misclassification: probability of mis-identification of the bycaught species
  # misclassification might come in later when we have multiple species which can be confused, and therefore individuals from nbycatch_sp_i can
  #be taken to add to nbycatch_sp_j
  misclassification = 0,
  
  
  # BPUE_real: how is this parameterized?
  # see simulation_example.R: BPUE_real = sum(fishing$nbycatch) / dim(fishing)[1]
  BPUE_real = 0,
  
) {
  
  
  # integrate observer procedure effect
  # this is protocol influence process
  # two potential effects:
  # 1. haul level effect: unobserved bycatch when monitored
  # lack of observation
  # 2. decrease in the number of animal reported on a bycatch event
  # incomplete observation, e.g. drop out, multiple location needed # unclear
  # observation probability function post sampling
  
  #second step: variance associated with observed ID # unclear
  
  # number of metiers (n in MS)
  nmetier = length(unique(fishing$metiers))
  
  
  # pre-allocate array for result
  # if sampling stratified by metier
  if (bymetier == FALSE) { 
    
    # one row per sample
    BPUE_est = array(NA, dim = nsample)
  } else {
    
    # one row per sample, one column per metier
    BPUE_est = array(NA, dim = c(nsample, nmetier))
  }
  
  # sample / monitor the fishing data nsample times 
  for (j in 1:nsample) { 
    
    # if sampling not stratified by metier (?)  
    if (bymetier == FALSE) {
      
      # if boat_samp is FALSE, sampling occurs at the fishing event level
      # Note: what about sampling at trip level? I.e. all hauls during a fishing day?
      if (boat_samp == FALSE) {
        
        # sample monitored fishing events
        monitored = sample(
          
          # sample from row index of fishing data frame
          x = c(1:dim(fishing)[1]), # or seq(nrow(fishing))
          
          # number of items to sample:
          # number of hauls (=rows of fishing) * proportion of hauls monitored for each vessel
          n = floor(dim(fishing)[1] * pmonitor),
          
          # sample without replacement
          replace = FALSE)
        
        
        # select hauls that were sampled for monitoring
        fishing_monitored = fishing[monitored, ]
        
        # sample hauls that observer failed to monitor
        not_observed = sample(
          
          # sample from row index of monitored fishing data frame
          x = c(1:dim(fishing_monitored)[1]),
          
          # number of haul to sample: number of rows * (1 - probability haul was observed) 
          n = floor(dim(fishing_monitored)[1] * (1 - p_haul_obs)),
          
          # sample without replacement                                       
          replace = FALSE)
        
        
        # for hauls that were not observed
        # set bycatch (0/1) to 0
        fishing_monitored$bycatch[not_observed] = 0
        
        # set number of bycatch to 0
        fishing_monitored$nbycatch[not_observed] = 0
        
        # account for detection probability
        # given true size of bycatch and detection probability of each individual
        # calculate the number bycatch detected
        fishing_monitored$nbycatch = sapply(fishing_monitored$nbycatch, function(x) rbinom(1, x, detect_prob))
        
        # calculate estimated BPUE (for each monitor of the fishing data)
        # sum number of bycatch / number of events monitored
        BPUE_est[j] = (sum(fishing_monitored$nbycatch) / length(monitored))
        
        
        # when boat_samp is TRUE: first sample vessels to be monitored, then hauls
      } else {
        
        # 1. sample vessels to be monitored
        boat_sampled = sample(
          
          # sample from vessel id
          x = unique(fishing$boat),
          
          # number of vessels to sample:
          # number of vessels * Proportion of vessels monitored
          size = floor(length(unique(fishing$boat)) * p_monitor_boat),
          
          # sample without replacement
          replace = FALSE)
        
        ## think about situations when boats are never going out in a year. at the moment, the observer programme allows to react and sample
        ## only those vessels that have been fishing at least once per year.
        
        
        # 2. select vessels that accept observers 
        boat_sampled = sample(
          
          # sample from sampled vessels
          x = boat_sampled,
          
          # number of vessels to sample:
          # number of vessels * Proportion of vessels that accept (not refuse)
          size = floor(length(boat_sampled) * (1 - refusal_rate)),
          replace = FALSE)
        
        # select sampled vessels from fishing data
        fleet_sampled = fishing[fishing$boat %in% boat_sampled, ]
        
        # from the selected vessels, select hauls to be monitored
        monitored = sample(
          
          # select from row index among the sampled vessels
          c(1:dim(fleet_sampled)[1]),
          
          # number of hauls to be monitored = 
          # number of rows in data of sampled vessels * proportion of hauls monitored for each vessel
          size = floor(dim(fleet_sampled)[1] * pmonitor),
          
          replace = FALSE) # sample without replacement (default, this arg can be removed)
        
        # from data of selected vessels, select hauls to be monitored
        fishing_monitored = fleet_sampled[monitored, ]
        
        # among selects hauls determine hauls that where not observed
        not_observed = sample(
          # select from row index of hauls to be monitored
          x = c(1:dim(fishing_monitored)[1]),
          
          # number of haul to sample: number of rows * (1 - probability haul was observed) 
          size = floor(dim(fishing_monitored)[1] * (1 - p_haul_obs)))
        
        # for unobserved hauls
        # set bycatch (0/1) to 0
        fishing_monitored$bycatch[not_observed] = 0
        
        # set number of bycatch to 0
        fishing_monitored$nbycatch[not_observed] = 0
        
        # account for detection probability 
        # given true size of bycatch and detection probability of each individual
        # calculate the number bycatch detected
        fishing_monitored$nbycatch = sapply(fishing_monitored$nbycatch,function(x) rbinom(1, x, detect_prob))
        
        # calculate estimated BPUE
        # sum number of bycatch / number of events monitored
        BPUE_est[j] = (sum(fishing_monitored$nbycatch) / length(monitored))
      }
      
      
      # if sampling stratified by metier (?)  
    } else {
      
      # count number of metiers
      nmetier = length(unique(fishing$metiers)) # for error catching later on
      
      
      # if sampling occurs at the fishing event level
      if (boat_samp == FALSE) {
        
        # count number of metiers. Note: already calculated above
        nmetier = length(unique(fishing$metiers)) # for error catching later on
        
        # calculate probability that a haul in a certain metier is monitored
        # proportion of hauls monitored for each vessel * 
        # proportion of monitoring allocated to (each?) métier
        monitored_by_metier = pmonitor * p_monitor_metier
        
        
        # sample monitored fishing events of metier 1
        # note: why not loop over all metiers?
        monitored = sample(
          # sample from row index of fishing data frame
          # Note: why not sample from row numbers?
          x = as.numeric(row.names(fishing[fishing$metiers == 1, ])),
          
          # number of items to sample:
          # number of hauls ( =rows of fishing) * proportion of hauls monitored for each vessel
          # Note: redundant calculation of row indexes 
          prob = floor(length(row.names(fishing[fishing$metiers == 1, ])) * monitored_by_metier[1]))
        
        
        # for each metier, sample monitored fishing events 
        for (i in 2:nmetier) {
          
          # combine with 'monitored'
          monitored = c(
            monitored,
            
            # sample monitored fishing events
            sample(
              
              # sample from row index of fishing data frame.
              x = as.numeric(row.names(fishing[fishing$metiers == i, ])),
              
              # number of items to sample:
              # number of hauls (rows in 'fishing') * proportion of hauls monitored for each vessel
              # Note: redundant calculation of row indexes 
              size = floor(length(row.names(fishing[fishing$metiers == i, ])) * monitored_by_metier[i])
            )
          )
          
        }
        
        
        # select monitored hauls from fishing
        fishing_monitored = fishing[monitored, ]
        
        
        # among selects hauls determine hauls that where not observed
        not_observed = sample(
          
          # select from row index of hauls to be monitored
          x = c(1:dim(fishing_monitored)[1]),
          
          # number of haul to sample: number of rows * (1 - probability haul was observed) 
          size = floor(dim(fishing_monitored)[1] * (1-p_haul_obs)))
        
        # for hauls that were not observed
        # set bycatch (0/1) to 0
        fishing_monitored$bycatch[not_observed] = 0
        
        # set number of bycatch to 0
        fishing_monitored$nbycatch[not_observed] = 0
        
        
        # account for detection probability of individuals
        # given true size of bycatch and detection probability of each individual
        # calculate the number bycatch detected
        fishing_monitored$nbycatch = sapply(fishing_monitored$nbycatch, function(x) rbinom(1, x, detect_prob))
        
        
        # for each metier, calculated estimated BPUE
        # note: this could be done outside the loop
        BPUE_est[j, ] = (
          
          # calculate estimated BPUE
          # sum number of bycatch / number of events monitored
          tapply(fishing_monitored$nbycatch, fishing_monitored$metiers, sum) / 
            tapply(fishing_monitored$nbycatch, fishing_monitored$metiers, length))
        
        
        # when boat_samp is TRUE: first sample vessels to be monitored, then hauls 
      } else {
        
        # joint probability that a boat is monitored AND a metier is monitored
        # p_monitor_metier: proportion of monitoring allocated to (each?) métier
        # Note: here p_monitor_metier is recycled over p_monitor_boat.
        # will this work with p_monitor_metier as a matrix, like in 'simulation_example.R'?  
        boat_monitored_by_metier = p_monitor_boat * p_monitor_metier 
        
        
        # sample vessels to be monitored
        # 1. sample vessels to be monitored from metier 1
        # Note: this could be put in loop as well?
        boat_sampled = sample(
          
          # sample from vessel id
          x = unique(fishing$boat[fishing$metiers == 1]),
          
          # number of vessels to sample:
          # number of vessels * Proportion of vessels monitored
          size = ceiling(length(unique(fishing$boat[fishing$metiers == 1])) * boat_monitored_by_metier[1]))
        
        # for each metier, sample monitored fishing events
        for (i in 2:nmetier) {
          
          # combine with 'boat_sampled'
          boat_sampled = c(
            boat_sampled,
            
            # sample monitored boats
            sample(
              
              # sample from vessel id
              x = unique(fishing$boat[fishing$metiers == i]),
              
              # number of vessels to sample:
              # number of vessels * Proportion of vessels monitored
              size = ceiling(length(unique(fishing$boat[fishing$metiers == i])) * boat_monitored_by_metier[1]))
          )
        }
        
        
        # 2. sample vessels that accept observers 
        boat_sampled = sample(
          
          # sample from sampled vessels
          x = boat_sampled,
          
          # number of vessels to sample:
          # number of vessels * Proportion of vessels that accept (not refuse)
          size = ceiling(length(boat_sampled) * (1 - refusal_rate)))
        
        
        # select sampled vessels from fishing data
        fleet_sampled = fishing[fishing$boat %in% boat_sampled, ]
        
        
        # sample hauls
        # from the selected vessels, sample hauls to be monitored
        monitored = sample(
          
          # select from row index among the sampled vessels
          x = c(1:dim(fleet_sampled)[1]),
          
          # number of hauls to be monitored = 
          # number of rows in data of sampled vessels * proportion of hauls monitored for each vessel
          size = floor(dim(fleet_sampled)[1] * pmonitor))
        
        
        # from data of selected vessels, select hauls to be monitored
        fishing_monitored = fleet_sampled[monitored, ]
        
        
        # among selects hauls determine hauls that where not observed
        not_observed = sample(
          
          # select from row index of hauls to be monitored
          x = c(1:dim(fishing_monitored)[1]),
          
          # number of haul to sample: number of rows * (1 - probability haul was observed) 
          floor(dim(fishing_monitored)[1] * (1-p_haul_obs)))
        
        # for unobserved hauls
        # set bycatch (0/1) to 0
        fishing_monitored$bycatch[not_observed] = 0
        
        # set number of bycatch to 0
        fishing_monitored$nbycatch[not_observed] = 0
        
        
        # account for detection probability 
        # given true size of bycatch and detection probability of each individual
        # calculate the number bycatch detected
        fishing_monitored$nbycatch = sapply(fishing_monitored$nbycatch, function(x) rbinom(1, x, detect_prob))
        
        
        # calculate estimated BPUE
        # sum number of bycatch / number of events monitored
        BPUE_est[j, ] = (tapply(fishing_monitored$nbycatch, fishing_monitored$metiers, sum) / 
                           tapply(fishing_monitored$nbycatch, fishing_monitored$metiers, length))
      }
      
    } #metier else bracket
    
  } # sample iteration loop
  
  # calculate estimated BPUE
  # not stratified by metier
  if (bymetier == FALSE) {
    
    # mean
    BPUE_est_mean = mean(BPUE_est, na.rm = TRUE) #if bymetier is TRUE then 2 element vector
    
    # CV
    BPUE_est_CV = sd(BPUE_est, na.rm = TRUE) / mean(BPUE_est, na.rm = TRUE) 
    
    # stratified by metier
  } else {
    
    # mean
    BPUE_est_mean = colMeans(BPUE_est, na.rm = TRUE) #if bymetier is TRUE then 2 element vector
    
    # CV
    # loop over columns to calculate sd
    BPUE_est_CV = apply(BPUE_est, 2, function(x) sd(x, na.rm = TRUE)) / colMeans(BPUE_est, na.rm = TRUE) 
  }
  
  return(list(BPUE_est = BPUE_est_mean, CV = BPUE_est_CV))
}

