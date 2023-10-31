# copied  from https://github.com/dlusseau/scoti/blob/main/make_fishing_year_metier.r

# add annotations and code formatting
# some description is added from 'SCOTI extension at WKPETSAMP2.docx', henceforth MS


# function to create a virtual fishing year  

# vessels
# fishing days
# fishing events (hauls)
# bycatch events
# number of individuals (normal / large events)



# parameters:
# nboat: number of vessels (n in MS) in fleet
# mean.fishing.event.boat.day: mean number of fishing events per vessel and day (fd in MS)
# p.metier: probability (proportion) of vessels belonging each metier (here: 2 metiers; p_metier in MS) 

# p.bycatch: probability of bycatch event for each metier (p_bycatch in MS)
# p.large.event: given that a bycatch event occurs, the probability of a large bycatch event (p_(bycatch=large|〖bycatch〗_(i,d,h)=1) in MS)
# mean.bycatch.event: given that a bycatch event occurs and that it is not a large event, the mean number of individuals caught at a 'normal' bycatch event (λ_bycatch in MS)
# mean.bycatch.large.event: given that a bycatch event occurs and that it is a large event, the mean number of individuals caught at a large bycatch event (λ_(bycatch,large) in MS)

# stochastic: vessel-specific mean number of events per boat, drawn from rtpois

make_fishing_year_metier<-function(
    nboat = 100,
    mean.fishing.event.boat.day = 2,
    p.metier = c(0.2, 0.8),
    p.bycatch = c(0.1, 0.01),
    p.large.event = 0.01,
    mean.bycatch.event = 1,
    mean.bycatch.large.event = 20,
    stochastic = TRUE) {
  
  #p.metier is the proportion of vessel in the, here, length of p.bycatch metiers 
  # p bycatch event alternative distribution particularly for low density species
  require(extraDistr)
  
  # number of metiers (n in MS)
  nmetier = length(p.bycatch) 
  
  # Day of year
  fishing.day = 1:365
  
  # Vessel IDs (vessel i in MS)
  fleet = 1:nboat
  
  # create a vector of metiers, one for each _vessel_
  # Note: should metier be a property of the vessel? Maybe rather assigned at haul level?
  
  # sample one metier per vessel
  # size: number of boats, nboats
  # prob: probability weights from p.metier
  metier = sample(1:nmetier, size = nboat, replace = TRUE, prob = p.metier) #here we deal with probability of metier, not proportion of metier hence sampling with replacement
  
  # create vessel-specific mean number of fishing event, lambda
  if (stochastic == TRUE) { # or if(stochastic)
    
    # here number of hauls is still not associated to "high res" metier
    # introduce stochasticity so that the mean number of events per boats vary, using rtpois
    # note: rather assign result to a vector with a different name than the name of the parameter value for lambda
    mean.fishing.event.boat.day <- rtpois(n = nboat,
                                          lambda = mean.fishing.event.boat.day,# n_event_fleet
                                          a = 0)
    
    # number of fishing events per boat, using vessel-specific lambda  
    fishing.event.per.boat <- rpois(n = nboat,
                                    lambda = mean.fishing.event.boat.day)
    
    
    # use a single lambda for mean number of fishing event for the entire fleet 
  } else {
    
    
    fishing.event.per.boat <- rpois(nboat, mean.fishing.event.boat.day) # uniform fishing behaviour
    
  }
  
  # create bycatch data
  # one row per vessel, day, and fishing event
  
  # initiate data on day 1
  # unclear why day 1 is calculated outside the loop over days, and then the same calculations are repeated in the loop
  i = 1
  
  fishing = data.frame(
    # day of year
    fishing.day = fishing.day[i],
    
    # vessel ID: repeat each vessel ID with the number of fishing events per boat and day
    boat = rep(fleet, fishing.event.per.boat),
    
    # metier: repeat each (vessel-specific) metier with the number of fishing events per boat and day
    metiers = rep(metier, fishing.event.per.boat),
    
    # bycatch: For each haul, determine if bycatch occurred
    # sample from a binomial distribution, 0/1
    # n: each fishing event (sum(fishing.event.per.boat)), 
    # prob: metier-specific bycatch probabilities, p.bycatch,
    # indexed by vessel-specific metiers repeated by number of fishing events per vessel  
    bycatch = rbinom(n = sum(fishing.event.per.boat), size = 1,
                     prob = p.bycatch[rep(metier, fishing.event.per.boat)]),
    
    # initiate bycatch column
    nbycatch = 0)
  
  # for hauls with bycatch (bycatch = 1),
  # determine if a 'normal' (0) or a 'large' (1) bycatch event using rbinom
  # n: the number of fishing events with bycatch (sum(fishing$bycatch))
  # prob: probability of a large event
  event.type = rbinom(n = sum(fishing$bycatch), size = 1,
                      prob = p.large.event)
  
  # for hauls with bycatch, determine number of individuals
  # from either a normal or a large bycatch event
  fishing$nbycatch[fishing$bycatch == 1] <- apply(
    cbind(
      # if event.type = 0 (normal bycatch), 
      (1 - event.type) * rtpois(n = sum(fishing$bycatch), lambda = mean.bycatch.event, a = 0), 
      
      # if event.type = 1 (large bycatch),
      event.type * rtpois(n = sum(fishing$bycatch), lambda = mean.bycatch.large.event, a = 0)),
    1, max)
  # note 1: it seems redundant to calculate both number of bycatch for a normal and a large event
  # and then use (slow) apply to select the non-zero result of the two rtpois
  
  
  # for each days in a year..
  # note: growing data in the loop. 
  for (i in 2:365) {
    
    # calculate number of hauls per boat
    if (stochastic == TRUE) { # or if(stochastic)
      # using vessel-specific lambda
      
      #mean.fishing.event.boat.day<-rtpois(nboat,mean.fishing.event.boat.day,a=0) #that's stays the same for the whole year #introduce stochasticity so that the mean number of events per boats vary
      
      # number of fishing events per boat, using vessel-specific lambda  
      fishing.event.per.boat = rpois(n = nboat,
                                     lambda = mean.fishing.event.boat.day)
    } else {
      # using a single lambda for the entire fleet
      
      fishing.event.per.boat = rpois(n = nboat,
                                     lambda = mean.fishing.event.boat.day)
    }
    
    # populate data frame
    temp = data.frame(
      # day of year
      fishing.day = fishing.day[i],
      
      # vessel ID: repeat each vessel ID with the number of hauls per boat and day
      boat = rep(fleet, fishing.event.per.boat),
      
      # metier: repeat each (vessel-specific) metier with the number of hauls per boat and day
      metiers = rep(metier, fishing.event.per.boat),
      
      # For each haul, determine if bycatch occurred
      bycatch = rbinom(n = sum(fishing.event.per.boat), 1,
                       prob = p.bycatch[rep(metier,fishing.event.per.boat)]),
      nbycatch = 0)
    
    # for hauls with bycatch (bycatch = 1),
    # determine if a 'normal' (0) or a 'large' (1) bycatch event using rbinom
    event.type <- rbinom(sum(temp$bycatch), 1, p.large.event)
    
    temp$nbycatch[temp$bycatch == 1] <- apply(
      cbind((1 - event.type) * rtpois(sum(temp$bycatch), mean.bycatch.event, a = 0),
            event.type * rtpois(sum(temp$bycatch), mean.bycatch.large.event, a = 0)),
      1, max)
    
    fishing <- rbind(fishing, temp)
    
  }
  #########
  ## so for this challenge we need to change the computation of the estimated total bycatch it becomes the estimated BPUE x estimated effort
  return(fishing)
}
