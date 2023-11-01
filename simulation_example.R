# this code gives an example of how the functions can be used to simulate fishing and monitoring (here 100 independent years simulated each monitored 1000 times)

# 100 fishing years are created
# To each fishing year, a simulated monitoring program is applied 1000 times 


# Set parameter values for simulation functions

# 1. make_fishing_year_metier.R
# create a virtual fishing year


# 2. monitor_BPUE_metier.R
# simulate sampling in a bycatch monitoring program  


# (3. estimate_fishing_effort_metier.R)
# estimates the fishing effort of the simulated fishing year
# This function is not described explicitly in in WKPETSAMP MS ('Report headings participants and text written 20231013 ... docx'), but see Figure 1: Schematic of simulation framework, Effort reporting. 
# A function with the same name appears below, but the code is commented away.


# For a description of parameters*, 'see simulation_parameters.xlsx'
# *so far only make_fishing_year_metier.R and monitor_BPUE_metier.R


# Note:
# reordered functions: first fishing year, then monitoring
# reordered arguments according to simulation_parameters.xlsx



# bymetier<-c("TRUE","FALSE")
# boat_samp<-c("TRUE","FALSE")

# ###
# pmonitor_boatFALSE<-c(.9,.5,.1,.05,.01,.001)




# Argument values for make_fishing_year_metier.R ----

# Note: may need to set additional, case-specific arguments

# p.metier (p_metier)
# probability (proportion) of vessels belonging each metier
# See also 'SCOTI extension...docx', Table 1

# matrix:
# one row per set of proportions
# one column per metier, here two.
p.metier = cbind(
  c(0.5, 0.2, 0.8),
  c(0.5, 0.8, 0.2))



# p.bycatch (p_bycatch_event)
# probability of bycatch event by haul and metier

# matrix
# one row per set of proportions
# one column per metier, here two.
p.bycatch = cbind(
  c(0.1, 0.01, 0.001, 0.001, 0.001),
  c(0.2, 0.02, 0.002, 0.01, 0.1))




# Argument values for monitor_BPUE_metier -----

# Note: may need to set additional, case-specific arguments


# p_monitor_boat_boatTRUE
# ~p_monitor_boat; assigned to p_monitor_boat (proportion of vessels monitored) below
# vector, one value per proportion
# Note: name could be improved
p_monitor_boat_boatTRUE = c(0.01, 0.05, 0.1, 0.2)
# pmonitor_boatFALSE<-c(.001,.01,.05,.1,.5,.9)



# pmonitor_boatTRUE
# ~pmonitor; assigned to pmonitor (proportion of hauls monitored for each vessel) below
# vector, one value per proportion
# Note: name could be improved
pmonitor_boatTRUE = c(0.01, 0.05, 0.1, 0.2)


# p_monitor_metier (p_sample_metier)
# set proportion of monitoring allocated to each of two metiers
# matrix, with one column per metier


# Note: shouldn't the proportions sum to one? See e.g. row 1 and 4
# But: Row one (1, 1) is used when if sampling _not_ stratified by metier:
# if(bymetier == FALSE); temp$p_monitor_metier = p_monitor_metier[p_m_m, 1] 
# See also Note in monitor_BPUE_metier.R, where the argument is a scalar

# matrix
# one row per set of proportions
# one column per metier, here two.

p_monitor_metier = cbind(
  c(1, 0.8, 0.2, 0.2),
  c(1, 0.2, 0.8, 0.2))


# bymetier (strat_metier)
# sampling stratified by metier?
# Note: why is argument provided as a character?
bymetier = "FALSE"


# boat_samp (strat_boat)
# if FALSE: sampling occurs at the fishing event level (haul);
# if TRUE: first sample vessels to be monitored, then sample 
# Note: why is argument provided as a character?

boat_samp = "TRUE"






# Note: unclear chunk.
# Lines commented by David
# See also separate file for function with the same name: estimate_fishing_effort_metier.R
# However, unclear how/where the function is used. Not in this file.

#estimate_fishing_effort_metier<-function(fishing=NA,p_report=.9,bymetier=FALSE,metierlevel=TRUE) {

#monitor_BPUE_metier<-function(pmonitor=0.5,nsample=1000,BPUE_real=0,fishing=NA, p_monitor_boat=.1,boat_samp=TRUE,
#p_haul_obs=1,detect_prob=1,refusal_rate=0, misclassification=0, bymetier=FALSE, p_monitor_metier=.5)

#make_fishing_year_metier<-function(mean.bycatch.event=1,mean.bycatch.large.event=20,p.large.event=0.01,
#							nboat=100,mean.fishing.event.boat.day=2,p.bycatch=c(0.1,.01),p.metier=c(.2,.8),stochastic=TRUE)


#fishing1<-make_fishing_year_metier(p.bycatch=p.bycatch[1,],p.metier=p.metier[1,])

#BPUE_real<-sum(fishing1$nbycatch)/dim(fishing1)[1]

#effort<-estimate_fishing_effort_metier(fishing=fishing1,p_report=p_report[1,],bymetier=TRUE,metierlevel=TRUE)


#monitoring<-monitor_BPUE_metier(pmonitor=pmonitor_boatTRUE[3],nsample=1000,BPUE_real=BPUE_real,fishing=fishing1, 
#p_monitor_boat=p_monitor_boat_boatTRUE[2],boat_samp=TRUE,p_haul_obs=1,detect_prob=1,refusal_rate=0, misclassification=0, bymetier=TRUE, p_monitor_metier=p_monitor_metier[1,])



# Arguments of estimate_fishing_effort_metier ------


# p_report
# Note: argument is used in estimate_fishing_effort_metier function above
# However, lines are commented and the resulting object 'effort' does not seem to be used
# also used in function with the same name in estimate_fishing_effort_metier.R

# matrix:
# one row per set of proportions
# one column per metier, here two

p_report = cbind(
  c(1, 0.8, 0.2, 0.2),
  c(1, 0.2, 0.8, 0.2))







# set up data frame to collect parameter values and results simulation  ----------

# Note: may need to add names of additional case-specific arguments


monitor_estimate = data.frame(
  
  # fishing parameters
  # fishing year
  year = NA,
  
  # probability of bycatch event by haul and metier
  # One column per metier, hardcoded, in p.bycatch matrix
  p_bycatch_1 = NA,
  p_bycatch_2 = NA,
  
  # probability (proportion) of vessels belonging each metier. One column per metier, hardcoded, in p.bycatch matrix 
  p_metier_1 = NA,
  p_metier_2 = NA,
  
  
  # sampling effort
  # proportion of vessels monitored
  p_monitor_boat = NA,
  
  # proportion of hauls monitored for each vessel
  pmonitor = NA,


  # sampling design
  # p_monitor_metier: proportion of monitoring allocated to (each?) métier
  p_monitor_metier = NA,
  
  # bymetier; is sampling stratified by metier?
  bymetier = NA,
  
  # boat_samp
  # if FALSE: sampling occurs at the fishing event level (haul); if TRUE: first sample vessels to be monitored, then sample hauls
  boat_samp = NA,
  
  
  # BPUE
  # sum number of bycatch individuals divided by number of hauls = number of rows in 'fishing'; see below
  BPUE_real = NA,
  
  # BPUE_est
  # sum number of bycatch / number of events monitored
  BPUE_est = NA,
  
  # BPUE_est_CV
  # sd(BPUE_est) / mean(BPUE_est) 
  BPUE_est_CV = NA)


# seq variables used in loop

# b: vector of row indices to select from p.bycatch (p_bycatch_event)
b = 1 # 1 to 5

# m: vector of row indices to select from p.metier (p_metier)
m = 1 # 1 to 3

# currently, we don't loop over rows of p.bycatch and p.metier




# Below we loop over:
# y: vector of fishing years
# pmonitor_boatTRUE (i.e. pmonitor, proportion of hauls monitored for each vessel)
# p_monitor_boat_boatTRUE (i.e. p_monitor_boat, proportion of vessels monitored)
# p_monitor_metier (p_sample_metier; proportion of monitoring allocated to each métier)

# Note: may need to loop over additional case-specific arguments


# y: vector of fishing years
# create 100 fishing years, i.e. run function make_fishing_year_metier 100 times
for (y in 1:100) {
  
  #iter<-1
  #for (m.s in 1:2) {
  
  #for (b.s in 1:2) {
  
  # for each row in p.bycatch matrix
  #for (b in 1:dim(p.bycatch)[1]) {
  
  # for each row in p.metier matrix
  #for (m in 1:dim(p.metier)[1]) {
 
   
  # create fishing year ---------
  
  # set p.catch and p.metier, using single values of b and m
  
  # Note: may need to set values of additional case-specific arguments
  # although defaults values may be fine, it may be better to be explicit here 
  fishing = make_fishing_year_metier(
    
    # select a row from p.metier
    # Note: assign the selection to an object with the same name?
    p.metier = p.metier[m, ],
    
    # select a row from p.bycatch 
    # Note: assign the selection to an object with the same name?
    p.bycatch = p.bycatch[b, ]
    )
  
  
  # calculate BPUE_real
  # sum number of bycatch individuals divided by number of hauls = number of rows in 'fishing'
  # Note: perhaps nrow instead of dim(...)[1]
  BPUE_real = sum(fishing$nbycatch) / dim(fishing)[1]
  
  print("fishing done")
  flush.console()
  
  
  # for each value in pmonitor_boatTRUE (i.e. pmonitor, proportion of hauls monitored for each vessel)
  # Note: think about argument name; see above 
  # Note: here and in other places: seq_along may be safer than 1:length 
  # https://r4ds.had.co.nz/iteration.html#for-loops
  
  for (pm.b in 1:length(pmonitor_boatTRUE)) {
    
    # for each value in p_monitor_boat_boatTRUE (i.e. p_monitor_boat, proportion of vessels monitored)
    # Note: think about argument name; see above
    for (p_m.b in 1:length(p_monitor_boat_boatTRUE)) {
      
      # for each row in p_monitor_metier (p_sample_metier; proportion of monitoring allocated to each métier)
      for (p_m_m in 1:dim(p_monitor_metier)[1]) {
        
        # if sampling not stratified by metier
        if(bymetier == FALSE) {
          
          
          # set up empty data frame to collect parameter values of monitoring and corresponding BPUE_est
          # Note 1: may need additional, case-specific arguments used in make_fishing_year or monitor_BPUE
          
          # Note 2: ordered variables according to simulation_parameters.xlsx
          # i.e. properties of fishing, bycatch, sampling, monitoring
         
          
          temp = data.frame(
            
            # fishing year. Here 100 years are created  
            year = y,
            
            
            # fishing
            
            # p_metier_ (p.metier in make_fishing; probability (proportion) of vessels belonging each metier)
            # one column for each metier, hard-coded
            p_metier_1 = NA,
            p_metier_2 = NA,
            
            # p_bycatch_ (p.bycatch in make_fishing; probability of bycatch event by haul and metier)
            # one column for each metier, hard-coded
            p_bycatch_1 = NA,
            p_bycatch_2 = NA,
            
            
            # sampling effort
        
            # proportion of vessels monitored
            p_monitor_boat = NA,
            
            # proportion of hauls monitored for each vessel
            pmonitor = NA,
            
            # proportion of monitoring allocated to each métier
            p_monitor_metier = NA,
            
            
            # sampling design
            
            # bymetier; is sampling stratified by metier?
            bymetier = NA,
            
            # boat_samp; if FALSE: sampling occurs at the fishing event level (haul);
            # if TRUE: first sample vessels to be monitored, then sample hauls
            boat_samp = NA,
            
            
            # BPUE, real and estimated
            BPUE_real = NA,
            BPUE_est = NA,
            BPUE_est_CV = NA)
          
          
          # add parameter values to temp data in each iteration
          # Note: may need to add values of additional case-specific arguments
          
          
          # fishing
          
          # select values from p.metier (matrix; one row per set of proportions; one column per metier, here two)
          # currently, we don't loop over rows; m is set to 1 above
          temp$p_metier_1 = p.metier[m, 1]
          temp$p_metier_2 = p.metier[m, 2]
          
          
          # bycatch
          
          # select values from p.bycatch (matrix; one row per set of proportions; one column per metier, here two)
          # currently, we don't loop over rows; b is set to 1 above
          temp$p_bycatch_1 = p.bycatch[b, 1]
          temp$p_bycatch_2 = p.bycatch[b, 2]
          
         
          # sampling effort
          
          temp$p_monitor_boat = p_monitor_boat_boatTRUE[p_m.b]
          temp$pmonitor = pmonitor_boatTRUE[pm.b]
          
          
          # sampling design
          
          # Row one of p_monitor_metier (1, 1) is used when sampling not stratified by metier:
          temp$p_monitor_metier = p_monitor_metier[p_m_m, 1]
          temp$bymetier = bymetier 
          temp$boat_samp = boat_samp
          
          # BPUE
          temp$BPUE_real = BPUE_real
         
          # if sampling is stratified by metier (here: two metiers) 
        } else {
          
          
          # set up empty data frame to collect parameter values of monitoring and corresponding BPUE_est
          temp = data.frame(
            
            # one row per fishing year and metier (here: 2)
            year = rep(y, 2),
            
            
            # fishing
            
            # p_metier_ (p.metier in make_fishing; probability (proportion) of vessels belonging each metier)
            # one column for each metier, hard-coded
            p_metier_1 = NA,
            p_metier_2 = NA,
            
            # p_bycatch_ (p.bycatch in make_fishing; probability of bycatch event by haul and metier)
            # one column for each metier, hard-coded
            p_bycatch_1 = NA,
            p_bycatch_2 = NA,
            
            
            # sampling effort
            
            # proportion of vessels monitored
            p_monitor_boat = NA,
            
            # proportion of hauls monitored for each vessel
            pmonitor = NA,
            
            # proportion of monitoring allocated to each métier
            p_monitor_metier = NA,
            
            
            # sampling design
            
            # bymetier; is sampling stratified by metier?
            bymetier = NA,
            
            # boat_samp; if FALSE: sampling occurs at the fishing event level (haul);
            # if TRUE: first sample vessels to be monitored, then sample hauls
            boat_samp = NA,
           
            # BPUE
            BPUE_real = NA,
            BPUE_est = NA,
            BPUE_est_CV = NA)
          
          
          # add parameter values to tmp data
          # add parameter values to temp data in each iteration
          # Note: may need to add values of additional case-specific arguments
          
          
          # fishing
          
          # select values from p.metier (matrix; one row per set of proportions; one column per metier, here two)
          # currently, we don't loop over rows; m is set to 1 above
          temp$p_metier_1 = p.metier[m, 1]
          temp$p_metier_2 = p.metier[m, 2]
          
          
          # bycatch
          
          # select values from p.bycatch (matrix; one row per set of proportions; one column per metier, here two)
          # currently, we don't loop over rows; b is set to 1 above
          temp$p_bycatch_1 = p.bycatch[b, 1]
          temp$p_bycatch_2 = p.bycatch[b, 2]
          
          
          # sampling effort
          
          temp$p_monitor_boat = p_monitor_boat_boatTRUE[p_m.b]
          temp$pmonitor = pmonitor_boatTRUE[pm.b]
          
          
          # sampling design
          
          temp$p_monitor_metier = p_monitor_metier[p_m_m, ]
          
          temp$boat_samp = boat_samp
          temp$bymetier = bymetier
          
          
          # BPUE
          temp$BPUE_real = BPUE_real
          
        }
        
        
        # apply bycatch monitoring to fishing data ---------
        monitoring = monitor_BPUE_metier(
          pmonitor = pmonitor_boatTRUE[pm.b],
          nsample = 1000,
          BPUE_real = BPUE_real,
          fishing = fishing,
          p_monitor_boat = p_monitor_boat_boatTRUE[p_m.b],
          boat_samp = boat_samp,
          p_haul_obs = 1,
          detect_prob = 1,
          refusal_rate = 0,
          misclassification = 0,
          bymetier = bymetier,
          p_monitor_metier = p_monitor_metier[p_m_m, ])
        
        
        # assign BPUE_est to tmp
        temp$BPUE_est = monitoring$BPUE_est
        temp$BPUE_est_CV = monitoring$CV
        
        
        # for each iteration, row bind result
        # Note: maybe pre-allocate data frame, instead of growing in each iteration
        monitor_estimate = rbind(monitor_estimate,
                                 temp)
        
        # print(b)
        # print(m)
        # flush.console()
      } #p_m_m
    } # p_m.b
  } # pm.b
  #} #m
  #} #b
  #} #b.s
  #} #m.s 
  
  print(y)
  flush.console()
} #y

