# --------------------------------------------------------------
# David Phillips
#
# 3/24/2017
# Objective function, various ways of computing accuracy
# Inputs
#    - simOut - (data.table) : output from simulate.r
#    - metric - (character) : 'aspbf_accuracy' or 'asfr_accuracy'. which metric of accuracy is desired
# Outputs
#    - accuracy (data.table) : two columns: var and accuracy
# --------------------------------------------------------------


# ---------------------------------------------
# Start function
objectiveFunction = function(simOut, metric) { 
# ---------------------------------------------
	
	# ----------------------------------------------
	# Set up R
	rm(list=ls()[!ls() %in% c('simOut', 'metric')])
	library(data.table)
	# ----------------------------------------------
	
	
	# -----------------------------------------------------------------------
	# Test inputs
	if (!'simOut' %in% ls()) stop('Must provide simOut')
	if (!'data.table' %in% class(simOut)) stop('simOut must be a data.table') 
	# -----------------------------------------------------------------------
	
	
	# -----------------------------------------------------------
	# Input Files
	popFile = './Data/Country_Data/WPP_Female_Pop_Estimates.csv'
	# -----------------------------------------------------------
	
	
	# --------------------------------------------------------------
	# ASPF accuracy function
	aspbfAccuracy = function(var, simOut) {
		min_fraction = min(simOut$aspbf)
		error = abs(simOut$aspbf - simOut[[var]])
		accuracy = 1 - (sum(error)/(2*(1-min_fraction))) # maybe 1* not 2?
		return(accuracy)
	}
	# ---------------------------------------------------------------
	
	
	# --------------------------------------------------------------
	# ASFR accuracy function
	asfrAccuracy = function(var, simOut) {
		truth = simOut[, list(births=sum(births)), by='age']
		truth = merge(truth, pop, by='age')
		truth[, asfr:=births/pop]
		est = copy(simOut)
		est[, births:=sum(births)*get(var)]
		est = est[, list(births=sum(births)), by='age']
		est = merge(est, pop, by='age')
		est[, asfr:=births/pop]
		truth = truth$asfr/sum(truth$asfr)
		est = est$asfr/sum(est$asfr)
		min_fraction = min(truth) # could probably refactor aspbfAccuracy and reuse it here
		error = abs(truth - est)
		accuracy = 1 - (sum(error)/(2*(1-min_fraction)))
		return(accuracy)
	}
	# ---------------------------------------------------------------
	
	
	# ------------------------------------------------------------------------------------
	# Compute accuracy for all aspbf_ variables
	
	# load/prep female pop if asfr_accuracy
	if (metric=='asfr_accuracy') {
		pop = fread(popFile)
		pop = pop[as.numeric(year)==2015]
		pop[, age:=as.numeric(age)]
		pop[, pop:=as.numeric(pop)]
	}
	
	# get list of variables
	vars = names(simOut)[grepl('aspbf_', names(simOut))]	
	
	# apply function
	if (metric=='aspbf_accuracy') accuracy_estimates = sapply(vars, aspbfAccuracy, simOut)
	if (metric=='asfr_accuracy') accuracy_estimates = sapply(vars, asfrAccuracy, simOut)
	
	# format
	accuracy = data.table(level=gsub('aspbf_', '', vars), accuracy=accuracy_estimates)
	# ------------------------------------------------------------------------------------
	
	
	# ---------------
	# Return output
	return(accuracy)
	# ---------------
	
# -------------
# End function
}
# -------------
