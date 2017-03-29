# --------------------------------------------------------------
# David Phillips
#
# 3/24/2017
# Objective function, various ways of computing accuracy
# Inputs
#    - sims - (data.table) : output from simulate.r
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
	
	
	# --------------------------------------------------------------
	# ASPF accuracy function
	aspbfAccuracy = function(var, simOut) {
		min_fraction = min(simOut$aspbf)
		error = simOut$aspbf - simOut[[var]]
		accuracy = 1 - (sum(error)/(2*(1-min_fraction))) # maybe 1* not 2?
		return(accuracy)
	}
	# ---------------------------------------------------------------
	
	
	# ------------------------------------------------------------------------------------
	# Compute accuracy for all aspbf_ variables
	
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
