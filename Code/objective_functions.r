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


# ------------------------------------------
# Start function
objectiveFunction = function(simOut, ) { 
# ------------------------------------------
	
	# -----------------------------
	# Set up R
	rm(list=ls()[ls()!='simOut'])
	library(data.table)
	# -----------------------------
	
	
	# -----------------------------------------------------------------------
	# Test inputs
	if (!'simOut' %in% ls()) stop('Must provide simOut')
	if (!'data.table' %in% class(simOut)) stop('simOut must be a data.table') 
	# -----------------------------------------------------------------------
	
	
	# --------------------------------------------------------------
	# ASPF accuracy function
	aspbfAccuracy = function(var, simOut) {
		min_error = min(simOut$aspbf)
		abs_error = abs(simOut$aspbf - simOut[[var]])
		accuracy = 1 - (sum(abs_error)/(2*(1-min_error))) # maybe 1* not 2?
		return(accuracy)
	}
	# ---------------------------------------------------------------

	
	# -----------------------------------------------------------
	# Compute accuracy for all aspbf_ variables
	
	# get list of variables
	vars = names(simOut)[grepl('aspbf_', names(simOut))]	
	
	# apply function
	accuracy_estimates = sapply(vars, accFunc, simOut)
	
	# format
	accuracy = data.table(level=gsub('aspbf_', '', vars),
									accuracy=accuracy_estimates)
	# -----------------------------------------------------------
	
	
	# ---------------
	# Return output
	return(accuracy)
	# ---------------
	
# -------------
# End function
}
# -------------
