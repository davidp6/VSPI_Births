# -------------------------------------------------------------------------
# David Phillips
#
# 3/21/2016
# Function that takes simulation population and sampling probabilities and 
# simulates varying levels of data quality for a specified indicator
# Inputs
#    - popFile (character): name of file with simulation populations
#    - probFile (character): name of file with simulation probabilities
#							must contain a variable matching `indicator`
#    - indicator (character): name of indicator being simulated
# Outputs
#    - out (class): description
# The current working directory should be one above Data/Simulation_Inputs/
# -------------------------------------------------------------------------


# ---------------------------------------------------------------
# Start function
simulate = function(popFile='simulation_population.csv', 
					probFile='unspecified_age_probabilities.csv', 
					indicator='unspecified_age') { 
# ---------------------------------------------------------------
		
	# ------------------------------------------
	# Set up R
	args = c('popFile', 'probFile', 'indicator')
	rm(list=ls()[!ls() %in% c(args, 'args')])
	library(data.table)
	# ------------------------------------------
	
	
	# -----------------------------------------------------------------------
	# Test inputs
	for(a in args) { 
		if (!a %in% ls()) stop(paste('Must provide', a))
		if (class(get(a))!='character') stop(paste0(a, 'must be character'))
	}
	# -----------------------------------------------------------------------
	
	
	# -----------------------------------------------------------
	# Files/directories/lists
	
	# root directory
	root = './Data/Simulation_Inputs/'
	
	# input files
	inFile1 = paste0(root, popFile) # population
	inFile2 = paste0(root, probFile) # probabilities
	
	# output file
	outFile = paste0(root, paste0(indicator, '_simulation.csv'))
	# -----------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
	# Load/prep data
	
	# population
	pop = fread(inFile1)
	
	# probabilities
	probs = fread(inFile2)
	
	# collapse out (from the populations) the variable being simulated if necessary
	byVars = names(pop)[names(pop) %in% names(probs)]
	pop = pop[, list(births=sum(births)), by=byVars]
	
	# rescale probabilities to sum to 1
	probs[, rescaled:=get(indicator)/sum(get(indicator))]
	
	# compute ASPBF: age-sex-parity birth fraction
	pop[, aspbf:=births/sum(births)]
	
	# test fractions
	if (round(sum(probs$rescaled),9)!=1) stop('Simulation probabilities don\'t sum to 1!') 
	if (round(sum(pop$aspbf),9)!=1) stop('Simulation ASPBFs don\'t sum to 1!') 
	
	# make sure pop and probs are in the same order/same length
	simData = merge(pop, probs, by=byVars)
	# -------------------------------------------------------------------------
	
	
	# ----------------------------------------------------------------------------
	# Define core simulation function
	# Inputs:
	# 	- l (numeric) : 1-proportion of births sampled, i.e. "loss"
	# 	- pop (data.table) : population
	# 	- probs (data.table) : probabilities
	# Output:
	# 	- new_aspbf (numeric vector) : simulated aspbf's under scenario of l loss
	simFunc = function(l, simData) {
		simData[, new_births:=births - (rescaled*(l*sum(births)))]
		new_births = simData$new_births
		new_births[new_births<0] = 0
		new_aspbf = new_births/sum(new_births)
		return(new_aspbf)
	}
	# ----------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
	# Simulate
	
	# increment vector
	b = .01
	inc = seq(b, 1, by=b)
	
	# apply simulation
	simOut = sapply(inc, simFunc, simData)
	# -------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------
	# Set up to return output
	
	# merge "truth" to simulated
	simOut = cbind(pop, simOut)
	
	# set names
	setnames(simOut, paste0('V', seq(length(inc))), paste0('aspbf_',inc))
	# --------------------------------------------------------------------
	
	
	# --------------
	# Return output
	return(simOut)
	# --------------
	
# -------------
# End function
}
# -------------
