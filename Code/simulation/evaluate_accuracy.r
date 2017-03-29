# --------------------------------------------------------------------
# David Phillips
#
# 3/24/2017
# Function that evaluates all simulations using an objective function
# Inputs:
# - toRun (character) : vector specifying which simulations to evaluate (all is allowed)
# - objective (character) : objective function to use (see objective_functions.r)
# Outputs:
# - nothing. saves to outFile
# Working directory should be VSPI_Births/
# --------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Start function
evalSims = function(toRun=c('age', 'sex', 'parity'), objective='aspbf_accuracy') { 
# --------------------------------------------------------------------------------
	
	# -------------------------------
	# Set up R
	args = c('toRun', 'objective')
	rm(list=ls()[!ls() %in% c('args', args)])
	library(data.table)
	# -------------------------------
	
	
	# -----------------------------------------------------------------------
	# Handle inputs
	for(a in args) { 
		if (!a %in% ls()) stop(paste('Must provide', a))
		if (class(get(a))!='character') stop(paste0(a, 'must be character'))
	}
	if ('all' %in% toRun) toRun = c('age', 'sex', 'parity', 'completeness')
	# -----------------------------------------------------------------------
	
	
	# ----------------------------------------------------------------
	# Directories, files and functions
	
	# input files
	inDir = paste0('./Data/Simulation_Outputs/')
	ageFile = paste0(inDir, 'age_simulations.csv')
	sexFile = paste0(inDir, 'sex_simulations.csv')
	parityFile = paste0(inDir, 'parity_simulations.csv')
	completenessFile = paste0(inDir, 'completeness_simulations.csv')
	
	# output file
	outFile = paste0(inDir, 'accuracy_estimates.csv')
	
	# load function
	source('./Code/simulation/objective_functions.r')
	# ----------------------------------------------------------------
	
	
	# -----------------------------------------
	# Load simulations
	for(t in toRun) {
		name = paste0(t, 'Sim')
		tmpSim = fread(get(paste0(t, 'File')))
		assign(name, tmpSim)
	}
	# -----------------------------------------
	
	
	# --------------------------------------------------------------------------
	# Evaluate simulations
	i=1
	for(t in toRun) {
		name = paste0(t, 'Sim')
		tmpAccuracy = objectiveFunction(get(name), objective)
		setnames(tmpAccuracy, 'accuracy', t)
		if (i==1) accuracy = tmpAccuracy
		if (i>1) accuracy = merge(accuracy, tmpAccuracy, by='level', all=TRUE)
		i=i+1
	}
	# --------------------------------------------------------------------------
	
	
	# --------------------------------------------
	# Save
	write.csv(accuracy, outFile, row.names=FALSE)
	# --------------------------------------------
	
# ------------
# End function
}
# ------------
