# ----------------------------------------------------------------------------------------
# David Phillips
#
# 3/23/2017
# Function that runs all simulations
# Inputs:
# - toRun (character) : vector specifying which simulations to evaluate (all is allowed)
# Outputs:
# - nothing. saves to ageFile, sexFile, parityFile and/or completenessFile
# ----------------------------------------------------------------------------------------


# --------------------------------
# Start function
runSims = function(toRun='all') { 
# --------------------------------
	
	# --------------------------
	# Set up R
	rm(list=ls()[ls()!='toRun'])
	library(data.table)
	# --------------------------
	
	
	# ---------------------------------------------------------------------------------
	# Handle inputs
	if (class(toRun)!='character') stop('Specify which simulations to run with toRun!')
	if ('all' %in% toRun) toRun = c('age', 'sex', 'parity', 'completeness')
	# ---------------------------------------------------------------------------------
	
	
	# ----------------------------------------------------------------
	# Directories, files and functions
	
	# output files
	outDir = paste0('./Data/Simulation_Outputs/')
	ageFile = paste0(outDir, 'age_simulations.csv')
	sexFile = paste0(outDir, 'sex_simulations.csv')
	parityFile = paste0(outDir, 'parity_simulations.csv')
	completenessFile = paste0(outDir, 'completeness_simulations.csv')
	
	# load function
	source('./Code/simulation/simulate.r')
	# ----------------------------------------------------------------
	
	
	# ----------------------------------------------------------------------
	# Run simulations
	
	# age
	if ('age' %in% toRun) {
		ageSim = simulate(popFile='simulation_population.csv', 
						probFile='unspecified_age_probabilities.csv', 
						indicator='unspecified_age')
	}
	
	# sex
	if ('sex' %in% toRun) {
		sexSim = simulate(popFile='simulation_population.csv', 
						probFile='unspecified_sex_probabilities.csv', 
						indicator='unspecified_sex')
	}
	
	# parity
	if ('parity' %in% toRun) {
		paritySim = simulate(popFile='simulation_population.csv', 
						probFile='unspecified_parity_probabilities.csv', 
						indicator='unspecified_parity')
	}
	
	# completeness
	if ('completeness' %in% toRun) {
		completenessSim = simulate(popFile='simulation_population.csv', 
						probFile='completeness_probabilities.csv', 
						indicator='completeness')
	}
	# ----------------------------------------------------------------------
	
	
	# ------------------------------------------------------------------------------------------
	# Save
	if ('age' %in% toRun) write.csv(ageSim, ageFile, row.names=FALSE)
	if ('sex' %in% toRun) write.csv(sexSim, sexFile, row.names=FALSE)
	if ('parity' %in% toRun) write.csv(paritySim, parityFile, row.names=FALSE)
	if ('completeness' %in% toRun) write.csv(completenessSim, completenessFile, row.names=FALSE)
	# ------------------------------------------------------------------------------------------
	
# ------------
# End function
}
# ------------
