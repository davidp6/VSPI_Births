# ----------------------------------------
# David Phillips
#
# 3/23/2017
# Function that runs all simulations
# Working directory should be VSPI_Births/
# ----------------------------------------


# ---------------------------------------------------------------
# Start function
simulate = function(toRun=c('age', 'sex', 'parity')) { 
# ---------------------------------------------------------------
	
	# ------------------
	# Set up R
	rm(list=ls())
	library(data.table)
	# ------------------
	
	
	# ---------------------------------------------------------------------------------
	# Handle inputs
	if (class(toRun)!='character') stop('Specify which simulations to run with toRun!')
	if ('all' %in% toRun) toRun = c('age', 'sex', 'parity', 'completeness')
	# ---------------------------------------------------------------------------------
	
	
	# ----------------------------------------------------------------
	# Directories, files and functions
	
	# output files
	outDir = paste0('./Simulation_Outputs/')
	ageFile = paste0(outDir, 'age_simulations.csv')
	sexFile = paste0(outDir, 'sex_simulations.csv')
	parityFile = paste0(outDir, 'parity_simulations.csv')
	completenessFile = paste0(outDir, 'completeness_simulations.csv')
	
	# load function
	source('./Code/simulate.r')
	# ----------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
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
						probFile='unspecified_completeness_probabilities.csv', 
						indicator='unspecified_completeness')
	}
	# -------------------------------------------------------------------------
	
	
	# -----------------------------------------------------------------------
	# Save
	if ('age' %in% toRun) fwrite(ageSim, ageFile)
	if ('sex' %in% toRun) fwrite(sexSim, sexFile)
	if ('parity' %in% toRun) fwrite(paritySim, parityFile)
	if ('completeness' %in% toRun) fwrite(completenessSim, completenessFile)
	# -----------------------------------------------------------------------
	
# ------------
# End function
}
# ------------
