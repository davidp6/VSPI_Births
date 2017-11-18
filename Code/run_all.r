# ----------------------------------------------------------------------------------
# David Phillips
#
# 4/7/2017
# Function that runs all code
#
# Envelope code
# - prep code for wpp and hfd
# - estimate envelopes
# Prep code 
# - simulation population
# - simulation probabilities
# Simulation code
# - run simulations
# - evaluate accuracy
# VSPI code
# - compute vspi
# Visualization code
# - simulation probabilities
# - simulation aspbfs
# - simulation accuracy
# - vspi time series
# - vspi maps
# - envelope graphs
# Note: if there's new data to evaluate, the file path should be updated in:
# prep_asp_simulation_probabilities.r, prep_completeness_simulation_probabilities.r, 
# compute_vspib.r, vspi_time_series.r and map_vspi.r. Probably should be refactored
# The working directory should be the root of this repo
# ----------------------------------------------------------------------------------


# --------------------------------
# Start function
runAll = function(envelope=TRUE, prep=TRUE, simulation=TRUE, vspi=TRUE, visualizations=TRUE) { 


# for debugging purposes
# envelope=TRUE
# prep=TRUE
# simulation=TRUE
# vspi=TRUE
# visualizations=TRUE
# --------------------------------
	
	# -------------------------------------------------------------------
	# Set up R
	args = c('envelope', 'prep', 'simulation', 'vspi', 'visualizations')
	rm(list=ls()[!ls() %in% c('args', args)])
	library(data.table)
	# -------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
	# Handle inputs
	for(a in args) {
		if (class(get(a))!='logical') stop(paste('Argument', a, 'not logical'))
	}
	# -------------------------------------------------------------------------
	
	
	# ----------------------------------------------------------------------------------
	# Run specified code
	
	# envelope prep code and estimation
	if (envelope) {
		print('Remaking envelopes...')
		source('./Code/envelope/set_up_envelope_data.r')
		source('./Code/envelope/estimate_envelope.r')
	}
	
	# other prep code (mostly simulations)
	if (prep) {
		print('Reprepping simulation inputs...')
		source('./Code/prep/prep_simulation_population.r')
		source('./Code/prep/prep_aspb_simulation_probabilities.r')
		source('./Code/prep/prep_completeness_simulation_probabilities.r')
	}
	
	# simulation code with default settings
	if (simulation) {
		print('Rerunning simulations...')
		source('./Code/simulation/run_simulations.r')
		runSims()
		source('./Code/simulation/evaluate_accuracy.r')
		evalSims()
	}
	
	# vspi code with default settings
	if (vspi) {
		print('Recomputing VSPI...')
		source('./Code/compute_vspib.r')
		computeVSPIB()
	}
	
	# visualization code
	if(visualizations) {
		print('Remaking visualizations...')
		source('./Code/visualization/graph_simulation_probabilities.r')
		source('./Code/visualization/graph_simulation_aspbfs.r')
		source('./Code/visualization/graph_accuracy.r')
		source('./Code/visualization/map_vspi.r')
		source('./Code/visualization/vspi_time_series.r')
		source('./Code/visualization/graph_envelope_time_series.r')
	}
	# ----------------------------------------------------------------------------------
}
	