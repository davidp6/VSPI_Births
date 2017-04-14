# -------------------------------
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
# -------------------------------


# --------------------------------
# Start function
runAll = function(envelope=FALSE, prep=TRUE, simulation=TRUE, vspi=TRUE, visualizations=TRUE) { 
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
		source('./Code/prep/prep_wpp_country_year_sex_estimates.r')
		source('./Code/prep/prep_wpp_country_year_tfr_estimates.r')
		source('./Code/prep/prep_wpp_country_year_age_estimates.r')
		source('./Code/prep/prep_hfd_country_year_age_parity_estimates.r')
		source('./Code/envelope/estimate_envelope.r')
	}
	
	# other prep code (mostly simulations)
	if (prep) {
		print('Reprepping simulation inputs...')
		source('./Code/prep/prep_simulation_population.r')
		source('./Code/prep/prep_asp_simulation_probabilities.r')
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
	}
	# ----------------------------------------------------------------------------------
}
	