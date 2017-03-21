# -------------------------------------------------------------------------
# David Phillips
#
# 3/21/2016
# Function that takes simulation population and sampling probabilities and 
# simulates varying levels of data quality for a specified indicator
# Inputs
#    - arg1 (class): description
#    - arg2 (class): description
# Outputs
#    - out (class): description
# -------------------------------------------------------------------------


# ------------------------------------------
# Start function
funcName = function(indicator='unspecified_age') { 
# ------------------------------------------
	
	# -----------------------------
	# Set up R
	args = c('arg1', 'arg2')
	rm(list=ls()[!ls() %in% args])
	library(data.table)
	library(ggplot2)
	library(RColorBrewer)
	# -----------------------------
	
	
	# --------------------------------------------------------------
	# Test inputs
	for(a in args) if (!a %in% ls()) stop(paste('Must provide', a))
	if (class(arg1)!='CLASS') stop('arg1 must be CLASS') 
	if (class(arg2)!='CLASS') stop('arg2 must be CLASS') 
	# --------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
	# Files/directories/lists
	
	# root directory
	root = './Data/Simulation_Inputs/'
	
	# input files
	inFile1 = paste0(root, 'filename') # population
	inFile2 = paste0(root, 'unspecified_age_probabilities.csv') # probabilities
	
	# output file
	outFile = paste0(root, 'filename')
	# -------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
	# Load/prep data
	
	# -------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
	# Do the thing
	
	# -------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
	# Set up to graph
	
	# -------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
	# Graph
	
	# -------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
	# Return output
	return()
	# -------------------------------------------------------------------------
	
# -------------
# End function
}
# -------------
