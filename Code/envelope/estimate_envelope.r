# ------------------------------------------------------------
# David Phillips
#
# 3/29/2017
# Compute country-year-age-sex-parity births from WPP and HPD
# Takes data from set_up_envelope_data.r and applies a function
# ------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(stringr)
# ------------------


# ------------------------------------------------------------------
# Files/directories/lists

# settings
run_name = 'sur_quadratic_full'
estFunction = 'sur_quadratic_full'

# functions
source('./Code/envelope/sur_linear.r')
source('./Code/envelope/sur_quadratic.r')
source('./Code/envelope/sur_quadratic_full.r')
source('./Code/envelope/sur_cubic.r')

# input file
inFile = './Data/Country_Data/Envelope_Input.csv'

# output file
outFile = paste0('./Data/Envelopes/Envelope_', run_name, '.csv')
# ------------------------------------------------------------------


# -------------------
# Load data
data = fread(inFile)
# -------------------


# -----------------------------------------------------------
# Predict parity proportions for each CYA

# run alternative models
if (estFunction=='sur_linear') data = sur_linear(data)
if (estFunction=='sur_quadratic') data = sur_quadratic(data)
if (estFunction=='sur_quadratic_full') data = sur_quadratic_full(data)
if (estFunction=='sur_cubic') data = sur_cubic(data)
# -----------------------------------------------------------


# ----------------------------------------
# Save
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------
