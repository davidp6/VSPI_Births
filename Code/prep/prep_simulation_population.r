# ------------------------------------------------
# David Phillips
#
# 3/22/2017
# Generate simulation population births estimates
# ------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
# ------------------


# --------------------------------------------------------------------
# Files/directories/lists

# input file
inFile = './Data/Country_Data/Country_Year_Age_Sex_Parity_Births.csv'

# output file
outFile = './Data/Simulation_Inputs/simulation_population.csv'
# --------------------------------------------------------------------


# ------------------------------------------------
# Load/prep data

# load data
data = fread(inFile)

# collapse to "global" level
byVars = c('age','sex','parity')
data =data[, list(births=sum(births)), by=byVars]

# reduce scale for storage
data[, births:=round(births/1000)]
# ------------------------------------------------


# ---------------------------------------------
# Save
write.csv(data, file=outFile, row.names=FALSE)
# ---------------------------------------------
