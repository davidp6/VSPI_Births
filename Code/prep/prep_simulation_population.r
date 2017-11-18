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
inFile = './Data/Envelopes/Envelope.csv'

# output file
outFile = './Data/Simulation_Inputs/simulation_population.csv'
# --------------------------------------------------------------------


# ------------------------------------------------
# Load/prep data

# load data
data = fread(inFile)

# keep only appropriate years
data = data[year>=1980 & year<=2017]

# collapse to "global" level
byVars = c('age','sex','parity','birthweight')
data =data[, list(births=sum(births)), by=byVars]

# reduce scale for storage
data[, births:=round(births/1000)]

# rename to match probabilities
setnames(data, 'birthweight', 'bw')

# format variables to match probabilities
data[, sex:=ifelse(sex==1, 'm', 'f')]
data[bw %in% c('1', '<2500'), bw:='2500']
data[bw %in% c('2', '2500-3499'), bw:='3000']
data[bw %in% c('3', '3500+'), bw:='3500']
# ------------------------------------------------


# ---------------------------------------------
# Save
write.csv(data, file=outFile, row.names=FALSE)
# ---------------------------------------------
