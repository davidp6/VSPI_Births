# ----------------------------------------------------------------------------------------------------
# David Phillips
#
# 3/21/2017
# Prep simulation probabilities for crude completeness using Eurostat tabulations and estimated births
# ----------------------------------------------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
# ------------------


# -----------------------------------------------------------------
# Files/directories/lists

# input file
inFile1 = './Data/Envelopes/Envelope.csv'
inFile2 = './Data/Country_Data/Data 050417.csv'

# country codes
ccFile = './Data/Country_Data/countrycodes.csv'

# output file
outFile = './Data/Simulation_Inputs/completeness_probabilities.csv'
# -----------------------------------------------------------------


# -------------------------------------------------------------------------
# Load/prep data

# load birth estimates
estimates = fread(inFile1)

# load data
data = fread(inFile2)

# drop blank columns/rows
keepVars = c('Country', 'Year', 'Age', 'Sex', 'Birth order', 'Number births')
data = data[!is.na(Year), keepVars, with=FALSE]
setnames(data, keepVars, c('country', 'year', 'age', 'sex', 'parity', 'births'))

# format variables
data[parity=='4+', parity:='4']
data[sex==1, sex_str:='m']
data[sex==2, sex_str:='f']
data[sex==3, sex_str:='both']
data[sex==99, sex_str:='99']
data$sex = NULL
setnames(data, 'sex_str', 'sex')
estimates[, parity:=as.character(parity)]

# bring in iso codes/drop non-GBD countries
codes = fread(ccFile)
codes = codes[ihme_indic_country==1, c('iso3', 'countryname'), with=FALSE]
codes = unique(codes)
data = merge(data, codes, by.x='country', by.y='countryname')
data$country = NULL

# merge estimates to data, keep only data that has all variables specified
setnames(estimates, 'births', 'estimate')
data = merge(data, estimates, by=c('iso3','year','age','parity','sex'))

# collapse to all countries/years
data = data[, list(births=sum(births), estimate=sum(estimate)), by=c('parity', 'age', 'sex')]

# compute crude asp completeness
data[, completeness:=births/estimate]
data[completeness>1, completeness:=1]
# -------------------------------------------------------------------------


# ----------------------------------------------------------------
# Save

# subset columns
data = data[, c('age','sex','parity','completeness'), with=FALSE]

# save
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------------------------------
