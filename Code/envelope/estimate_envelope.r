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
run_name = ''
estFunction = 'sur_quadratic_full'

# functions
source('./Code/envelope/sur_linear.r')
source('./Code/envelope/sur_quadratic.r')
source('./Code/envelope/sur_quadratic_full.r')
source('./Code/envelope/sur_cubic.r')

# input file
inFile = './Data/Country_Data/Envelope_Input.csv'

# birthweight proportions file
bwFile = './Data/Country_Data/Data 200617.csv'

# output file
outFile = paste0('./Data/Envelopes/Envelope', run_name, '.csv')
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


# -----------------------------------------------------------
# Add birthweight based on empirical proportions
# assumes space-time invariance

# load
bwData = fread(bwFile)

# prep
	# drop blank columns/rows
	keepVars = c('Country', 'Year', 'Age', 'Sex', 'Birth order', 'Birthweight', 'Number births')
	bwData = bwData[!is.na(Year), keepVars, with=FALSE]
	setnames(bwData, keepVars, c('country', 'year', 'age', 'sex', 'parity', 'bw', 'births'))

	# format variables
	bwData[parity=='4+', parity:='4']
	bwData[sex==1, sex_str:='m']
	bwData[sex==2, sex_str:='f']
	bwData[sex==3, sex_str:='both']
	bwData[sex==99, sex_str:='99']
	bwData$sex = NULL
	setnames(bwData, 'sex_str', 'sex')
	bwData[, births:=as.numeric(births)]
	bwData = bwData[!is.na(births)]
	bwData[bw %in% c('1', '<2500'), bw:='2500']
	bwData[bw %in% c('2', '2500-3499'), bw:='3000']
	bwData[bw %in% c('3', '3500+'), bw:='3500']
	bwData[bw=='Unknown', bw:='99']
	bwData[bw=='N/A', bw:='All']

	# keep only country-years that have the intersection of all variables
	# all/both is not to be confused with unspecified (99). 
	# so far there's no code for all ages, so I'm creating it based on a mean of 99, just to be safe
	bwData[, tmp:=mean(age),by=c('country','year')]
	bwData = bwData[sex!='both' & tmp!=99 & parity!='All' & bw!='All']
	bwData$tmp = NULL
	
	# drop unknowns
	bwData = bwData[age!=99 & parity!=99 & sex!='99' & bw!='99']

# compute proportions
bwData = bwData[, list(births=sum(births)), by=c('age','parity','sex','bw')]
bwData[, prop:=births/sum(births), by=c('age','parity','sex')]
bwData$births = NULL
bwData[, parity:=as.numeric(parity)]

# expand out envelopes, merge proportions and multiply
tmp1 = copy(data)
tmp2 = copy(data)
data[, bw:='2500']
tmp1[, bw:='3000']
tmp2[, bw:='3500']
data = rbind(data, tmp1, tmp2)
data = merge(data, bwData, by=c('age','parity','sex','bw'))
data[, births:=births*prop]
data$prop = NULL
# -----------------------------------------------------------


# ----------------------------------------
# Save
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------
