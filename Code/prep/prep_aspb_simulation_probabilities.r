# ---------------------------------------------------------------------------
# David Phillips
#
# 3/21/2017
# Prep simulation probabilities for age/sex/parity using Eurostat tabulations
# ---------------------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
# ------------------


# --------------------------------------------------------------------------------
# Files/directories/lists

# root directory
root = './Data/'

# input file
inFile = paste0(root, 'Country_Data/Data 200417.csv')

# output file
outFile1 = paste0(root, 'Simulation_Inputs/unspecified_age_probabilities.csv')
outFile2 = paste0(root, 'Simulation_Inputs/unspecified_sex_probabilities.csv')
outFile3 = paste0(root, 'Simulation_Inputs/unspecified_parity_probabilities.csv')
# --------------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

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
data[, births:=as.numeric(births)]
data = data[!is.na(births)]

# collapse to all countries/years
denominator = data[, list(births=sum(births, na.rm=TRUE)), by=c('parity', 'age', 'sex')]
denominator = denominator[parity!='All' & sex!='both'] # drop all/both (not to be confused with unspecified). so far there's no code for all ages
numerators = denominator[parity=='99' | age==99 | sex=='99']
denominator = denominator[parity!='99' & age!=99 & sex!='99']

# isolate unspecified counts
unspecified_age = numerators[age==99 & parity!='99' & sex!='99']
unspecified_parity = numerators[parity=='99' & age!=99 & sex!='99']
unspecified_sex = numerators[sex=='99' & parity!='99' & age!=99]
unspecified_age$age = NULL
unspecified_parity$parity = NULL
unspecified_sex$sex = NULL
setnames(unspecified_age, 'births', 'unspecified_age')
setnames(unspecified_parity, 'births', 'unspecified_parity')
setnames(unspecified_sex, 'births', 'unspecified_sex')

# collapse denominators
age_denominator = denominator[, list(births=sum(births)), by=c('parity', 'sex')]
sex_denominator = denominator[, list(births=sum(births)), by=c('parity', 'age')]
parity_denominator = denominator[, list(births=sum(births)), by=c('age', 'sex')]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Compute probabilities

# merge
unspecified_age = merge(unspecified_age, age_denominator, by=c('parity', 'sex'))
unspecified_sex = merge(unspecified_sex, sex_denominator, by=c('parity', 'age'))
unspecified_parity = merge(unspecified_parity, parity_denominator, by=c('age', 'sex'))

# compute
unspecified_age[ , unspecified_age:=unspecified_age/births]
unspecified_sex[ , unspecified_sex:=unspecified_sex/births]
unspecified_parity[ , unspecified_parity:=unspecified_parity/births]

# drop unnecessary variables
unspecified_age$births = NULL
unspecified_sex$births = NULL
unspecified_parity$births = NULL

# expand across target variable, assume equal probabilities
# e.g. the probability of being an unknown age is the same for all ages
ages = unique(denominator$age)
parities = unique(denominator$parity)
sexes = unique(denominator$sex)
expIdxa = rep(seq_len(nrow(unspecified_age)), length(ages))
expIdxp = rep(seq_len(nrow(unspecified_parity)), length(parities))
expIdxs = rep(seq_len(nrow(unspecified_sex)), length(sexes))
unspecified_age = unspecified_age[expIdxa]
unspecified_parity = unspecified_parity[expIdxp]
unspecified_sex = unspecified_sex[expIdxs]
unspecified_age[, age:=rep(ages, each=length(sexes)*length(parities))]
unspecified_parity[, parity:=rep(parities, each=length(ages)*length(sexes))]
unspecified_sex[, sex:=rep(sexes, each=length(ages)*length(parities))]
# -------------------------------------------------------------------------------------


# ------------------------------------------------------
# Save
write.csv(unspecified_age, outFile1, row.names=FALSE)
write.csv(unspecified_sex, outFile2, row.names=FALSE)
write.csv(unspecified_parity, outFile3, row.names=FALSE)
# ------------------------------------------------------
