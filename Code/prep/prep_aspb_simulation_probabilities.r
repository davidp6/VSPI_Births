# ---------------------------------------------------------------------------
# David Phillips
#
# 3/21/2017
# Prep simulation probabilities for age/sex/parity/birthweight using microdata
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
inFile = paste0(root, 'Country_Data/Data 061117.csv')

# output file
outFile1 = paste0(root, 'Simulation_Inputs/unspecified_age_probabilities.csv')
outFile2 = paste0(root, 'Simulation_Inputs/unspecified_sex_probabilities.csv')
outFile3 = paste0(root, 'Simulation_Inputs/unspecified_parity_probabilities.csv')
outFile4 = paste0(root, 'Simulation_Inputs/unspecified_bw_probabilities.csv')
# --------------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# drop blank columns/rows
keepVars = c('Country', 'Year', 'Age', 'Sex', 'Birth order', 'Birthweight', 'Number births')
data = data[!is.na(Year), keepVars, with=FALSE]
setnames(data, keepVars, c('country', 'year', 'age', 'sex', 'parity', 'bw', 'births'))

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
data[bw %in% c('1', '<2500'), bw:='2500']
data[bw %in% c('2', '2500-3499'), bw:='3000']
data[bw %in% c('3', '3500+'), bw:='3500']
data[bw=='Unknown', bw:='99']
data[bw=='N/A', bw:='All']

# keep only country-years that have the intersection of all variables
# all/both is not to be confused with unspecified (99). 
# so far there's no code for all ages, so I'm creating it based on a mean of 99, just to be safe
data[, tmp:=mean(age),by=c('country','year')]
data = data[sex!='both' & tmp!=99 & parity!='All' & bw!='All']
data$tmp = NULL

# collapse to all countries/years
denominator = data[, list(births=sum(births, na.rm=TRUE)), by=c('parity', 'age', 'sex', 'bw')]
numerators = denominator[parity=='99' | age==99 | sex=='99' | bw=='99']
denominator = denominator[parity!='99' & age!=99 & sex!='99' & bw!='99']

# isolate unspecified counts
unspecified_age = numerators[age==99 & parity!='99' & sex!='99' & bw!='99']
unspecified_parity = numerators[parity=='99' & age!=99 & sex!='99' & bw!='99']
unspecified_sex = numerators[sex=='99' & parity!='99' & age!=99 & bw!='99']
unspecified_bw = numerators[bw=='99' & parity!='99' & age!=99 & sex!='99']
unspecified_age$age = NULL
unspecified_parity$parity = NULL
unspecified_sex$sex = NULL
unspecified_bw$bw = NULL
setnames(unspecified_age, 'births', 'unspecified_age')
setnames(unspecified_parity, 'births', 'unspecified_parity')
setnames(unspecified_sex, 'births', 'unspecified_sex')
setnames(unspecified_bw, 'births', 'unspecified_bw')

# collapse denominators
age_denominator = denominator[, list(births=sum(births)), by=c('parity', 'sex', 'bw')]
sex_denominator = denominator[, list(births=sum(births)), by=c('parity', 'age', 'bw')]
parity_denominator = denominator[, list(births=sum(births)), by=c('age', 'sex', 'bw')]
bw_denominator = denominator[, list(births=sum(births)), by=c('age', 'sex', 'parity')]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Compute probabilities

# merge
unspecified_age = merge(unspecified_age, age_denominator, by=c('parity', 'sex', 'bw'))
unspecified_sex = merge(unspecified_sex, sex_denominator, by=c('parity', 'age', 'bw'))
unspecified_parity = merge(unspecified_parity, parity_denominator, by=c('age', 'sex', 'bw'))
unspecified_bw = merge(unspecified_bw, bw_denominator, by=c('age', 'sex', 'parity'))

# compute
unspecified_age[ , unspecified_age:=unspecified_age/births]
unspecified_sex[ , unspecified_sex:=unspecified_sex/births]
unspecified_parity[ , unspecified_parity:=unspecified_parity/births]
unspecified_bw[ , unspecified_bw:=unspecified_bw/births]

# drop unnecessary variables
unspecified_age$births = NULL
unspecified_sex$births = NULL
unspecified_parity$births = NULL
unspecified_bw$births = NULL

# make sure proportions are sqaure
ages = unique(denominator$age)
parities = unique(denominator$parity)
sexes = unique(denominator$sex)
bws = unique(denominator$bw)
square = data.table(expand.grid(ages, sexes, parities, bws))
setnames(square, c('age','sex','parity','bw'))
unspecified_age = merge(unspecified_age, square[,c('parity','sex','bw'),with=FALSE], all=TRUE, by=c('parity','sex','bw'))
unspecified_sex = merge(unspecified_sex, square[,c('parity','age','bw'),with=FALSE], all=TRUE, by=c('parity','age','bw'))
unspecified_parity = merge(unspecified_parity, square[,c('age','sex','bw'),with=FALSE], all=TRUE, by=c('age','sex','bw'))
unspecified_bw = merge(unspecified_bw, square[,c('parity','sex','age'),with=FALSE], all=TRUE, by=c('parity','sex','age'))

# fill in non-square proportions with the minimum
unspecified_age[, unspecified_age:=ifelse(is.na(unspecified_age), min(unspecified_age,na.rm=TRUE), unspecified_age)]
unspecified_sex[, unspecified_sex:=ifelse(is.na(unspecified_sex), min(unspecified_sex,na.rm=TRUE), unspecified_sex)]
unspecified_parity[, unspecified_parity:=ifelse(is.na(unspecified_parity), min(unspecified_parity,na.rm=TRUE), unspecified_parity)]
unspecified_bw[, unspecified_bw:=ifelse(is.na(unspecified_bw), min(unspecified_bw,na.rm=TRUE), unspecified_bw)]

# expand across target variable, assume equal probabilities
# e.g. the probability of being an unknown age is the same for all ages
expIdxa = rep(seq_len(nrow(unspecified_age)), length(ages))
expIdxp = rep(seq_len(nrow(unspecified_parity)), length(parities))
expIdxs = rep(seq_len(nrow(unspecified_sex)), length(sexes))
expIdxb = rep(seq_len(nrow(unspecified_bw)), length(bws))
unspecified_age = unspecified_age[expIdxa]
unspecified_parity = unspecified_parity[expIdxp]
unspecified_sex = unspecified_sex[expIdxs]
unspecified_bw = unspecified_bw[expIdxb]
unspecified_age[, age:=rep(ages, length(sexes)*length(parities)*length(bws))]
unspecified_parity[, parity:=rep(parities, length(ages)*length(sexes)*length(bws))]
unspecified_sex[, sex:=rep(sexes, length(ages)*length(parities)*length(bws))]
unspecified_bw[, bw:=rep(bws, length(sexes)*length(ages)*length(parities))]
unspecified_age = unique(unspecified_age)
unspecified_parity = unique(unspecified_parity)
unspecified_sex = unique(unspecified_sex)
unspecified_bw = unique(unspecified_bw)
# -------------------------------------------------------------------------------------


# ------------------------------------------------------
# Save
write.csv(unspecified_age, outFile1, row.names=FALSE)
write.csv(unspecified_sex, outFile2, row.names=FALSE)
write.csv(unspecified_parity, outFile3, row.names=FALSE)
write.csv(unspecified_bw, outFile4, row.names=FALSE)
# ------------------------------------------------------
