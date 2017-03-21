# ---------------------------------------------------------------------------
# David Phillips
#
# 3/21/2017
# Prep simulation probabilities for age/sex/parity using Eurostat tabulations
# ---------------------------------------------------------------------------


# -----------------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
# -----------------------------


# --------------------------------------------------------------------------------
# Files/directories/lists

# root directory
root = './Data/'

# input file
inFile = paste0(root, 'Misc/Eurostat.csv')

# output file
outFile1 = paste0(root, 'Simulation_Inputs/unspecified_age_probabilities.csv')
outFile2 = paste0(root, 'Simulation_Inputs/unspecified_sex_probabilities.csv')
outFile3 = paste0(root, 'Simulation_Inputs/unspecified_parity_probabilities.csv')
# --------------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# format ages
data[birthorder=='4+', birthorder:='4']
data[, birthorder:=as.numeric(birthorder)]

# collapse to all countries/years
denominator = data[, list(numberbirths=sum(numberbirths, na.rm=TRUE)), by=c('birthorder', 'age')]
numerators = denominator[birthorder==99 | age==99]
denominator = denominator[birthorder!=99 & age!=99]

# isolate unspecified counts
unspecified_age = numerators[age==99 & birthorder!=99]
unspecified_parity = numerators[birthorder==99 & age!=99]
unspecified_age$age = NULL
unspecified_parity$birthorder = NULL
setnames(unspecified_age, 'numberbirths', 'unspecified_age')
setnames(unspecified_parity, 'numberbirths', 'unspecified_parity')

# add fake sex for now
denominator = rbind(denominator, denominator)
denominator[, sex:=rep(c('m', 'f'), each=36)]
unspecified_age = rbind(unspecified_age, unspecified_age)
unspecified_age[, sex:=rep(c('m', 'f'), each=4)]
unspecified_parity = rbind(unspecified_parity, unspecified_parity)
unspecified_parity[, sex:=rep(c('m', 'f'), each=9)]
unspecified_sex = data.table(expand.grid(unique(unspecified_age$birthorder), 
					unique(unspecified_parity$age)))
setnames(unspecified_sex, c('Var1', 'Var2'), c('birthorder', 'age'))
unspecified_sex[, unspecified_sex:=5000+(500*birthorder)+(-50*age)] # fake numbers

# collapse denominators
age_denominator = denominator[, list(numberbirths=sum(numberbirths)), by=c('birthorder', 'sex')]
sex_denominator = denominator[, list(numberbirths=sum(numberbirths)), by=c('birthorder', 'age')]
parity_denominator = denominator[, list(numberbirths=sum(numberbirths)), by=c('age', 'sex')]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Compute probabilities

# merge
unspecified_age = merge(unspecified_age, age_denominator, by=c('birthorder', 'sex'))
unspecified_sex = merge(unspecified_sex, sex_denominator, by=c('birthorder', 'age'))
unspecified_parity = merge(unspecified_parity, parity_denominator, by=c('age', 'sex'))

# compute
unspecified_age[ , unspecified_age:=unspecified_age/numberbirths]
unspecified_sex[ , unspecified_sex:=unspecified_sex/numberbirths]
unspecified_parity[ , unspecified_parity:=unspecified_parity/numberbirths]

# drop unnecessary variables
unspecified_age$numberbirths = NULL
unspecified_sex$numberbirths = NULL
unspecified_parity$numberbirths = NULL
# -------------------------------------------------------------------------------------


# --------------------------------------
# Save
write.csv(unspecified_age, outFile1)
write.csv(unspecified_sex, outFile2)
write.csv(unspecified_parity, outFile3)
# --------------------------------------
