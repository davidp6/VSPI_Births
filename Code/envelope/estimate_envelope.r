# ------------------------------------------------------------
# David Phillips
#
# 3/29/2017
# Compute country-year-age-sex-parity births from DHS and WPP
# Takes data from set_up_envelope_data.r and applies a function
# ------------------------------------------------------------

# To Do:
# Maybe SUR is still the right way to go so it can assure that the proportions sum to 1?


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(stringr)
library(boot)
library(lme4)
library(merTools) # for prediction intervals
# ------------------


# ------------------------------------------------------------------
# Files/directories/lists

# settings
run_name = ''

# input file
inFile = './Data/Country_Data/Envelope_Input.csv'

# output file
outFile = paste0('./Data/Envelopes/Envelope', run_name, '.csv')

# graph file
outFile = paste0('./Data/Envelopes/Envelope', run_name, '.csv')
# ------------------------------------------------------------------


# -------------------------------------------------------------
# Load data
data = fread(inFile)

# drop super regions with no data
data = data[, all_missing:=all(is.na(prop)), by='super_region']
data = data[all_missing==FALSE]
data$all_missing = NULL

# logit transform
quantiles = quantile(data[prop>0 & prop<1]$prop, c(.025, .975), na.rm=TRUE)
data[, logit_prop:=logit(prop)]
data[prop==0, logit_prop:=logit(quantiles[1])]
data[prop==1, logit_prop:=logit(quantiles[2])]
# -------------------------------------------------------------


# --------------------------------------------------------------------------------------
# Predict ASPB proportions for each CYA and compute births

# run mixed effects model
# lmeFit = lmer(logit_prop ~ super_region + year + age + sex + parity + birthweight + 
				# (1 | super_region) + (1 | sex) + (1 | birthweight), data)
				
# run OLS with full interactions
fit = lm(logit_prop ~ super_region*year*age*sex*parity*birthweight, data)

# predict
# data[, c('pred','upper','lower'):=predictInterval(lmeFit, newdata=data, n.sims=2)] # really slow
data[, 'pred':=predict(fit, newdata=data)]

# rescale to sum to 1
data[, pred:=inv.logit(pred)]
data[, pred:=pred/sum(pred), by=c('country','year','age')]

# compute births
data[, births_est:=pred*births]
# data[, births_upper:=pred_upper*births]
# data[, births_lower:=pred_lower*births]
# --------------------------------------------------------------------------------------


# ----------------------------------------
# Save
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------
