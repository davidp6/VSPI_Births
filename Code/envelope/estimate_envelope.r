# ------------------------------------------------------------
# David Phillips
#
# 3/29/2017
# Compute country-year-age-sex-parity births from DHS and WPP
# Takes data from set_up_envelope_data.r and applies a function
# ------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(stringr)
library(boot)
library(lme4)
library(merTools) # for prediction intervals
library(systemfit)
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
# fit = lm(logit_prop ~ super_region*year*age*sex*parity*birthweight, data)

# run SUR with full interactions
# SUR formulae
wide = dcast(data, super_region+iso3+year+age~sex+parity+birthweight, value.var='logit_prop')
vars = names(wide)[!names(wide) %in% c('super_region','iso3','year','age')]
setnames(wide, vars, paste0('p',seq(length(vars))))
system = lapply(paste0('p',seq(length(vars))), function(x) { as.formula(paste(x, '~super_region*year*age')) })
sFit = systemfit(system, data=wide)

# predict
# data[, c('pred','upper','lower'):=predictInterval(lmeFit, newdata=data, n.sims=2)] # really slow
# data[, 'pred':=predict(fit, newdata=data)]
preds = predict(sFit, newdata=wide)
wide = cbind(wide, preds)
long = melt(wide, id.vars=c('super_region','iso3','year','age'), measure.vars=names(preds))
vars = data.table(vars)
vars[, c('sex','parity','birthweight'):=tstrsplit(vars, '_', fixed=TRUE)]
vars = cbind(vars, names(preds))
long = merge(long, vars, by.x='variable', by.y='V2')
long$vars = NULL
long$variable = NULL
setnames(long, 'value', 'pred')
long[, sex:=as.integer(sex)]
long[, parity:=as.integer(parity)]
data = merge(data, long, by=c('iso3','year','age','sex','parity','birthweight'))

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
