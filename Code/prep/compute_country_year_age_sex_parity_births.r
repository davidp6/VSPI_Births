# ------------------------------------------------------------
# David Phillips
#
# 3/29/2017
# Compute country-year-age-sex-parity births from WPP and HPD
# Takes cya, cys and cy-tfr from WPP, cyap from HPD
# Assumes sex ratio is independent of age
# Assumes parity is independent of sex
# Assumes tfr is independent of age within a country-year
# Assumes parity is a linear function of age and TFR
# The current working directory should be VSPI_B/
# ------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(readxl)
library(data.table)
library(stringr)
library(boot)
library(systemfit)
library(ggplot2)
library(gridExtra)
# ------------------


# ---------------------------------------------------------------------------
# Files/directories/lists

# input file
inFilecya = './Data/Country_Data/WPP_Country_Year_Age_Estimates.csv'
inFilecys = './Data/Country_Data/WPP_Country_Year_Sex_Estimates.csv'
inFiletfr = './Data/Country_Data/WPP_Country_Year_TFR_Estimates.csv'
inFilecyap = './Data/Country_Data/HFD_Country_Year_Age_Parity_Estimates.csv'

# output file
outFile = './Data/Country_Data/Country_Year_Age_Sex_Parity_Births.csv'
# ---------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# Load/prep data

# load country-year-age births
cya = fread(inFilecya)

# load country-year-sex ratios
cys = fread(inFilecys)

# load country-year TFR
tfr = fread(inFiletfr)

# load country-year-age-parity births (HFD is high-income only)
cyap = fread(inFilecyap)

# compute proportion of births in each parity by cya
cyap[, parity_proportion:=births/sum(births), by=c('iso3','year','age')]
cyap$births = NULL
cyap = dcast.data.table(cyap, iso3+year+age+country+super_region~parity)
setnames(cyap, c('1','2','3','4'), paste0('parity_proportion', seq(4)))

# merge everything together
data = merge(cya, cys, by=c('iso3','year','country','super_region'))
data = merge(data, tfr, by=c('iso3','year','country','super_region'))
data = merge(data, cyap, by=c('iso3','year','age','country','super_region'), all.x=TRUE)
# --------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Predict parity proportions for each CYA

# precision variables
data[, year2:=year^2]
data[, age2:=age^2]

# SUR formulae
form1 = logit(parity_proportion1) ~ year*age + mf_ratio + births*age + tfr + year2*age2
form2 = logit(parity_proportion2) ~ year*age + mf_ratio + births*age + tfr + year2*age2
form3 = logit(parity_proportion3) ~ year*age + mf_ratio + births*age + tfr + year2*age2
form4 = logit(parity_proportion4) ~ year*age + mf_ratio + births*age + tfr + year2*age2
system = list(proportion1=form1, proportion2=form2, 
				proportion3=form3, proportion4=form4)

# fit system of equations
sFit = systemfit(system, data=data)

# predict 
preds = predict(sFit, newdata=data)
for(v in names(preds)) preds[[v]] = inv.logit(preds[[v]])
data = cbind(data, preds)

# rescale
data[, proportion1_rescaled:=proportion1.pred/(proportion1.pred+proportion2.pred+proportion3.pred+proportion4.pred)]
data[, proportion2_rescaled:=proportion2.pred/(proportion1.pred+proportion2.pred+proportion3.pred+proportion4.pred)]
data[, proportion3_rescaled:=proportion3.pred/(proportion1.pred+proportion2.pred+proportion3.pred+proportion4.pred)]
data[, proportion4_rescaled:=proportion4.pred/(proportion1.pred+proportion2.pred+proportion3.pred+proportion4.pred)]

# graph predictions in-sample
p1 = ggplot(data) + 
	geom_point(aes(y=proportion1_rescaled, x=parity_proportion1)) +
	geom_abline(intercept=0, slope=1) + 
	labs(title='Parity 1', y='Predicted Proportion', x='Observed Proportion')
p2 = ggplot(data) + 
	geom_point(aes(y=proportion2_rescaled, x=parity_proportion2)) +
	geom_abline(intercept=0, slope=1) + 
	labs(title='Parity 2', y='Predicted Proportion', x='Observed Proportion')
p3 = ggplot(data) + 
	geom_point(aes(y=proportion3_rescaled, x=parity_proportion3)) +
	geom_abline(intercept=0, slope=1) + 
	labs(title='Parity 3', y='Predicted Proportion', x='Observed Proportion')
p4 = ggplot(data) + 
	geom_point(aes(y=proportion4_rescaled, x=parity_proportion4)) +
	geom_abline(intercept=0, slope=1) + 
	labs(title='Parity 4', y='Predicted Proportion', x='Observed Proportion')
grid.arrange(p1,p2,p3,p4)
# ---------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------
# Compute births by CYASP

# drop unnecessary variables
vars = c('iso3','year','age','country','super_region', 'births', 'mf_ratio', 
		'proportion1_rescaled', 'proportion2_rescaled', 'proportion3_rescaled', 'proportion4_rescaled')
data = data[, vars, with=FALSE]

# compute male and female births assuming independence
# ratio is m/f, so m/f=ratio and m+f=T, so (1+ratio)*f=T or f=T/(1+ratio)
data[, birthsf:=births/(mf_ratio+1)]
data[, birthsm:=births-birthsf]

# compute sex-parity births
data[, birthsf1:=birthsf*proportion1_rescaled]
data[, birthsf2:=birthsf*proportion2_rescaled]
data[, birthsf3:=birthsf*proportion3_rescaled]
data[, birthsf4:=birthsf*proportion4_rescaled]
data[, birthsm1:=birthsm*proportion1_rescaled]
data[, birthsm2:=birthsm*proportion2_rescaled]
data[, birthsm3:=birthsm*proportion3_rescaled]
data[, birthsm4:=birthsm*proportion4_rescaled]

# drop everything but IDs and final estimates
data$births = NULL
data$birthsf = NULL
data$birthsm = NULL
data$mf_ratio = NULL
data$proportion1_rescaled=NULL
data$proportion2_rescaled=NULL
data$proportion3_rescaled=NULL
data$proportion4_rescaled=NULL

# reshape long
data = melt(data, id.vars=c('iso3','year','age','country','super_region'), value.name='births')

# parse sex and parity
data[, parity:=as.numeric(str_sub(variable, -1, -1))]
data[, sex:=str_sub(variable, -2, -2)]
data$variable = NULL
# ------------------------------------------------------------------------------------------------------


# ----------------------------------------
# Save
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------
