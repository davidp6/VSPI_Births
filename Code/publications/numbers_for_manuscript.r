# ---------------------------------------------------------------------------------------------
# David Phillips
#
# 11/25/2017
# Produce various numbers to insert in manuscript: 
# A global, systematic assessment of birth registration: the Vital Statistics Performance Index 
# ---------------------------------------------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(ggplot2)
# --------------------



# ----------------------------------------------
# Files and directories

# input file
vspiFile = './Data/VSPI_Estimates/VSPI_B_Data 061117.csv'

# envelope file
envFile = './Data/Envelopes/Envelope.csv'

# country codes
ccFile = './Data/Country_Data/countrycodes.csv'
# ----------------------------------------------


# ----------------------------------------------
# Load/prep data

# load VSPI estimates
data = fread(vspiFile)

# load envelope
envelope = fread(envFile)

# bring in regions
cc = fread(ccFile)
cc = cc[!is.na(iso3) & iso3!='' & gbd_super_region_name_reporting!='']
cc = unique(cc[,c('iso3','gbd_super_region_name_reporting'),with=FALSE])
data = merge(data, cc, by='iso3')
setnames(data, 'gbd_super_region_name_reporting', 'region')

# collapse envelope
byVars = c('iso3','year')
envelope = envelope[, list(envelope=sum(births_est)), by=byVars]
envelope = merge(envelope, data[,c(byVars, 'total'),with=FALSE], by=byVars, all.x=TRUE)
# ----------------------------------------------


# ---------------------------------------------------------------------------------------------------
# Print numbers

# DATA AVAILABILITY
# country-years with actual data
nrow(unique(data[!is.na(unspecified_age),c('iso3','year'),with=F]))

# countries with actual data
nrow(unique(data[!is.na(unspecified_age),c('iso3'),with=F]))

# year range
c(min(data[!is.na(unspecified_age)]$year), max(data[!is.na(unspecified_age)]$year))

# countries with >=30 years, >=20 years, <=5 years
sum(table(data[!is.na(unspecified_age)]$iso3)>=30)
sum(table(data[!is.na(unspecified_age)]$iso3)>=20)
sum(table(data[!is.na(unspecified_age)]$iso3)<=5)

# countries with any data by region
unique(data[!is.na(unspecified_age), c('iso3','region'), with=FALSE])[, .N, by='region']

# COMPLETENESS
# total births in data
tmp = data[, sum(total,na.rm=T), by='year']
mean(tmp[V1>0]$V1)
tmp[V1>0][V1==max(V1) | V1==min(V1)]
sum(tmp[V1>0]$V1)

# global completeness
tmp = envelope[,list(env=sum(envelope),total=sum(total,na.rm=T)),by='year']
tmp[, completeness:=total/env]
mean(tmp[completeness>0]$completeness)
tmp[completeness>0][completeness==max(completeness) | completeness==min(completeness) | year==2015]

# countries by completeness categories
envelope[, completeness:=total/envelope]
envelope[completeness>1, completeness:=1]
envelope[, cat:=cut(completeness, c(0,.5,.8,.9,1))]
envelope[!is.na(total), maxyear:=max(year), by=iso3]
envelope[year==maxyear, .N, by=cat]

# proportion of country-years that aren't 100% missing
inds = c('unspecified_bw','unspecified_parity','unspecified_sex','unspecified_age')
tmp = melt(data, id.vars=c('iso3','year'), measure.vars=inds)
tmp[, mean(value!=1, na.rm=TRUE), by=variable]

# proportion of births with missing indicator among those that aren't 100% missing
tmp[value!=1, mean(value, na.rm=TRUE), by=variable]

# number of countries in each VSPI category
data[!is.na(unspecified_age), maxyear:=max(year), by=iso3]
nrow(data[year==maxyear & vspi_b_ma>=.9])
nrow(data[year==maxyear & vspi_b_ma>=.8 & vspi_b_ma<.9])
nrow(data[year==maxyear & vspi_b_ma>=.6 & vspi_b_ma<.8])
nrow(data[year==maxyear & vspi_b_ma>=.3 & vspi_b_ma<.6])
nrow(data[year==maxyear & vspi_b_ma<.3])

# number of births in each category of country
envelope = merge(envelope, data[, c(byVars, 'vspi_b_ma'), with=FALSE], by=byVars, all.x=TRUE)
envelope[, cat:=cut(vspi_b_ma, c(0,.9,1))]
envelope[year==2014, sum(envelope)/1000000, by=cat][order(cat)]
# ----------------------------------------------------------------------------------------------------
