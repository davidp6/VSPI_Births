# -----------------------------------------------------
# David Phillips
#
# 3/29/2017
# Prep HFD births estimates by country-year-age-parity
# -----------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(readxl)
library(data.table)
library(stringr)
library(stats)
# ------------------


# -----------------------------------------------------------------------
# Files/directories/lists

# input file
inFiles = list.files('./Data/Raw/HFD')

# country codes
ccFile = './Data/Country_Data/countrycodes.csv'

# output file
outFile = './Data/Country_Data/HFD_Country_Year_Age_Parity_Estimates.csv'
# -----------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Load/prep data

# load
for(f in seq_along(inFiles)) {
	tmp = fread(paste0('./Data/Raw/HFD/', (inFiles[f])))
	tmp[, country:=str_split(inFiles[f], '_')[[1]][2]]
	if (f==1) data = tmp
	if (f>1) data = rbind(data, tmp)
}

# set names
setnames(data, names(data), c('year', 'age', 'total', '1', '2', '3', '4', '5', 'country'))
data$total = NULL

# keep only ages in WPP range
data[age=='12-', age:='12']
data[age=='55+', age:='55']
data[, age:=as.numeric(age)]
data = data[age>=15 & age<=49]

# bring in iso codes
codes = fread(ccFile)
codes = codes[ihme_indic_country==1, c('countryname_ihme', 'iso3', 
				'countryname', 'gbd_super_region_name'), with=FALSE]
codes = unique(codes[countryname %in% unique(data$country)])
data = merge(data, codes, by.x='country', by.y='countryname')
data$country = NULL
setnames(data, c('countryname_ihme', 'gbd_super_region_name'), 
				c('country', 'super_region'))

# melt parity long
data[, ('4'):=get('4')+get('5')]
data[, ('5'):=NULL]
idVars = c('iso3', 'country', 'super_region', 'year', 'age')
data = melt(data, id.vars=idVars, variable.name='parity', value.name='births')
data[, parity:=as.numeric(as.character(parity))]

# collapse to 5-year age groups
data[, age:=5*floor(age/5)]
data = data[, list('births'=sum(births)), by=c(idVars, 'parity')]
# ------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------------
# Extrapolate years

# expand to all years
blankData = data[, min:=min(year), by='iso3']
blankData = blankData[year==min]
blankData[, births:=NA]
blankData[, min:=NULL]
data[, min:=NULL]
for(y in seq(min(data$year), 2020)) {
	if(y %in% unique(data$year)) next
	blankData[, year:=y]
	data = rbind(data, blankData)
}

# make sure there are no zero counts
data[births==0, births:=1]

# cubic spline interpolation
j=1
for(i in unique(data$iso3)) {
	for(a in unique(data$age)) {
		for(p in unique(data$parity)) {
			fitData = data[iso3==i & age==a & parity==p]
			lFit = smooth.spline(y=fitData[!is.na(births)]$births, fitData[!is.na(births)]$year)
			pred = predict(lFit, x=fitData$year)$y
			pred = data.table(iso3=i, year=fitData$year, age=fitData$age, parity=fitData$parity, interpolation=pred)
			if (j==1) interpolations = pred
			if (j>1) interpolations = rbind(interpolations, pred)
			j=j+1
		}
	}
}
data = merge(data, interpolations, by=c('iso3','year','age','parity'))

# graph 4 random countries to confirm that it looks ok
ggplot(data[iso3 %in% sample(unique(data$iso3), 4) & parity==1], aes(y=births, x=year, color=factor(age), group=factor(age))) + 
	geom_point() + 
	geom_line(aes(y=interpolation)) + 
	facet_wrap(~country, scales='free')
	
# graph 4 random countries to confirm that it looks ok
ggplot(data[iso3 %in% sample(unique(data$iso3), 4) & parity==2], aes(y=births, x=year, color=factor(age), group=factor(age))) + 
	geom_point() + 
	geom_line(aes(y=interpolation)) + 
	facet_wrap(~country, scales='free')
	
# graph 4 random countries to confirm that it looks ok
ggplot(data[iso3 %in% sample(unique(data$iso3), 4) & parity==3], aes(y=births, x=year, color=factor(age), group=factor(age))) + 
	geom_point() + 
	geom_line(aes(y=interpolation)) + 
	facet_wrap(~country, scales='free')
# -----------------------------------------------------------------------------------------------------


# ----------------------------------------
# Save

# finish up
data[, births:=interpolation]
data$interpolation = NULL

# limit this to positive values (log-transforming leads to run-away estimates)
data[births<0, births:=1]

# Save
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------
