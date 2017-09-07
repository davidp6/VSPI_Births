# --------------------------------------
# David Phillips
#
# 4/13/2017
# Make graphs of birth estimates by asp
# --------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(boot)
library(ggplot2)
library(RColorBrewer)
# -------------------


# ------------------------------------------------------------------------------------
# Files/directories/lists

# envelope model version
run_name = ''

# envelope file
inFileEnv = paste0('./Data/Envelopes/Envelope', run_name, '.csv')

# output file
outFile1 = paste0('./Visualizations/Envelopes/envelope_model_fit_year', run_name, '.pdf')
outFile2 = paste0('./Visualizations/Envelopes/envelope_model_fit_age', run_name, '.pdf')
outFile3 = paste0('./Visualizations/Envelopes/envelope_model_fit_year', run_name, '.pdf')
outFile4 = paste0('./Visualizations/Envelopes/envelope_model_fit_year', run_name, '.pdf')
outFile5 = paste0('./Visualizations/Envelopes/envelope_model_fit_year', run_name, '.pdf')
outFile6 = paste0('./Visualizations/Envelopes/envelope_model_fit_year', run_name, '.pdf')
# ------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFileEnv)

# format
data[, sex:=ifelse(sex==1, 'Male', 'Female')]
data[, parity:=ifelse(parity==4, '4+', as.character(parity))]
data[, parity:=paste('Birth Order:', parity)]
data[, birthweight:=paste(birthweight, 'g')]

# collapse out certain strata
byVars = c('iso3','country','year','age','parity','birthweight')
noSex = data[, list(births_est=sum(births_est), births_obs=sum(births_obs)), by=byVars]
byVars = c('iso3','country','year','age','birthweight')
noParity = data[, list(births_est=sum(births_est), births_obs=sum(births_obs)), by=byVars]
byVars = c('iso3','country','year','age','parity')
noBW = data[, list(births_est=sum(births_est), births_obs=sum(births_obs)), by=byVars]
byVars = c('iso3','country','year','age')
CYA = data[, list(births_est=sum(births_est), births_obs=sum(births_obs)), by=byVars]

# recompute CYA proportions in collapsed data
noSex[, prop:=births_obs/sum(births_obs), by=c('iso3','year','age')]
noSex[, pred:=births_est/sum(births_est), by=c('iso3','year','age')]
noParity[, prop:=births_obs/sum(births_obs), by=c('iso3','year','age')]
noParity[, pred:=births_est/sum(births_est), by=c('iso3','year','age')]
noBW[, prop:=births_obs/sum(births_obs), by=c('iso3','year','age')]
noBW[, pred:=births_est/sum(births_est), by=c('iso3','year','age')]
# ---------------------------------------------------------------------------------


# -------------------------------------
# Set up to graph

# levels of graph
iso3s = unique(data$iso3)

# colors
colors = brewer.pal(8, 'YlGnBu')[2:8]
# -------------------------------------


# -------------------------------------------------------------------------------
# Graph

# plots over time
timePlots = list()
n=1
for(i in iso3s) { 
	country = unique(data[iso3==i]$country)
	
	timePlots[[i]] = ggplot(data[iso3==i], aes(x=year, y=prop, color=age, group=age)) + 
		geom_point() + 
		geom_line(aes(y=pred)) + 
		facet_grid(parity~birthweight+sex, scales='free') + 
		scale_color_gradientn('Maternal Age', colors=colors) + 
		labs(title=country, subtitle='Births by Maternal Age, Birth Weight, Sex and Parity', 
			y='Proportion of Births', x='') + 
		scale_x_continuous(breaks=c(1990, 2010)) + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
			axis.title=element_text(size=14))
	n=n+1
}

# plots over age
agePlots = list()
n=1
for(i in iso3s) { 
	country = unique(data[iso3==i]$country)
	
	agePlots[[i]] = ggplot(data[iso3==i & year %in% seq(1980, 2016, by=5)], aes(x=age, y=prop, color=year, group=year)) + 
		geom_point() + 
		geom_line(aes(y=pred)) + 
		facet_grid(parity~birthweight+sex, scales='free') + 
		scale_color_gradientn('Year', colors=colors) + 
		labs(title=country, subtitle='Births by Year, Birth Weight, Sex and Parity', 
			y='Proportion of Births', x='Maternal Age') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
			axis.title=element_text(size=14))
	n=n+1
}

# plots without sex
collapsedPlots1 = list()
n=1
for(i in iso3s) { 
	country = unique(data[iso3==i]$country)
	
	collapsedPlots1[[i]] = ggplot(noSex[iso3==i & year %in% seq(1980, 2016, by=5)], aes(x=age, y=prop, color=year, group=year)) + 
		geom_point() + 
		geom_line(aes(y=pred)) + 
		facet_grid(parity~birthweight, scales='free') + 
		scale_color_gradientn('Year', colors=colors) + 
		labs(title=country, subtitle='Births by Year, Birth Weight and Parity', 
			y='Proportion of Births', x='Maternal Age') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
			axis.title=element_text(size=14))
	n=n+1
}

# plots without sex and parity
collapsedPlots2 = list()
n=1
for(i in iso3s) { 
	country = unique(data[iso3==i]$country)
	
	collapsedPlots3[[i]] = ggplot(noParity[iso3==i & year %in% seq(1980, 2016, by=5)], aes(x=age, y=prop, color=year, group=year)) + 
		geom_point() + 
		geom_line(aes(y=pred)) + 
		facet_wrap(~birthweight, scales='free') + 
		scale_color_gradientn('Year', colors=colors) + 
		labs(title=country, subtitle='Births by Year and Birth Weight', 
			y='Proportion of Births', x='Maternal Age') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
			axis.title=element_text(size=14))
	n=n+1
}

# plots without sex and birthweight
collapsedPlots3 = list()
n=1
for(i in iso3s) { 
	country = unique(data[iso3==i]$country)
	
	collapsedPlots3[[i]] = ggplot(noBW[iso3==i & year %in% seq(1980, 2016, by=5)], aes(x=age, y=prop, color=year, group=year)) + 
		geom_point() + 
		geom_line(aes(y=pred)) + 
		facet_wrap(~parity, scales='free') + 
		scale_color_gradientn('Year', colors=colors) + 
		labs(title=country, subtitle='Births by Year and Parity', 
			y='Proportion of Births', x='Maternal Age') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
			axis.title=element_text(size=14))
	n=n+1
}

# plots without sex, birthweight and parity
# -------------------------------------------------------------------------------


# -------------------------------------------
# Save
pdf(outFile, height=6, width=10)
p1
for(i in seq_along(iso3s)) print(plots[[i]])
dev.off()
# -------------------------------------------
