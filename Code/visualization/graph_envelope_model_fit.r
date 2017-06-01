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
library(ggplot2)
library(RColorBrewer)
# -------------------


# ------------------------------------------------------------------------------------
# Files/directories/lists

# envelope model version
run_name = 'sur_quadratic_full'

# input file
inFilecya = './Data/Country_Data/WPP_Country_Year_Age_Estimates.csv'
inFilecys = './Data/Country_Data/WPP_Country_Year_Sex_Estimates.csv'
inFiletfr = './Data/Country_Data/WPP_Country_Year_TFR_Estimates.csv'
inFilecyap = './Data/Country_Data/HFD_Country_Year_Age_Parity_Estimates.csv'

# envelope file
inFileEnv = paste0('./Data/Envelopes/Envelope_', run_name, '.csv')

# output file
outFile = paste0('./Visualizations/Envelopes/births_estimates_fit_', run_name, '.pdf')
# ------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Load/prep data

# load WPP data
wpp = fread(inFilecya)
hfd = fread(inFilecyap)
wppsex = fread(inFilecys)

# load
estimates = fread(inFileEnv)

# collapse to age and age-parity levels
ageLevel = estimates[, list(births=sum(births)), by=c('iso3','year','age')]
sexRatios = estimates[, list(births=sum(births)), by=c('iso3','year','sex')]
sexRatios = dcast.data.table(sexRatios, iso3+year~sex)
sexRatios[, mf_ratio:=m/f]
apLevel = estimates[, list(births=sum(births)), by=c('iso3','year','age','parity')]
wpp = merge(wpp, ageLevel, by=c('iso3','year','age'), suffixes=c('','_est'))
hfd = merge(hfd, apLevel, by=c('iso3','year','age','parity'), suffixes=c('','_est'))
wppsex = merge(wppsex, sexRatios, by=c('iso3','year'), suffixes=c('','_est'))

# format
hfd[, parity:=as.character(parity)]
hfd[parity=='4', parity:='4+']
hfd[, parity:=paste('Birth Order:',parity)]
hfd[, age:=factor(age, levels=rev(unique(age)), order=TRUE)]
# ---------------------------------------------------------------------------------


# -------------------------------------
# Set up to graph

# levels of graph
iso3s = unique(hfd$iso3)

# colors
colors = brewer.pal(8, 'YlGnBu')[2:8]
# -------------------------------------


# -------------------------------------------------------------------------------
# Graph
plots = list()
n=1
for(i in iso3s) { 
	country = unique(hfd[iso3==i]$country)
	
	plots[[i]] = ggplot(hfd[iso3==i], aes(x=year, y=births, color=age)) + 
		geom_point() + 
		geom_line(aes(y=births_est)) + 
		facet_wrap(~parity, scales='free') + 
		scale_color_manual('Maternal Age', values=colors) + 
		labs(title=country, y='Births', x='Maternal Age') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=16), axis.title=element_text(size=14))
	n=n+1
}

p1 = ggplot(hfd, aes(x=births, y=births_est, color=super_region)) + 
	geom_point(alpha=.5) + 
	geom_abline(slope=1, intercept=0, color='red', linetype='dashed') + 
	scale_color_manual('', values=brewer.pal(3, 'Set1')) + 
	labs(title='All Countries', y='Estimated Births', x='Input Births') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5, size=16), axis.title=element_text(size=14))
# -------------------------------------------------------------------------------


# -------------------------------------------
# Save
pdf(outFile, height=6, width=10)
p1
for(i in seq_along(iso3s)) print(plots[[i]])
dev.off()
# -------------------------------------------
