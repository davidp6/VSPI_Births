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
library(grid)
library(gridExtra)
# -------------------


# -------------------------------------------------------------------
# Files/directories/lists

# envelope model version
run_name = ''

# envelope file
inFile = paste0('./Data/Envelopes/Envelope', run_name, '.csv')

# output file
outFile = './Visualizations/Envelopes/births_estimates.pdf'
# -------------------------------------------------------------------


# --------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# exclude before 1980 and after 2017
data = data[year>=1980 & year<=2017]

# format labels
data[, sex:=ifelse(sex==1, 'Male', 'Female')]
data[, parity:=as.character(parity)]
data[, parity:=ifelse(parity==4, '4+', as.character(parity))]
data[, parity:=paste('Birth Order:', parity)]
data[, birthweight:=paste(birthweight, 'g')]
data[, age:=factor(age, levels=rev(unique(age)), order=TRUE)]

# 1000s of births
data[, births_est:=births_est/1000]

# aggregate
national = data[, list(births_est=sum(births_est)), by=c('iso3','country','year')]
bySex = data[, list(births_est=sum(births_est)), by=c('iso3','country','year','sex')]
byAge = data[, list(births_est=sum(births_est)), by=c('iso3','country','year','age')]
byParity = data[, list(births_est=sum(births_est)), by=c('iso3','country','year','parity')]
byBirthweight = data[, list(births_est=sum(births_est)), by=c('iso3','country','year','birthweight')]
byVars = c('age','sex','parity','birthweight')
noCY = data[, list(births_est=sum(births_est)), by=byVars]
noCY[, pred:=births_est/sum(births_est), by='age']
# --------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Set up to graph

# levels of graph
iso3s = unique(data$iso3)

# colors
colors = brewer.pal(8, 'YlGnBu')[2:8]
colorsCat = brewer.pal(7, 'Paired')
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------
# Graph

# plots without country or year
p1 = ggplot(noCY, aes(x=age, y=pred, color=sex, group=sex)) + 
		geom_line() + 
		facet_grid(birthweight~parity, scales='free') + 
		scale_color_manual('Maternal Age', values=colors[c(2,4)]) + 
		labs(title='Global Distributions', subtitle='Births by Maternal Age, Birth Weight, Sex and Parity', 
			y='Proportion of Births', x='Maternal Age') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=16), plot.subtitle=element_text(hjust=.5), 
			axis.title=element_text(size=14))

plots = list()
n=1
for(i in iso3s) { 
	country = unique(data[iso3==i]$country)
	
	# assemble multiple aggregated plots 
	tmpPlot1 = ggplot(national[iso3==i], aes(x=year, y=births_est)) + 
		geom_point() + 
		geom_line() + 
		labs(title='National', y='Births (1,000s)', x='Year') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=12), axis.title=element_text(size=11))
	
	tmpPlot2 = ggplot(bySex[iso3==i], aes(x=year, y=births_est, color=sex, group=sex)) + 
		geom_point() + 
		geom_line() + 
		scale_color_manual('', values=colorsCat) + 
		labs(title='By Sex', y='Births (1,000s)', x='') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=12), legend.justification = c(0, 1), legend.position = c(0, 1), legend.text=element_text(size=6), legend.background=element_rect(fill='transparent'))
	
	tmpPlot3 = ggplot(byAge[iso3==i], aes(x=year, y=births_est, color=age, group=age)) + 
		geom_point() + 
		geom_line() + 
		scale_color_manual('', values=colors) + 
		labs(title='By Age', y='Births (1,000s)', x='') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=12), legend.justification = c(0, 1), legend.position = c(0, 1), legend.text=element_text(size=6), legend.background=element_rect(fill='transparent'))
	
	tmpPlot4 = ggplot(byParity[iso3==i], aes(x=year, y=births_est, color=parity, group=parity)) + 
		geom_point() + 
		geom_line() + 
		scale_color_manual('', values=colorsCat) + 
		labs(title='By Parity', y='Births (1,000s)', x='Year') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=12), axis.title=element_text(size=11), legend.justification = c(0, 1), legend.position = c(0, 1), legend.text=element_text(size=6), legend.background=element_rect(fill='transparent'))
	
	tmpPlot5 = ggplot(byBirthweight[iso3==i], aes(x=year, y=births_est, color=birthweight, group=birthweight)) + 
		geom_point() + 
		geom_line() + 
		scale_color_manual('', values=colorsCat) + 
		labs(title='By Birth Weight', y='Births (1,000s)', x='') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=12), legend.justification = c(0, 1), legend.position = c(0, 1), legend.text=element_text(size=6), legend.background=element_rect(fill='transparent'))
		
	# assemble aggregate graphs
	plots[[n]] = grid.arrange(tmpPlot1, tmpPlot2, tmpPlot3, tmpPlot4, tmpPlot5, 
		ncol=3, top=textGrob(country,gp=gpar(fontsize=14)))
	n=n+1
	
	# graph everything together disaggregated
	plots[[n]] = ggplot(data[iso3==i], aes(x=year, y=births_est, color=factor(age), group=factor(age))) + 
		geom_point() + 
		geom_line() + 
		scale_color_manual('Maternal Age', values=colors) + 
		facet_grid(parity~birthweight+sex, scales='free') + 
		labs(title=country, y='Births (1,000s)', x='Year') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=16), axis.title=element_text(size=14))
	n=n+1
}
# ---------------------------------------------------------------------------------------------------------


# -------------------------------------------
# Save
pdf(outFile, height=6, width=10)
p1 
for(i in seq(length(plots))) plot(plots[[i]])
dev.off()
# -------------------------------------------
