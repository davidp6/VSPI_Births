# --------------------------------------------
# David Phillips
#
# 3/30/2017
# Make standard time series graphs of VSPI-B
# --------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
# --------------------


# --------------------------------------------------------------
# Files/directories/lists

# input file
inFile ='./Data/VSPI_Estimates/VSPI_B_Data 200417.csv'

# country codes
ccFile = './Data/Country_Data/countrycodes.csv'

# output file
outFile = './Visualizations/VSPI/vspi_estimates_Data 200417.pdf'
# --------------------------------------------------------------


# -------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)
data[, births:=as.numeric(births)]
data = data[!is.na(births)]

# country codes
codes = fread(ccFile)
codes = codes[ihme_indic_country==1, c('iso3', 'countryname_ihme'), with=FALSE]
setnames(codes, 'countryname_ihme', 'country')
codes = unique(codes)
data = merge(data, codes, by='iso3')
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Create graphing function
graph = function(subData, showOrig) {
	# country
	c = unique(subData$country)
	
	# whether there is data
	subData[is.na(unspecified_age), isdata:='No Data']
	subData[!is.na(unspecified_age), isdata:='Data']

	# graph completeness inset
	plotc = ggplot(subData, aes(y=completeness, x=year)) + 
				geom_line() + 
				labs(title='Completeness', x='', y='') + 
				ylim(0,1) + 
				theme_bw() + 
				theme(plot.title=element_text(hjust=.5), 
					panel.grid.major=element_line(linetype='dotted'),
					panel.grid.minor=element_line(linetype='dotted'))
	
	# graph age inset
	plota = ggplot(subData, aes(y=1-unspecified_age, x=year)) + 
				geom_line() + 
				labs(title='Age Unspecified', x='', y='') + 
				ylim(0,1) + 
				theme_bw() + 
				theme(plot.title=element_text(hjust=.5), 
					panel.grid.major=element_line(linetype='dotted'),
					panel.grid.minor=element_line(linetype='dotted'))
	
	# graph sex inset
	plots = ggplot(subData, aes(y=1-unspecified_sex, x=year)) + 
				geom_line() + 
				labs(title='Sex Unspecified', x='', y='') + 
				ylim(0,1) + 
				theme_bw() + 
				theme(plot.title=element_text(hjust=.5), 
					panel.grid.major=element_line(linetype='dotted'),
					panel.grid.minor=element_line(linetype='dotted'))
	
	# graph parity inset
	plotp = ggplot(subData, aes(y=1-unspecified_parity, x=year)) + 
				geom_line() + 
				labs(title='Parity Unspecified', x='', y='') + 
				ylim(0,1) + 
				theme_bw() + 
				theme(plot.title=element_text(hjust=.5), 
					panel.grid.major=element_line(linetype='dotted'),
					panel.grid.minor=element_line(linetype='dotted'))
	
	# graph main plot
	plotm = ggplot(subData, aes(y=vspi_b_ma, x=year, shape=isdata, group=iso3)) +
				geom_hline(yintercept=.6, color='#ff0000', lty='longdash') + 
				geom_hline(yintercept=.8, color='#ff7f00', lty='longdash') + 
				geom_hline(yintercept=.9, color='#00b000', lty='longdash') + 
				geom_line(size=1, color='#008BA0') + 
				scale_shape_manual('', values=c(19, 0)) + 
				labs(title=c, subtitle='VSPI-B', x='', y='Performance') + 
				ylim(0,1) + 
				theme_bw() + 
				theme(plot.title=element_text(hjust=.5, size=16), 
					plot.subtitle=element_text(hjust=.5, size=12), 
					legend.title = element_blank(), 
					legend.box='vertical', 
					legend.background=element_rect(size=0.5, linetype='solid', color='black'), 
					panel.grid.major=element_line(linetype='dotted'),
					panel.grid.minor=element_line(linetype='dotted'))

	# option to show original (unsmoothed) values or not
	if (showOrig) {
		colors = c('Observed'='grey70', 'Smoothed'='#008BA0')
		plotm = plotm + geom_point(aes(y=vspi_b, color='Observed'), size=2.5, alpha=.7) + 
		geom_point(size=2.5, aes(color='Smoothed')) + 
		scale_color_manual('', values=colors) + 
		theme(legend.position=c(0.15,0.23)) 
	}
	if (!showOrig) { 
		plotm = plotm + geom_point(size=2.5, color='#008BA0') + 
		theme(legend.position=c(0.15,0.15)) 
	}
	
	p = arrangeGrob(plotm, arrangeGrob(plotc, plota, plots, plotp), ncol=2)
}
# -------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Make graphs

# produce graphs
plots = lapply(unique(data$iso3), function(i) graph(data[iso3==i], showOrig=TRUE))

# Save
pdf(outFile, height=6, width=10)
for(p in seq(length(plots))) { 
	grid.newpage()
	grid.draw(plots[[p]])
}
dev.off()
# --------------------------------------------------------------------------------
