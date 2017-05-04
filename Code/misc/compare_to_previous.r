# ----------------------------------------------
# David Phillips
# 
# 5/3/2017
# Compare new estimates to a previous run
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
# --------------------


# ----------------------------------------------------------------------------------------------
# Files/directories/lists

# dates associated with old and new files
oldDate ='050417'
newDate ='010517'

# input files
rootFile ='./Data/VSPI_Estimates/VSPI_B_Data .csv'
oldFile = gsub('.csv', paste0(oldDate, '.csv'), rootFile)
newFile = gsub('.csv', paste0(newDate, '.csv'), rootFile)

# output file
outFile = paste('./Visualizations/Comparisons/VSPI Comparison', oldDate, 'vs', newDate, '.pdf')
# ----------------------------------------------------------------------------------------------


# --------------------------------------------------
# Load/prep data

# load data
oldData = fread(oldFile)
newData = fread(newFile)

# merge
data = merge(oldData, newData, by=c('iso3','year'))
# --------------------------------------------------


# --------------------------------------------------
# Graph

p1 = ggplot(data, aes(y=vspi_b_ma.y, x=vspi_b_ma.x)) + 
	geom_point() + 
	geom_abline(slope=1, intercept=0, color='red', lty='dashed', size=1.25) + 
	labs(y='New VSPI', x='Old VSPI', title='Old vs New', subtitle='All Country Years') + 
	theme_bw()

iso3s = unique(data$iso3)
inc = 12
plots = list()
j=1
for(i in seq(1, length(iso3s), by=inc)) {
	tmpiso3s = iso3s[i:(i+inc-1)]
	plots[[j]] = ggplot(data[iso3 %in% tmpiso3s], aes(y=vspi_b_ma.y, x=vspi_b_ma.x)) + 
		geom_point() + 
		geom_abline(slope=1, intercept=0, color='red', lty='dashed', size=1.25) + 
		facet_wrap(~iso3) + 
		labs(y='New VSPI', x='Old VSPI', title='Old vs New', subtitle='Only Observations with a Large Difference') + 
		theme_bw()
	j=j+1
}
# --------------------------------------------------


# --------------------------------------------------
# Save graphs
pdf(outFile, height=6, width=9)
p1
for(i in seq(length(plots))) print(plots[[i]])
dev.off()
# --------------------------------------------------
