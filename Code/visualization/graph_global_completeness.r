# -----------------------------------------------------
# David Phillips
#
# 2/3/2018
# Make a graph of global completeness in available data
# -----------------------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
# -------------------


# -----------------------------------------------------------
# Files/directories/lists

# input file
inFile = './Data/VSPI_Estimates/VSPI_B_Data 041217.csv'

# birth estimates to compute completeness
birthFile = './Data/Envelopes/Envelope.csv'

# output file
outFile = './Visualizations/Envelopes/global_completeness.pdf'
# -----------------------------------------------------------


# -----------------------------------------------------------------------
# Load/prep data

# load
allData = fread(inFile)

# load envelope
envelope = fread(birthFile)

# collapse both to global level
data = allData[, list(registered_births=sum(total,na.rm=TRUE)), by='year']
envelope = envelope[, list(births=sum(births_est,na.rm=TRUE)), by='year']

# merge
data = merge(data, envelope, by='year', all=TRUE)

# compute completeness
data[, completeness:=registered_births/births*100]

# exclude years with no data
data = data[year<2016]
# -----------------------------------------------------------------------


# -------------------------------------------------------------------------
# Set up to graph

# titles
title = ''
ytitle = 'Births Registered in Public Databases (%)'
xtitle = ''

# mark notable time points
indiaYear = 2010
indiaHeight = 27
declineYear = 2013
declineHeight = 40.5
oddYear = 1990
oddHeight = 19
increaseYear = 2005
increaseHeight = 15.5

# labels for notable time points
indiaLabel = 'India begins\npublicly-reporting\nbirths (2011)'
declineLabel = 'Many countries\nhave reporting delays\nby 4 years or more'
oddLabel = 'Data availability\ninconsistent\n(early 1990\'s)'
increaseLabel = 'Brazil, Mexico, South Africa\nimprove completeness\n(late 2000\'s)'
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Graph
p = ggplot(data, aes(x=year, y=completeness)) + 
	geom_line(size=1.25, color=brewer.pal(6,'Blues')[6]) +
	annotate('text', y=indiaHeight, x=indiaYear, label=indiaLabel, hjust=1) + 
	annotate('text', y=declineHeight, x=declineYear, label=declineLabel, hjust=0) + 
	annotate('text', y=oddHeight, x=oddYear, label=oddLabel, hjust=1) + 
	annotate('text', y=increaseHeight, x=increaseYear, label=increaseLabel, hjust=0) + 
	labs(title=title, y=ytitle, x=xtitle) + 
	scale_x_continuous(limits=c(1980, 2020)) + 
	scale_y_continuous(limits=c(10, 45)) + 
	theme_bw(base_size=16) + 
	theme(plot.title=element_text(hjust=.5), plot.caption=element_text(size=8))
# -------------------------------------------------------------------------------------


# ------------------------------
# Save
pdf(outFile, height=6, width=9)
p
dev.off()
# ------------------------------
