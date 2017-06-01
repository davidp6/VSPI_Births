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


# -------------------------------------------------------------------
# Files/directories/lists

# input file
inFile = './Data/Country_Data/Country_Year_Age_Sex_Parity_Births.csv'

# output file
outFile = './Visualizations/Envelopes/births_estimates.pdf'
# -------------------------------------------------------------------


# ---------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# exclude before 1980 and after 2017
data = data[year>=1980 & year<=2017]

# format labels
data[sex=='m', sex:='Male']
data[sex=='f', sex:='Female']
data[, parity:=as.character(parity)]
data[parity=='4', parity:='4+']
data[, parity:=paste('Birth Order:',parity)]
data[, age:=factor(age, levels=rev(unique(age)), order=TRUE)]

# 1000s of births
data[, births:=births/1000]
# ---------------------------------------------


# ---------------------------------------------------------------------------
# Set up to graph

# levels of graph
iso3s = unique(data$iso3)

# colors
colors = brewer.pal(8, 'YlGnBu')[2:8]
# ---------------------------------------------------------------------------


# -------------------------------------------------------------------------------
# Graph
plots = list()
n=1
for(i in iso3s) { 
	country = unique(data[iso3==i]$country)
	
	plots[[i]] = ggplot(data[iso3==i], aes(x=year, y=births, color=factor(age), group=factor(age))) + 
		geom_point() + 
		geom_line() + 
		scale_color_manual('Maternal Age', values=colors) + 
		facet_grid(sex~parity, scales='free') + 
		labs(title=country, y='Births (1,000s)', x='Maternal Age') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5, size=16), axis.title=element_text(size=14))
	n=n+1
}
# -------------------------------------------------------------------------------


# -------------------------------------------
# Save
pdf(outFile, height=6, width=10)
for(i in seq_along(iso3s)) print(plots[[i]])
dev.off()
# -------------------------------------------
