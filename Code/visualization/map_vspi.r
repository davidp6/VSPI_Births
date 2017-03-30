# load the function
rm(list=ls())
source('./Code/visualization/gbd_map_function.r')
library(foreign)
library(RColorBrewer)

# read in and format data
data = fread('./Data/VSPI_Estimates/Data 240317.csv')
data[, mapvar:=vspi_b_ma]

# keep only most recent year with data available
data[!is.na(unspecified_age), tmp:=year]
data[, maxyear:=max(tmp, na.rm=TRUE), by='iso3']
data = data[year==maxyear]
data = data.frame(data)

colors = brewer.pal(5, 'RdYlBu')

# make map
gbd_map(data=data,
			limits=c(0, .3, .6, .8, .9, 1), 
			label=c('<.3', '.3-.6', '.6-.8', '.8-.9', '.9-1'), 
			col=colors,
			col.reverse=FALSE, 
			title='VSPI-B', 
			fname='./Visualizations/vspi_map.pdf') 
