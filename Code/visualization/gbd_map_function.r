################################################################################
## Author: Laura Dwyer-Lindgren
## Date Created: 13 July 2011
## Edited by Roy Burstein 20 July 2011 .. and again on 30 Aug 2011
## Description: mapping function that takes the following arguments:
##
##  REQUIRED INPUT:
##    data        ->  A data frame with two variables, 'iso3' and 'mapvar'.
##    limits      ->  A vector that defines the bins for the map. This should be
##                    the minimum value, each of the break points in ascending order
##                    and then the maximum value. By default, the bins are currently
##                    assigned such that a value equal to a breakpoint is included in
##                    the bin where that breakpoint is the lower bound.
##
##  OPTIONAL INPUT:
##    labels      ->  Vector of the labels to use in the legend for each group.
##                    If this is not specified, these are assigned automatically
##                    based on the provided values in 'limits'.
##    pattern     ->  A vector, with length equal to the number of bins, of 
##                    of patterns to be used for shading in countries. 0 = no 
##                    shading (the default), 1=horizontal lines, 2=positively 
##                    sloping lines at a 45 degree angle, 3 = vertical lines, 
##                    4 = negatively sloping lines at a 45 degree angle. 
##    col         ->  Either a string specifying the name of a color palette from
##                    ColorBrewer () or a vector of colors.
##    col.reverse ->  Logical. Should the order of the colors be reversed (this
##                    may be useful when specifying a palette from ColorBrewer)
##    na.color    ->  The color to be used for countries not in the dataset
##                    (defaults to white)
##    title       ->  A string specifying the title (defaults to "")
##    fname       ->  The filename for saving the map. Currently supports '.tif'
##                    '.eps', and '.pdf'. If nothing is specifed, the map is
##                    printed to screen so long as there is not a pdf device
##                    already open (if there is a pdf device already open, the
##                    map is saved to this device; this is to allow for making
##                    pdfs with multiple figures using the onefile=T option in
##                    the pdf function)
##    legend.title -> The title for the legend
##    legend.columns -> The number of columns in the lengend. Defaults to 1 for 
##                    7 or less groups and 2 otherwise.   
##    legend.cex   -> Multiplier for the size of the legend
##    legend.shift -> Shift the legend (vector of size 2: horizontal shift and 
##                    vertical shift)
################################################################################



gbd_map <- function(data, limits, 
                    labels=NULL, 
                    pattern=NULL, col="RdYlBu", col.reverse=FALSE, na.color = "white", 
                    title="", fname=NULL, 
                    legend.title=NULL, legend.columns = NULL, legend.cex=1, legend.shift=c(0,0)) {
                    
  ## load libraries
  library(RColorBrewer)
  library(maptools)

  ## test for consistency
  if (length(limits)-1 != length(labels) & !is.null(labels)) stop("length(limits) must equal length(labels) + 1")
  if (length(limits)-1 != length(col) & length(col)>1) stop("number of colors does not match number of groups")
  if (length(limits)-1 != length(pattern) & !is.null(pattern)) stop("number of patterns does not match number of groups")
  if (!"iso3" %in% names(data) | !"mapvar" %in% names(data)) stop("data is missing iso3 or mapvar variables")

  ## set how device opens
  if (is.null(fname)) {
    if (!"pdf" %in% names(dev.list())) dev.new(width=10,height=7)
  } else if (grepl(".tif", fname, fixed=T)) {
    tiff(fname, width=10, height=6, units="in", res=400)
  } else if (grepl(".eps", fname, fixed=T)) {
    postscript(fname)
  } else if (grepl(".pdf", fname, fixed=T)) {
    pdf(fname, width=10, height=6)
  } else {
    stop("invalid file type: must be .tif, .eps, or .pdf")
  }
  
  ## load Roy's shapefile and prep data
  if (Sys.info()["sysname"] == "Linux") j <- "/home/j"
  else j <- "J:"
  load('./Data/Raw/Other/GBD_WITH_INSETS_NOSUBS_PREPPED.RData')
  data <- merge(data, map@data[,c("iso3", "ID")], all.y=T)
  kmlPolygon(border=.01)

  ## assign colors and patterns 
  data$color <- data$pattern <- NA
  n <- length(limits) - 1
  
  if (length(col)==1) {
    if (brewer.pal.info[col,1] >= n) mapcolors <- brewer.pal(n, col)
    else mapcolors <- colorRampPalette(brewer.pal(brewer.pal.info[col,1], col))(n)
  } else {
    mapcolors <- col
  } 
  if (col.reverse) mapcolors <- mapcolors[n:1]
  if (is.null(pattern)) pattern <- rep(0, n) 
  
  for (g in 1:n) {
    ii <- (data$mapvar >= limits[g] & data$mapvar <= limits[g+1]) 
    data$color[ii] <- mapcolors[g]
    data$pattern[ii] <- pattern[g]
  }
  
  data$color[is.na(data$color)] <- na.color
  data$pattern[is.na(data$pattern)] <- 0 
  
  data$density <- ifelse(data$pattern==0, 0, 30)
  data$angle <- as.numeric(as.character(factor(data$pattern, levels=1:4, label=c(0,45,90,135))))
  
  ## generate labels if necessary
  if (is.null(labels)) {
    for (g in 1:n) {
      labels <- c(labels, paste(limits[g], " to ", limits[g+1]))
    }
  }

  ## plot map
  par(lwd=0.1, mai=c(.02,.02,.4,.02))
  plot(map, col=data$color[order(data$ID)], cex=0.5)
  plot(map, density=data$density[order(data$ID)], angle=data$angle[order(data$ID)], add=T)
  
  ## plot legend
  angle <- as.numeric(as.character(factor(pattern, levels=1:4, label=c(0,45,90,135))))
  if (n <= 7) {
    if (is.null(legend.columns)) legend.columns <- 1
    legend(-170+legend.shift[1], 10+legend.shift[2], labels, fill=mapcolors, border=mapcolors, 
           cex=0.75*legend.cex, bg="white", box.col="white", ncol=legend.columns, title=legend.title)
    if (sum(pattern)>0) legend(-170+legend.shift[1], 10+legend.shift[2], labels, fill=NA, border=NA, density=ifelse(pattern==0, 0, 60), angle=angle, 
                               cex=0.75*legend.cex, box.col="white", ncol=legend.columns, title=legend.title) 
  } else { 
    if (is.null(legend.columns)) legend.columns <- 2
    legend(-180+legend.shift[1], 10+legend.shift[2], labels, fill=mapcolors, border=mapcolors, 
           cex=0.6*legend.cex, bg="white", box.col="white", ncol=legend.columns, title=legend.title)
    if (sum(pattern)>0) legend(-180+legend.shift[1], 10+legend.shift[2], labels, fill=NA, border=NA, density=ifelse(pattern==0, 0, 60), angle=angle, 
                               cex=0.6*legend.cex, box.col="white", ncol=legend.columns, title=legend.title) 
  } 
  title(main=title, cex=0.95)
     
  ## add boxes and labels
  draw.box <- function(v1,v2) lines(c(v1[1], v2[1], v2[1], v1[1], v1[1]), c(v1[2], v1[2], v2[2], v2[2], v1[2]))
  text(-140, -122.4, "Caribbean", pos=4, cex=0.35)
    draw.box(c(-174.2, -125), c(-83.2, -60.85))

  text(-80, -123, "LCA", pos=4, cex=.35)
    draw.box(c(-77, -125), c(-60.5,-105.1))
  text(-80, -100.1, "DMA", pos=4, cex=.35)
    draw.box(c(-77, -102.1), c(-60.5,-83.7))	
  text(-80, -78.7, "ATG", pos=4, cex=.35)
    draw.box(c(-77, -80.7), c(-60.5,-60.85))

  text(-60.5, -123, "TTO", pos=4, cex=.35)
    draw.box(c(-57.5, -125), c(-41,-105.1))
  text(-60.5, -100.1, "GRD", pos=4, cex=.35)
    draw.box(c(-57.5, -102.1), c(-41,-83.7))	
  text(-60.5, -78.7, "VCT", pos=4, cex=.35)
    draw.box(c(-57.5, -80.7), c(-41,-60.85))

  text(-41, -123, "TLS", pos=4, cex=.35)
    draw.box(c(-38, -125), c(-21.87,-105.1))	
  text(-41, -100.1, "MDV", pos=4, cex=.35)
    draw.box(c(-38, -102.1), c(-21.87,-83.7))
  text(-41, -78.7, "BRB", pos=4, cex=.35)
    draw.box(c(-38, -80.7), c(-21.87,-60.85))

  text(-21.87, -123, "SYC", pos=4, cex=.35)
    draw.box(c(-18.87, -125), c(-2.2,-105.1))
  text(-21.87, -100.1, "MUS", pos=4, cex=.35)
    draw.box(c(-18.87, -102.1), c(-2.2,-83.7))
  text(-21.87, -78.7, "COM", pos=4, cex=.35)
    draw.box(c(-18.87, -80.7), c(-2.2,-60.85))	

  text(1.8, -122.4, "Persian Gulf", pos=4, cex=0.35)
    draw.box(c(4.8,-125), c(28.932, -84.885))
  text(1.8, -81.5, "W Africa", pos=4, cex=0.35)
    draw.box(c(4.8, -79.8), c(28.932,-60.85))
  text(30.6, -81.5, "E Med.", pos=4, cex=0.35)
    draw.box(c(33.6, -79.84), c(55.993,-60.85))

  text(30.6, -100.1, "MLT", pos=4, cex=0.35)
    draw.box(c(33.6, -102.1), c(55.993,-84.885))
  text(30.6, -123, "SGP", pos=4, cex=0.35)
    draw.box(c(33.6, -125), c(55.993,-105.1))

  text(59.76, -122.4, "Balkan Peninsula", pos=4, cex=0.35)
    draw.box(c(62.76,-125), c(133.9, -60.85))	

  text(160, -123, "TON", pos=4, cex=0.35)
    draw.box(c(163,-125), c(182,-110))
  text(160, -106, "WSM", pos=4, cex=0.35)
    draw.box(c(163, -108), c(182, -96))
  text(160, -92, "FSM", pos=4, cex=0.35)
    draw.box(c(163, -94), c(182, -82))
   text(160, -78, "KIR", pos=4, cex=0.35)
    draw.box(c(163, -80), c(182,-60.8))

  text(136, -123, "FJI", pos=4, cex=0.35)
    draw.box(c(139,-125), c(159,-110))
  text(136, -106, "VUT", pos=4, cex=0.35)
    draw.box(c(139, -108), c(159, -96))
  text(136, -92, "SLB", pos=4, cex=0.35)
    draw.box(c(139, -94), c(159, -82))
   text(136, -78, "MHL", pos=4, cex=0.35)
    draw.box(c(139, -80), c(159,-60.8))	

  ## close device
  if(!is.null(fname)) dev.off()
}

