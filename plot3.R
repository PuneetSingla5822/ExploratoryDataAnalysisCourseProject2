## Assignment: Exploratory data analysis - Course Project 2
## Author: Puneet Singla
## Problem Statement: Of the four types of sources indicated by the type(point, nonpoint, onroad, nonroad)
## variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
## Which have seen increases in emissions from 1999–2008?
## Use the ggplot2 plotting system to make a plot answer this question.

## This R script has one function: plot3

plot3 <- function() {
  
  library(dplyr)
  library(ggplot2)
  
  ## Declare file path
  url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  flnm_zip = "emissions_data.zip"
  
  ## Download the zip file and unzip it
  if(!file.exists(flnm_zip)) {
    download.file(url, flnm_zip)
    unzip(flnm_zip)
  }
  
  ## Read the data file
  pm2_5 <- readRDS("summarySCC_PM25.rds")
  src_class_cd <- readRDS("Source_Classification_Code.rds")
  
  ## Transform data
  pm2_5_bltmre <- subset(pm2_5, fips == "24510")
  pm2_5_bltmre <- group_by(pm2_5_bltmre, type, year)
  pm2_5_bltmre_yr_typ <- summarize(pm2_5_bltmre, ttl_Emissions=sum(Emissions))
  
  ## Generate Plot1
  par(mfrow = c(1,1))
  out <- qplot(year, ttl_Emissions, data = pm2_5_bltmre_yr_typ, facets = .~type, geom = "line",
               xlab = "Year", ylab = "Total PM2.5 Emissions (in tons)", main = "Total PM2.5 Emissions in Baltimore",
               ylim = range(0, max(pm2_5_bltmre_yr_typ$ttl_Emissions)))
  print(out+theme_bw())
  
  ## Generate PNG file
  dev.copy(png, file="plot3.png")
  dev.off()
  
}