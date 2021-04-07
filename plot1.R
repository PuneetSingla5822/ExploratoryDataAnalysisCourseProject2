## Assignment: Exploratory data analysis - Course Project 2
## Author: Puneet Singla
## Problem Statement: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
## Using the base plotting system, make a plot showing the total PM2.5 emission
## from all sources for each of the years 1999, 2002, 2005, and 2008.

## This R script has one function: plot1

plot1 <- function() {
  
  library(dplyr)
  
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
  pm2_5 <- group_by(pm2_5, year)
  pm2_5_yr <- summarize(pm2_5, ttl_Emissions=sum(Emissions))
  
  ## Generate Plot1
  par(mfrow = c(1,1))
  plot(pm2_5_yr$year, pm2_5_yr$ttl_Emissions, type = "n",
       xlab = "Year", ylab = "Total PM2.5 Emissions (in tons)", main = "Total PM2.5 Emissions in the US",
       ylim = range(0, max(pm2_5_yr$ttl_Emissions)))
  points(pm2_5_yr$year, pm2_5_yr$ttl_Emissions, pch = 8, col = "blue", lwd = 2)
  lines(pm2_5_yr$year, pm2_5_yr$ttl_Emissions, col = "blue", pch = 8, lwd = 2)
  
  ## Generate PNG file
  dev.copy(png, file="plot1.png")
  dev.off()
  
}