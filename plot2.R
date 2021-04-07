## Assignment: Exploratory data analysis - Course Project 2
## Author: Puneet Singla
## Problem Statement: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
## (fips == "24510") from 1999 to 2008?
## Use the base plotting system to make a plot answering this question.

## This R script has one function: plot2

plot2 <- function() {
  
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
  pm2_5_bltmre <- subset(pm2_5, fips == "24510")
  pm2_5_bltmre <- group_by(pm2_5_bltmre, year)
  pm2_5_bltmre_yr <- summarize(pm2_5_bltmre, ttl_Emissions=sum(Emissions))
  
  ## Generate Plot1
  par(mfrow = c(1,1))
  plot(pm2_5_bltmre_yr$year, pm2_5_bltmre_yr$ttl_Emissions, type = "n",
       xlab = "Year", ylab = "Total PM2.5 Emissions (in tons)", main = "Total PM2.5 Emissions in Baltimore",
       ylim = range(0, max(pm2_5_bltmre_yr$ttl_Emissions)))
  points(pm2_5_bltmre_yr$year, pm2_5_bltmre_yr$ttl_Emissions, pch = 8, col = "blue", lwd = 2)
  lines(pm2_5_bltmre_yr$year, pm2_5_bltmre_yr$ttl_Emissions, col = "blue", pch = 8, lwd = 2)
  
  ## Generate PNG file
  dev.copy(png, file="plot2.png")
  dev.off()
  
}