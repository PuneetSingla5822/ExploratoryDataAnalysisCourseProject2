## Assignment: Exploratory data analysis - Course Project 2
## Author: Puneet Singla
## Problem Statement: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

## This R script has one function: plot5

plot5 <- function() {
  
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
  src_class_cd_mv <- src_class_cd[grep("vehicle",src_class_cd$EI.Sector,ignore.case = TRUE),]
  pm2_5_bltmre <- subset(pm2_5, fips == "24510")
  pm2_5_bltmre_mv <- merge(pm2_5_bltmre, src_class_cd_mv, by.x = "SCC", by.y = "SCC")
  pm2_5_bltmre_mv <- group_by(pm2_5_bltmre_mv, year)
  pm2_5_bltmre_mv_yr <- summarize(pm2_5_bltmre_mv, ttl_Emissions=sum(Emissions))
  
  ## Generate Plot1
  par(mfrow = c(1,1))
  plot(pm2_5_bltmre_mv_yr$year, pm2_5_bltmre_mv_yr$ttl_Emissions, type = "n",
       xlab = "Year", ylab = "Total PM2.5 Emissions (in tons)",
       main = "Total PM2.5 Emissions in Baltimore from Motor Vehicle",
       ylim = range(0, max(pm2_5_bltmre_mv_yr$ttl_Emissions)))
  points(pm2_5_bltmre_mv_yr$year, pm2_5_bltmre_mv_yr$ttl_Emissions, pch = 8, col = "blue", lwd = 2)
  lines(pm2_5_bltmre_mv_yr$year, pm2_5_bltmre_mv_yr$ttl_Emissions, col = "blue", pch = 8, lwd = 2)
  
  ## Generate PNG file
  dev.copy(png, file="plot5.png")
  dev.off()
  
}