## Assignment: Exploratory data analysis - Course Project 2
## Author: Puneet Singla
## Problem Statement: Compare emissions from motor vehicle sources in Baltimore City with emissions
## from motor vehicle sources in Los Angeles County, California (fips == "06037").
## Which city has seen greater changes over time in motor vehicle emissions?

## This R script has one function: plot6

plot6 <- function() {
  
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
  pm2_5_bltmre_la <- subset(pm2_5, fips == "24510" | fips == "06037")
  pm2_5_bltmre_la_mv <- merge(pm2_5_bltmre_la, src_class_cd_mv, by.x = "SCC", by.y = "SCC")
  pm2_5_bltmre_la_mv <- group_by(pm2_5_bltmre_la_mv, year, fips)
  pm2_5_bltmre_la_mv_fips_yr <- summarize(pm2_5_bltmre_la_mv, ttl_Emissions=sum(Emissions))
  
  ## Generate Plot1
  par(mfrow = c(1,1))
  out <- qplot(year, ttl_Emissions, data = pm2_5_bltmre_la_mv_fips_yr, color = fips, geom = "line",
               xlab = "Year", ylab = "Total PM2.5 Emissions (in tons)",
               main = "Total PM2.5 Emissions in Baltimore & Los Angeles from Motor Vehicle",
               ylim = range(0, max(pm2_5_bltmre_la_mv_fips_yr$ttl_Emissions)))
  print(out+theme_bw())
  
  ## Generate PNG file
  dev.copy(png, file="plot6.png")
  dev.off()
  
}