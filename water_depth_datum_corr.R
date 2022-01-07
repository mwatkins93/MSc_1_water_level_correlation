####-------------------------
## Matt Watkins
## Date: Nov. 1st/21
## Project: Water depth datum correlation
## Objective: Tie all water depth data to rebar measurements for each catchment
## Inputs: water depth RDS file / rebar measurement excel file
## Outputs: a clean, adjoined dataframe
####-------------------------

## Prepare
##---------------------------

library(tidyverse)
library(readxl) # that should be okay for preliminary stuff
library(openxlsx)
library(lubridate)


## Import
##---------------------------

rebar <- read.xlsx("rebar_calc_2021_v2.xlsx", detectDates = TRUE) # read in rebar measurements

water_depth <- readRDS("water_depth_final.RDS") # read in water depth dataframe

## Tidy / Process
##---------------------------

## 1. Clean up rebar dataframe in prep for merge

rebar$date <- convertToDateTime(rebar$date) # convert excel numeric to functional date time!

rebar$notes <- NULL # remove notes column

colnames(rebar)[4] <- "time" # rename date column to parallel water depth df

## 2. Assign logger IDs to column based on site

rebar_ids <- rebar %>% 
  mutate(water.loggerID = case_when(
  site == "WS 11" ~ "20402300",
  site == "WS 17" ~ "20440793",
  site == "SBC" ~ "20440791",
  site == "SSR" ~ "20715043",
  site == "WS 54" ~ "20715030",
  site == "WS 52" ~ "20402298",
  site == "WS 47" ~ "20525614",
  site == "WS 46" ~ "20525616",
  site == "WS 45" ~ "20440766",
  site == "WS 44" ~ "20484018",
  site == "WS 43" ~ "20440889",
  site == "WS 40" ~ "20440923",
  site == "WS 36" ~ "20996920",
  site == "WS 66" ~ "20879414",
  site == "WS 67" ~ "20999513",
  site == "BL1" ~ "20959000",
  site == "BL2" ~ "20833388",
  site == "WS 73" ~ "20833380",
  site == "WS 110" ~ "20958989",
  site == "WS 84" ~ "20879408",
  site == "WS 87" ~ "20833386",
  site == "WS 92" ~ "20999525",
  site == "WS 93" ~ "20996919",
  site == "WS 96" ~ "20996923",
  site == "WS 104" ~ "20996916",
  site == "WS 108" ~ "20996921"))

rebar_ids$water.loggerID <- as.numeric(rebar_ids$water.loggerID)

## 3. Merge dfs

attr(water_depth$time, "tzone") ## check timezone of each
attr(rebar_ids$time, "tzone") ## this is probably the problem!

rebar_ids$time <- as.POSIXct(format(rebar_ids$time), tz = "America/Los_Angeles") ## just change this to A/V to avoid time-shifting problems in later scripts

datJoin <- left_join(water_depth, rebar_ids)

attr(datJoin$time, "tzone") # triple confirm - we're good.

## Plotting
##---------------------------

## Saving / Exporting
##---------------------------

## Save adjoined rebar/water depth dataframe as an RDS

saveRDS(datJoin, file = "rebar_waterdepth.RDS")

## Notes / Junk Code
##---------------------------
