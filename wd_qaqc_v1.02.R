####-------------------------
## Matt Watkins
## Date: Nov. 15th/21
## Project: Water depth QA/QC v1.02
## Objective: To prepare a continous water level at a corrected daturm for each site!
## Inputs: rebar_waterdepth.RDS
## Outputs: (1) corrected graphs for each site; (2) diagnostic graphs for each site
####-------------------------

## 0. Notes ----
##---------------------------

## Every finished best case WS: 'wsXX_dat' except 'ws44_out'

## Finished dataframe graphs format: 'wsXX_offset'

## Nine special case watersheds for either (1) rebar movements or (2) unique cases: WS 108, WS 104, WS 93, WS 92, WS 77/76, WS 73, WS 67, WS 17, WS 11

## 1. Prepare ----
##---------------------------

library(tidyverse)
library(dygraphs)
library(plotly)
library(lubridate)
library(readxl)

options(scipen = 999)

## 2. Import ----
##---------------------------

water_depth <- readRDS("rebar_waterdepth.RDS") ## corrected water depth

ssm_precip <- read_csv("May_to_Oct Precipitation.csv") ## precip

## 3. Tidy / Process ----
##---------------------------

## 3.1 Start with WS44 - rebar looks okay (within 3.4cm for all bed to top measurements - most likely due to inconsistent tape stake depth in channel bed beside rebar or sediment movement) ----
##--------------------------------------------

ws44_dat <- water_depth %>% 
  filter(water.loggerID %in% "20484018") ## start with ws44

ws44_datum <- ws44_dat %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average)

ws44_out <- ws44_datum %>% 
  mutate(water.metres = water.depth * (1000 / 9806.65)) # Convert water depth (kPa) to metres to match rebar units = 1000kPa:1kPa; 1mH20:9806.65Pal thus, kPa = 1000Pa / 9806.65 gets metres.

## 3.2 Start removing the easy data (pre-installation and post-removal) ----
##--------------------------------------------------------------------------

is.na(ws44_out$`water.metres`) <- ws44_out$time < "2021-05-31 12:00:00" ## logger installed at 12:15 on May 31st - every value before 12PM on that date set to NA.

is.na(ws44_out$`water.metres`) <- ws44_out$time >= "2021-10-24 12:00:00" ## everything from this hour onward includes post-removal

## 3.3 Adjust water level to rebar datum ----
##-------------------------------------------

## Reinstall offset 1: June 13th 1PM  to > Aug. 11th 1PM

ws44_out$water.metres[407:1822] <- ws44_out$water.metres[407:1822] + 0.0504468 # increase to match (difference between the averaged water level the hour prior to reinstalling and the new water level after reinstalling logger)

## Reinstall offset 2: >= Aug 11th 1PM - <= Oct. 24th 11AM

ws44_out$water.metres[1823:3597] <- ws44_out$water.metres[1823:3597] + .1330608 ## increase to match - WS 44 is done now

## 3.4 Continue process for all remaining watersheds ----
##-------------------------------------------------------

## WS 108 - has out of water data (removed it, but do not know if this is correct)
##-------

ws108_dat <- water_depth %>% 
  filter(water.loggerID %in% "20996921") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws108_dat$`water.metres`) <- ws108_dat$time < "2021-06-04 16:00:00" # prior to installation

is.na(ws108_dat$`water.metres`) <- ws108_dat$time > "2021-10-26 10:00:00" # post removal

is.na(ws108_dat$`water.metres`) <- ws108_dat$time == "2021-06-16 14:00:00" # restabilisation period on reinstall #1 - removed

is.na(ws108_dat$`water.metres`) <- ws108_dat$time == "2021-07-28 10:00:00"

is.na(ws108_dat$`water.metres`) <- ws108_dat$time == "2021-07-28 11:00:00" ## Doesn't remove both in one line of code...restabilisation after moving logger down - removed

## Aug. 12th = 3mm adjustment at changeout - leaving it

## RATING CURVE 1 - Reinstall offset 1:
##-------------------------------------

ws108_dat$water.metres[334:1336] <- ws108_dat$water.metres[334:1336] - .0085948 # decrease to match (this puts some values out of the water???)

## RAIING CURVE 2: Shifting logger down offset 2:
##-----------------------------------------------

ws108_dat$water.metres[1699:3497] <- ws108_dat$water.metres[1699:3497] - .02809318 # decrease to match

## WS 104
##-------

ws104_dat <- water_depth %>% 
  filter(water.loggerID %in% "20996916") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws104_dat$`water.metres`) <- ws104_dat$time < "2021-06-04 14:00:00" # prior to installation

is.na(ws104_dat$`water.metres`) <- ws104_dat$time >= "2021-10-26 10:00:00" # post removal

is.na(ws104_dat$`water.metres`) <- ws104_dat$time == "2021-06-15 13:00:00" # no idea what happened here - 8cm increase followed by a 6cm drop - removed and corrected

is.na(ws104_dat$`water.metres`) <- ws104_dat$time == "2021-06-16 12:00:00" # arrival/reinstall restabilisation - removed

## Error offset readjust:

ws104_dat$water.metres[309:330] <- ws104_dat$water.metres[309:330] - .0195616 # decrease to match

## Reinstall offset 1:

ws104_dat$water.metres[332:1096] <- ws104_dat$water.metres[332:1096] + .0021073 # increase to match

## WS 96 - rebar within 7.1cm
##------

ws96_dat <- water_depth %>% 
  filter(water.loggerID %in% "20996923") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws96_dat$`water.metres`) <- ws96_dat$time <= "2021-06-04 11:00:00" # prior to installation

is.na(ws96_dat$`water.metres`) <- ws96_dat$time >= "2021-10-26 10:00:00" # post removal

is.na(ws96_dat$`water.metres`) <- ws96_dat$time == "2021-06-16 10:00:00" # this weird hourly drop probably includes measurements taken out of water - removed

is.na(ws96_dat$`water.metres`) <- ws96_dat$time == "2021-08-12 10:00:00" # 5cm drop in water level right at the time of retrieval/reinstall, probably a measurement averaged within the hour out of the water - removed

## Reinstall offset 1: June 16th 11AM to > Aug. 12th 10AM

ws96_dat$water.metres[330:1697] <- ws96_dat$water.metres[330:1697] - .0086166 # decrease to match

## Reinstall offset 2:

ws96_dat$water.metres[1698:3496] <- ws96_dat$water.metres[1698:3496] - .05703612 # decrease to match - WS 96 done

## WS 93 - 
##------

ws93_dat <- water_depth %>% 
  filter(water.loggerID %in% "20996919") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65)) 

# Aug. 25th measurement is weird - water pressure was lower than barometric pressure so water depth is negative...logger malfunction here? Makes no sense as logger measurement confirms logger was under water.

is.na(ws93_dat$`water.metres`) <- ws93_dat$time < "2021-06-04 11:00:00" # prior to installation

is.na(ws93_dat$`water.metres`) <- ws93_dat$time > "2021-10-26 07:00:00" # post removal

is.na(ws93_dat$`water.metres`) <- ws93_dat$time == "2021-06-16 09:00:00" # restabilisation during June 16th reinstall - removed

is.na(ws93_dat$`water.metres`) <- ws93_dat$time == "2021-07-28 08:00:00" # July 28th restabilisation after moving rebar - removed

is.na(ws93_dat$`water.metres`) <- ws93_dat$time == "2021-08-12 10:00:00" # includes out of water within average - removed


## RATING CURVE 1 - Reinstall offset 1 June 16th:

# Offset negligible (~2mm) - leaving it

# RC 1 runs untils July 28th

## RAIING CURVE 2:  offset 2:

ws93_dat$water.metres[1698:3494] <- ws93_dat$water.metres[1698:3494] - .050322995 # decrease to match

## WS 92 - also has out of water periods, I do not know what to do with this right now (trying to remove them by setting is.na < 0 causes funny stuff)
##------

ws92_dat <- water_depth %>% 
  filter(water.loggerID %in% "20999525") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

## This is another mess, because water.metres is showing negative values for three of the discharge measurements - that is impossible as the rebar measurements and site visit Q estimates say otherwise.

is.na(ws92_dat$`water.metres`) <- ws92_dat$time < "2021-06-04 10:00:00" # prior to installation

is.na(ws92_dat$`water.metres`) <- ws92_dat$time > "2021-10-26 07:00:00" # post removal

is.na(ws92_dat$`water.metres`) <- ws92_dat$time == "2021-06-16 08:00:00" # weird value at reinstall time/arrival - removed

is.na(ws92_dat$`water.metres`) <- ws92_dat$time == "2021-08-12 10:00:00" # restabilisation value for reinstall - removed

# July 7th ~ 7:45AM - rebar was at 45 degrees indicating something happened. Looking at data, something happened on July 5th (spiked upwards over 2hrs) and then again on July 7th (spiked downward over 2hrs). I have no rainfall to check this.

# Aug. 12th - we moved the logger because flow was low and it was out of the water

## RATING CURVE 1: Install - Aug. 12th
##------------------------------------

## Reinstall offset 1: 

ws92_dat$water.metres[328:1696] <- ws92_dat$water.metres[328:1696] - .017947006 # decrease to match - but, this causes water level to dip into the negative...

## RAIING CURVE 2:  Aug. 12th onward:
##-----------------------------------

## No need for offset

## WS 82 - 
##------

ws82_dat <- water_depth %>% 
  filter(water.loggerID %in% "20996924") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws82_dat$`water.metres`) <- ws82_dat$time < "2021-06-04 09:00:00" # prior to install

is.na(ws82_dat$`water.metres`) <- ws82_dat$time > "2021-10-25 14:00:00" # post removal

is.na(ws82_dat$`water.metres`) <- ws82_dat$time == "2021-06-15 17:00:00" # 8cm jump, but not equivalent to new install position of 11cm = out of water averaged in - removed

## Reinstall offset #1:

ws82_dat$water.metres[313:1696] <- ws82_dat$water.metres[313:1696] - .03991704 # decrease to match

## Reinstall offset #2:

ws82_dat$water.metres[1697:3477] <- ws82_dat$water.metres[1697:3477] - .05708227 # decrease to match - WS 82 is done

## WS 87 - rebar within 7.5cm
##------

ws87_dat <- water_depth %>% 
  filter(water.loggerID %in% "20833386") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws87_dat$`water.metres`) <- ws87_dat$time < "2021-06-04 08:00:00" # prior to install

is.na(ws87_dat$`water.metres`) <- ws87_dat$time > "2021-10-25 14:00:00" # post removal

is.na(ws87_dat$`water.metres`) <- ws87_dat$time == "2021-06-15 16:00:00" # 10cm drop probably includes out of water data in the average - removed

## Reinstall offset #1:

ws87_dat$water.metres[312:1696] <- ws87_dat$water.metres[312:1696] - .00370497 # decrease to match

## Reinstall offset #2:

ws87_dat$water.metres[1697:3477] <- ws87_dat$water.metres[1697:3477] - .01845687 # decrease to match - prior to completing offset #1, at time "2021-08-12 09:00:00", there was a 3mm offset, but I left it in due to how small it was...

## WS 84 - rebar within 8.9cm
##------

ws84_dat <- water_depth %>% 
  filter(water.loggerID %in% "20879408") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws84_dat$`water.metres`) <- ws84_dat$time < "2021-06-03 19:00:00" # prior to install

is.na(ws84_dat$`water.metres`) <- ws84_dat$time > "2021-10-25 13:00:00" # post removal

is.na(ws84_dat$`water.metres`) <- ws84_dat$time == "2021-06-15 15:00:00" # > than a 1cm offset here - contains out of water averages - removed

is.na(ws84_dat$`water.metres`) <- ws84_dat$time == "2021-08-12 09:00:00" # > than a 1cm offset here - contains out of water averages - removed

is.na(ws84_dat$`water.metres`) <- ws84_dat$time == "2021-10-10 07:00:00" # something weird happened here that caused a major 3cm spike downward then upward again (Line 3109: .1292 -> Line 3110: 0.098 -> Line 3111: .1235) - removed

## Reinstall offset #1:

ws84_dat$water.metres[311:1695] <- ws84_dat$water.metres[311:1695] - .035894 # decrease to match

## Reinstall offset #2:

ws84_dat$water.metres[1697:3476] <- ws84_dat$water.metres[1697:3476] - .05603339 # decrease to match

## WS 110 - rebar within 3.5cm
##-------

ws110_dat <- water_depth %>% 
  filter(water.loggerID %in% "20958989") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws110_dat$`water.metres`) <- ws110_dat$time < "2021-06-03 17:00:00" # prior to install

is.na(ws110_dat$`water.metres`) <- ws110_dat$time > "2021-10-25 13:00:00" # post removal

is.na(ws110_dat$`water.metres`) <- ws110_dat$time == "2021-06-15 14:00:00" # 2.9 cm offset right before time of reinstall - removed

## Reinstall offset #1:

ws110_dat$water.metres[310:1695] <- ws110_dat$water.metres[310:1695] - .09495743 # decrease to match

## Reinstall offset #2:

ws110_dat$water.metres[1696:3476] <- ws110_dat$water.metres[1696:3476] - .10567805 # decrease to match

## It is corrected, but the correction falls into the negative water depth - what does this mean? Is that time the logger was out of the water?

## WS 76/77
##---------

ws77_dat <- water_depth %>% 
  filter(water.loggerID %in% "20996915") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws77_dat$`water.metres`) <- ws77_dat$time < "2021-06-03 20:00:00" # prior to install

is.na(ws77_dat$`water.metres`) <- ws77_dat$time > "2021-10-25 12:00:00" # post removal

is.na(ws77_dat$`water.metres`) <- ws77_dat$time < "2021-06-04 10:00:00" # something weird happened the morning of June 4th - removed prior to this point now

## Graph to see drops - I cannot see the reinstall points at all, done for now.

ws77_dat %>% 
  ggplot(aes(x = time, y = water.metres)) +
  geom_line()

## WS 75
##------

ws75_dat <- water_depth %>% 
  filter(water.loggerID %in% "20958988") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws75_dat$`water.metres`) <- ws75_dat$time < "2021-06-03 16:00:00" # prior to install

is.na(ws75_dat$`water.metres`) <- ws75_dat$time > "2021-10-25 11:00:00" # post removal

is.na(ws75_dat$`water.metres`) <- ws75_dat$time == "2021-06-15 13:00:00" # 2cm offset at time of arrival/reinstall so it has out of water influence - removed

is.na(ws75_dat$`water.metres`) <- ws75_dat$time == "2021-08-12 08:00:00" # 5cm drop at time of arrival/reinstall so - removed

## Reinstall offset #1:

ws75_dat$water.metres[309:1694] <- ws75_dat$water.metres[309:1694] + .0589518 # increase to match (less than 5mm difference, I could probably revert it and leave it alone)

## Reinstall offset #2:

ws75_dat$water.metres[1696:3474] <- ws75_dat$water.metres[1696:3474] + .0634556 # increase to match

## WS 73
##------

ws73_dat <- water_depth %>% 
  filter(water.loggerID %in% "20833380") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws73_dat$`water.metres`) <- ws73_dat$time < "2021-06-03 14:00:00" # prior to install

is.na(ws73_dat$`water.metres`) <- ws73_dat$time > "2021-10-25 11:00:00" # post removal

is.na(ws73_dat$`water.metres`) <- ws73_dat$time == "2021-06-15 11:00:00" # restabilisation included - removed

is.na(ws73_dat$`water.metres`) <- ws73_dat$time == "2021-08-12 07:00:00" # restabilisation on Aug. 12th retrieval - removed

### Reinstall offset #1: 

ws73_dat$water.metres[307:1693] <- ws73_dat$water.metres[307:1693] - .0297539 # decrease to match

### Reinstall offset #2:

ws73_dat$water.metres[1695:3474] <- ws73_dat$water.metres[1695:3474] - .0157498 # decrease to match

## WS BL1 - rebar within 6.4cm
##-------

wsBL1_dat <- water_depth %>% 
  filter(water.loggerID %in% "20959000") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(wsBL1_dat$`water.metres`) <- wsBL1_dat$time < "2021-06-03 12:00:00" # prior to install

is.na(wsBL1_dat$`water.metres`) <- wsBL1_dat$time > "2021-10-25 09:00:00" # post removal

is.na(wsBL1_dat$`water.metres`) <- wsBL1_dat$time == "2021-06-15 09:00:00" # includes time out of water - removed

is.na(wsBL1_dat$`water.metres`) <- wsBL1_dat$time == "2021-08-11 16:00:00" # spike of 1.1cm at arrival - removed

## Reinstall offset #1:

wsBL1_dat$water.metres[305:1678] <- wsBL1_dat$water.metres[305:1678] - .0236574 # deccrease to match

## Reinstall offset #2:

wsBL1_dat$water.metres[1679:3472] <- wsBL1_dat$water.metres[1679:3472] - .00669952 # decrease to match

## WS BL2 - rebar within 10.1cm
##-------

wsBL2_dat <- water_depth %>% 
  filter(water.loggerID %in% "20833388") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(wsBL2_dat$`water.metres`) <- wsBL2_dat$time < "2021-06-03 13:00:00" # prior to install

is.na(wsBL2_dat$`water.metres`) <- wsBL2_dat$time > "2021-10-25 10:00:00" # post removal

is.na(wsBL2_dat$`water.metres`) <- wsBL2_dat$time == "2021-06-15 10:00:00" # 2+cm offset at arrival/reinstall hour (contains out of water time) - removed

is.na(wsBL2_dat$`water.metres`) <- wsBL2_dat$time == "2021-08-11 15:00:00" ## contains out of water time - removed

## Reinstall offset #1:

wsBL2_dat$water.metres[306:1677] <- wsBL2_dat$water.metres[306:1677] - .0101461 # deccrease to match

## Reinstall offset #2:

wsBL2_dat$water.metres[1679:3473] <- wsBL2_dat$water.metres[1679:3473] - .04966011 # decrease to match

## WS 67 - 
##------

ws67_dat <- water_depth %>% 
  filter(water.loggerID %in% "20999513") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws67_dat$`water.metres`) <- ws67_dat$time < "2021-06-03 11:00:00" # prior to install

is.na(ws67_dat$`water.metres`) <- ws67_dat$time > "2021-10-24 07:00:00" # post removal

is.na(ws67_dat$`water.metres`) <- ws67_dat$time == "2021-06-13 07:00:00" # restabilisation on reinstall - removed

is.na(ws67_dat$`water.metres`) <- ws67_dat$time == "2021-08-22 07:00:00" # restabilisation when we moved logger (22nd Aug) - removed

## RATING CURVE 1: From install - Aug. 22nd

## Reinstall offset #1: Minimal change in height (i.e., 8mm) - leaving it

## Reinstall offset #2:

ws67_dat$water.metres[1751:2006] <- ws67_dat$water.metres[1751:2006] - .05700553536 # increase to match

## RATING CURVE 2: Aug. 22nd - removal

## WS 66 - rebar within 2.2cm
##------

ws66_dat <- water_depth %>% 
  filter(water.loggerID %in% "20879414") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws66_dat$`water.metres`) <- ws66_dat$time < "2021-06-03 10:00:00" # prior to install

is.na(ws66_dat$`water.metres`) <- ws66_dat$time > "2021-10-24 08:00:00" # post removal

is.na(ws66_dat$`water.metres`) <- ws66_dat$time == "2021-06-13 08:00:00" # 10cm drop contains out of water - removed

is.na(ws66_dat$`water.metres`) <- ws66_dat$time == "2021-08-11 14:00:00" # ~1cm increase (out of water mixed in) - removed for now, can always revert

## Reinstall offset #1:

ws66_dat$water.metres[329:1750] <- ws66_dat$water.metres[329:1750] + .1167648 # increase to match

## Reinstall offset #2:

ws66_dat$water.metres[1751:3520] <- ws66_dat$water.metres[1751:3520] + .0204187 # increase to match

## WS 36 - rebar within 39.9cm
##------

ws36_dat <- water_depth %>% 
  filter(water.loggerID %in% "20996920") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws36_dat$`water.metres`) <- ws36_dat$time < "2021-05-31 17:00:00" # prior to install

is.na(ws36_dat$`water.metres`) <- ws36_dat$time > "2021-10-24 08:00:00" # post removal

is.na(ws36_dat$`water.metres`) <- ws36_dat$time == "2021-06-13 10:00:00" # ~2cm adjustment at time of arrival/reinstall - removed

## For Aug. 11th, values are very close - no row to remove near reinstall time

## Reinstall offset #1:

ws36_dat$water.metres[331:1750] <- ws36_dat$water.metres[331:1750] - .0121653 # decrease to match

## Reinstall offset #2:

ws36_dat$water.metres[1751:3520] <- ws36_dat$water.metres[1751:3520] + .0047416 # increase to match

## WS 40 - rebar within 32.7cm
##------

ws40_dat <- water_depth %>% 
  filter(water.loggerID %in% "20440923") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws40_dat$`water.metres`) <- ws40_dat$time < "2021-05-31 15:00:00" # prior to install

is.na(ws40_dat$`water.metres`) <- ws40_dat$time > "2021-10-24 09:00:00" # post removal

is.na(ws40_dat$`water.metres`) <- ws40_dat$time == "2021-06-13 11:00:00" # 3+ cm jump here at arrival - removed

is.na(ws40_dat$`water.metres`) <- ws40_dat$time == "2021-08-11 14:00:00"

is.na(ws40_dat$`water.metres`) <- ws40_dat$time == "2021-08-11 15:00:00" # this is strange, two 1.5cm drops over 2 hours near reinstall - removing for now

## Reinstall offset #1:

ws40_dat$water.metres[405:1822] <- ws40_dat$water.metres[405:1822] - .06201574 # decrease to match

## Reinstall offset #2:

ws40_dat$water.metres[1825:3594] <- ws40_dat$water.metres[1825:3594] - .05989133 # decrease to match

## WS 43 - rebar within 8.3cm
##------

ws43_dat <- water_depth %>% 
  filter(water.loggerID %in% "20440889") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws43_dat$`water.metres`) <- ws43_dat$time < "2021-05-31 16:00:00" # prior to install - a few stabilisation hourly measurement, but one contains a rise of 8cm which is inconsistent with the rest of the water level. Removed from this point backward for now.

is.na(ws43_dat$`water.metres`) <- ws43_dat$time > "2021-10-24 11:00:00" # post removal

is.na(ws43_dat$`water.metres`) <- ws43_dat$time == "2021-08-11 13:00:00" # 3cm drop near arrival/reinstall - removed

## Reinstall offset #1:

ws43_dat$water.metres[406:1822] <- ws43_dat$water.metres[406:1822] + .0671823 # increase to match - levels around arrival look fine here

## Reinstall offset #2:

ws43_dat$water.metres[1824:3597] <- ws43_dat$water.metres[1824:3597] + .0097894 # increase to match

## WS 45 - rebar within 8.1cm
##------

ws45_dat <- water_depth %>% 
  filter(water.loggerID %in% "20440766") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws45_dat$`water.metres`) <- ws45_dat$time < "2021-05-31 10:00:00" # prior to install

is.na(ws45_dat$`water.metres`) <- ws45_dat$time > "2021-10-24 11:00:00" # post removal

is.na(ws45_dat$`water.metres`) <- ws45_dat$time == "2021-06-13 14:00:00" # major drop at time of arrival - removed

is.na(ws45_dat$`water.metres`) <- ws45_dat$time == "2021-08-11 12:00:00" # 3cm drop at time of arrival - removed

## Reinstall offset #1:

ws45_dat$water.metres[409:1821] <- ws45_dat$water.metres[409:1821] - .0237934 # decrease to match

## Reinstall offset #2:

ws45_dat$water.metres[1823:3597] <- ws45_dat$water.metres[1823:3597] + .0702244 # increase to match

## WS 46 - rebar within 25.8cm (only one outlier brings this way down; otherwise within ~6cm)
##------

ws46_dat <- water_depth %>% 
  filter(water.loggerID %in% "20525616") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws46_dat$`water.metres`) <- ws46_dat$time < "2021-05-31 10:00:00" # prior to install

is.na(ws46_dat$`water.metres`) <- ws46_dat$time > "2021-10-23 16:00:00" # post removal

is.na(ws46_dat$`water.metres`) <- ws46_dat$time == "2021-06-12 16:00:00" # drop at arrival - removed

## For Aug. 11th, values are very close - no row to remove near reinstall time

## Reinstall offset #1:

ws46_dat$water.metres[387:1821] <- ws46_dat$water.metres[387:1821] - .0475188 # decrease to match

## Reinstall offset #2:

ws46_dat$water.metres[1822:3578] <- ws46_dat$water.metres[1822:3578] - .0376343 # decrease to match

## WS 47 - rebar within 5.7cm
##------

ws47_dat <- water_depth %>% 
  filter(water.loggerID %in% "20525614") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws47_dat$`water.metres`) <- ws47_dat$time < "2021-05-28 17:00:00" # prior to install

is.na(ws47_dat$`water.metres`) <- ws47_dat$time > "2021-10-23 15:00:00" # post removal

is.na(ws47_dat$`water.metres`) <- ws47_dat$time == "2021-06-12 15:00:00" # arrival/out of water drop - removed

is.na(ws47_dat$`water.metres`) <- ws47_dat$time == "2021-08-11 11:00:00" # 1.5cm drop at arrival time - removed

## Reinstall offset #1:

ws47_dat$water.metres[386:1820] <- ws47_dat$water.metres[386:1820] + .0018185 # increase to match

## Reinstall offset #2:

ws47_dat$water.metres[1822:3577] <- ws47_dat$water.metres[1822:3577] - .0303195 # decrease to match

## WS 52 - rebar within 6.3cm
##------

ws52_dat <- water_depth %>% 
  filter(water.loggerID %in% "20402298") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws52_dat$`water.metres`) <- ws52_dat$time < "2021-05-28 16:00:00" # prior to install

is.na(ws52_dat$`water.metres`) <- ws52_dat$time > "2021-10-23 14:00:00" # post removal

## June 12th - only 3mm offset at time of arrival so keeping that hour of data for now

## Aug. 11th - just adjusting, no removing. Significant rises/drops (i.e., 3-8cm) in stream level throughout the dataset so

## Reinstall offset #1:

ws52_dat$water.metres[384:1820] <- ws52_dat$water.metres[384:1820] + .01390699 # increase to match

## Reinstall offset #2:

ws52_dat$water.metres[1821:3576] <- ws52_dat$water.metres[1821:3576] - .01552882 # decrease to match

## WS 54 - rebar within 4.5cm
##------

ws54_dat <- water_depth %>% 
  filter(water.loggerID %in% "20715030") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws54_dat$`water.metres`) <- ws54_dat$time < "2021-05-28 15:00:00" # prior to install

is.na(ws54_dat$`water.metres`) <- ws54_dat$time > "2021-10-23 13:00:00" # post removal

is.na(ws54_dat$`water.metres`) <- ws54_dat$time == "2021-06-12 13:00:00" # resetting/adjusting after arrival - removed

is.na(ws54_dat$`water.metres`) <- ws54_dat$time == "2021-08-11 10:00:00" # arrival discrepancy here - removed

## Reinstall offset #1:

ws54_dat$water.metres[384:1819] <- ws54_dat$water.metres[384:1819] - .1055914 # decrease to match

## Reinstall offset #2:

ws54_dat$water.metres[1821:3575] <- ws54_dat$water.metres[1821:3575] - .09932014 # decrease to match

## WS SSR - rebar within 5.3cm
##------

wsSSR_dat <- water_depth %>% 
  filter(water.loggerID %in% "20715043") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(wsSSR_dat$`water.metres`) <- wsSSR_dat$time < "2021-05-28 12:00:00" # prior to install

is.na(wsSSR_dat$`water.metres`) <- wsSSR_dat$time > "2021-10-23 11:00:00" # post removal

is.na(wsSSR_dat$`water.metres`) <- wsSSR_dat$time == "2021-06-12 11:00:00" # restabilisation data point at arrival - removed

is.na(wsSSR_dat$`water.metres`) <- wsSSR_dat$time == "2021-08-11 09:00:00" # 3cm drop at arrival/reinstall - removed

## Reinstall offset #1:

wsSSR_dat$water.metres[382:1818] <- wsSSR_dat$water.metres[382:1818] + .1467372 # increase to match

## Reinstall offset #2:

wsSSR_dat$water.metres[1820:3573] <- wsSSR_dat$water.metres[1820:3573] + .2376109 # increase to match

## WS SBC - rebar within 6.5cm
##-------

wsSBC_dat <- water_depth %>% 
  filter(water.loggerID %in% "20440791") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(wsSBC_dat$`water.metres`) <- wsSBC_dat$time < "2021-05-28 10:00:00" # prior to install - 4mm change at Q estimate point to next hourly point, leaving it for now.

is.na(wsSBC_dat$`water.metres`) <- wsSBC_dat$time > "2021-10-23 11:00:00" # post removal

is.na(wsSBC_dat$`water.metres`) <- wsSBC_dat$time == "2021-06-12 10:00:00" # 1.1cm adjustment at arrival time - removed

is.na(wsSBC_dat$`water.metres`) <- wsSBC_dat$time == "2021-08-11 09:00:00" # 3cm adjustment at data collection time - removed

## Also on Aug. 11th, about 8hrs after reinstall there is a 5cm jump in water level with frequent jumps up and down shortly afterwards (Lines 1828-1835). Interesting dynamics.

## Reinstall offset #1:

wsSBC_dat$water.metres[381:1818] <- wsSBC_dat$water.metres[381:1818] - .0160605 # decrease to match

## Reinstall offset #2:

wsSBC_dat$water.metres[1820:3573] <- wsSBC_dat$water.metres[1820:3573] + .02858607 # increase to match

## WS 17 - rebar within 
##-------

ws17_dat <- water_depth %>% 
  filter(water.loggerID %in% "20440793") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws17_dat$`water.metres`) <- ws17_dat$time < "2021-05-28 09:00:00" # prior to install

is.na(ws17_dat$`water.metres`) <- ws17_dat$time > "2021-10-23 09:00:00" # post removal

is.na(ws17_dat$`water.metres`) <- ws17_dat$time == "2021-06-09 12:00:00" # restabilisation after move on June 9th - removed

is.na(ws17_dat$`water.metres`) <- ws17_dat$time == "2021-08-11 08:00:00" # most likely contains out of water measurements - removed

## RATING CURVE 1:

# May 28th 9AM - June 9th 11AM

## RATING CURVE 2: June 9th 1PM onward

### Rebar offset #1: June 12th offset is negligible (~2mm) - leaving it

### Rebar offset #2:

ws17_dat$water.metres[1818:3571] <- ws17_dat$water.metres[1818:3571] + .0659587 # increase to match

## WS 11 - rebar within ~9.5cm
##-------

ws11_dat <- water_depth %>% 
  filter(water.loggerID %in% "20402300") %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average,
         water.metres = water.depth * (1000 / 9806.65))

is.na(ws11_dat$`water.metres`) <- ws11_dat$time < "2021-05-28 08:00:00" # prior to install

is.na(ws11_dat$`water.metres`) <- ws11_dat$time > "2021-10-23 08:00:00" # post removal

is.na(ws11_dat$`water.metres`) <- ws11_dat$time >= "2021-06-09 09:00:00" & ws11_dat$time <= "2021-06-09 12:00:00" # large sway in values before June 9th move

is.na(ws11_dat$`water.metres`) <- ws11_dat$time == "2021-08-11 07:00:00" # restabilisation at reinstall Aug. 11th - removed

## RATING CURVE 1: Data for 1st rebar measurement is missing in the water level so I cannot use it (May 25th - June 9th 12PM cannot be used)

## RATING CURVE 2:

### Reinstall offset #1: Neglible offset at reinstall time on June 12th: ~0.5mm - leaving it

### Reinstall offset #2:

ws11_dat$water.metres[1818:3570] <- ws11_dat$water.metres[1818:3570] - .0405337 # decrease to match

#### 4. Plotting ----
##---------------------------

## 4.1 Take a look at an interactive plot ----
##--------------------------------------------

wsBL1_dat %>%
plot_ly(x = ~time, y = ~water.metres)

## 4.2 Take a look at precipitation quickly

ssm_precip %>%
  plot_ly(x = ~`Date/Time (UTC)`, y = ~`Precip. Amount (mm)`)

## 4.3 Plot up unadjusted and adjusted plot figures for each station ----
##-----------------------------------------------------------------------

## WS44
##-----

ws44_out %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS44 Unadjusted")
  ## unadjusted 

## figure out how Jason arrived at .155m correction

ws44_out %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .157

ws44_offset <- ws44_out %>% 
  mutate(water.metres.adj = water.metres + 0.155) ## I guess you can arbitrarily eye ball closeness here

ws44_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS44 Adjusted") ## adjusted

## WS 108
##-------

## RATING CURVE 1:

ws108_wl_1 <- ws108_dat %>% 
  filter(time >= "2021-06-04 14:00:00" & time <= "2021-07-28 11:00:00") %>% 
  mutate(water.level.one = rebar.datum - water.metres)

ws108_wl_1 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS108 Unadjusted - 1st Datum") ## unadjusted

ws108_wl_1_off <- ws108_wl_1 %>% 
  mutate(water.metres.adj = water.metres + 0.123)

ws108_wl_1_off %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS108 Adjusted - 1st Datum") ## adjusted - one on, two off..

## RATING CURVE 2:

ws108_wl_2 <- ws108_dat %>% 
  filter(time >= "2021-07-28 12:00:00") %>% 
  mutate(water.level.two = rebar.datum - water.metres)

ws108_wl_2 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS108 Unadjusted - 2nd Datum") ## unadjusted

ws108_wl_2_off <- ws108_wl_2 %>% 
  mutate(water.metres.adj = water.metres - 0.018)

ws108_wl_2_off %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS108 Adjusted - 2nd Datum") ## adjusted - two points off

## WS 104
##-------

ws104_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS104 Unadjusted") ## unadjusted

ws104_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .2912

ws104_offset <- ws104_dat %>% 
  mutate(water.metres.adj = water.metres + 0.2912)

ws104_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS104 Adjusted") ## adjusted - one point well off

## WS 96
##------

ws96_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS96 Unadjusted") ## unadjusted

ws96_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .077

ws96_offset <- ws96_dat %>% 
  mutate(water.metres.adj = water.metres + 0.064)
# .082
ws96_offset %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS96 Adjusted") ## adjusted - 3 points are well off

## WS 93
##------

## RATING CURVE 1:

ws93_wl_1 <- ws93_dat %>% 
  filter(time <= "2021-07-09 16:00:00") %>% 
  mutate(water.level.one = rebar.datum - water.metres)

ws93_wl_1 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS93 Unadjusted - Datum 1") ## unadjusted

ws93_wl_1_off <- ws93_wl_1 %>% 
  mutate(water.metres.adj = water.metres + 0.042)

ws93_wl_1_off %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS93 Adjusted - Datum 1") ## adjusted - two on, one off

 ## RATING CURVE 2:

ws93_wl_2 <- ws93_dat %>% 
  filter(time >= "2021-07-28 09:00:00") %>% 
  mutate(water.level.two = rebar.datum - water.metres)

ws93_wl_2 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS93 Unadjusted - Datum 2") ## unadjusted

ws93_wl_2_off <- ws93_wl_2 %>% 
  mutate(water.metres.adj = water.metres + 0.49)

ws93_wl_2_off %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS93 Adjusted - Datum 2") ## adjusted - two points off

## WS 92
##------

## RATING CURVE 1:

ws92_wl_1 <- ws92_dat %>% 
  filter(time <= "2021-08-12 09:00:00") %>% 
  mutate(water.level.one = rebar.datum - water.metres)

ws92_wl_1 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS92 Unadjusted - Datum 1") ## unadjusted

ws92_wl_1_off <- ws92_wl_1 %>% 
  mutate(water.metres.adj = water.metres + 0.085)

ws92_wl_1_off %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS92 Adjusted - Datum 1") ## adjusted - meh, one on, two close, one off

## RATING CURVE 2:

ws92_wl_2 <- ws92_dat %>% 
  filter(time >= "2021-08-12 11:00:00") %>% 
  mutate(water.level.two = rebar.datum - water.metres)

ws92_wl_2 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS92 Unadjusted - Datum 2") ## unadjusted

ws92_wl_2_off <- ws92_wl_2 %>% 
  mutate(water.metres.adj = water.metres + 0.025)

ws92_wl_2_off %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS92 Adjusted - Datum 2") ## adjusted - one on, one close

## WS 82
##------

ws82_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS82 Water Level")
  

# No discharge

## WS 87
##------

ws87_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS87 Unadjusted") ## unadjusted

ws87_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .021

ws87_offset <- ws87_dat %>% 
  mutate(water.metres.adj = water.metres + 0.020)

ws87_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS87 Adjusted") ## adjusted - 2 are way off here

## WS 84
##------

ws84_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS84 Unadjusted") ## unadjusted

ws84_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .058

ws84_offset <- ws84_dat %>% 
  mutate(water.metres.adj = water.metres + 0.062)

ws84_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS84 Adjusted") ## adjusted - some are well off here

## WS 110
##-------

ws110_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS110 Unadjusted") ## unadjusted

ws110_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .125

ws110_offset <- ws110_dat %>% 
  mutate(water.metres.adj = water.metres + 0.126)

ws110_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS110 Adjusted") ## adjusted - three are slightly low

## WS 77 / 76 / 75
##----------------

ws77_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS77 & WS76 Water Level")

ws75_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS75 Water Level")
  

## WS 73
##------

ws73_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS73 Unadjusted") ## unadjusted

ws73_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .081

ws73_offset <- ws73_dat %>% 
  mutate(water.metres.adj = water.metres + 0.115)

ws73_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS73 Adjusted") ## adjusted - pretty bad: two one and five off...

## WS BL1
##-------

wsBL1_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WSBL1 Unadjusted") ## unadjusted

wsBL1_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .05

wsBL1_offset <- wsBL1_dat %>% 
  mutate(water.metres.adj = water.metres + 0.068)

wsBL1_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WSBL1 Adjusted") ## adjusted - two low

## WS BL2
##-------

wsBL2_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WSBL2 Unadjusted") ## unadjusted

wsBL2_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .071

wsBL2_offset <- wsBL2_dat %>% 
  mutate(water.metres.adj = water.metres + 0.048)

wsBL2_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WSBL2 Adjusted") ## adjusted - this one is a mess

## WS 67
##------

## RATING CURVE 1:

ws67_wl_1 <- ws67_dat %>% 
  filter(time <= "2021-08-22 06:00:00") %>% 
  mutate(water.level.one = rebar.datum - water.metres)

ws67_wl_1 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS67 Unadjusted - Datum 1") ## unadjusted

ws67_wl_1_off <- ws67_wl_1 %>% 
  mutate(water.metres.adj = water.metres + 0.104)

ws67_wl_1_off %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS67 Adjusted - Datum 1") ## adjusted - two on, two off.

## RATING CURVE 2:

ws67_wl_2 <- ws67_dat %>% 
  filter(time >= "2021-08-22 08:00:00") %>% 
  mutate(water.level.two = rebar.datum - water.metres)

ws67_wl_2 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS67 Unadjusted - Datum 2") ## unadjusted

ws67_wl_2_off <- ws67_wl_2 %>% 
  mutate(water.metres.adj = water.metres + 0.04)

ws67_wl_2_off %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS67 Adjusted - Datum 2") ## adjusted - two on, one way off

## WS 66
##------

ws66_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS66 Unadjusted") ## unadjusted

ws66_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .188

ws66_offset <- ws66_dat %>% 
  mutate(water.metres.adj = water.metres + 0.185)

ws66_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS66 Adjusted") ## adjusted - slightly off in places

## WS 36
##------

ws36_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS36 Unadjusted") ## unadjusted

ws36_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .315

ws36_offset <- ws36_dat %>% 
  mutate(water.metres.adj = water.metres + 0.212)

ws36_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS36 Adjusted") ## adjusted - a disaster of rebar measurements

## WS 40
##------

ws40_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS40 Unadjusted") ## unadjusted

ws40_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .270

ws40_offset <- ws40_dat %>% 
  mutate(water.metres.adj = water.metres + 0.306)

ws40_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS40 Adjusted") ## adjusted - another inconsistent set of rebar measurements

## WS 43
##------

ws43_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS43 Unadjusted") ## unadjusted

ws43_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .179

ws43_offset <- ws43_dat %>% 
  mutate(water.metres.adj = water.metres + 0.197)

ws43_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS43 Adjusted") ## adjusted - slightly okay here as 3 are off

## WS 45
##------

ws45_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS45 Unadjusted") ## unadjusted

ws45_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .205

ws45_offset <- ws45_dat %>% 
  mutate(water.metres.adj = water.metres + 0.45)

ws45_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS45 Adjusted") ## adjusted - this is quite bad 

## WS 46
##------

ws46_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS46 Unadjusted") ## unadjusted

ws46_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .078

ws46_offset <- ws46_dat %>% 
  mutate(water.metres.adj = water.metres + 0.092)

ws46_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS46 Adjusted") ## adjusted - pretty meh here (off quite a bit) 

## WS 47
##------

ws47_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS47 Unadjusted") ## unadjusted

ws47_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .175

ws47_offset <- ws47_dat %>% 
  mutate(water.metres.adj = water.metres + 0.192)

ws47_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS47 Adjusted") ## adjusted - close but not great

## WS 52
##------

ws52_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS52 Unadjusted") ## unadjusted

ws52_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .038

ws52_offset <- ws52_dat %>% 
  mutate(water.metres.adj = water.metres + 0.055)

ws52_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS52 Adjusted") ## adjusted - pretty good with an outlier

## WS 54
##------

ws54_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS54 Unadjusted") ## unadjusted

ws54_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .14

ws54_offset <- ws54_dat %>% 
  mutate(water.metres.adj = water.metres + 0.133)

ws54_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS54 Adjusted") ## adjusted - not terrible

## WS SSR
##------

wsSSR_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS SSR unadjusted") ## unadjusted

wsSSR_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .407

wsSSR_offset <- wsSSR_dat %>% 
  mutate(water.metres.adj = water.metres + 0.395)

wsSSR_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS SSR Adjusted") ## adjusted - pretty good

## WS SBC
##-------

wsSBC_dat %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS SBC Unadjusted") ## unadjusted

wsSBC_dat %>% 
  mutate(rebar.water.offset = rebar.datum - water.metres) %>% 
  summary() ## mean is .023

wsSBC_offset <- wsSBC_dat %>% 
  mutate(water.metres.adj = water.metres + 0.005)

wsSBC_offset %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS SBC Adjusted") ## adjusted - not great, 4 close, 3 off

## WS 17 ----
##-----------

ws17_wl_1 <- ws17_dat %>% 
  filter(time <=  "2021-06-09 09:00:00") %>% 
  mutate(water.level.one = rebar.datum - water.metres)

ws17_wl_1 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS17 Unadjusted - Datum 1 - No May 25th data")## unadjusted with no rebar measurement because the 25th data is gone here too...

## RATING CURVE 2:

ws17_wl_2 <- ws17_dat %>% 
  filter(time >= "2021-06-09 10:00:00") %>% 
  mutate(water.level.two = rebar.datum - water.metres)

ws17_wl_2 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS17 Unadjusted - Datum 2") ## unadjusted - 

ws17_wl_2_off <- ws17_wl_2 %>% 
  mutate(water.metres.adj = water.metres + 0.255)

ws17_wl_2_off %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS17 Adjusted - Datum 2") ## adjusted - three points off...

## WS 11 ----
##-----------

## RATING CURVE 1:

ws11_wl_1 <- ws11_dat %>% 
  filter(time <= "2021-06-09 12:00:00") %>% 
  mutate(water.level.one = rebar.datum - water.metres)

ws11_wl_1 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS11 Unadjusted - Datum 1 - No May 25th data") ## unadjusted with no rebar measurement.

## RATING CURVE 2:

ws11_wl_2 <- ws11_dat %>% 
  filter(time >= "2021-06-09 13:00:00") %>% 
  mutate(water.level.two = rebar.datum - water.metres)

ws11_wl_2 %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS11 Unadjusted - Datum 2") ## unadjusted - 10-15cm increase and subsequent 30cm drop in 1 hour from 3PM-7PM on Oct. 12th looks suspect...

ws11_wl_2_off <- ws11_wl_2 %>% 
  mutate(water.metres.adj = water.metres + 0.21)

ws11_wl_2_off %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = water.metres.adj)) +
  geom_point(aes(y = rebar.datum, color = "red")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle("WS11 Adjusted - Datum 2") ## adjusted - three points off

    
## 5. Saving / Exporting ----
##---------------------------

## 6. Notes / Junk Code ----
##---------------------------
