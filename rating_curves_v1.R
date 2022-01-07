####-------------------------
## Name: Matt Watkins
## Date: Wed. Dec. 15th/21
## Project: 
## Objective: Build scatterplots (Q estimates with rebar measurements) for each site
## Inputs: discharge rds / rebar measurements
## Outputs: scatterplots; pdf of graphs
####-------------------------

## 0. Notes
##---------------------------

## Main idea - bring in all necessary files and start creating in a similar format to water level graphs

## 1. Prepare
##---------------------------

library(tidyverse)
library(openxlsx)
library(lubridate)
library(purrr)
library(gridExtra)

## 2. Import
##---------------------------

## Bring in Q estimates

datQ <- readRDS("Q_estimates.rds")

## Bring in rebar measurements

rebar <- read.xlsx("rebar_calc_2021_v2.xlsx")

## 3. Tidy / Process
##---------------------------

## 3.1 Re-create datum column and prep df for merge

rebar_out <- rebar %>% 
  mutate(rebar.datum = top.to.bed.average - top.to.surface.average)

rebar_out$totalQ <- NULL

## 3.2 Adjust numeric date to datetime

rebar_out$date <- convertToDateTime(rebar_out$date)

rebar_out$date <- substr(rebar_out$date, 0, 10)

rebar_out$date <- as.Date(rebar_out$date)

## 3.3 Join based on site and time

datJoin <- full_join(rebar_out, datQ)

## 3.4 Remove negative Q values from dataset

dat_upd <- datJoin %>% 
  filter(totalQ > 0)

## 3.5 Automate exploratory plots for each watershed in this format

datJoin %>% 
ggplot(aes(x = totalQ, y = rebar.datum)) +
  geom_point() +
  theme_bw() +
  labs(x = "Discharge", y = "Stage")

# How do I split this by site into 26 plots quickly...

datOut <- dat_upd %>%
  group_by(site) %>%
  summarise(plot = list(ggplot(cur_data(), aes(x = totalQ, y = rebar.datum)) +
                          geom_point() +
                          labs(x = "Discharge", y = "Stage") + 
                          theme_bw() +
                          ggtitle(sprintf('%s', cur_group()$site)) +
                          theme(plot.title = element_text(hjust = 0.5)))) ## this is a solution

## 4. Plotting
##---------------------------


## 5. Saving / Exporting
##---------------------------

ggsave(file.path(getwd(), "rc_plots_v1.01.pdf"), marrangeGrob(datOut$plot, nrow = 1, ncol = 1), device = "pdf") ## save all graphs to pdf

## 6. Notes / Junk Code
##---------------------------

datJoin %>% 
  filter(site == "")
  ggplot(aes(x = totalQ, y = rebar.datum)) +
  geom_point() +
  theme_bw() +
  labs(x = "Discharge", y = "Stage")
