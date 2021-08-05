
# ----------------------------------------------
# Forest health indicators for SIMO
# ----------------------------------------------

# Shelter:   forest density
# Serenity:  distance to roads
# Diversity: deadwood, diversity of tre species
# Nature:    mean age
# Cohesion:  stand area

# Combination of dimensions: How to combine?

# Edges/glades:    shelter, nature, diversity
# Deep forest:     nature, shelter, serenity
# Spacious forest: cohesion, serenity, nature

rm(list = ls())

setwd("C:/MyTemp/myGitLab/windDamage")

# Read libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rgdal)
library(dplyr)
library(spData)
library(sf)


# Get example dataset for Korsnas, no clim change
# Get data ------------------------------------------------------------------------------
inPath = "C:/MyTemp/myGitLab/windDamage/manuscript_regimes"
inFolder = "input_CC"

# Read data
df <- data.table::fread(paste(inPath, inFolder, 'without_MV_Korsnas_rsu.csv',  sep = "/"),  # 
                        data.table=FALSE, stringsAsFactors = FALSE)

# Get histograms for selected variables
df %>% 
  filter(H_dom < quantile(H_dom, 0.95, na.rm = T))  %>%
  nrow()


  mutate(shelter = 
           case_when(
             (N > 5000 & H_dom < 5) ~ 1,
              TRUE ~0) ) %>% 
  filter(shelter == 1) %>%
  nrow()


  group_by(regime, year) %>% 
  tally()
  
  
# What is the H_dom  

  filter(regime == "BAU") %>% 
  ggplot(aes(N,
             #fill = as.factor(year),
             #color = as.factor(year), # as.factor(
             fill = as.factor(year)
              )) +
  geom_histogram(alpha=0.5, position = 'identity') +
  guides(fill=guide_legend(ncol=20)) +
  geom_vline(xintercept = 5000) +
 # facet_grid(.~year) + 
  theme(legend.position = "none")


