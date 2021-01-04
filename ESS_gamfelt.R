
# --------------------------------------
# Get models from Gamfelt et al. 2013
# --------------------------------------

# higher levels of multiple ESS are found in forests with more tree species

# biodiversity is suggested to positively influence multiple ESS but evidence is scarced
# ESS: 

# How does tree species richness predict: 
# - 1. production of tree biomass
# - 2. topsoil carbon storage
# - 3. berry production 
# - 4. game production potential ~ occurence of 15 plant species important for large herbivores
#                       - measured in the same ways as bilberry cover
# - 5. Understory plant species richness: proportion of richness from the regional richness
# - 6. probability of deadwood: presence/absence
# no single tree species was able to promote all services 

# Input data: 
# -------------
# Swedish national forest inventory and Survey of forest soils and vegetation
# only on 'productive forests' = average production of standing volume, stem volume over bark >1m3/ha/year
# which has not been harvested, cleared, thnned the 5 years before teh survey
# non peat soil
# get effect sizes to understand the contribution of individual factors


# Explanatory variables:
# --------------------
# - temperature - total accumulated average daily temperature (degree C) over 5C during veget.period
# - humidity    - difference in mm between precipitation and evaporation
# - nitrogen deposition (kg N/ha/year) - meteo 
# - c_n = carbon to nitrogen ration i the top soils
# - soil moisture : categorical: 1 (driest) -5 (wettest) ~ corresponds to continuous % volumetric soil content
# - etc, specified below as dummy data

# 2021/01/04 
# Seems that example works for the tree biomass production

# To solve: 
# - need to check/ask how the PEAT is coded ? Should be 0, 1 as binary, but mean is 0.08? Should we treat it as categoriacal (dummy variable?)
# - no transformations are included (Table S2) - is this ok for all services?
# - if calculation for biomass is correct, need that Tord will provide estimates from Fig. 4 - now we need to get them visually !!
# - how to create from data the Bayesian uncertainity values? is it ok to include the range of the beta estimates into the model to get the range?
# - for further SIMO variables: only age, species and maybe richness will change. 
# - maybe we can get spatially differentiated predictiorns in temperature, mosture and humidity values, to apply it over Finland on NFI dataset for Clemens?




# ------------------------------------------
# Generate dummy predictors
# ------------------------------------------

# use truncnorm library to define normal distribution both min, man, mean and sd
# https://stackoverflow.com/questions/19343133/setting-upper-and-lower-limits-in-rnorm

library(truncnorm)
library(dplyr)

#rtruncnorm(n=10, a=0, b=340, mean=39.4, sd=25.09)
set.seed(5)

n_row = 10  # get lenght of generated numbers 

# List variables 
temp      = rtruncnorm(n=n_row, a=420,  b=1690, mean=1087, sd=276)
humidity  = rtruncnorm(n=n_row, a=-60,  b=225,  mean=43,   sd=59.5)
N_depo    = rtruncnorm(n=n_row, a=2.0,  b=18.5, mean=6.6,  sd=2.8)  
richness  = rtruncnorm(n=n_row, a=0,    b=10,   mean=2.5,  sd=1.1)
age       = rtruncnorm(n=n_row, a=1,    b=315,  mean=64.5, sd=45.6)
pH        = rtruncnorm(n=n_row, a=3.1,  b=7.9,  mean=4,    sd=0.63) 
c_n_ratio = rtruncnorm(n=n_row, a=11,   b=90,   mean=30.8, sd=9.7)
moisture  = rtruncnorm(n=n_row, a=0.15, b=0.35, mean=0.22, sd=0.03)
peat      = sample(c(0,1), replace = TRUE, size = n_row)  # peat should be binary; how it coud be mean 0.08 and sd 0.27? is it propostion of sites?
spruce    = rtruncnorm(n=n_row, a=0, b=59.4,    mean=4.5,  sd=6.7)
pine      = rtruncnorm(n=n_row, a=0, b=33.6,    mean=3.6,  sd=4.6)
birch     = rtruncnorm(n=n_row, a=0, b=21.0,    mean=0.97, sd=2.0)
oak       = rtruncnorm(n=n_row, a=0, b=27.2,    mean=0.14, sd=1.2)
aspen     = rtruncnorm(n=n_row, a=0, b=24.6,    mean=0.12, sd=0.9)
beech     = rtruncnorm(n=n_row, a=0, b=33.8,    mean=0.11, sd=1.3)


# Create table, include the squared variables and interactions to have one column for one 
# variable
# need to be in same order as the subsequenct model estimates
df <- data.frame(temp,
                 humidity,
                 N_depo, 
                 richness,
                 age,
                 pH,
                 c_n_ratio,
                 moisture,
                 peat,
                 spruce,
                 pine,
                 birch, 
                 oak,
                 aspen,
                 beech) #, 
                

# Center and standardize the row data, then add square variables and interactions
# -------------------------------------

# need to center (substract the mean) and standardize (divide by sd) teh variables
# centering the variables: mean of predictors = 0
# facilitate interpretation of intercept = Y, if teh X are set to their means
# intercept = Y if all X are 0
# scaling useful if variables are on different scales
# Centering must be done on raw values, before interaction, and before setting polynomial terms (squaring!)

# Make a function
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# Centralize and standardize the raw values
df_cent <- 
  df %>% 
  mutate_all(scale2)


# Complete centered table
# --------------------------
# Add squared and interaction predictors,
# add for intercept value 1 as it will be later added
df_cent <- df_cent %>% 
  mutate(intercept       = 1, 
         temp_N_depo     = temp*N_depo,
         humidity_N_depo = humidity*N_depo,
         richness_sq     = richness^2,
         age_sq          = age^2,
         rich_age        = richness*age,
         pH_sq           = pH^2, 
         moisture_sq     = moisture^2,
         rich_pH         = richness*pH,
         rich_c_n        = richness*c_n_ratio,
         rich_moisture   = richness*moisture,
         spruce_age      = spruce*age,
         pine_age        = pine*age,
         birch_age       = birch*age)
  



# List the columns order to following parameters estimates 
col_order = c('intercept',
              'richness',
              'richness_sq', 
              'age',
              'age_sq',
              'pH',
              'pH_sq',
              'rich_age',
              'c_n_ratio',
              'peat',
              'moisture',
              'moisture_sq',
              'rich_pH',
              'rich_c_n',
              'rich_moisture',
              'spruce',
              'pine',
              'birch',
              'oak',
              'aspen',
              'beech',
              'spruce_age',
              'pine_age',
              'birch_age',
              'temp',
              'humidity',
              'N_depo',
              'temp_N_depo',
              'humidity_N_depo')


# Order df colums to fit coefficients vector
# -------------------------------------------
df_cent <- df_cent[, col_order]

# !!! ---------------------------
# List coefficients: numbers are guessed from Fig. 4 as they are not included in 
# the article, neither in Suipplementary materials
# !!! ---------------------------
coeff_biomass = c( 1,    # for intercept
                   0.08,
                  -0.01,
                  -0.22,
                   0.05,  # age_sq
                   0.01,
                   0,     
                  -0.02,  # rich_age
                  -0.018, # C_n
                   0,
                  -0.01,
                   0,
                   0,    # rich_ph,
                  -0.01,
                   0.01,
                   0.25, # spruce
                   0.15, # pine
                   0.07, # birch
                   0, 
                   0.02,
                   0,     # beech
                  -0.15,  #spruce_age
                  -0.11,
                  -0.05,
                   0.01, # temp
                   0.01, # humid
                   0,
                   0,
                   0)


# Seems that individual ESS have individual gamma, from Supplementary Table S3
# I will neglect all sigmas values. my tracts variables are already included
# Get estimate for biomass
gamma_biomass = 0.51

# add the intercept value as the first column in a dataframe
df_cent$intercept = gamma_biomass

                  
# Multiple the df by the coefficients column-wise 
# get the sum and add the 
df_multiply = sweep(df_cent, 2, coeff_biomass, "*")


# Sum up the rows
df_sums = rowSums(df_multiply)


# get mean 
mean(df_sums)

# 0.5517183, +- correspond to 0.43 as mean in the output table





























# -----------------------

# Hierarchical models:

library(HBglm)


data("linear_list", package = "HBglm")
fm <- formula(y ~ x1 + x2 + x3 - 1 | z1 + z2 + z3 - 1 | grpID)
fm.fixed <- formula(y ~ f1 + f2)
samp.control = hbglm.sampler.control(num.samples = 20)
fm



# http://www.stat.columbia.edu/~gelman/presentations/usertalk.pdf

# Or bayesglm()
# regression coefficient: student-t prior distributions
# Hierarchical glm (HGLM) - relax the assumption that error components are independent 
# compred to glm()

# https://rdrr.io/cran/arm/man/bayesglm.html
