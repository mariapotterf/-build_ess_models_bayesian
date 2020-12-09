
# --------------------------------------
# Get models from Gamfelt et al. 2013
# --------------------------------------

# higher levels of multiple ESS are found in forests with more tree species

# biodiversity is siggested to positively influence multiple ESS but evidence is scarced
# ESS: 

# How does tree species richness predict 
# - production of tree biomass
# - soil carbon storage
# - berry production 
# - game production potential ~ occurence of 15 splat species important for large herbivores
#                       - measured in teh same ways as bilberry cover
# - Understory plant species richness: proportion of ruichness from teh regional richness
# - probability of deadwood: presence/absence
# no single tree species was able to promome all services 

# Input data: Swedist national forest inventory and Survey of forest soils and vegetation
# only on 'productive forests' = average production of standing volume, stem volume over bark >1m3/ha/year
# which has not been harvested, cleared, thnned the 5 years before teh survey
# non peat soil

# get effect sizes to understand teh contribution of individual factors


# Explanatory variables:
# temperature - total accumulated average dialy temperature (degree C) over 5C during veget.period
# humidity    - difference in mm between precipitation and evaporation
# nitrogen deposition (kg N/ha/year) - meteo 
# c_n = carbon to nitrogen ration i the top soils
# soil moisture : categorical: 1 (driest) -5 (wettest) ~ corresponds to continuous % volumetric soil content
# 


# Generate dummry predictores



# Hierarchi cal models:

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

