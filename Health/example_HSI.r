
# health indices
# Build health indices working example from Mikkos' Paper



library(dplyr)

# Capercaille is based on pine volume, spruce volume and Density of trees

# based on subutility functions
# get examples for volumes from lexicon

# V is from 0-600 m3/ha
set.seed(3)
V_pine = sample.int(100, 10)  # I keep 100 to have a nie numbers, as 600 is extreme value for single species


for (i in V_pine){
 # print (i)
  if (i <= 60) {
    i <- 0
    } else if (i > 60 & i<= 80) {
      i <- (0.05* i )-3
    } else if (i > 80) {
      i<- 1
    }
  print(i)
}

 

df <- data.frame(V_pine)

#df %>% 

if(V_pine <= 60){print ("positive")}


f_w_pine = function(x) {
  print(x)
  if(x <= 60){print ("positive")}
  print(x)
}
 
f_w_pine(V_pine)


# firts reclassify the values:
V_pine = replace(V_pine, V_pine <= 60, 0)
V_pine = replace(V_pine, V_pine >  80, 1)
V_pine = replace(V_pine, V_pine >  80, 1)


#revalue(V_pine, c(a = "A", c = "C"))

w_pine = 0.05