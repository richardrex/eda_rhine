Temperatures = c(3,6,10,14)
WEIGHTS = c(1,0.8,1.2,1)
library(data.table)
aa = function(x,y){
  x*y
  }
results <- aa(Temperatures,WEIGHTS)
#explorers question 2
Rc = 185000 #catchment area in km2
Rc
L = 1233000 #length of Rhine
rw = 185000 * 1000000/1233000 #rhine average width
rw
perc = 0.05 * 24 #precipitation in 24 h
perc
ro = (0.05/3600) * rw #increase in runoff
ro
