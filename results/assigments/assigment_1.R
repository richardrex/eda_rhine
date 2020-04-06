Temperatures= c (3,6,10,14)
WEIGHTS = c (1,0.8,1.2,1)
library(data.table)
aa = function(x,y){
  x*y
  }
results <- aa(Temperatures,WEIGHTS)
