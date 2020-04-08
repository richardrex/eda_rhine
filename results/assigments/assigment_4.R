library(ggplot2)
runoff_stats
runoff_melt <- melt(runoff_stats, id.vars = 'sname')
ggplot(runoff_melt, 
       aes(x = sname, y = value, shape = "cyl", color = "cyl", size = 0.5)) + 
  geom_point(aes(col = variable, shape = variable))
