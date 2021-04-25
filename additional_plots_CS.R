library (ggplot2)
library (dplyr)

# save (base.df, file = "clean_base.df.rda")
load ("clean_base.df.rda")

#scatter plot
  ggplot(data=base.df) + 
  geom_point (aes(x = .40 * Tot.Scr.Time, y = .6 * Pickups, color = Sequence))
  
  
#scatter plot facetted by sex
  ggplot(data=base.df) + 
    geom_point (aes(x = .40 * Tot.Scr.Time, y = .6 * Pickups, color = Sequence)) +
    facet_grid(~sex)
