library (ggplot2)
library (dplyr)

# save (base.df, file = "clean_base.df.rda")
load ("clean_base.df.rda")

#scatter plot
  ggplot(data=base.df, aes(x = .40 * Tot.Scr.Time, y = .6 * Pickups, color = Sequence)) + 
  geom_point ()
  
  
#scatter plot facetted by sex
  ggplot(data=base.df, aes(x = .40 * Tot.Scr.Time, y = .6 * Pickups, color = Sequence)) + 
    geom_point () +
    facet_grid(~sex)

  
#spaghetti plot 
  ggplot(data=base.df, aes(x = index, y = Tot.Scr.Time, color = Sequence, group = pseudo_id)) + 
  geom_line () 
  