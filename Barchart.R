

library("tidyverse")





summary_stats <- read_csv("table1.csv")


tidy <- summary_stats %>%
  filter(
    Statistic == "Successful one-dart finish (proportion, first only)"
  ) %>%
  pivot_longer(
    c(2:5),
    names_to = "Comp",
    values_to = "Value"
  ) 


ggplot(tidy, aes(x = reorder(Comp, Value), y = Value, fill = Comp)) +
  geom_col(color = "black") +
  coord_flip() +
  xlab("") +
  ylab("Proportion") +
  ggtitle("Successful One-Dart Finishes by Competition", subtitle = "Proportion, First only") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 18),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14)
  )




rm(list=ls())
