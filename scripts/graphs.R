library(tidyverse)
library(ggplot)
library(hexbin)

df_store <- read.csv("data/out/params_fmeasures.csv")
summary(df_store)

#f_measure comparison boxplots
boxplot(f_measure ~ id, data = df_store)
boxplot(f_measure ~ minpts, data = df_store)
boxplot(f_measure ~ eps, data = df_store)
boxplot(f_measure ~ eps2, data = df_store)

#comparison minpts vs. eps
hist(df_store$minpts)
hist(df_store$eps)

##Scatter plot with heatmap
counts <- as.data.frame(table(df_store$minpts, df_store$eps))
names(counts) <- c("minpts", "eps", "Freq")

ggplot(counts, aes(x = minpts, y = factor(eps))) +
  geom_point(aes(fill = Freq), shape = 21, size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "minpts", y = "eps") +
  theme_minimal()
