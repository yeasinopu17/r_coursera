library(dplyr)
iris <- as_tibble(iris)

subset %>% pivot_longer(c(Sepal.Length, Petal.Length))
