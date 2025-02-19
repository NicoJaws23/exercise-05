library(tidyverse)
#link dataset
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/IMDB-movies.csv"
#load dataset
d <- read_csv(f)
names(d)
#filter dataset for just movies made from 1920-1979 and have a run time of 1 to 3 hours
#and add a new column "decade" based on "startYear"
d2 <- d |>
  filter(startYear >= 1920 & startYear <= 1979) |>
  filter(runtimeMinutes >= 60 & runtimeMinutes <= 180) |>
  mutate(decade = case_when(
    startYear >= 1920 & startYear <= 1929 ~ "20s",
    startYear >= 1930 & startYear <= 1939 ~ "30s",
    startYear >= 1940 & startYear <= 1949 ~ "40s",
    startYear >= 1950 & startYear <= 1959 ~ "50s",
    startYear >= 1960 & startYear <= 1969 ~ "60s",
    startYear >= 1970 & startYear <= 1979 ~ "70s"
  ))  

#histograms for each decade showing distriution of movie run time
ggplot(data = d2, mapping = aes(x = runtimeMinutes)) +
  geom_histogram() +
  facet_wrap(vars(decade))

#mean and standard deviation of runtime by decade
results <- d2 |>
  group_by(decade) |>
  summarise(popMean = mean(runtimeMinutes), popSD = sd(runtimeMinutes))


