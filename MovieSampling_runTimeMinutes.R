library(tidyverse)
library(mosaic)
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

#create separate dataframes for each decade
d20s <- filter(d2, decade == "20s")
d30s <- filter(d2, decade == "30s")
d40s <- filter(d2, decade == "40s")
d50s <- filter(d2, decade == "50s")
d60s <- filter(d2, decade == "60s")
d70s <- filter(d2, decade == "60s")

#sample 100 movies from each decade and calculate the sample mean
#and standard deviation in runtimeMinutes
d20Sample <- slice_sample(d20s, n = 100)
d20SampMeanSD <- d20Sample |>
  summarise(SampleMean = mean(runtimeMinutes), SampleSD = sd(runtimeMinutes))

d30Sample <- slice_sample(d30s, n = 100)
d30SampMeanSD <- d30Sample |>
  summarise(SampleMean = mean(runtimeMinutes), SampleSD = sd(runtimeMinutes))

d40Sample <- slice_sample(d40s, n = 100)
d40SampMeanSD <- d40Sample |>
  summarise(SampleMean = mean(runtimeMinutes), SampleSD = sd(runtimeMinutes))

d50Sample <- slice_sample(d50s, n = 100)
d50SampMeanSD <- d50Sample |>
  summarise(SampleMean = mean(runtimeMinutes), SampleSD = sd(runtimeMinutes))

d60Sample <- slice_sample(d60s, n = 100)
d60SampMeanSD <- d60Sample |>
  summarise(SampleMean = mean(runtimeMinutes), SampleSD = sd(runtimeMinutes))

d70Sample <- slice_sample(d70s, n = 100)
d70SampMeanSD <- d70Sample |>
  summarise(SampleMean = mean(runtimeMinutes), SampleSD = sd(runtimeMinutes))

#calculate the standard error for each decade
#Need to use sd/sqrt(length(x)) method, x here is the sample, ex d20Sample

se20s <- d20SampMeanSD$SampleSD/sqrt(length(d20Sample))
se30s <- d30SampMeanSD$SampleSD/sqrt(length(d30Sample))
se40s <- d40SampMeanSD$SampleSD/sqrt(length(d40Sample))
se50s <- d50SampMeanSD$SampleSD/sqrt(length(d50Sample))
se60s <- d60SampMeanSD$SampleSD/sqrt(length(d60Sample))
se70s <- d70SampMeanSD$SampleSD/sqrt(length(d70Sample))

#Generating sampling distribution drawing 1000 random samples of 100 movies
#by decade. Calculating the mean and sd in runtimeMinutes
reps <- 1000
sampleDist20s <- do(reps) * mean(rnorm(n = d20Sample, mean = 96.25658, sd = 26.20133))

