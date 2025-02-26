#Step 1, loading libraries and data sets
library(tidyverse)
library(mosaic)
#link dataset
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/IMDB-movies.csv"
#load dataset
d <- read_csv(f)
names(d)
#Step 2: filter dataset for just movies made from 1920-1979 and have a run time of 1 to 3 hours
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

#Step 3: histograms for each decade showing distriution of movie run time
ggplot(data = d2, mapping = aes(x = runtimeMinutes)) +
  geom_histogram() +
  facet_wrap(vars(decade))

#Step 4: mean and standard deviation of runtime by decade
results <- d2 |>
  group_by(decade) |>
  summarise(popMean = mean(runtimeMinutes), popSD = sd(runtimeMinutes), pop = n())

#Step 5: sample 100 moives from each decade
#sampling function, d2 is dataframe we are pulling from
movieSamp <- function(df, decadeVal, variable, num){
  data <- filter(df, decade == decadeVal)
  samp <- slice_sample(data, n = num)
  sampMean <- mean(samp[[variable]])
  sampSD <- sd(samp[[variable]])
  data <- data.frame(runTimeMean = sampMean, runTimeSD = sampSD)
  return(data)
}

d20s <- movieSamp(d2, "20s", "runtimeMinutes", 100)
d30s <- movieSamp(d2, "30s", "runtimeMinutes", 100)
d40s <- movieSamp(d2, "40s", "runtimeMinutes", 100)
d50s <- movieSamp(d2, "50s", "runtimeMinutes", 100)
d60s <- movieSamp(d2, "60s", "runtimeMinutes", 100)
d70s <- movieSamp(d2, "70s", "runtimeMinutes", 100)


#Step 6: calculate the standard error for each decade
#Need to use sd/sqrt(length(x)) method, x here is the sample, ex d20s
#Function for generating and storing the standard error of each sample
#need to use d20s-d70s data created with movieSamp()
se <- function(df, sdVariable){
  sd <- df[[sdVariable]]
  se <- sd/sqrt(length(df))
  return(se)
}
d20s <- d20s |>
  mutate(runTimeSE = se(d20s, "runTimeSD")) |
d30s <- d30s |>
  mutate(runTimeSE = se(d30s, "runTimeSD"))
d40s <- d40s |>
  mutate(runTimeSE = se(d40s, "runTimeSD"))
d50s <- d50s |>
  mutate(runTimeSE = se(d50s, "runTimeSD"))
d60s <- d60s |>
  mutate(runTimeSE = se(d60s, "runTimeSD"))
d70s <- d70s |>
  mutate(runTimeSE = se(d70s, "runTimeSD"))
#Step 7: Compare these sampled means and the standard error
#calculate the actual means, sd, and se of the population for each decade
#reults data
d20se <- 26.20133/sqrt(152)
d30se <- 17.28879/sqrt(530)
d40se <- 19.12372/sqrt(782)
d50se <- 19.20646/sqrt(1081)
d60se <- 21.23202/sqrt(1386)
d70se <- 17.95934/sqrt(1720)
#Step 8: Generating sampling distribution drawing 1000 random samples of 100 movies
#by decade. Calculating the mean and sd in runtimeMinutes
sampMetricsDecade <- function(df, variable, decadeVal, sampReps, sampNum){
  data <- filter(df, decade == decadeVal)
  sampDistMean <- as.numeric(sampReps)
  sampDistSD <- as.numeric(sampReps)
  for(i in 1:sampReps){
    sample <- slice_sample(data, n = sampNum)
    sampDistMean[i] <- mean(sample[[variable]])
    sampDistSD[i] <- sd(sample[[variable]])
  }
  print(paste("Mean and standard deviation for", sampReps, "random samples of", sampNum, "survivors is in dataframe!"))
  return(data.frame(means = sampDistMean, standard_deviation = sampDistSD))
}
d20samp <- sampMetricsDecade(d2, "runtimeMinutes", "20s", 1000, 100)
d30samp <- sampMetricsDecade(d2, "runtimeMinutes", "30s", 1000, 100)
d40samp <- sampMetricsDecade(d2, "runtimeMinutes", "40s", 1000, 100)
d50samp <- sampMetricsDecade(d2, "runtimeMinutes", "50s", 1000, 100)
d60samp <- sampMetricsDecade(d2, "runtimeMinutes", "60s", 1000, 100)
d70samp <- sampMetricsDecade(d2, "runtimeMinutes", "70s", 1000, 100)

#Step 9: Calc the mean and standard deviation of the sample dist
#of the sample means for each decade and plot on histogram
#the sampling distribution, sd of the sample means of the sample
#create histograms of the sampling distributions for each decade
sampMeanSD <- function(df, Meanvariable){
  sampDistMean <- mean(df[[Meanvariable]])
  sampDistSD <- sd(df[[Meanvariable]])
  print(paste("Mean =", sampDistMean, "; SD =", sampDistSD))
  return(data.frame(sampleDistMean = sampDistMean, sampleDistSD = sampDistSD))
}

d20distMeanSD <- sampMeanSD(d20samp, "means")
d30distMeanSD <- sampMeanSD(d30samp, "means")
d40distMeanSD <- sampMeanSD(d40samp, "means")
d50distMeanSD <- sampMeanSD(d50samp, "means")
d60distMeanSD <- sampMeanSD(d60samp, "means")
d70distMeanSD <- sampMeanSD(d70samp, "means")

hist(d20samp$means)
hist(d30samp$means)
hist(d40samp$means)
hist(d50samp$means)
hist(d60samp$means)
hist(d70samp$means)

#Step 10: Answer questions for quarto document