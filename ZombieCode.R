#Step 1
library(tidyverse)
library(ggplot2)
library(mosaic)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
z <- read_csv(f)

#Step 2: population mean and sd for each quantative random variable
#height, weight, age, number of zombies killed, years of education
n <- 1000
MeanSDz <- z |>
  group_by(gender) |>
  summarise(meanHeight = mean(height), meanWeight = mean(weight), meanAge = mean(age),
            meanZKills = mean(zombies_killed), meanEdYrs = mean(years_of_education),
         sdHeight = sqrt((n - 1)/n)*sd(height), sdWeight = sqrt((n-1)/n)*sd(weight),
         sdAge = sqrt((n-1)/n)*sd(age), sdZKills = sqrt((n-1)/n)*sd(zombies_killed),
         sdEdYrs = sqrt((n-1)/n)*sd(years_of_education))

#Step 3 box plot for each variable by gender

#Height
ggplot(data = z, mapping = aes(x = gender, y = height))+
  geom_boxplot() +
  ggtitle("Height by Gender")
#Weight
ggplot(data = z, mapping = aes(x = gender, y = weight))+
  geom_boxplot() +
  ggtitle("Weight by Gender")
#Age
ggplot(data = z, mapping = aes(x = gender, y = age))+
  geom_boxplot() +
  ggtitle("Age by Gender")
#Number of Zombies killed
ggplot(data = z, mapping = aes(x = gender, y = zombies_killed))+
  geom_boxplot() +
  ggtitle("Zombie Kills by Gender")
#Years of education
ggplot(data = z, mapping = aes(x = gender, y = years_of_education))+
  geom_boxplot() +
  ggtitle("Years of Education by Gender")

#Step 4: create scatterplots of height and weight by age
#Height and age
ggplot(data = z, mapping = aes(x = age, y = height, color = gender)) +
  geom_point() +
  ggtitle("Height in relation to Age")

#Weight and age
ggplot(data = z, mapping = aes(x = age, y = weight, color = gender)) +
  geom_point() +
  ggtitle("Weight in relation to Age")


#Step 5: Determine of data is drawn from a normal distribution using histograms 
#and QQ plots for each quantitative variable

#Height
hist(z$height) #looks normal
qqnorm(z$height) #normal

#Weight
hist(z$weight) #normal
qqnorm(z$weight) #normal

#Age
hist(z$age) #normal
qqnorm(z$age) #normal

#Zombie Kills
hist(z$zombies_killed) #not normal
qqnorm(z$zombies_killed) #not normal

#Years of Education
hist(z$years_of_education) #not normal
qqnorm(z$years_of_education) #not normal

#Step 6: Sample ONE subset of 50 survivors, calculate mean and sd of each variable
#Estimate standard error of the sample and make 95% CIs for each
#MEAN, can use either standard normal or Students t dist to
#derive the critical values to make upper and lower CIs
sampleSurvivors <- slice_sample(z, n = 50)
#For fun, this function will do what all of the variables above did but with less
#work on my end. stores all the values in a dataframe
metrics <- function(data, variable, lowCIBound, highCIBound){
  sampMean <- mean(data[[variable]])
  sampSD <- sd(data[[variable]])
  sampSE <- sampSD/sqrt(length(data))
  sampCI <- sampMean + qnorm(c(lowCIBound, highCIBound)) * sampSE
  print(paste(variable, "mean:", sampMean))
  print(paste(variable, "standard deviation:", sampSD))
  print(paste(variable, "standard error:", sampSE))
  print(paste(variable, "lower confidence intervals:", sampCI[1], "upper confience interval:", sampCI[2]))
  return(data.frame(metric = c("mean", "standard deviation", "standard error", "low confidence interval", "high confidence interval"), value = (c(sampMean, sampSD, sampSE, sampCI[1], sampCI[2]))))
}
heightMetrics <- metrics(sampleSurvivors, "height", 0.025, 0.975)
weightMetrics <- metrics(sampleSurvivors, "weight", 0.025, 0.975)
ageMetrics <- metrics(sampleSurvivors, "age", 0.025, 0.975)
zombieKillsMetrics <- metrics(sampleSurvivors, "zombies_killed", 0.025, 0.975)
educationMetrics <- metrics(sampleSurvivors, "years_of_education", 0.025, 0.975)
#Height
#sampHeightM <- mean(sampleSurvivors$height)
#sampHeightSD <- sd(sampleSurvivors$height)
#sampHeightSE <- sampHeightSD/sqrt(length(sampleSurvivors))
#sampHeightCI <- sampHeightM + qnorm(c(0.025, 0.975)) * sampHeightSE
#Weight
#sampWeightM <- mean(sampleSurvivors$weight)
#sampWeightSD <- sd(sampleSurvivors$weight)
#sampWeightSE <- sampWeightSD/sqrt(length(sampleSurvivors))
#sampWeightCI <- sampWeightM + qnorm(c(0.025, 0.975)) * sampWeightSE
#Age
#sampAgeM <- mean(sampleSurvivors$age)
#sampAgeSD <- sd(sampleSurvivors$age)
#sampAgeSE <- sampAgeSD/sqrt(length(sampleSurvivors))
#sampAgeCI <- sampAgeM + qnorm(c(0.025, 0.975)) * sampAgeSE
#Zombie Kills
#sampZKM <- mean(sampleSurvivors$zombies_killed)
#sampZKSD <- sd(sampleSurvivors$zombies_killed)
#sampZKSE <- sampZKSD/sqrt(length(sampleSurvivors))
#sampZKCI <- sampZKM + qnorm(c(0.025, 0.975)) * sampZKSE
#Years of Education
#sampYEM <- mean(sampleSurvivors$years_of_education)
#sampYESD <- sd(sampleSurvivors$years_of_education)
#sampYESE <- sampYESD/sqrt(length(sampleSurvivors))
#sampYECI <- sampYEM + qnorm(c(0.025, 0.975)) * sampYESE


#Step 7: Draw 199 random samples of 50 survivors, calculate mean
#and standard deviations
#Function to do this
sampMetrics <- function(df, variable, sampReps, sampNum){
  sampDistMean <- as.numeric(sampReps)
  sampDistSD <- as.numeric(sampReps)
  for(i in 1:sampReps){
    sample <- slice_sample(df, n = sampNum)
    sampDistMean[i] <- mean(sample[[variable]])
    sampDistSD[i] <- sd(sample[[variable]])
  }
  print(paste("Mean and standard deviation for", sampReps, "random samples of", sampNum, "survivors is in dataframe!"))
  return(data.frame(means = sampDistMean, standard_deviation = sampDistSD))
}
heightSamp <- sampMetrics(z, "height", 199, 50) #mean: 66.01 - 69.18, sd: 3.4-5.74 / 50Mean: 68.63, 50SD: 3.9, 50SE: 1.23
weightSamp <- sampMetrics(z, "weight", 199, 50) #mean: 136.54-149.8, sd: 13.86-22.69 / 50Mean: 148.07, 50SD: 16.63, 50SE: 5.25
ageSamp <- sampMetrics(z, "age", 199, 50) #mean: 18.95-21.37, sd: 2.16-3.73 / 50Mean: 20.86, 50SD: 3.04, 50SE: 0.96
zombieKillsSamp <- sampMetrics(z, "zombies_killed", 199, 50) #mean: 2.36-3.58, sd: 1.17-2.23 / 50Mean: 2.68, 50SD: 1.58, 50SE: 0.5
educationSamp <- sampMetrics(z, "years_of_education", 199, 50) #mean: 2.38-3.60, sd: 1.24-2.17 / 50Mean: 3.14, 50SD: 1.74, 50SE: 0.55

#Step 8: plot the sampling dist for each variable mean, histograms
#Height
hist(heightSamp$means) #normal dist
#weight
hist(weightSamp$means) #normalish?? Biased towards higher end
#age
hist(ageSamp$means) #normalish?? Some irregularities
#zombie kills
hist(zombieKillsSamp$means) #kinda normal?
#years of education
hist(educationSamp$means) #kinda normal

#Step 9: create 95% confidence intervals for the means of each value
#based on the sampling distribution
samp95CI <- function(df, meanVariable){
  m <- mean(df[[meanVariable]])
  se <- sd(df[[meanVariable]])
  ci <- m + qnorm(c(0.025, 0.975)) * se
  print("Confidene intervals have been caluclated and stored in values")
  return(ci)
}
heightSampCI <- samp95CI(heightSamp, "means")
weightSampCI <- samp95CI(weightSamp, "means")
ageSampCI <- samp95CI(ageSamp, "means")
zombieKillSampCI <- samp95CI(zombieKillsSamp, "means")
edYearsSampCI <- samp95CI(educationSamp, "means")

#Step 10: Use bootstrapping to generate 95% CIs for each variable
#Resampling 1000 samples, with replacement, from original sample
#bootstrap function
bootstrap <- function(df, variable, resampNum){
  n_boot <- resampNum
  boot <- vector(length=n_boot)
  n <- length(df)
  for(i in 1:n_boot){
    boot[[i]] <- mean(sample(df[[variable]], n, replace = TRUE))
  }
  ci <- quantile(boot, probs = c(0.025, 0.975))
  print(ci)
  return(ci)
}
heightBoot <- bootstrap(z, "height", 1000)
weightBoot <- bootstrap(z, "weight", 1000)
ageBoot <- bootstrap(z, "age", 1000)
zombieKillsBoot <- bootstrap(z, "zombies_killed", 1000)
edYearsBoot <- bootstrap(z, "years_of_education", 1000)
