library(tidyverse)
library(ggplot2)
library(mosaic)
f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/zombies.csv"
z <- read_csv(f)

#population mean and sd for each quantative random variable
#height, weight, age, number of zombies killed, years of education
n <- 1000
MeanSDz <- z |>
  group_by(gender) |>
  summarise(meanHeight = mean(height), meanWeight = mean(weight), meanAge = mean(age),
            meanZKills = mean(zombies_killed), meanEdYrs = mean(years_of_education),
         sdHeight = sqrt((n - 1)/n)*sd(height), sdWeight = sqrt((n-1)/n)*sd(weight),
         sdAge = sqrt((n-1)/n)*sd(age), sdZKills = sqrt((n-1)/n)*sd(zombies_killed),
         sdEdYrs = sqrt((n-1)/n)*sd(years_of_education))

#box plot for each variable by gender

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

#create scatterplots of height and weight by age
#Height and age
ggplot(data = z, mapping = aes(x = age, y = height, color = gender)) +
  geom_point() +
  ggtitle("Height in relation to Age")

#Weight and age
ggplot(data = z, mapping = aes(x = age, y = weight, color = gender)) +
  geom_point() +
  ggtitle("Weight in relation to Age")


#Determine of data is drawn from a normal distribution using histograms 
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

#Sample ONE subset of 50 survivors, calculate mean and sd of each variable
#Estimate standard error of the sample and make 95% CIs for each
#MEAN, can use either standard normal or Students t dist to
#derive the critical values to make upper and lower CIs
sampleSurvivors <- slice_sample(z, n = 50)
#Height
sampHeightM <- mean(sampleSurvivors$height)
sampHeightSD <- sd(sampleSurvivors$height)
sampHeightSE <- sampHeightSD/sqrt(length(sampleSurvivors))
sampHeightCI <- sampHeightM + qnorm(c(0.025, 0.975)) * sampHeightSE
#Weight
sampWeightM <- mean(sampleSurvivors$weight)
sampWeightSD <- sd(sampleSurvivors$weight)
sampWeightSE <- sampWeightSD/sqrt(length(sampleSurvivors))
sampWeightCI <- sampWeightM + qnorm(c(0.025, 0.975)) * sampWeightSE
#Age
sampAgeM <- mean(sampleSurvivors$age)
sampAgeSD <- sd(sampleSurvivors$age)
sampAgeSE <- sampAgeSD/sqrt(length(sampleSurvivors))
sampAgeCI <- sampAgeM + qnorm(c(0.025, 0.975)) * sampAgeSE
#Zombie Kills
sampZKM <- mean(sampleSurvivors$zombies_killed)
sampZKSD <- sd(sampleSurvivors$zombies_killed)
sampZKSE <- sampZKSD/sqrt(length(sampleSurvivors))
sampZKCI <- sampZKM + qnorm(c(0.025, 0.975)) * sampZKSE
#Years of Education
sampYEM <- mean(sampleSurvivors$years_of_education)
sampYESD <- sd(sampleSurvivors$years_of_education)
sampYESE <- sampYESD/sqrt(length(sampleSurvivors))
sampYECI <- sampYEM + qnorm(c(0.025, 0.975)) * sampYESE

