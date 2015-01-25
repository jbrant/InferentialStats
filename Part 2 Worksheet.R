####################################
## Part 2 Worksheet
####################################

library(datasets)
library(ggplot2)
library(pander)

#############
## Problem 1
#############

## Load Tooth Growth dataset
data(ToothGrowth)

## Look at the first 6 rows in the dataset
head(ToothGrowth)

## Determine the structure of the dataset
str(ToothGrowth)

## Get an idea of the spread of the data
summary(ToothGrowth)

#############
## Problem 2
#############

## Calculate the mean of each combination of supplement and dosage
mean.analysis <- aggregate(ToothGrowth$len, list(ToothGrowth$supp, ToothGrowth$dose), mean)

## Calculate the standard deviation of each combination of supplement and dosage (rounding to 2)
sd.analysis <- aggregate(ToothGrowth$len, list(ToothGrowth$supp, ToothGrowth$dose), sd)

## Calculate the variance of each combination of supplement and dosage (rounding to 2)
var.analysis <- aggregate(ToothGrowth$len, list(ToothGrowth$supp, ToothGrowth$dose), var)

## Round standard deviation and variance to 2 decimals
sd.analysis[,3] <- round(sd.analysis[,3], 2)
var.analysis[,3] <- round(var.analysis[,3], 2)

## Set the column names on the resulting data frames
colnames(mean.analysis) <- c("Supplement", "Dosage", "Mean Length")
colnames(sd.analysis) <- c("Supplement", "Dosage", "S.D. of Length")
colnames(var.analysis) <- c("Supplement", "Dosage", "Variance of Length")

## Merge the mean and variance data frames and print
## (in the report, this is done using pandoc)
merge(mean.analysis, 
      merge(sd.analysis, 
            var.analysis, 
            by = c("Supplement", "Dosage")),
      by = c("Supplement", "Dosage"))

## Construct box plot of differences between dosage and supplements split into separate facets
dosage.vs.supp.plot <- ggplot(ToothGrowth, aes(x = supp, y = len)) +
  geom_boxplot(
    aes(fill = factor(dose)), 
    position = position_dodge(1)) +
  labs(
    title = "Supplement vs. Length by Dosage", 
    x = "Supplement", 
    y = "Tooth Length", 
    fill = "Dosage") +
  theme(plot.title = element_text(face = "bold", size = 16, vjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))

## Display the box plot
suppressMessages(grid.arrange(dosage.vs.supp.plot, 
                              sub = textGrob(
                                "Figure 1: Effect of Supplement and Dosage on Tooth Length", 
                                gp = gpar(fontface = "bold", col = "grey20", fontsize = 12))))

#############
## Problem 3
#############

## Split up the dataset into the two separate supplements
vitamin.c <- subset(ToothGrowth, ToothGrowth$supp == "VC")
orange.juice <- subset(ToothGrowth, ToothGrowth$supp == "OJ")

## Hypothesis will need to be defined per dosage

## Hypothesis 1
### H0: There is no difference in tooth growth between orange juice and vitamin C at a dosage of 0.5 mg
### Ha: There is a difference in tooth growth between orange juice and vitamin C at a dosage of 0.5 mg

## Run confidence interval t-test for Hypothesis 1 (reject H0 for OJ)
hypothesis.1 <- t.test(
  subset(orange.juice, dose == 0.5)$len - subset(vitamin.c, dose == 0.5)$len, 
  var.equal=TRUE)

## Hypothesis 2
### H0: There is no difference in tooth growth between orange juice and vitamin C at a dosage of 1 mg
### Ha: There is a difference in tooth growth between orange juice and vitamin C at a dosage of 1 mg

## Run confidence interval t-test for Hypothesis 2 (reject H0 for OJ)
hypothesis.2 <- t.test(
  subset(orange.juice, dose == 1)$len - subset(vitamin.c, dose == 1)$len, 
  var.equal=TRUE)

## Hypothesis 3
### H0: There is no difference in tooth growth between orange juice and vitamin C at a dosage of 2 mg
### Ha: There is a difference in tooth growth between orange juice and vitamin C at a dosage of 2 mg

## Run confidence interval t-test for Hypothesis 3 (reject H0 for VC)
hypothesis.3 <- t.test(
  subset(orange.juice, dose == 2)$len - subset(vitamin.c, dose == 2)$len, 
  var.equal=TRUE)

## Concatenate all t-statistics
t.stats <- c(
  round(hypothesis.1$statistic,2), 
  round(hypothesis.2$statistic,2), 
  round(hypothesis.3$statistic,2))

## Concatenate all degrees of freedom
deg.freedom <- c(hypothesis.1$parameter, hypothesis.2$parameter, hypothesis.3$parameter)

## Concatenate all p-values
p.values <- c(
  round(hypothesis.1$p.value,2), 
  round(hypothesis.2$p.value,2), 
  round(hypothesis.3$p.value))

## Construct and format the hypothesis data frame
hypothesis.df <- data.frame(t.stats, deg.freedom, p.values, 
                            row.names = c("Hypothesis 1", "Hypothesis 2", "Hypothesis 3"))
colnames(hypothesis.df) <- c("t-Statistic", "Degrees of Freedom", "p-Value")

## Print the hypothesis table
pander(hypothesis.df, caption = "Mean, Standard Deviation, and Variance of Distributions")