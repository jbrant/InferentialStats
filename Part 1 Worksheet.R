####################################
## Part 1 Worksheet
####################################

## Motivating example with distribution of 1,000 random uniforms
hist(runif(1000))

## Compared to distribution of 1,000 averages of 40 random uniforms
mns = NULL
for (i in 1:1000)
  mns = c(mns, mean(runif(40)))
hist(mns)

## Set the seed for repeatability purposes
set.seed(1234)

## Set for desired result precision
precision <- 3

## Setup simulation parameters

lambda <- 0.2                # Rate parameter
n <- 40                      # Population
total.simulations <- 1:1000  # Number of simulations

## Run the simulation
simulation.matrix <- data.frame(
  sim.means = sapply(
    total.simulations, 
    function(x) 
      mean(rexp(n, lambda))))

#############
## Problem 1
#############

## Calculate the sample mean (rounding to three decimals places)
sample.mean <- round(mean(simulation.matrix$sim.means), precision)

## Print sample mean
sprintf("Sample Mean: %g", sample.mean)

## Calculate the theoretical mean
theoretical.mean <- 1 / lambda

## Print the theoretical mean
sprintf("Theoretical mean: %g", theoretical.mean)

#############
## Problem 2
#############

## Calculate the sample standard deviation
sample.sd <- round(sd(simulation.matrix$sim.means), precision)

## Print sample standard deviation
sprintf("Sample Standard Deviation: %g", sample.sd)

## Calculate the theoretical standard deviation
theoretical.sd <- round(1/lambda/sqrt(n), precision)

## Print theoretical standard deviation
sprintf("Theoretical Standard Deviation: %g", theoretical.sd)

## Calculate the sample variance
sample.variance <- round(sample.sd^2, precision)

## Print sample variance
sprintf("Sample Variance: %g", sample.variance)

## Calculate the theoretical variance
theoretical.variance <- round(theoretical.sd^2, precision)

## Print theoretical variance
sprintf("Theoretical Variance: %g", theoretical.variance)

#############
## Problem 3
#############

## Import necessary plotting libraries
suppressMessages(library(ggplot2))

## Plot the sample distributions
ggplot(data = simulation.matrix, aes(x = sim.means)) +
  geom_histogram(
    aes(y = ..density..), 
    fill = "darkgreen", 
    binwidth = 1/6, 
    color = "black", 
    alpha=1/2) +
  stat_function(
    fun = dnorm, 
    args = list(theoretical.mean, theoretical.variance), 
    aes(color = "Theoretical Density Curve"),
    size = 1) +
  geom_density(
    aes(color = "Sample Density Curve"), 
    size = 1, 
    show_guide= FALSE) +
  geom_vline(        
    aes(color = "Theoretical Mean", xintercept = theoretical.mean),     
    size = 1,
    linetype = "longdash") +
  geom_vline(
    aes(color = "Sample Mean", xintercept = sample.mean), 
    size = 1,
    linetype = "dashed") +
  labs(
    title = "Distributions of Sample Means", 
    x = "Sample Means", 
    y = "Density") +
  theme(plot.title = element_text(face = "bold", size = 16, vjust = 1),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.title = element_blank())

## Plot sample quantiles against each other for validation of normality
qqnorm(
  simulation.matrix$sim.means, 
  col="darkgreen", 
  main="Sample Distribution Q-Q Plot")
qqline(simulation.matrix$sim.means, col="darkred")