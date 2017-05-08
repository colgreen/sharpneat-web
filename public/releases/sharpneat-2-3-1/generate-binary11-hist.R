library("ggplot2")
setwd("D:/home/websites/sharpneat-web/public/releases/sharpneat-2-3-1")

# A colourblind friendly palette.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbPalette1 <- c("#F0E442", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbPalette2 <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#E69F00")
cbPalette3 <- c("#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#E69F00", "#56B4E9", "#009E73")

# Load two data frames
dat1 <- read.table("binary11-samples-relu.csv", sep=",", head=TRUE)
dat2 <- read.table("binary11-samples-relucentered.csv", sep=",", head=TRUE)
dat3 <- read.table("binary11-samples-sigmoid.csv", sep=",", head=TRUE)
dat4 <- read.table("binary11-samples-sigmoidapprox.csv", sep=",", head=TRUE)

# Combine the two dataframes into one.
# First create a new column in each.
dat1$fn <- 'relu'
dat2$fn <- 'relucentered'
dat3$fn <- 'sigmoid'
dat4$fn <- 'sigmoidapprox'

# Plot two overlaid histograms (best fitness; relu, sigmoid, sigmoidapprox)
dat <- rbind(dat1, dat3, dat4)

p1 = ggplot(dat, aes(bestfitness, fill = fn )) + 
   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
   scale_fill_manual(values=cbPalette, name='Activation\nfunction', labels=c('relu', 'sigmoid', 'sigmoidapprox')) +
   labs(x='evaluation count') + 
   ggtitle("Binary11 Multiplexer: Best Fitness Histograms") 

ggsave(filename="binary11-bestfitness-relu-sigmoid-sigmoidapprox.png", plot=p1, height=4, width=8, units="in", dpi=100)


# Plot two overlaid histograms (eval count; relu, sigmoid, sigmoidapprox)
p1 = ggplot(dat, aes(evalcount, fill = fn )) + 
   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
   scale_fill_manual(values=cbPalette2, name='Activation\nfunction', labels=c('relu', 'sigmoid', 'sigmoidapprox')) +
   labs(x='evaluation count') + 
   ggtitle("Binary11 Multiplexer: Evaluation Count Histograms") 

ggsave(filename="binary11-evalcount-relu-sigmoid-sigmoidapprox.png", plot=p1, height=4, width=8, units="in", dpi=100)


