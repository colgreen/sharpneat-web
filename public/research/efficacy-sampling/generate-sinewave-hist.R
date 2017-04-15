library("ggplot2")
setwd("D:/home/websites/sharpneat-web/public/research/efficacy-sampling")

# A colourblind friendly palette.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Load two data frames, binary 11 task with 50% crossover & with 0% crossover.
dat50 <- read.table("sinewave-samples.csv", sep=",", head=TRUE)
dat0 <- read.table("sinewave-samples-xover0.csv", sep=",", head=TRUE)

# Combine the two dataframes into one.
# First create a new column in each.
dat50$xover <- 'x50'
dat0$xover <- 'x0'

# and combine into a new data frame.
dat <- rbind(dat50, dat0)

# Plot single histogram for the 50% crossover rate; and save to png.
# Generative Sinewave
p1 = ggplot(dat50, aes(x=dat50$bestfitness, y=..density..)) + 
   geom_histogram(color='black', fill=cbPalette[2], alpha='0.8') + 
   labs(x="fitness") + 
   ggtitle("Generative Sinewave")

ggsave(filename="sinewave-fitness-60sec-hist.png", plot=p1, height=3, width=4.45, units="in", dpi=100)

# Generation count.
p2 = ggplot(dat50, aes(x=dat50$gens, y=..density..)) + 
   geom_histogram(color='black', fill=cbPalette[3], alpha='0.8') + 
   labs(x="generations") + 
   ggtitle("Generative Sinewave")

ggsave(filename="sinewave-gens-60sec-hist.png", plot=p2, height=3, width=4.45, units="in", dpi=100)

# Evaluation count.
p3 = ggplot(dat50, aes(x=dat50$evalcount, y=..density..)) + 
   geom_histogram(color='black', fill=cbPalette[4], alpha='0.8') + 
   labs(x="evaluations") + 
   ggtitle("Generative Sinewave")

ggsave(filename="sinewave-evalcount-60sec-hist.png", plot=p3, height=3, width=4.45, units="in", dpi=100)


# Plot two overlaid histograms, one for each of the two dataframes
p4= ggplot(dat, aes(bestfitness, fill = xover )) + 
   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
   scale_fill_manual(values=cbPalette, name='x-over\nRate', labels=c('0%','50%')) +
   labs(x='fitness') + 
   ggtitle("Generative Sinewave\nCrossover Rate Comparison") 

ggsave(filename="sinewave-fitness-60sec-xover-hist.png", plot=p4, height=3, width=4.45, units="in", dpi=100)

