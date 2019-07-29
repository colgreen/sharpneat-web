library("ggplot2")
setwd("D:/home/websites/sharpneat-web/public/releases/sharpneat-2-4-3")

# A colourblind friendly palette.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPaletteB <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#E69F00")
cbPaletteC <- c("#97d343", "#fed325")

# Load two data frames
dat1 <- read.table("sinewave-samples-v2-4-2-mon.csv", sep=",", head=TRUE)
dat2 <- read.table("sinewave-samples-v2-4-3.csv", sep=",", head=TRUE)

# Combine the two dataframes into one.
# First create a new column in each.
dat1$ver <- 'v242'
dat2$ver <- 'v243'

# and combine into a new data frame.
dat <- rbind(dat1, dat2)

# Plot two overlaid histograms, one for each of the two dataframes
p4 = ggplot(dat, aes(bestfitness, fill = ver )) + 
   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
   scale_fill_manual(values=cbPalette, name='Version', labels=c('v 2.4.2','v 2.4.3')) +
   labs(x='fitness') + 
   ggtitle("Generative Sinewave Task") +
   theme(plot.title = element_text(size = 12))

ggsave(filename="sinewave-bestfitness-v242-v243-hist.png", plot=p4, height=4, width=8, units="in", dpi=100)

# Eval count.
p2 = ggplot(dat, aes(evalcount, fill = ver )) + 
   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
   scale_fill_manual(values=cbPaletteB, name='Version', labels=c('v 2.4.2','v 2.4.3')) +
   labs(x='evaluation count') + 
   ggtitle("Generative Sinewave Task") +
   theme(plot.title = element_text(size = 12))

ggsave(filename="sinewave-evalcount-v242-v243-hist.png", plot=p2, height=4, width=8, units="in", dpi=100)

# Mean complexity.
p2 = ggplot(dat, aes(meancomplexity, fill = ver )) + 
   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
   scale_fill_manual(values=cbPaletteC, name='Version', labels=c('v 2.4.2','v 2.4.3')) +
   labs(x='mean complexity') + 
   ggtitle("Generative Sinewave Task") +
   theme(plot.title = element_text(size = 12))

ggsave(filename="sinewave-meancomplexity-v242-v243-hist.png", plot=p2, height=4, width=8, units="in", dpi=100)


