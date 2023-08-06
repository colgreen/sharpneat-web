library("ggplot2")
setwd("D:/home/websites/sharpneat-web/public/releases/sharpneat-4-0-0")

# A colourblind friendly palette.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPaletteB <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","#CC79A7", "#999999", "#E69F00")
cbPaletteC <- c("#97d343", "#fed325")

# Load two data frames
dat1 <- read.table("binary11-samples-v2-4-4.csv", sep=",", head=TRUE)
dat2 <- read.table("binary11-samples-v4-0-1.csv", sep=",", head=TRUE)

# Combine the two dataframes into one.
# First create a new column in each.
dat1$ver <- 'v244'
dat2$ver <- 'v401'

# and combine into a new data frame.
dat <- rbind(dat1, dat2)

# Plot two overlaid histograms, one for each of the two dataframes
p1 = ggplot(dat, aes(bestfitness, fill = ver )) + 
   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
   scale_fill_manual(values=cbPalette, name='Version', labels=c('v 2.4.4','v 4.0.1')) +
   labs(x='fitness') + 
   ggtitle("Binary11 Multiplexer Task") +
   theme(plot.title = element_text(size = 12))

ggsave(filename="binary11-bestfitness-v244-v401-hist.png", plot=p1, height=4, width=8, units="in", dpi=100)

# Eval count.
p2 = ggplot(dat, aes(evalcount, fill = ver )) + 
   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
   scale_fill_manual(values=cbPaletteB, name='Version', labels=c('v 2.4.4','v 4.0.1')) +
   labs(x='evaluation count') + 
   ggtitle("Binary11 Multiplexer Task") +
   theme(plot.title = element_text(size = 12))

ggsave(filename="binary11-evalcount-v244-v401-hist.png", plot=p2, height=4, width=8, units="in", dpi=100)

# Mean complexity.
p2 = ggplot(dat, aes(meancomplexity, fill = ver )) + 
   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
   scale_fill_manual(values=cbPaletteC, name='Version', labels=c('v 2.4.4','v 4.0.1')) +
   labs(x='mean complexity') + 
   ggtitle("Binary11 Multiplexer Task") +
   theme(plot.title = element_text(size = 12))

ggsave(filename="binary11-meancomplexity-v244-v401-hist.png", plot=p2, height=4, width=8, units="in", dpi=100)

