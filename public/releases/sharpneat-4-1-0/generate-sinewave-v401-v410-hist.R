library("ggplot2")
setwd("D:/home/websites/sharpneat-web/public/releases/sharpneat-4-1-0")

# A colourblind friendly palette.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPaletteB <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#E69F00")
cbPaletteC <- c("#97d343", "#fed325")

# Load two data frames
dat1 <- read.table("sinewave-samples-v4_0_1.csv", sep=",", head=TRUE)
dat2 <- read.table("sinewave-samples-v4_1_0.csv", sep=",", head=TRUE)

# Combine the two dataframes into one.
# First create a new column in each.
dat1$ver <- 'v401'
dat2$ver <- 'v410'

# and combine into a new data frame.
dat <- rbind(dat1, dat2)

# Function to create and save a plot
createAndSavePlot <- function(data, aesMapping, fillPalette, fileName, xlab) {
  p <- ggplot(data, aes_string(aesMapping, fill = "ver")) + 
    geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
    scale_fill_manual(values=fillPalette, name='Version', labels=c('v 4.0.1','v 4.1.0')) +
    labs(x=xlab) + 
    ggtitle("Generative Sinewave Task") +
    theme(plot.title = element_text(size = 12))
  
  ggsave(filename=fileName, plot=p, height=4, width=8, units="in", dpi=100)
}

# Create and save plots
createAndSavePlot(dat, 'bestfitness', cbPalette, "sinewave-bestfitness-v401-v410-hist.png", 'fitness')
createAndSavePlot(dat, 'evalcount', cbPaletteB, "sinewave-evalcount-v401-v410-hist.png", 'evaluation count')
createAndSavePlot(dat, 'meancomplexity', cbPaletteC, "sinewave-meancomplexity-v401-v410-hist.png", 'mean complexity')
