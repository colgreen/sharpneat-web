# Clear any existing data.
rm(datA)
rm(datB)
rm(dat)

# Init script parameters.
experimentCode <- 'binary11'
fnA <- 'logisticsteep'
fnB <- 'logisticapproxsteep'
filenameA <- paste(experimentCode, "-samples-", fnA, ".csv", sep="") 
filenameB <- paste(experimentCode, "-samples-", fnB, ".csv", sep="") 

fitnessImgFilename = paste(experimentCode, "-bestfitness-", fnA, "-", fnB ,".png", sep="") 
evalcountImgFilename = paste(experimentCode, "-evalcount-", fnA, "-", fnB ,".png", sep="") 

# Init ggplot and working dir.
library("ggplot2")
setwd("D:/home/websites/sharpneat-web/public/research/activation-fn-review")

# A colourblind friendly palette.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPaletteB <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#E69F00")

# Load two data frames
datA <- read.table(filenameA, sep=",", head=TRUE)
datB <- read.table(filenameB, sep=",", head=TRUE)

# Combine the two dataframes into one.
# First create a new column in each.
datA$ver <- rep(fnA, nrow(datA))
datB$ver <- rep(fnB, nrow(datB))

# and combine into a new data frame.
dat <- rbind(datA, datB)

# R/ggplot will order labelled groups alphabetically by name; here we override that
# behaviour by stating an explicit order.
dat$ver <- factor(dat$ver, levels=c(fnA,fnB))

# Plot two overlaid (fitness) histograms, one for each of the two dataframes
p1 = ggplot(dat, aes(bestfitness, fill = ver )) + 
   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
   scale_fill_manual(values=cbPalette) +
   labs(x='fitness', fill='') + 
   theme(plot.title = element_text(size = 12), 
         legend.position="top",
         legend.direction="horizontal")

p1
ggsave(filename=fitnessImgFilename, plot=p1, height=4, width=4, units="in", dpi=100)

# Plot two more overlaid histograms, comparing eval counts.
p2 = ggplot(dat, aes(evalcount, fill = ver )) + 
   geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') +
   scale_fill_manual(values=cbPaletteB) +
   labs(x='evaluation count', fill='') + 
   theme(plot.title = element_text(size = 12), 
         legend.position="top",
         legend.direction="horizontal")

p2
ggsave(filename=evalcountImgFilename , plot=p2, height=4, width=4, units="in", dpi=100)

