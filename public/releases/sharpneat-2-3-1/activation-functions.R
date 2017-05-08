
# See:
# http://stackoverflow.com/questions/10732027/fast-sigmoid-algorithm


# Define logistic fn and it's first derivative.
f1 = function(x) { 1 / (exp(-x)+1) }
f2 = function(x) {

   ((x >= -1) * (x < 0) * (x+1) * (x+1) * 0.5) +
   ((x >=0) * (x < 1) * (1-((x-1) * (x-1) * 0.5))) +
   ((x >= 1) * 1)
}

f3 = function(x) {
   abs = abs(x);
   abs2 = abs*abs;
   e = 1.0 + abs + abs2*0.555 + abs2*abs2*0.143

   z = ((x > 0) * (1 / e)) + (x <= 0) *  e

   1.0 / (1.0 + z)
}

# import ggplot.
library("ggplot2")

# generate plot
ggplot(data.frame(x = c(-10, 10)), aes(x)) + 
   stat_function(fun = f1, aes(colour="Steepened\nsigmoid\n")) + 
   stat_function(fun = f2, aes(colour="Steepened\nsigmoid\napprox.\n")) + 
   stat_function(fun = f3, aes(colour="f3")) + 
   ggtitle("Logistic function and Gradient") + 
   xlab("x") + 
   ylab("y") + 
   theme(legend.title=element_blank())

