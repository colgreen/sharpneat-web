# Set working folder.
setwd("D:/home/websites/sharpneat-web/public/research/activation-fn-review")

# Define activation functions.
f1 = function(x) { 1 / (exp(-4.9*x)+1) }   		# logistic-steep
f2 = function(x) { 0.5 + (x / (2.0*(0.2+abs(x)))) }	# softsign-steep

# polyapprox-steep
f3 = function(x) {	
  x = x * 4.9
  x2 = x * x
  e = 1.0 + abs(x) + (x2 * 0.555) + (x2 * x2 * 0.143)
  f = (x > 0) * (1.0 / e) + (x <= 0) * e 
  1.0 / (1.0 + f)
}

# quadratic
f4 = function(x) {

  t = 0.999
  a = 0.00001

  s = sign(x)
  x = abs(x)

  y = ((x >=0) & (x < t)) * (t - ((x - t) * (x - t))) +
      ((x >=t) * (t + (x - t) * a))

  (y * s * 0.5) + 0.5
}


# leakyrelu
f5 = function(x) {

  a = 0.001

  y = ((x > 0) * x) +
	 ((x <=0) * x * a)
  y
}

# leakyrelu-shifted
f6 = function(x) {

  a = 0.001

  x = x + 0.5

  y = ((x > 0) * x) +
	 ((x <=0) * x * a)
  y
}

# srelu
f7 = function(x) {

  tl = 0.001
  tr = 0.999
  a = 0.00001

  y = (((x > tl) & (x < tr)) * x) +
      (x <= tl) * (tl + (x - tl) * a) +
      (x >= tr) * (tr + (x - tr) * a)
  y
}

# srelu-shifted
f8 = function(x) {

  tl = 0.001
  tr = 0.999
  a = 0.00001

  x = x + 0.5

  y = (((x > tl) & (x < tr)) * x) +
      (x <= tl) * (tl + (x - tl) * a) +
      (x >= tr) * (tr + (x - tr) * a)
  y
}

# import ggplot.
library("ggplot2")

# generate plot and save png
p1 = ggplot(data.frame(x = c(-2, 2)), aes(x)) + 
   stat_function(fun = f1, aes(colour="logistic-steep")) + 
   stat_function(fun = f2, aes(colour="softsign-steep")) + 
   xlab("x") + 
   ylab("y") + 
   theme(legend.position="top",
         legend.direction="horizontal",
         legend.title=element_blank())

ggsave(filename="actfn-logisticsteep-softsignsteep.png", plot=p1, height=4, width=4, units="in", dpi=100)

# generate plot and save png
p1 = ggplot(data.frame(x = c(-2, 2)), aes(x)) + 
   stat_function(fun = f1, aes(colour="logistic-steep")) + 
   stat_function(fun = f3, aes(colour="polyapprox-steep")) + 
   xlab("x") + 
   ylab("y") + 
   theme(legend.position="top",
         legend.direction="horizontal",
         legend.title=element_blank())

ggsave(filename="actfn-logisticsteep-polyapproxsteep.png", plot=p1, height=4, width=4, units="in", dpi=100)

# generate plot and save png
p1 = ggplot(data.frame(x = c(-2, 2)), aes(x)) + 
   stat_function(fun = f1, aes(colour="logistic-steep")) + 
   stat_function(fun = f4, aes(colour="quadratic")) + 
   xlab("x") + 
   ylab("y") + 
   theme(legend.position="top",
         legend.direction="horizontal",
         legend.title=element_blank())

ggsave(filename="actfn-logisticsteep-quadratic.png", plot=p1, height=4, width=4, units="in", dpi=100)


# generate plot and save png
p1 = ggplot(data.frame(x = c(-2, 2)), aes(x)) + 
   stat_function(fun = f1, aes(colour="logistic-steep")) + 
   stat_function(fun = f5, aes(colour="leakyrelu")) + 
   xlab("x") + 
   ylab("y") + 
   theme(legend.position="top",
         legend.direction="horizontal",
         legend.title=element_blank()) + 
   guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
   scale_colour_manual(values = c("leakyrelu" = "#00BFC4", "logistic-steep" = "#F8766D"))

ggsave(filename="actfn-logisticsteep-leakyrelu.png", plot=p1, height=4, width=4, units="in", dpi=100)


# generate plot and save png
p1 = ggplot(data.frame(x = c(-2, 2)), aes(x)) + 
   stat_function(fun = f1, aes(colour="logistic-steep")) + 
   stat_function(fun = f6, aes(colour="leakyrelu-shifted")) + 
   xlab("x") + 
   ylab("y") + 
   theme(legend.position="top",
         legend.direction="horizontal",
         legend.title=element_blank()) + 
   guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
   scale_colour_manual(values = c("leakyrelu-shifted" = "#00BFC4", "logistic-steep" = "#F8766D"))

ggsave(filename="actfn-logisticsteep-leakyrelu-shifted.png", plot=p1, height=4, width=4, units="in", dpi=100)


# generate plot and save png
p1 = ggplot(data.frame(x = c(-2, 2)), aes(x)) + 
   stat_function(fun = f1, aes(colour="logistic-steep")) + 
   stat_function(fun = f7, aes(colour="srelu")) + 
   xlab("x") + 
   ylab("y") + 
   theme(legend.position="top",
         legend.direction="horizontal",
         legend.title=element_blank())

ggsave(filename="actfn-logisticsteep-srelu.png", plot=p1, height=4, width=4, units="in", dpi=100)


# generate plot and save png
p1 = ggplot(data.frame(x = c(-2, 2)), aes(x)) + 
   stat_function(fun = f1, aes(colour="logistic-steep")) + 
   stat_function(fun = f8, aes(colour="srelu-shifted")) + 
   xlab("x") + 
   ylab("y") + 
   theme(legend.position="top",
         legend.direction="horizontal",
         legend.title=element_blank())

ggsave(filename="actfn-logisticsteep-srelushifted.png", plot=p1, height=4, width=4, units="in", dpi=100)


# Generate plot for logistic-approx-steep (from data output from a C# app).
setwd("D:/home/websites/sharpneat-web/public/research/activation-fn-review")
fndata <- read.table("logistic-approx-steep.csv", sep=",", head=TRUE)

p1 = ggplot(data = fndata, aes(x=x, y=y, color="logistic-approx-steep")) + 
   stat_function(fun = f1, aes(colour="logistic-steep")) + 
   geom_line() +    
   xlab("x") + 
   ylab("y") + 
   theme(legend.position="top",
         legend.direction="horizontal",
         legend.title=element_blank()) + 
   guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
   scale_colour_manual(values = c("logistic-approx-steep" = "#00BFC4", "logistic-steep" = "#F8766D"))

ggsave(filename="actfn-logisticsteep-logisticapproxsteep.png", plot=p1, height=4, width=4, units="in", dpi=100)
