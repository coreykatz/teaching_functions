###Functions that use ggplot to plot pdfs of common continuous distributions 
### and calcualte probabilities of regions. 
###The plots show the shaded area as the probability that is calculated. 
library(tidyverse)

plot_normal <- function(mu=0,sd=1,lower=-Inf,upper=Inf,xaxis = "X"){
  
  # These define the three possible probability statements
  if (lower == -Inf) {
    prob <- pnorm(upper,mean = mu, sd = sd)#calculates the probability for (-inf,upper)
    prob_string <- paste("P(X<",upper,")=",round(prob,digits = 4),sep = "")#creates string for plotting
  } else if (upper == Inf) {
    prob <- pnorm(lower,mean = mu, sd = sd, lower.tail = FALSE)#calculates the probability for (lower,Inf)
    prob_string <- paste("P(X>",lower,")=",round(prob,digits = 4),sep = "")#creates string for plotting
  } else {
    prob <- pnorm(upper,mean = mu,sd=sd) - pnorm(lower,mean = mu,sd=sd)#calculates the probability between (lower,upper)
    prob_string <- paste("P(",lower,"<X<",upper,")=",round(prob,digits = 4),sep = "")#creates string for plotting
  }
  
  x <- seq(mu - 4*sd,mu + 4*sd,length.out = 100)#creates a vector to evaluate the pdf
  dat <- data.frame(x= x, f_x = dnorm(x,mu,sd),bot = 0)#creates a data frame of x,pdf, and 0 for plotting 
  
  
  #creates the plot
  ggplot(dat,aes(x=x,y=f_x)) + 
    geom_line() +
    geom_ribbon(data=subset(dat, lower <= x & x <= upper),
                aes(ymin=bot,ymax=f_x), 
                fill="blue", 
                alpha="0.5")+
    scale_y_continuous(name = "Density",
                       limits = c(0,max(dat$f_x)))+
    scale_x_continuous(name = xaxis,
                       limits = c(mu - 4*sd,mu + 4*sd),
                       breaks = seq(mu - 4*sd,mu + 4*sd,sd))+
    geom_text(x = mu+2*sd, 
              y = max(dat$f_x)-max(dat$f_x)/10,
              label = prob_string)+
    ggtitle(paste("Distribution of", xaxis))+
    theme_classic()
}

#Example
plot_normal(1815,sd=40,1750,1790)


plot_exp <- function(lambda = 1,lower=0,upper=Inf,xaxis = "X"){
  # These define the three possible probability statements
  if (lower == 0) {
    prob <- pexp(upper,rate=lambda)#calculates the probability for (0,upper)
    prob_string <- paste("P(X<",upper,")=",round(prob,digits = 4),sep = "")#creates string for plotting
  } else if (upper == Inf) {
    prob <- pexp(lower,rate = lambda, lower.tail = FALSE)#calculates the probability for (lower,Inf)
    prob_string <- paste("P(X>",lower,")=",round(prob,digits = 4),sep = "")#creates string for plotting
  } else {
    prob <- pexp(upper,rate = lambda) - pexp(lower,rate = lambda)#calculates the probability between (lower,upper)
    prob_string <- paste("P(",lower,"<X<",upper,")=",round(prob,digits = 4),sep = "")#creates string for plotting
  }
  mu <- 1/lambda#computes mean of exp. distribution
  sd <- sqrt(1/(lambda^2))#computes sd of exp. distribution
  x <- seq(0,mu + 5*sd,length.out = 100)#creates a vector to evaluate the pdf
  dat <- data.frame(x= x, f_x = dexp(x,rate = lambda),bot = 0)#creates a data frame of x,pdf, and 0 for plotting
  
  
  #creates the plot
  ggplot(dat,aes(x=x,y=f_x)) + 
    geom_line() +
    geom_ribbon(data=subset(dat, lower <= x & x <= upper),
                aes(ymin=bot,ymax=f_x), 
                fill="blue", 
                alpha="0.5")+
    scale_y_continuous(name = "Density",
                       limits = c(0,max(dat$f_x)))+
    scale_x_continuous(name = xaxis,
                       breaks = seq(0,mu + 5*sd,sd))+
    geom_text(x = mu+2*sd, 
              y = max(dat$f_x)-max(dat$f_x)/10,
              label = prob_string)+
    ggtitle(paste("Distribution of", xaxis))+
    theme_classic()
}

#Example
plot_exp(lambda = .5,lower = 3,upper = 10)



plot_unif <- function(a=0,b=1,lower=a,upper=b,xaxis = "X"){
  # These define the three possible probability statements
  if (lower > upper | a > b){
    return("Error: Check Bounds.")
  }
  if (lower == a) {
    prob <- punif(upper,min = a,max = b)#calculates the probability for (a,upper)
    prob_string <- paste("P(X<",upper,")=",round(prob,digits = 4),sep = "")#creates string for plotting
  } else if (upper == b) {
    prob <- punif(lower,min = a,max = b, lower.tail = FALSE)#calculates the probability for (lower,b)
    prob_string <- paste("P(X>",lower,")=",round(prob,digits = 4),sep = "")#creates string for plotting
  } else {
    prob <- punif(upper,min = a,max = b) - punif(lower,min = a,max = b)#calculates the probability for (lower,upper)
    prob_string <- paste("P(",lower,"<X<",upper,")=",round(prob,digits = 4),sep = "")#creates string for plotting
  }
  
  
  x <- seq(a,b,length.out = 100)#creates a vector to evaluate the pdf
  dat <- data.frame(x= x, f_x = dunif(x,min = a,max = b),bot = 0)#creates a data frame of x,pdf, and 0 for plotting
  
  
  #creates the plot
  ggplot(dat,aes(x=x,y=f_x)) + 
    geom_line() +
    geom_ribbon(data=subset(dat, lower <= x & x <= upper),
                aes(ymin=bot,ymax=f_x), 
                fill="blue", 
                alpha="0.5")+
    scale_y_continuous(name = "Density",
                       limits = c(0,max(dat$f_x)+max(dat$f_x)/10))+
    scale_x_continuous(name = xaxis,
                       breaks = seq(a,b,(a+b)/10))+
    geom_text(x = (lower+upper)/2, 
              y = max(dat$f_x)+max(dat$f_x)/10,
              label = prob_string)+
    ggtitle(paste("Distribution of", xaxis))+
    theme_classic()
}

#Example
plot_unif(3,5,3.3,4.5)

