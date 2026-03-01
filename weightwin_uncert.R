data(Offspring)
data(OffspringClimate)

# Test for climate windows between 365 and 0 days ago (range = c(365, 0))
# Fit a quadratic term for the mean weighted climate (func="quad")
# in a Poisson regression (offspring number ranges 0-3)
# Test a variable window (type = "absolute")
# Test at the resolution of days (cinterval="day")
# Uses a Weibull weight function (weightfunc="week")

weight <- weightwin(xvar = list(Temp = OffspringClimate$Temperature), 
                    cdate = OffspringClimate$Date, 
                    bdate = Offspring$Date, 
                    baseline = glm(Offspring ~ 1, family = poisson, data = Offspring), 
                    range = c(365, 0), func = "quad", 
                    type = "relative", weightfunc = "W", cinterval = "day", 
                    par = c(3, 0.2, 0), control = list(ndeps = c(0.01, 0.01, 0.01)), 
                    method = "L-BFGS-B", n = 20) 

## For all the weights, we can take the 10 values and take the mean and SE.
weight_mean <- c()
weight_SE <- c()
weight_min <- c()
weight_max <- c()

## Identify models that are within 2 AIC of the top model
mod_index <- which((weight$iterations$deltaAICc - min(weight$iterations$deltaAICc)) <= 2)

for(i in 1:length(weight[[1]]$Weights)){
  
  all_weights <- c()
  
  for(j in mod_index){
    
    all_weights <- append(all_weights, weight[[j]]$Weights[i])
    
  }
  
  weight_mean <- append(weight_mean, mean(all_weights))
  weight_SE <- append(weight_SE, sd(all_weights)/sqrt((length(weight) - 1)))
  weight_min <- append(weight_min, min(all_weights))
  weight_max <- append(weight_max, max(all_weights))
  
}

library(ggplot2)
ggplot() +
  geom_ribbon(aes(x = 1:366,
                  ymin = weight_min,
                  ymax = weight_max),
              fill = "grey", alpha = 1) +
  # geom_ribbon(aes(x = 1:366,
  #                 ymin = weight_mean - weight_SE,
  #                 ymax = weight_mean + weight_SE),
  #             fill = "grey", alpha = 0.75) +
  # geom_line(aes(x = 1:366, y = weight_mean))

