

a <- c(4,4,7,7,7)
b <- c(12,12,12,12,15)
c <- c(8,8,8,12,12)

## mean -- these are the same
wts <- c(2,3,4,1,3,2)
wt.mean.1 <- weighted.mean(c(4,7,12,15,8,12), wts)
wt.mean.2 <- mean(c(a,b,c))


## SD -- these are not the same !
V1 <- sum(wts) 
V2 <- sum(wts^2)

wt.sd.1 <- sqrt((V1 / (V1^2 - V2)) * sum( wts * (c(4,7,12,15,8,12) - wt.mean.1)^2 ))
wt.sd.2 <- sd(c(a,b,c))

library(quantreg)




## quantiles -- these are the same

# lumped data
 coef(rq(c(a,b,c) ~ 1, tau=c(0.05, 0.25, 0.5, 0.75, 0.95)))
            tau= 0.05 tau= 0.25 tau= 0.50 tau= 0.75 tau= 0.95
(Intercept)         4         7         8        12        15

# weighted
coef(rq(c(4,7,12,15,8,12) ~ 1, weights=wts, tau=c(0.05, 0.25, 0.5, 0.75, 0.95)))
            tau= 0.05 tau= 0.25 tau= 0.50 tau= 0.75 tau= 0.95
(Intercept)         4         7         8        12        15

