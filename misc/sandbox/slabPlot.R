

## TODO:
# * store key attributes in slab() output
# * pre-compute most of the manually-specified parameters
# * lattice AND ggplot2 output
# * enhanced formular for upper / lower?

slabPlot <- function(x, fm, lower = NULL, upper = NULL, ylab = 'Depth (cm)', xlab = 'median bounded by 5th and 95th percentiles', alpha = 0.5, ...) {
  
  
  .lwr <- lower
  .upr <- upper
  .cf <- x[['contributing_fraction']]
  
  
  xyplot(
    fm, 
    data = x, 
    lower = .lwr, 
    upper = .upr, 
    cf = .cf,
    alpha = alpha,
    ylim = c(105,-5),
    panel = panel.depth_function,
    prepanel = prepanel.depth_function,
    ylab = ylab,
    xlab = xlab,
    ...
    
    
    
    
    
    # par.settings = tactile.theme(superpose.line = list(lwd = 2)),
    # strip = strip.custom(bg = grey(0.85))
  )
  
  
   
}
