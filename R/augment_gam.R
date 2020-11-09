#' Augment GAM models akin to broom::augments. .
#'
#' This function replicates broom::augment() for GAMs. 
#'
#' @param x a fitted GAM object. 
#' @param ... not used at present
#'
#' @return NULL called for its side effect, making a plot
#' @export
#'
#' @examples
#' x <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
#' augment_gam(x)
#'
augment_gam <- function(x){
  
  aug <- model.frame(x)
  aug$.fitted <- x[["fitted.values"]]
  aug$.resid <- x[["residuals"]]
  aug$.std.resid <- aug$.resid/sd(aug$.resid)
  #aug.$hat <- hat(x) # What to do here???
  aug$.sigma <- sigma(x)
  aug$.cooksd <- cooks.distance(x)
  
   return(aug)
  
}  
