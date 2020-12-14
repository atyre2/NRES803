#' Augment a gam model with regression diagnostic information
#'
#' @param model fitted model object from mgcv::gam()
#'
#' @return a dataframe with the input data, fitted values, and regression diagnostics
#' @export
#'
#' @examples
#' x <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
#' augment(x)
#' 
#' dat <- mgcv::gamSim(1,n=400,dist="binary",scale=.33)
#' lr.fit <- mgcv::gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=binomial,
#'               data=dat,method="REML")
#'
augment.gam <- function(model){
  r <- stats::model.frame(model)
  r$.fitted <- stats::fitted(model)
  r$.resid <- mgcv::residuals.gam(model)
  r$.std.resid <- mgcv::residuals.gam(model, type = "pearson")
  if(stats::family(model)$family %in% c("binomial", "poisson", "gamma")) {
    r$.rq.resid <- statmod::qresiduals(model)
  }
  r$.hat <- model$hat
  r$.cooksd <- stats::cooks.distance(model)
  return(r)
}
