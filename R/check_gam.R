#' Plot residuals of a fitted generalised additive model.
#'
#' This function replicates plot.lm() functionality with ggplot
#'
#' @param x a fitted GAM object that has broom tidiers
#' @param ... not used at present
#'
#' @return NULL called for its side effect, making a plot
#' @export
#'
#' @examples
#' x <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
#' check_gam(x)
#' 
check_gam<- function(x, ...){
  if (!inherits(x,"gam")){
    stop("check_gam only works with GAM objects.")
  }
  
  my_theme <- ggplot2::theme_classic() +
    ggplot2::theme(text=ggplot2::element_text(size=ggplot2::rel(4)))
  
  # get the residuals etc.
  rr <- augment_gam(x)
  ## plot 0 resid vs. fitted
  rr0 <- ggplot2::ggplot(rr, ggplot2::aes(x = .fitted, y = .std.resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method="loess", formula = y~x) +
    ggplot2::geom_hline(yintercept = 0, linetype=2) +
    my_theme +
    ggplot2::labs(x = "Fitted values", y = "Standardised residuals")
  ## plot 1 qq plot
  # get int and slope for qqline
  probs <- c(0.25,0.75)
  y <- stats::quantile(rr$.std.resid, probs, names = FALSE, na.rm = TRUE)
  x <- stats::qnorm(probs)
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  rr1 <- ggplot2::ggplot(rr, ggplot2::aes(sample=.std.resid)) +
    ggplot2::geom_abline(intercept = int, slope = slope, linetype = 2, size = 2, col = "red") +
    ggplot2::geom_qq(size=ggplot2::rel(4)) +
    my_theme + 
    ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles")
  
  ## plot 2 scale location plot
  rr2 <- ggplot2::ggplot(rr, ggplot2::aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method="loess", formula = y~x) +
    ggplot2::geom_hline(yintercept  = 0.822179) +
    my_theme + 
    ggplot2::labs(x = "Fitted values", y = expression(sqrt("Standardised residuals")))
  
  ## plot 3 Histograms of residuals. 
  rr3 <- ggplot2::ggplot(rr, ggplot2::aes(.resid)) +
    ggplot2::geom_histogram(col = "black", fill = "grey50") + 
    my_theme + 
    ggplot2::labs(x = "Residuals", y = "Count")
  
  ## plot 4 Cook's D. 
  rr4 <- ggplot2::ggplot(rr, ggplot2::aes(1:nrow(rr), .cooksd)) + 
    ggplot2::geom_col(col = "black", fill = "grey50") + 
    ggplot2::geom_hline(yintercept = 1, col = "red") + 
    my_theme + 
    ggplot2::labs(x = "Index", y = "Cook's Distance")
    
  ggpubr::ggarrange(plotlist = list(rr0, rr1, rr2, rr3, rr4), ncol = 2, nrow = 3)
  
  
}

