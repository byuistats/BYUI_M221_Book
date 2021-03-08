#' @title Quantile Line for QQ-Plots
#' @description Adds a fit ab line using the output from an object created with stat_qq
#' @param x is a ggplot2 object
#' @export
ggQQline = function(x,...){
  
  x_data = ggplot_build(x)$data[[1]]
  x+ geom_smooth(data=x_data,aes(x=x,y=y),method="lm",lwd=.5,colour="darkgrey",se=FALSE)+geom_point(data=x_data,aes(x=x,y=y),...)
}

