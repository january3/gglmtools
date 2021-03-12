

#' Creates new data and prediction/confidence intervals
#' @param mod linear regression model
#' @param steps how many new data points
#' @export
make_intervals <- function(mod, steps=100) {

  predictor <- setdiff(names(mod$coefficients), "(Intercept)")[1]
  pred_range <- range(mod$model[[predictor]])
  new_df <- data.frame(seq(pred_range[1], pred_range[2], length.out=steps))
  colnames(new_df) <- predictor

  pred <- predict(mod, newdata=new_df, interval = "pred")
  conf <- predict(mod, newdata=new_df, interval = "conf")
  colnames(pred) <- paste0("pred.", colnames(pred))
  colnames(conf) <- paste0("conf.", colnames(conf))

  new_df <- cbind(new_df, pred, conf)

  return(new_df)
}

#' Plot linear regression model
#'
#' Plot linear regression model with confidence and prediction intervals
#' @param mod linear regression model.
#' @importFrom ggplot2 ggplot aes geom_point geom_line
#' @export
ggplot_lm <- function(mod) {

  new_df  <- make_intervals(mod)
  df      <- mod$model
  predvar <- colnames(new_df)[1]
  depvar  <- colnames(df)[1]

  ggplot(new_df, aes(x=.data[[predvar]], y=.data$conf.fit)) + geom_line() +
    geom_line(aes(y=.data$conf.lwr), linetype="twodash") +
    geom_line(aes(y=.data$conf.upr), linetype="twodash") +
    geom_line(aes(y=.data$pred.lwr), linetype="dotted") +
    geom_line(aes(y=.data$pred.upr), linetype="dotted") +
    geom_point(data=df, mapping=aes(x=df[[predvar]], y=df[[depvar]]))

}

