#' Print method for objects of class "af4"
#'
#' Print method for objects of class "af4"
#'
#' @param x Object of class "af4".
#' @param digits Integer specifying the number of decimal places to display.
#' @param ... Other arguments (ignored).
#' @return No value is returned.
#' @seealso \code{\link{af4}}
#'
#' @examples
#' res <- af4(data = dat.sim,
#'            X_names = c("X1", "X2"), X_values = c(0, 1),
#'            Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)
#' print(res)
#'
#' @export

print.af4 <- function(x, digits = 4, ...){
  if (!inherits(x, "af4")){
    stop("Argument 'x' must be an object of class \"af4\".")
  }

  cat('AF4 METHOD FOR CONDITIONAL MEAN ESTIMATION\n')
  cat("==========================================\n\n")
  cat("Setting:\n")
  cat(sprintf("  %-24s %s\n", "Outcome variable type:", x$Y_type))
  cat(sprintf("  %-24s %s\n", "Auxiliary variable type:", x$W_type))
  X_temp <- paste(paste0(x$X_names, "=", x$X_values), collapse = ", ")
  cat(sprintf("  %-24s %s\n", "Predictor values:", X_temp))

  cat("\nResults:\n")
  cat(sprintf("  %-24s %s\n", "Mean estimate:",
              formatC(x$mean_est, digits = digits, format = "f")))
}
