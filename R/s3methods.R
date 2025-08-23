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
#'            X_names = c("X1", "X2"),
#'            X_values_1 = c(0, 1), X_values_2 = c(0, 0),
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


  cat("\nResults:\n")
  X_temp <- paste(paste0(x$X_names, "=", x$X_values_1), collapse = ", ")
  cat(sprintf("  %-24s %s\n", "Predictor values:", X_temp))
  cat(sprintf("  %-24s %s\n", "Mean estimate:",
              formatC(x$mean_est_1, digits = digits, format = "f")))

  if (!is.null(x$X_values_2)){
    X_temp_2 <- paste(paste0(x$X_names, "=", x$X_values_2), collapse = ", ")
    cat(sprintf("\n  %-24s %s\n", "Predictor values:", X_temp_2))
    cat(sprintf("  %-24s %s\n", "Mean estimate:",
                formatC(x$mean_est_2, digits = digits, format = "f")))

    if (!is.na(x$contrast_est)){
      cat(sprintf("\n  %-24s %s\n", paste0('Mean ', x$contrast_type, ' estimate:'),
                  formatC(x$contrast_est, digits = digits, format = "f")))
    }
  }
}
