#' af4
#'
#' This function is a simple implementation of AF4 to estimate
#' \deqn{
#' E_{AF4} [ Y | X=x ] = \int_{W} E [ Y | X=x, W, M=1 ] p( W | X=x, R_W = R_X = 1 ) dW
#' }
#' The function supports estimating \eqn{E_{AF4} [ Y | X=x_1 ]} and, optionally, \eqn{E_{AF4} [ Y | X=x_2 ]} as well as contrasts between \eqn{E_{AF4} [ Y | X=x_1 ]} vs \eqn{E_{AF4} [ Y | X=x_2 ]} (differences, ratios). This function considers the setting of a univariate W and Y, where Monte Carlo integration is used to compute the integral.
#'
#' @param data Data frame containing the observed data
#' @param X_names Vector of character strings specifying the value of the predictor variable(s) X
#' @param X_values_1 Numeric vector specifying the value of the predictor variable(s) X, i.e. \eqn{x_1} in \eqn{E_{AF4} [ Y | X=x_1 ]}.
#' @param X_values_2 (Optional) Additional vector specifying the value of the predictor variable(s) X, i.e. \eqn{x_2} in \eqn{E_{AF4} [ Y | X=x_2 ]}.
#' @param contrast_type (Optional) Character string specifying the type of contrast to use when comparing \eqn{E_{AF4} [ Y | X=x_1 ]} and \eqn{E_{AF4} [ Y | X=x_2 ]}. Options include \code{"difference"}, \code{"ratio"}, and \code{"none"}.
#' @param Y_model Model statement for the outcome model
#' @param W_model Model statement for the auxiliary variable
#' @param W_type  (Optional) Vector of character strings specifying the "type" of each auxiliary variable. Options include \code{"binary"}, \code{"categorical"}, and \code{"normal"}.
#' @param Y_type (Optional) Character string specifying the "type" of outcome variable. Options include \code{"binary"} and \code{"continuous"}.
#' @param n_mc Integer specifying the number of Monte Carlo samples to use
#'
#' @return An object of class "af4". This object is a list with the following elements:
#' \item{mean_est_1}{conditional outcome mean estimate under \code{X_values_1}}
#' \item{mean_est_2}{conditional outcome mean estimate under \code{X_values_2}}
#' \item{contrast_est}{contrast of conditional outcome mean estimates between \code{X_values_1} and \code{X_values_2}}
#' \item{fit_W}{fitted model for W}
#' \item{fit_Y}{fitted model for Y}
#' \item{...}{additional elements}
#'
#' @references
#' Mathur M, Zhang W, McGrath S, Seaman S, Shpitser I. (In preparation). \emph{Estimating conditional means under missingness-not-at-random with incomplete auxiliary variables}.
#'
#' @examples
#'
#' af4(data = dat.sim,
#'     X_names = c("X1", "X2"),
#'     X_values_1 = c(0, 1), X_values_2 = c(0, 0),
#'     Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)
#'
#'
#' @export

af4 <- function(data, X_names, X_values_1, X_values_2,
                contrast_type,
                Y_model, Y_type,
                W_model, W_type, n_mc = 10000) {

  # Checking that data has the correct column names
  missing_cols <- setdiff(X_names, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("The following columns are listed in X_names but missing from the data:",
               paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  if (!'W' %in% colnames(data)){
    stop("The observed data must include a column called 'W' indicating the auxiliary variable.", call. = FALSE)
  }
  if (!'Y' %in% colnames(data)){
    stop("The observed data must include a column called 'Y' indicating the outcome variable.", call. = FALSE)
  }

  # Checking X_values_1 and X_values_2
  if (length(X_names) != length(X_values_1)){
    stop("The arguments 'X_names' and 'X_values_1' must be of the same length.", call. = FALSE)
  }
  if (!missing(X_values_2)){
    if (length(X_names) != length(X_values_2)){
      stop("The arguments 'X_names' and 'X_values_2' must be of the same length.", call. = FALSE)
    }
    if (missing(contrast_type)){
      contrast_type <- 'difference'
    }
  } else {
    X_values_2 <- NULL
    contrast_type <- NA
  }

  # Checking variable types are appropriately set
  if (!missing(W_type)){
    if (!W_type %in% c('binary', 'categorical', 'normal')){
      stop("W_type must be set to either 'binary', 'categorical', or 'normal'.", call. = FALSE)
    }
    if (W_type == 'categorical'){
      if (length(unique(stats::na.omit(data$W))) == 2){
        stop("W_type should be set to 'binary' when W only has two levels.", call. = FALSE)
      } else if (!is.factor(data$W)){
        stop("Auxiliary variables with type 'categorical' need to be encoded as factors in data.", call. = FALSE)
      }
    }
  }
  if (!missing(Y_type)){
    if (!Y_type %in% c('binary', 'continuous')){
      stop("Y_type must be set to either 'binary' or 'continuous'.", call. = FALSE)
    }
  }

  # Creating datasets for fitting models
  R_X <- apply(!is.na(data[, X_names]), 1, all)
  R_W <- !is.na(data$W)
  R_Y <- !is.na(data$Y)
  data_fit_Y <- data[R_X == 1 & R_W == 1 & R_Y == 1, ]
  data_fit_W <- data[R_X == 1 & R_W == 1, ]

  # Fitting W model
  if (missing(W_type)){
    W_levels <- unique(stats::na.omit(data$W))
    if (length(W_levels) == 2){
      W_type <- 'binary'
    } else if (is.factor(data$W)){
      W_type <- 'categorical'
    } else if (is.numeric(data$W)){
      W_type <- 'normal'
    }
  }
  fit_W <- safe_fit(variable_name = 'W', variable_type = W_type,
                    formula = W_model, data = data_fit_W)

  # Fitting Y model
  if (missing(Y_type)){
    Y_levels <- unique(stats::na.omit(data$Y))
    if (length(Y_levels) == 2){
      Y_type <- 'binary'
    } else if (is.numeric(data$Y)){
      Y_type <- 'continuous'
    }
  }
  fit_Y <- safe_fit(variable_name = 'Y', variable_type = Y_type,
                    formula = Y_model, data = data_fit_Y)

  # Simulation of W and Y (for X_values_1)
  df_X <- data.frame(matrix(X_values_1, nrow = 1))
  colnames(df_X) <- X_names
  W_sim <- sim_W(df = df_X, fit = fit_W, type = W_type, n_mc = n_mc,
                 levels = levels(data_fit_W$W))
  df_XW <- cbind(df_X, data.frame(W = W_sim))
  Y_mean <- get_Y_mean(df = df_XW, fit_Y = fit_Y, Y_type = Y_type)

  # Simulation of W and Y (for X_values_2)
  if (!is.null(X_values_2)){
    df_X_2 <- data.frame(matrix(X_values_2, nrow = 1))
    colnames(df_X_2) <- X_names
    W_sim_2 <- sim_W(df = df_X_2, fit = fit_W, type = W_type, n_mc = n_mc,
                     levels = levels(data_fit_W$W))
    df_XW_2 <- cbind(df_X_2, data.frame(W = W_sim_2))
    Y_mean_2 <- get_Y_mean(df = df_XW_2, fit_Y = fit_Y, Y_type = Y_type)
    if (contrast_type == 'difference'){
      contrast_est <- Y_mean - Y_mean_2
    } else if (contrast_type == 'ratio'){
      contrast_est <- Y_mean / Y_mean_2
    } else if (contrast_type != 'none'){
      contrast_est <- NA
    }
  } else {
    Y_mean_2 <- contrast_est <- NA
  }

  out <- list(
    mean_est_1 = Y_mean,
    mean_est_2 = Y_mean_2,
    contrast_est = contrast_est,
    fit_Y = fit_Y, fit_W = fit_W,
    W_type = W_type, Y_type = Y_type,
    X_names = X_names,
    X_values_1 = X_values_1, X_values_2 = X_values_2,
    contrast_type = contrast_type
  )
  class(out) <- 'af4'
  return(out)
}

sim_W <- function(df, fit, type, n_mc, levels){
  if (type == 'binary'){
    prob_W <- stats::predict(fit, type = 'response', newdata = df)
    W_sim <- stats::rbinom(n = n_mc, size = 1, prob = prob_W)
  } else if (type == 'categorical') {
    prob_W <- stats::predict(fit, type = 'probs', newdata = df)
    W_sim <- sample(levels, size = n_mc, replace = TRUE, prob = prob_W)
    W_sim <- as.factor(W_sim)
  } else if (type == 'normal'){
    mean_W <- stats::predict(fit, newdata = df)
    sd_W <- summary(fit)$sigma
    W_sim <- stats::rnorm(n = n_mc, mean = mean_W, sd = sd_W)
  }
  return(W_sim)
}

get_Y_mean <- function(df, fit_Y, Y_type){
  if (Y_type == 'binary'){
    Y_mean <- stats::predict(fit_Y, type = 'response', newdata = df)
  } else if (Y_type == 'continuous'){
    Y_mean <- stats::predict(fit_Y, newdata = df)
  }
  return(mean(Y_mean))
}

safe_fit <- function(variable_type, variable_name, formula, data) {
  fit <- tryCatch({
    if (variable_type == 'binary'){
      stats::glm(formula, data = data, family = stats::binomial())
    } else if (variable_type == 'categorical'){
      nnet::multinom(formula, data = data, trace = FALSE)
    } else if (variable_type %in% c('normal', 'continuous')){
      stats::lm(formula, data = data)
    }
  },
  error = function(e) {
    stop(paste0("Error in fitting the model for ", variable_name, ": ", conditionMessage(e)), call. = FALSE)
  }
  )
  return(fit)
}

