#' af4
#'
#' This function is a simple implementation of AF4 to estimate
#' \deqn{
#' E_{AF4} \big[ Y \mid X=x \big] = \int_{W} E \big[ Y \mid X=x, W, M=1 \big] \; p( W \mid X=x, R_W = R_X = 1 ) dW
#' }
#' This function considers the setting of a univariate W and Y, where Monte Carlo integration is used to compute the integral.
#'
#' @param data Data frame containing the observed data
#' @param X_names Vector of character strings specifying the value of the predictor variable(s) X
#' @param X_values Numeric scalar specifying the value of the predictor variable(s) X
#' @param Y_model Model statement for the outcome model
#' @param W_model Model statement for the auxiliary variable
#' @param W_type  (Optional) Vector of character strings specifying the "type" of each auxiliary variable. Options include \code{"binary"}, \code{"categorical"}, and \code{"normal"}.
#' @param Y_type (Optional) Character string specifying the "type" of outcome variable. Options include \code{"binary"} and \code{"continuous"}.
#' @param n_mc Integer specifying the number of Monte Carlo samples to use
#'
#' @return A list with the following elements:
#' \item{mean_est}{conditional outcome mean esitmate}
#' \item{fit_W}{fitted model for W}
#' \item{fit_Y}{fitted model for Y}
#'
#' @references
#' Mathur M, Zhang W, McGrath S, Seaman S, Shpitser I. (In preparation). \emph{Estimating conditional means under missingness-not-at-random with incomplete auxiliary variables}.
#'
#' @examples
#'
#' af4(data = dat.sim,
#'     X_names = c("X1", "X2"), X_values = c(0, 1),
#'     Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)
#'
#'
#' @export

af4 <- function(data, X_names, X_values,
                Y_model, Y_type,
                W_model, W_type, n_mc = 10000) {

  # Checking that data has the correct column names
  for (i in 1:length(X_names)){
    if (!X_names[i] %in% colnames(data)){
      stop(paste0("The observed data must include a column called ", X_names[i]))
    }
  }
  if (!'W' %in% colnames(data)){
    stop("The observed data must include a column called 'W' indicating the auxiliary variable.")
  }
  if (!'Y' %in% colnames(data)){
    stop("The observed data must include a column called 'Y' indicating the outcome variable.")
  }

  # Checking variable types are appropriately set
  if (!missing(W_type)){
    if (!W_type %in% c('binary', 'categorical', 'normal')){
      stop("W_type must be set to either 'binary', 'categorical', or 'normal'.")
    }
    if (W_type == 'categorical'){
      if (length(unique(stats::na.omit(data$W))) == 2){
        stop("W_type should be set to 'binary' when W only has two levels.")
      } else if (!is.factor(data$W)){
        stop("Auxiliary variables with type 'categorical' need to be encoded as factors in data.")
      }
    }
  }
  if (!missing(Y_type)){
    if (!Y_type %in% c('binary', 'continuous')){
      stop("Y_type must be set to either 'binary' or 'continuous'.")
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
  if (W_type == 'binary'){
    fit_W <- stats::glm(W_model, data = data_fit_W, family = stats::binomial)
  } else if (W_type == 'categorical'){
    fit_W <- nnet::multinom(W_model, data = data_fit_W, trace = FALSE)
  } else if (W_type == 'normal'){
    fit_W <- stats::lm(W_model, data = data_fit_W)
  }

  # Simulation of W
  df_X <- data.frame(matrix(X_values, nrow = 1))
  colnames(df_X) <- X_names
  W_sim <- sim_W(df = df_X, fit = fit_W, type = W_type, n_mc = n_mc,
                 levels = levels(data_fit_W$W))

  # Fitting Y model
  if (missing(Y_type)){
    Y_levels <- unique(stats::na.omit(data$Y))
    if (length(Y_levels) == 2){
      Y_type <- 'binary'
    } else if (is.numeric(data$Y)){
      Y_type <- 'continuous'
    }
  }
  df_XW <- cbind(df_X, data.frame(W = W_sim))
  if (Y_type == 'binary'){
    fit_Y <- stats::glm(Y_model, data = data_fit_Y, family = stats::binomial)
    Y_mean <- stats::predict(fit_Y, type = 'response', newdata = df_XW)
  } else if (Y_type == 'continuous'){
    fit_Y <- stats::lm(Y_model, data = data_fit_Y)
    Y_mean <- stats::predict(fit_Y, newdata = df_XW)
  }

  out <- list(
    mean_est = mean(Y_mean),
    fit_Y = fit_Y, fit_W = fit_W
  )
  return(out)
}

sim_W <- function(df, fit, type, n_mc, levels){
  if (type == 'binary'){
    prob_W <- stats::predict(fit, type = 'response', newdata = df)
    W_sim <- stats::rbinom(n = n_mc, size = 1, prob = prob_W)
  } else if (type == 'categorical') {
    prob_W <- stats::predict(fit, type = 'probs', newdata = df)
    W_sim <- sample(names(prob_W), size = n_mc, replace = TRUE, prob = prob_W)
    W_sim <- as.factor(W_sim)
  } else if (type == 'normal'){
    mean_W <- stats::predict(fit, newdata = df)
    sd_W <- summary(fit)$sigma
    W_sim <- stats::rnorm(n = n_mc, mean = mean_W, sd = sd_W)
  }
  return(W_sim)
}
