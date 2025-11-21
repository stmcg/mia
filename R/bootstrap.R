#' Bootstrap-based confidence intervals for AF4
#'
#' This function applies nonparametric bootstrap to construct confidence intervals around the conditional mean estimates obtained by \code{\link{af4}}. This function is a wrapper for the \code{\link[boot]{boot}} and \code{\link[boot]{boot.ci}} functions from the \pkg{boot} package.
#'
#' @param af4_res Output from the \code{af4} function.
#' @param n_boot Numeric scalar specifying the number of bootstrap replicates to use
#' @param type Character string specifying the type of confidence interval. The options are \code{"norm"}, \code{"basic"}, \code{"perc"}, and \code{"bca"}.
#' @param conf Numeric scalar specifying the level of the confidence interval. The default is \code{0.95}.
#' @param boot_args A list of additional arguments to pass to the \code{\link[boot]{boot}} function. Note that this includes parallelization options.
#' @param boot.ci_args A list of additional arguments to pass to the \code{\link[boot]{boot.ci}} function
#' @param show_progress Logical scalar indicating whether to show a progress bar during bootstrap. Default is \code{TRUE}. The progress bar will not be displayed when parallelization is used.
#'
#' @return An object of class "af4_ci". This object is a list with the following elements:
#' \item{ci_1}{An object of class "boot.ci" which contains the output of the \code{\link[boot]{boot.ci}} function applied for the confidence interval around the mean under \code{X_values_1} in  \code{\link{af4}}.}
#' \item{ci_2}{An object of class "boot.ci" which contains the output of the \code{\link[boot]{boot.ci}} function applied for the confidence interval around the mean under \code{X_values_2} in  \code{\link{af4}} (if applicable).}
#' \item{ci_contrast}{An object of class "boot.ci" which contains the output of the \code{\link[boot]{boot.ci}} function applied for the confidence interval around the contrast between mean under \code{X_values_1} versus \code{X_values_2} in \code{\link{af4}} (if applicable).}
#' \item{bres}{An object of class "boot" which contains the output of the \code{\link[boot]{boot}} function. Users can access the bootstrap replicates through the element \code{t} in this object.}
#' \item{...}{additional elements}
#'
#' @examples
#' set.seed(1234)
#' res <- af4(data = dat.sim,
#'            X_names = c("X1", "X2"),
#'            X_values_1 = c(0, 1), X_values_2 = c(0, 0),
#'            Y_model = Y ~ W + X1 + X2, W_model = W ~ X1 + X2)
#' res_ci <- get_CI(af4_res = res, n_boot = 50, type = 'perc')
#' res_ci
#'
#' ## Example with parallelization
#' res_par <- get_CI(res, n_boot = 100, type = 'perc',
#'                  boot_args = list(parallel = "snow", ncpus = 2))
#'
#'
#' @export
#'
get_CI <- function(af4_res, n_boot = 1000, type = 'bca', conf = 0.95,
                   boot_args = list(), boot.ci_args = list(), show_progress = TRUE) {

  # Error checking for misunderstandings about how arguments are passed into the boot and boot.ci functions
  if (length(type) > 1){
    stop("The argument 'type' must be of length 1.", call. = FALSE)
  }
  if (length(conf) > 1){
    stop("The argument 'conf' must be of length 1.", call. = FALSE)
  }
  if (!type %in% c('norm', 'basic', 'perc', 'bca')){
    stop("The argument 'type' must be set to either 'norm', 'basic', 'perc', or 'bca'.", call. = FALSE)
  }
  if ('R' %in% names(boot_args)){
    warning("The element 'R' in the argument 'boot_args' is not used. The number of bootstrap replicates is instead specified by the argument 'n_boot' in the get_CI function", call. = FALSE)
  }
  if ('type' %in% names(boot.ci_args)){
    warning("The element 'type' in the argument 'boot.ci_args' is not used. The type of confidence interval is instead specified by the argument 'type' in the get_CI function", call. = FALSE)
  }
  if ('conf' %in% names(boot.ci_args)){
    warning("The element 'conf' in the argument 'boot.ci_args' is not used. The level of confidence interval is instead specified by the argument 'conf' in the get_CI function", call. = FALSE)
  }

  # Create progress bar for bootstrap (if requested)
  parallel_enabled <- (!is.null(boot_args$parallel) &&
                       boot_args$parallel %in% c("multicore", "snow")) ||
                      !is.null(boot_args$cl)

  if (show_progress && n_boot > 1 && !parallel_enabled) {
    pb <- progress::progress_bar$new(
      format = "  Bootstrapping [:bar] :percent | Elapsed: :elapsed | Time Remaining: :eta",
      total = n_boot, clear = FALSE, width = 80, show_after = 0
    )
  } else {
    pb <- NULL
  }

  boot_func <- function(data, i){
    dat_boot    <- data[i, ]
    fit <- af4(data = dat_boot,
               X_names = af4_res$X_names,
               X_values_1 = af4_res$X_values_1,
               X_values_2 = af4_res$X_values_2,
               contrast_type = af4_res$contrast_type,
               Y_model = af4_res$Y_model,
               W_model = af4_res$W_model,
               Y_type = af4_res$Y_type, W_type = af4_res$W_type,
               n_mc = af4_res$n_mc)
    if (!is.null(af4_res$X_values_2)){
      if (af4_res$contrast_type == 'none'){
        out <- c(fit$mean_est_1, fit$mean_est_2)
      } else {
        out <- c(fit$mean_est_1, fit$mean_est_2, fit$contrast_est)
      }
    } else {
      out <- c(fit$mean_est_1)
    }

    # Update progress bar
    if (!is.null(pb) && !pb$finished) {
      pb$tick()
    }

    return(out)
  }

  boot_args$data <- af4_res$data
  boot_args$statistic <- boot_func
  boot_args$R <- n_boot
  bres <- do.call(boot::boot, boot_args)
  bres$call <- quote(do.call(boot::boot, boot_args))

  boot.ci_args$boot.out <- bres
  boot.ci_args$type <- type
  boot.ci_args$conf <- conf
  if (type == 'bca' &
      boot.ci_args$boot.out$R < nrow(boot.ci_args$boot.out$data) &
      !('L' %in% names(boot.ci_args))){
    # The default type "reg" fails in this setting. Changing to type = "jack"
    message(paste0("Using jackknife to estimate the acceleration parameter in the BCa interval because the number of bootstrap replicates is smaller than the sample size. This step can be computationally intensive. Consider increasing the number of bootstrap replicates to be larger than the sample size to avoid using jackknife."))
    boot.ci_args$L <- boot::empinf(boot.ci_args$boot.out, type = "jack")
  }
  ci_1 <- bca_safe_ci(boot.ci_args, index = 1)
  ci_2 <- NULL; ci_contrast <- NULL
  if (!is.null(af4_res$X_values_2)){
    ci_2 <- bca_safe_ci(boot.ci_args, index = 2)
    if (af4_res$contrast_type != 'none'){
      ci_contrast <- bca_safe_ci(boot.ci_args, index = 3)
    }
  }

  out <- list(ci_1 = ci_1, ci_2 = ci_2, ci_contrast = ci_contrast, bres = bres,
              n_boot = n_boot, type = type, conf = conf, af4_res = af4_res)
  class(out) <- 'af4_ci'

  return(out)

}

bca_safe_ci <- function(boot.ci_args, index){
  boot.ci_args$index <- index

  if (boot.ci_args$type == 'bca'){
    cis <- tryCatch({
      do.call(boot::boot.ci, boot.ci_args)
    },
    error = function(e) {
      if (index == 1){
        error_message_temp <- 'X_values_1'
      } else if (index == 2){
        error_message_temp <- 'X_values_2'
      } else if (index == 3){
        error_message_temp <- 'the contrast between X_values_1 and X_values_2'
      }
      message(paste0("Error in obtaining the adjusted bootstrap percentile (BCa) interval corresponding to ", error_message_temp, ". The percentile method will be used instead. Error message from the boot::boot.ci function: ", conditionMessage(e)))
      boot.ci_args$type <- 'perc'
      do.call(boot::boot.ci, boot.ci_args)
    })
  } else {
    cis <- do.call(boot::boot.ci, boot.ci_args)
  }
  cis$call <- quote(do.call(boot::boot.ci, boot.ci_args))
  return(cis)
}

