#' Generate a sample dataset
#'
#' @param arg_n_obs the number of observations.
#' @param arg_n_factors the number of factors.
#' @param arg_r_factors the factor correlation between factors.
#' @param arg_lambda the factor loading.
#' @param arg_n_items the number of items per factor.
#'
#' @export
simulate_sample <- function(arg_n_obs, arg_m_type, arg_n_items, arg_r_factors, arg_lambda, arg_t_type) {

  if (arg_m_type == "a") {
    population_cor <- diag(1, arg_n_items)
  }
  if (arg_m_type == "b") {
    lambda_vec     <- rep(arg_lambda, arg_n_items)
    lambda_matrix  <- kronecker(diag(1, 1), lambda_vec)
    population_cor <- lambda_matrix %*% t(lambda_matrix)
    diag(population_cor) <- 1
  }
  if (arg_m_type == "c") {
    lambda_vec     <- rep(arg_lambda, arg_n_items)
    lambda_matrix  <- kronecker(diag(1, 1), lambda_vec)
    lambda_matrix[1:4] <- 0
    population_cor <- lambda_matrix %*% t(lambda_matrix)
    diag(population_cor) <- 1
  }
  if (arg_m_type == "d") {
    phi <- matrix(0, 2, 2)
    phi[lower.tri(phi)] <- arg_r_factors
    phi <- phi + t(phi)
    diag(phi) <- 1
    lambda_vec     <- rep(arg_lambda, arg_n_items)
    lambda_matrix  <- kronecker(diag(1, 2), lambda_vec)
    population_cor <- lambda_matrix %*% phi %*% t(lambda_matrix)
    diag(population_cor) <- 1
  }
  if (arg_m_type == "e") {
    lambda_vec     <- rep(0.5, arg_n_items)
    lambda_matrix  <- kronecker(diag(1, 2), lambda_vec)
    lambda_matrix[, 1] <- arg_lambda
    population_cor <- lambda_matrix %*% t(lambda_matrix)
    diag(population_cor) <- 1
  }

  sample_data <- rmvn(
    n = arg_n_obs,
    mu = rep(0, dim(population_cor)[1]),
    sigma = population_cor
  )

  if (arg_t_type == "uniform") {
    tau <- matrix(0, 8, 1)
  }
  if (arg_t_type == "mixed") {
    tau <- matrix(rep(c(-.5, .5), 4), 8, 1)
  }

  sample_data_int <- tau_mesh(sample_data, tau)
  return(sample_data_int)

}
