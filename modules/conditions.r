# Conditions
get_conditions_matrix <- function() {

  IV1 <- c(200, 400)      # n_obs    # Sample size
  IV2 <- c("a")           # m_type   # Model type
  IV3 <- c(8)             # n_item   # Items per factor
  IV4 <- c(.0)            # w_lambda # Factor loading
  IV5 <- c(.0)            # r_factor # Factor correlation
  IV6 <- c("uniform", "mixed") # t_type   # Threshold type
  IVs1 <- expand.grid(IV1, IV2, IV3, IV4, IV5, IV6, stringsAsFactors = FALSE)

  IV1 <- c(200, 400)      # n_obs    # Sample size
  IV2 <- c("b", "c")      # m_type   # Model type
  IV3 <- c(8)             # n_item   # Items per factor
  IV4 <- c(.5, .7)        # w_lambda # Factor loading
  IV5 <- c(.0)            # r_factor # Factor correlation
  IV6 <- c("uniform", "mixed") # t_type   # Threshold type
  IVs2 <- expand.grid(IV1, IV2, IV3, IV4, IV5, IV6, stringsAsFactors = FALSE)

  IV1 <- c(200, 400)      # n_obs    # Sample size
  IV2 <- c("d")           # m_type   # Model type
  IV3 <- c(4)             # n_item   # Items per factor
  IV4 <- c(.5, .7)        # w_lambda # Factor loading
  IV5 <- c(.0, .5, .8)    # r_factor # Factor correlation
  IV6 <- c("uniform", "mixed") # t_type   # Threshold type
  IVs3 <- expand.grid(IV1, IV2, IV3, IV4, IV5, IV6, stringsAsFactors = FALSE)

  IV1 <- c(200, 400)      # n_obs    # Sample size
  IV2 <- c("e")           # m_type   # Model type
  IV3 <- c(4)             # n_item   # Items per factor
  IV4 <- c(.5, .7)        # w_lambda # Factor loading
  IV5 <- c(.0)            # r_factor # Factor correlation
  IV6 <- c("uniform", "mixed") # t_type   # Threshold type
  IVs4 <- expand.grid(IV1, IV2, IV3, IV4, IV5, IV6, stringsAsFactors = FALSE)

  IVs <- rbind(IVs1, IVs2, IVs3, IVs4)
  colnames(IVs) <- get_IV_names()

  conditions <- IVs

  return(conditions)

}

# Constants
const_n_conditions <- function() {
  n_conditions <- dim(get_conditions_matrix())[1]
  return(n_conditions)
}
const_n_trials     <- function() {
  return(1000)
}
