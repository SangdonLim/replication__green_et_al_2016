# Replicate Green et al. (2016)
# Accuracy of revised and traditional parallel analyses
# for assessing dimensionality with binary data
# ------------------------------------------------------------------------------

# Load all modules
for (f in list.files("modules")) {
  source(file.path("modules", f))
}

library(mnormt)
library(psych)
library(mvnfast)
library(compiler)
library(progress)
library(parallel)
library(doParallel)

enableJIT(3)

# Prepare functions
patch_functions()

# Initialize parallel clusters
n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)
registerDoParallel(cl)
message(sprintf(
  "Using %s threads", n_cores
))
clusterExport(cl, c(
  "NNpatched", "VMpatched",
  "rmvn", "polychoric")
)

# Initialize task list
conditions <- get_conditions_matrix()
task_list <- expand.grid(
  idx_condition = 1:dim(conditions)[1],
  idx_trial     = 1:1000
)
n_tasks <- dim(task_list)[1]
tasks <- 1:n_tasks
tasks <- sample(tasks)

# Begin main loop --------------------------------------------------------------

results <- foreach(
  task = tasks, .combine = rbind
) %dopar% {

  PA_loops <- 100

  idx_condition <- task_list$idx_condition[task]
  idx_trial     <- task_list$idx_trial[task]
  fn <- sprintf("results/%s_%s.csv", idx_condition, idx_trial)
  if (file.exists(fn)) {
    return(NULL)
  }

  # create IV variables and assign their value
  for (IV in get_IV_names()) {
    assign(IV, conditions[idx_condition, IV])
  }

  # ----------------------------------------------------------------------------
  # Generate a sample dataset
  # ----------------------------------------------------------------------------

  set.seed(idx_trial)

  # data generation ------------------------------------------------------------

  positive_definite <- FALSE
  while (!positive_definite) {
    X_int <- simulate_sample(n_obs, m_type, n_item, r_factor, w_lambda, t_type)
    o <- polychoric(X_int, smooth = FALSE)
    positive_definite <- all(eig(o$rho) >= 0)
  }

  # ----------------------------------------------------------------------------
  # Prepare sample data
  # ----------------------------------------------------------------------------

  set.seed(idx_trial + 1)

  results_IV <- conditions[idx_condition, get_IV_names()]
  n_obs   <- dim(X_int)[1]
  n_items <- dim(X_int)[2]

  results_CV <- data.frame(t(rep(-1, get_CV_length())))
  colnames(results_CV) <- get_CV_names()
  results_CV["idx_condition"] <- idx_condition
  results_CV["idx_trial"]     <- idx_trial

  results_DV <- data.frame(t(rep(-1, get_DV_length())))
  colnames(results_DV)  <- get_DV_names()
  results_DV["n_items"] <- n_items

  X_tau    <- as.matrix(o$tau)

  X_rf     <- o$rho
  X_rr     <- reduce_pafa_smc(X_rf)

  X_rf_ev  <- eig(X_rf)
  X_rr_ev  <- eig(X_rr)

  results_DV["f_halt"] <- max(which(X_rf_ev > 1))
  results_DV["r_halt"] <- max(which(X_rr_ev > 0))

  # ----------------------------------------------------------------------------
  # Perform PA
  # ----------------------------------------------------------------------------

  set.seed(idx_trial + 2)

  PA_float <-
  PA_int   <-
  PA_rf    <-
  PA_rr    <- vector("list", PA_loops)

  # PA-PCA: make 100 internal datasets
  for (i in 1:PA_loops) {
    positive_definite <- FALSE
    while (!positive_definite) {
      PA_float[[i]] <- rmvn(n_obs, rep(0, n_items), diag(1, n_items))
      PA_int[[i]]   <- tau_mesh(PA_float[[i]], X_tau)
      PA_rf[[i]]    <- polychoric(PA_int[[i]], smooth = FALSE)$rho
      positive_definite <- all(eig(PA_rf[[i]]) >= 0)
    }
  }

  # PA-PAF: get SMC-reduced internal datasets
  PA_rr <- lapply(PA_rf, reduce_pafa_smc)

  # Get eigenvalue distributions
  PA_rf_ev <- do.call(rbind, lapply(PA_rf, eig))
  PA_rr_ev <- do.call(rbind, lapply(PA_rr, eig))

  # Get threshold eigenvalues
  PA_cut_rfm <- apply(PA_rf_ev, 2, mean)
  PA_cut_rrm <- apply(PA_rr_ev, 2, mean)
  PA_cut_rfu <- apply(PA_rf_ev, 2, quantile, probs = .95)
  PA_cut_rru <- apply(PA_rr_ev, 2, quantile, probs = .95)

  # Get estimated number of factors
  results_DV["T_fm"] <- vec_filter(X_rf_ev, PA_cut_rfm) # PA-PCA-m
  results_DV["T_rm"] <- vec_filter(X_rr_ev, PA_cut_rrm) # PA-PAF-m
  results_DV["T_fu"] <- vec_filter(X_rf_ev, PA_cut_rfu) # PA-PCA-95
  results_DV["T_ru"] <- vec_filter(X_rr_ev, PA_cut_rru) # PA-PAF-95

  # ----------------------------------------------------------------------------
  # Perform RPA
  # ----------------------------------------------------------------------------

  set.seed(idx_trial + 3)

  flags <- rep(FALSE, 4)
  max_n_factors <- get_max_n_factors(n_items)

  for (k in 0:max_n_factors) {

    RPA_float <-
    RPA_int   <-
    RPA_rf    <-
    RPA_rr    <- vector("list", PA_loops)

    if (k == 0) {

      RPA_rf <- PA_rf
      RPA_rr <- PA_rr

    }

    if (k > 0) {

      # Perform k-factor EFA and get factor loadings

      set.seed(idx_trial + 3 + k)

      ok <- FALSE
      n_starts <- 1 # number of starting points in EFA
      while (!ok) {
        # this has a random component
        RPA_weights <- try(factanal(
          covmat = X_rf, n_obs = n_obs, factors = k, rotation = "none", nstart = n_starts)$loadings[])
        if (class(RPA_weights)[1] == "try-error") n_starts <- n_starts * 2
        if (class(RPA_weights)[1] == "matrix")    ok <- TRUE
        if (n_starts > 1024) break
      }
      if (n_starts > 1024) break

      # Reproduce the correlation matrix with factor loadings
      RPA_pop_rf <- RPA_weights %*% t(RPA_weights)
      diag(RPA_pop_rf) <- 1

      # RPA-PCA: make 100 internal datasets
      set.seed(idx_trial + 3 + k)
      for (i in 1:PA_loops) {
        positive_definite <- FALSE
        while (!positive_definite) {
          RPA_float[[i]] <- rmvn(n_obs, rep(0, n_items), RPA_pop_rf)
          RPA_int[[i]]   <- tau_mesh(RPA_float[[i]], X_tau)
          RPA_rf[[i]]    <- polychoric(RPA_int[[i]], smooth = FALSE)$rho
          positive_definite <-
            all(eig(RPA_rf[[i]]) >= 0) &
            dim(RPA_rf[[i]])[1] == n_items
        }
      }

      # RPA-PAF
      RPA_rr <- lapply(RPA_rf, reduce_pafa_smc)

    }

    # Get eigenvalue distributions
    RPA_rf_ev <- do.call(rbind, lapply(RPA_rf, eig))
    RPA_rr_ev <- do.call(rbind, lapply(RPA_rr, eig))

    RPA_cut_rfu <- apply(RPA_rf_ev, 2, quantile, probs = .95)[k + 1]
    RPA_cut_rru <- apply(RPA_rr_ev, 2, quantile, probs = .95)[k + 1]

    RPA_cut_rfm <- apply(RPA_rf_ev, 2, mean)[k + 1]
    RPA_cut_rrm <- apply(RPA_rr_ev, 2, mean)[k + 1]

    if ((flags[1] == FALSE) && (X_rf_ev[k + 1] < RPA_cut_rfu)) {
      flags[1] <- TRUE
      results_DV["R_fu"] <- k
    }
    if ((flags[2] == FALSE) && (X_rr_ev[k + 1] < RPA_cut_rru)) {
      flags[2] <- TRUE
      results_DV["R_ru"] <- k
    }
    if ((flags[3] == FALSE) && (X_rf_ev[k + 1] < RPA_cut_rfm)) {
      flags[3] <- TRUE
      results_DV["R_fm"] <- k
    }
    if ((flags[4] == FALSE) && (X_rr_ev[k + 1] < RPA_cut_rrm)) {
      flags[4] <- TRUE
      results_DV["R_rm"] <- k
    }

    if (sum(flags) == 4) break

  }

  # ----------------------------------------------------------------------------
  # Save results
  # ----------------------------------------------------------------------------

  o  <- cbind(results_CV, results_DV)
  write.csv(o, fn, row.names = FALSE)

  NULL

}

stopCluster(cl)
