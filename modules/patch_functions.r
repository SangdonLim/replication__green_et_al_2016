#' Patch functions
#'
#' Create patched versions of 'semTools' 0.5.2 and 'RGenData' v1.0 with following changes:
#'
#' \code{NNpatched()} is based on \code{semTools::mvrnonnorm()}
#' - Change \code{MASS::mvrnorm()} to \code{mvnfast::rmvn()}
#' - Change \code{ValeMaurelli1983copied()} to \code{VMpatched()}
#'
#' \code{VMpatched()} is based on \code{semTools:::ValeMaurelli1983copied()}
#' - Change \code{MASS::mvrnorm()} to \code{mvnfast::rmvn()}
#'
#' @param clean if \code{TRUE} then delete the code afterwards.
#'
#' @export
patch_functions <- function(clean = TRUE) {

  o <- all(
    c("NNpatched", "VMpatched")
    %in% ls(envir = .GlobalEnv)
  )
  if (o) {
    return(TRUE)
  }

  get_package("semTools", "0.5-2")
  require(digest)

  # Patch NN -------------------------------------------------------------------

  # Load
  NNpatched      <- eval(parse(text = "semTools::mvrnonnorm"))
  NNpatched_code <- capture.output(NNpatched)
  NNpatched_code <- NNpatched_code[1:34]
  check_hash(NNpatched_code, "684b9b098c69b4d1c982418d167fee9b", "semTools::mvrnonnorm")

  # Patch
  NNpatched_code[16] <- "        X <- mvnfast::rmvn(n = n, mu = mu, Sigma = Sigma)"
  NNpatched_code[26] <- "        Z <- VMpatched(n = n, COR = cov2cor(Sigma), skewness = skewness, kurtosis = kurtosis)"
  NNpatched_code <- NNpatched_code[-27]
  NNpatched_code <- c(
    "NNpatched <-", NNpatched_code
  )
  message("NNpatched: created the patched function NNpatched()")
  check_hash(NNpatched_code, "223036b627241b788d407fdf2d4ac7dd", "NNpatched")

  # Source
  writeLines(NNpatched_code, "NNpatched.r")
  source("NNpatched.r")
  message("NNpatched: complete")

  # Patch VM -------------------------------------------------------------------

  # Load
  VMpatched      <- eval(parse(text = "semTools:::ValeMaurelli1983copied"))
  VMpatched_code <- capture.output(VMpatched)
  VMpatched_code <- VMpatched_code[1:99]
  check_hash(VMpatched_code, "11415e1e08e8ac2de40cbfd3928b9d0a", "semTools:::ValeMaurelli1983copied")

  # Patch
  VMpatched_code[93] <- "        X <- Z <- mvnfast::rmvn(n = n, mu = rep(0, nvar), sigma = ICOR)"
  VMpatched_code <- c(
    "VMpatched <-", VMpatched_code
  )
  check_hash(VMpatched_code, "555923bf88e494f3cbbbee729989640a", "VMpatched")

  # Source
  writeLines(VMpatched_code, "VMpatched.r")
  source("VMpatched.r")
  message("VMpatched: complete")

  # Finalize -------------------------------------------------------------------

  message("Patch complete")

  if (clean) {

    o <- remove_file_safely("NNpatched.r")
    o <- remove_file_safely("VMpatched.r")

  }

}

#' Remove a file safely
#'
#' Remove a file and make sure it is removed.
#'
#' @export
remove_file_safely <- function(f) {
  while(file.exists(f)) {
    o <- file.remove(f)
    Sys.sleep(1)
  }
  return(TRUE)
}

#' Check hash
#'
#' @export
check_hash <- function(x, expected_hash, fn_name) {
  hash_ok <- digest(x) == expected_hash
  if (!hash_ok) {
    stop(sprintf(
      "%s: hash mismatch", fn_name
    ))
  }
  message(sprintf(
    "%s: hash ok", fn_name
  ))
}
