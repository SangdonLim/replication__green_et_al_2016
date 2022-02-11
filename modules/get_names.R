#' return IV names
#'
#' @export
get_IV_names <- function() {
  tmp <- c(
    "n_obs",
    "m_type",
    "n_item",
    "w_lambda",
    "r_factor",
    "t_type"
  )
  return(tmp)
}

#' return CV names
#'
#' @export
get_CV_names <- function() {
  tmp <- c(
    "idx_condition",
    "idx_trial"
  )
  return(tmp)
}

#' return DV names
#'
#' @export
get_DV_names <- function() {
  tmp <- c(
    "T_fm", "T_fu", "T_rm", "T_ru",
    "R_fm", "R_fu", "R_rm", "R_ru",
    "f_halt", "r_halt",
    "n_items"
  )
  return(tmp)
}

#' return the number of IVs
#'
#' @export
get_IV_length <- function() {
  return(length(get_IV_names()))
}

#' return the number of CVs
#'
#' @export
get_CV_length <- function() {
  return(length(get_CV_names()))
}

#' return the number of DVs
#'
#' @export
get_DV_length <- function() {
  return(length(get_DV_names()))
}

#' return the index of IVs
#'
#' @param vec_names a vector containing IV names.
#'
#' @export
get_IV_idx <- function(vec_names) {
  return(vec_names %in% get_IV_names())
}

#' return the index of CVs
#'
#' @param vec_names a vector containing CV names.
#'
#' @export
get_CV_idx <- function(vec_names) {
  return(vec_names %in% get_CV_names())
}

#' return the index of DVs
#'
#' @param vec_names a vector containing DV names.
#'
#' @export
get_DV_idx <- function(vec_names) {
  return(vec_names %in% get_DV_names())
}

#' return full method names
#'
#' @param vec_names a vector containing DV names.
#'
#' @export
get_DV_full_names <- function(vec_names) {

  vec_names[which(vec_names == "T_fm")] <- "PA-PCA-m"
  vec_names[which(vec_names == "T_fu")] <- "PA-PCA-95"
  vec_names[which(vec_names == "T_rm")] <- "PA-PAF-m"
  vec_names[which(vec_names == "T_ru")] <- "PA-PAF-95"
  vec_names[which(vec_names == "R_fm")] <- "RPA-PCA-m"
  vec_names[which(vec_names == "R_fu")] <- "RPA-PCA-95"
  vec_names[which(vec_names == "R_rm")] <- "RPA-PAF-m"
  vec_names[which(vec_names == "R_ru")] <- "RPA-PAF-95"

  return(vec_names)

}
