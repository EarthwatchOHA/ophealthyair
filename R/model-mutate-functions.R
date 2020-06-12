extract_p <- function(mod) {
  glanced_df <- broom::glance(mod)
  val <- glanced_df[["p.value"]][[1]]
  return(val)
}

extract_R2 <- function(mod, adjusted = FALSE) {
  glanced_df <- broom::glance(mod)
  if (adjusted) {
    val <- glanced_df[["adj.r.squared"]][[1]]
  } else {
    val <- glanced_df[["r.squared"]]
  }
  return(val)
}

extract_slope <- function(mod) {
  tidied_df <- broom::tidy(mod)
  val <- tidied_df[["estimate"]][[2]]
  return(val)
}

extract_int <- function(mod) {
  tidied_df <- broom::tidy(mod)
  val <- tidied_df[["estimate"]][[1]]
  return(val)
}