#' Creates matrix of beta coefficients.
#'
#' @param dat A data frame with columns `id`, `time` and `y1` to `y6` for the six channels.
#'
#' @return a list
#' @export
#'
#' @examples
#' # to do
create.beta <- function(dat, ...){
  R <- ncol(dat) - 2
  idd <- unique(dat$id)
  N <- length(idd)
  # Fit models
  val <- matrix(NA, nrow = N, ncol = 6 * R)
  vcm <- vector("list", length = N)
  val_ind <- val
  vcm_ind <- vcm
  for(i in 1:N){
    dat_sub <- dat %>%
      filter(id == idd[i])
    y1_i <- dat_sub$y1
    y2_i <- dat_sub$y2
    y3_i <- dat_sub$y3
    y4_i <- dat_sub$y4
    y5_i <- dat_sub$y5
    y6_i <- dat_sub$y6
    B_i <- splines::bs(dat_sub$time, ...)
    mod <- lm(cbind(y1_i, y2_i, y3_i, y4_i, y5_i, y6_i) ~ B_i)
    # Save output
    val[i,] <- coef(mod)
    vcm[[i]] <- vcov(mod)
  }
  out <- list(val = val, vcm = vcm, N = N)
  return(out)
}
