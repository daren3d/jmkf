#' Creates functional data with six channels.
#'
#' @param sigma the standard deviation.
#' @param omega the CS correlation between channels
#'
#' @return a list.
#' @export
#'
#' @examples
#' cd <- create.data()
create.data <- function(sigma = 1, omega = 0.5){
  ni <- 50
  NG <- c(50, 50, 50)
  G <- length(NG)
  uid <- 1:sum(NG)
  N <- length(uid)
  or <- lapply(1:G, function(x){rep(LETTERS[x], each = NG[x])}) |>
    do.call(what = c)
  dat_oracle <- data.frame(id = uid,
                           oracle = or)
  t2 <- runif(ni * N, 0, 10 / ni)
  t3 <- c(replicate(N, sort(runif(ni, min = 0, max = 10))))
  dat_time <- data.frame(id = rep(uid, each = ni),
                         t0 = rep(seq.int(ni), times = N),
                         t1 = rep(seq(0, 10, length.out = ni), times = N),
                         t2 = t2,
                         t3 = t3)
  dat_time$time <- dat_time$t1
  dat_time <- dat_time %>%
    left_join(dat_oracle, by = "id")
  n <- nrow(dat_time)

  create.A <- function(t){
    y <- log(t + 0.1, base = 10) + 1
    return(y)
  }
  create.B <- function(t){
    y <- cos(2 * pi / 10 * t - pi) + 1
    return(y)
  }
  create.C <- function(t){
    y <- -((t - 5) / 5)^3 + 1
    return(y)
  }

  # Responses
  dat_A <- dat_time %>%
    filter(oracle == "A") %>%
    mutate(f1 = create.A(time),
           f2 = create.A(time),
           f3 = create.B(time),
           f4 = create.B(time),
           f5 = create.C(time),
           f6 = create.C(time))
  dat_B <- dat_time %>%
    filter(oracle == "B") %>%
    mutate(f1 = create.A(time),
           f2 = create.C(time),
           f3 = create.C(time),
           f4 = create.B(time),
           f5 = create.A(time),
           f6 = create.B(time))
  dat_C <- dat_time %>%
    filter(oracle == "C") %>%
    mutate(f1 = create.B(time),
           f2 = create.A(time),
           f3 = create.C(time),
           f4 = create.A(time),
           f5 = create.C(time),
           f6 = create.B(time))

  # Error
  S1 <- sigma^2 * (matrix(omega, nrow = R, ncol = R) - (omega - 1) * diag(nrow = R))
  dat_er <- mvtnorm::rmvnorm(n, sigma = S1)
  colnames(dat_er) <- paste0("er", 1:R)

  # Put together
  dat <- rbind(dat_A, dat_B, dat_C) %>%
    cbind(dat_er) %>%
    mutate(y1 = f1 + er1,
           y2 = f2 + er2,
           y3 = f3 + er3,
           y4 = f4 + er4,
           y5 = f5 + er5,
           y6 = f6 + er6) %>%
    dplyr::select(id, oracle, t0, t1, time, y1, y2, y3, y4, y5, y6)

  return(list(dat = dat, oracle = dat_oracle$oracle))
}
