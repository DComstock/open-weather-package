#' Detect changepoints (in means) in weather patterns using bayesian approach
#'
#' @importFrom stats rgamma dt
#' @param data dataframe containing weather information
#' @param chains number of markov chains
#' @param burn_in number of observations to exclude ( burn-in period)
#' @param plot allows the user to plot the changepoint vs. see descriptive summary
#' @return returns the detected changepoint
#' @export
# dataframe should have two standard fields 'dt' : date/time stamp; 'y' <- weather information (temperature,sea level)
detect_changepoint <- function(data, chains = 1000, burn_in = 200, plot = FALSE){

  n <- nrow(data) # number of observations
  m <- chains # number of chains
  k <- numeric(n)
  mu <- numeric(n)
  lambda <- numeric(n)

# initialize values
  L <- numeric(n) # likelihood fxn has one slot per year
  k[1] <- sample(1:n,1) # pick a random date-point

  mu[1] <- 1
  lambda[1] <- 1
  b1 <- 1
  b2 <- 1

  # transform the temperature to accomodate rgamma function
  y <- round(data$y/10)

for (i in 2:m) {
    kt <- k[i-1] # start date
    r <- .5 + sum(y[1:kt])
    mu[i] <- rgamma(1,shape = r,rate = kt+b1)
    r <- ifelse(kt+1 > n,  0.5 + sum(y) , 0.5 + sum(y[(kt+1):n]))

    lambda[i] <- rgamma(1,shape = r,rate = n-kt+b2)
    b1 <- rgamma(1,shape = 0.5,rate = mu[i]+1)
    b2 <- rgamma(1,shape = 0.5,rate=lambda[i]+1)

  for (j in 1:n) {
  L[j] <- exp((lambda[i]-mu[i])*j) * (mu[i]/lambda[i])^sum(y[1:j])
    } # end of j loop

  L <- L/sum(L)
  # determine the next date range to switch to
  k[i] <- sample(1:n,prob = L,size = 1)
} # end of i loop

temp_loc <- round(mean(k[burn_in:chains]),0)

temp_cp1 <- temp_loc + 1
change_dt <- data$dt[temp_loc]

m1 <- round(mean(data$y[1:temp_loc]),2)
m2 <- round(mean(data$y[temp_cp1:nrow(data)]),2)

change_info <- data.frame("change_dt" = change_dt, "Mean_Prior" = m1, "Mean_After" = m2)

ifelse(plot == TRUE, return(plot_changepoint(data, temp_loc)), return(change_info))

} # end of function

#' Function to plot changepoint
#'
#' @param data data
#' @param change_loc changepoints
#' @return returns plots
plot_changepoint <- function(data, change_loc){

  cp1 <- NULL
  cp1_temp <- NULL
  n_temp <- NULL
  m1 <- NULL
  m2 <- NULL
  x_range <- NULL

  cp1 <- round(change_loc)
  cp1_temp <- cp1 + 1

  # cat(cp1_temp)
  # cat(n_temp)
  #op_dt <- seq.Date(from = as.Date("2019-01-01"), by = "day", length.out = 112)

  n_temp <- nrow(data)
  x_range <- c(cp1_temp:n_temp)
  m1 <- mean(data$y[1:cp1])
  m2 <- mean(data$y[x_range])



  # create data frame to hold changepoints
  df_cp1 <- data.frame(dt = c(1:cp1), y = m1)
  df_cp2 <- data.frame(dt = x_range, y = m2)

  p_cp <- NULL

  p_cp <- ggplot2::ggplot() + ggplot2::geom_line(data = data, ggplot2::aes(x = dt, y = y, color = "Original")) +
    ggplot2::theme_minimal()
  p_cp <- p_cp +
    ggplot2::geom_line(data = df_cp1, ggplot2::aes(x = dt,y = y, color = "Prior Mean")) +
    ggplot2::geom_line(data = df_cp2, ggplot2::aes(x = dt,y = y, color = "Post Mean")) +
    ggplot2::labs(x = "dt") +
    ggplot2::labs(title = "ChangePoint Detection") +
    ggplot2::scale_color_manual(values = c("black", "blue", "darkblue")) +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())

return(p_cp)
}

