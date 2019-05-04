
#Checks to see if a given probability is valid or not.

check_prob <- function(prob) {
  if (prob < 0 | prob > 1) {
    stop("\ninvalid prob value")
  }
  TRUE
}

#Checks to see if a given number of trials is valid or not.

check_trials <- function(trials) {
  if (trials < 0 | trials %% 1 != 0) {
    stop("\ninvalid trials value")
  }
  TRUE
}

#Checks to see if a given success vector and given number of trials are valid or not.

check_success <- function(success, trials) {
  for (x in success) {
    if (x < 0) {
      stop("\ninvalid success value")
    }
    if (x > trials) {
      stop("\nsuccess cannot be greater than trials")
    }
  }
  TRUE
}

#The expected value or mean of binomial distribution using number of trials and probability of success.

aux_mean <- function(trials, prob) {
  return(trials * prob)
}

#Computes the variance of binomial distribution using number of trials and probability of success.

aux_variance <- function(trials, prob) {
  left <- trials * prob
  right <- 1 - prob
  return(left * right)
}


#Computes the mode of a binomial distribution using number of trials and probability of success.

aux_mode <- function(trials, prob) {
  left <- trials * prob
  return(floor(left + prob))
}

#Computes the skewness using trials and probability of success - the measure of asymmetry of a probability distribution of a random variable about its mean.

aux_skewness <- function(trials, prob) {
  top <- 1 - (2 * prob)
  bottom1 <- trials * prob
  bottom2 <- 1 - prob
  return(top / sqrt(bottom1 * bottom2))
}

#Computes the shape of a probability distribution using trials and probability.

aux_kurtosis <- function(trials, prob) {
  inverser <- (1 - prob)
  top <- 1 - 6*prob*inverser
  bottom <- (trials * prob) * inverser
  return(top / bottom)
}


#' @title bin_choose
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials
#' @param k number of successes
#' @return the number of combinations
#' @export
#' @examples
#' bin_choose(5, 2)
#' bin_choose(n = 5, k = 3)

bin_choose <- function(n, k) {

  answers <- c()
  for (x in k) {
    if (x > n) {
      stop("\nk cannot be greater than n")
    }
    top <- factorial(n)
    bottom <- factorial(x) * factorial(n - x)
    answers <- c(answers, top/bottom)
  }

  return(answers)
}



#' @title bin_probability
#' @description calculates the probability distribution using trials, success and probability
#' @param success the number of successes from trials
#' @param trials number of fixed trials
#' @param prob probability of success on each trial
#' @return integer that represents probability
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(0:2, 5, 0.5)

bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)

  first <- bin_choose(trials, success)
  middle <- prob^success
  right <- (1 - prob)^(trials - success)

  return(first * middle * right)
}



#' @title bin_distribution
#' @description calculates the distribution of probability over a number of successes
#' @param trials number of fixed trials
#' @param prob probability of success on each trial
#' @return data frame with success and probability sections
#' @export
#' @examples
#' bin_dstribution(trials = 5, prob = 0.5)
#' bin_distribution(5, 0.3)

bin_distribution <- function(trials, prob) {
  answers <- bin_probability(0:trials, trials, prob)

  df <- data.frame(
    success = 0:trials,
    probability = answers
  )

  class(df) <- "bindis"
  class(df) <- append(class(df), "data.frame")

  return(df)

}



#' @export
plot.bindis <- function(data) {
  barplot(data$probability, names = data$success, xlab = "successes", ylab = "probability")
}


#' @title bin_cumulative
#' @description calculates the distribution of probability over a number of successes and the cumulative probability
#' @param trials number of fixed trials
#' @param prob probability of success on each trial
#' @return data frame with success, probability and cumulative sections
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
#' bin_cumulative(5, 0.3)

bin_cumulative <- function(trials, prob) {
  bd <- bin_distribution(trials, prob)

  cumuls <- c(0)
  counter <- 1

  for (x in bd$probability) {
    cumuls[counter] <- tail(cumuls, 1) + x
    counter <- counter + 1
  }

  cd <- data.frame(
    success = bd$success,
    probability = bd$probability,
    cumulative = cumuls
  )

  class(cd) <- "bincum"
  class(cd) <- append(class(cd), "data.frame")

  return(cd)

}

#' @export
plot.bincum <- function(data) {
  plot(data$success, data$cumulative, xlab = "successes", ylab = "probability")
  lines(data$success, data$cumulative)
}


#' @title bin_variable
#' @description lists trials and probability in a binomial random variable object
#' @param trials number of fixed trials
#' @param prob probability of success on each trial
#' @return a binomial random variable object, "binvar"
#' @export
#' @examples
#' bin_variable(trials = 5, prob = 0.5)
#' bin_variable(5, 0.3)

bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  lst <- list(trials, prob)
  class(lst) <- "binvar"
  return(lst)
}

#' @export
print.binvar <- function(lst) {
  lst <- unlist(lst)
  cat("Binomial Variable")
  cat("\n")
  cat("\nParameters")
  cat("\n- number of trials:", lst[1])
  cat("\n- prob of success :", lst[2])
}

#' @export
summary.binvar <- function(lst) {
  lst <- unlist(lst)
  mn <- aux_mean(lst[1], lst[2])
  vr <- aux_variance(lst[1], lst[2])
  md <- aux_mode(lst[1], lst[2])
  sk <- aux_skewness(lst[1], lst[2])
  kr <- aux_kurtosis(lst[1], lst[2])
  n_lst <- list(lst[1], lst[2], mn, vr, md, sk, kr)
  class(n_lst) <- "summary.binvar"
  return(n_lst)
}

#' @export
print.summary.binvar <- function(lst) {
  lst <- unlist(lst)
  cat("Summary Binomial")
  cat("\n")
  cat("\nParameters")
  cat("\n- number of trials:", lst[1])
  cat("\n- prob of success :", lst[2])
  cat("\n")
  cat("\nMeasures")
  cat("\n- mode    :", lst[3])
  cat("\n- variance:", lst[4])
  cat("\n- mode    :", lst[5])
  cat("\n- skewness:", lst[6])
  cat("\n- kurtosis:", lst[7])
}

#' @title bin_mean
#' @description main function that computes mean using trials and probability
#' @param trials number of fixed trials
#' @param prob probability of success on each trial
#' @return the mean
#' @export
#' @examples
#' bin_mean(trials = 10, prob = 0.5)
#' bin_mean(10, 0.3)

bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title bin_variance
#' @description main function that computes variance using trials and probability
#' @param trials number of fixed trials
#' @param prob probability of success on each trial
#' @return the variance
#' @export
#' @examples
#' bin_variance(trials = 10, prob = 0.5)
#' bin_variance(10, 0.5)

bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}


#' @title bin_mode
#' @description main function that computes the mode using trials and probability
#' @param trials number of fixed trials
#' @param prob probability of success on each trial
#' @return the mode
#' @export
#' @examples
#' bin_mode(trials = 10, prob = 0.3)
#' bin_mode(10, 0.3)

bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title bin_skewness
#' @description main function that finds skewness of distribution using trials and probability
#' @param trials number of fixed trials
#' @param prob probability of success on each trial
#' @return the skewness
#' @export
#' @examples
#' bin_skewness(trials = 10, prob = 0.3)
#' bin_skewness(10, 0.3)

bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title bin_kurtosis
#' @description main function that computes kurtosis using trials and probability
#' @param trials number of fixed trials
#' @param prob probability of success on each trial
#' @return the kurtosis
#' @export
#' @examples
#' bin_kurtosis(trials = 10, prob = 0.3)
#' bin_kurtosis(10, 0.3)

bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}




