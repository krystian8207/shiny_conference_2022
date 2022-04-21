genid <- function() {
  paste(sample(letters, 5), collapse = "")
}

get_state <- function(input) {
  state <- reactiveValuesToList(input)
  return(
    state[setdiff(names(state), c("confirm", "delete"))] 
  )
}

gen_col_character <- function(state, n_rows) {
  if (state$charminmax[1] == state$charminmax[2]) {
    nchars <- state$charminmax[1]
  } else {
    nchars <- sample(state$charminmax[1]:state$charminmax[2], n_rows, replace = TRUE)
  }
  return(
    stringi::stri_rand_strings(n_rows, nchars, state$charpattern) 
  )
}

gen_col_numeric <- function(state, n_rows) {
  if (state$numtype == "numrange") {
    res <- runif(n_rows, min = state$numminmax[1], max = state$numminmax[2])
  }
  if (state$numtype == "numdistro") {
    res <- switch(
      state$numdistro,
      "normal" = rnorm(n_rows, mean = state$normal_mean, sd = state$normal_sd),
      "exponential" = rexp(n_rows, rate = state$exponential_rate)
    )
  }
  return(res)
}

gen_col_integer <- function(state, n_rows) {
  if (state$inttype == "intrange") {
    res <- sample(state$intminmax[1]:state$intminmax[2], n_rows, replace = TRUE)
  }
  if (state$inttype == "intdistro") {
    if (state$intdistro == "binomial") {
      res <- rbinom(n_rows, size = state$binomial_size, prob = state$binomial_prob)
    }
    if (state$intdistro == "poisson") {
      res <- rpois(n_rows, lambda = state$poisson_lambda)
    }
  }
  return(res)
}
 
gen_column <- function(state, n_rows) {
  method <- switch(
    state$type,
    "numeric" = gen_col_numeric,
    "integer" = gen_col_integer,
    "character" = gen_col_character
  )
  res <- list(method(state, n_rows))
  names(res) <- state$name
  return(res)
}

gen_table <- function(states, n_rows) {
  purrr::map_dfc(unname(states), gen_column, n_rows = n_rows)
}
