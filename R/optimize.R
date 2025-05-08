#' Optimize a function with the PB-Hybrid algorithm
#'
#' @description
#' \loadmathjax
#'
#' This function implements a two-step, population-based, hybrid optimization
#'  algorithm, for solving problems of the form:
#'
#' \mjsdeqn{
#' \max_{x, \tilde{x}} f(x, \tilde{x}) ~~s.t.~~ g(x, \tilde{x}) \geq 0,~ \tilde{g}(\tilde{x}) \geq 0\\
#' x \in X,~ \tilde{x} \in \tilde{X}
#' }
#'
#' See the readme for more details on this format of problems, and when a pb-hy
#'  algorithm would be useful.
#'
#' @param f Objective function \mjseqn{f(x, \tilde{x})}.
#' @param g,gtil Constraint functions \mjseqn{g(x, \tilde{x})} and
#'  \mjseqn{\tilde{g}(\tilde{x})}. By default, never-binding ones.
#' @param x_dom,xtil_dom Domains \mjseqn{X} and \mjseqn{\tilde{X}}. These will
#'  be used by `optimizer` (for \mjseqn{X}), and `initializer`/`updater` (for
#'  \mjseqn{\tilde{X}}). Should be a list with \mjseqn{m} and \mjseqn{\tilde{m}}
#'  entries.
#' @param initializer Operator (function) to initialize the population, such
#'  that \mjseqn{S_0 = \text{initializer}(\tilde{X}, \tilde{g})} (see details).
#' @param optimizer Operator (function) to solve the reduced problem, such that
#'  \mjseqn{R_t = \text{optimizer}(X, g, t, S_t)} (see details).
#' @param updater Operator (function) to update the population, such that
#'  \mjseqn{S_{t+1} = \text{updater}(\tilde{X}, \tilde{g}, t, S_t, R_t)}.
#' @param stopper Stopping criteria for the algorithm, created via
#'  [flow_stopper()].
#' @param logger Logger for tracking the process, created via [flow_logger()].
#' @param check_samples Number of samples to be expected in \mjseqn{S_t}. Set to
#'  `NULL` to not check (required if it changes with iterations \mjseqn{t}).
#' @param check_op Integer vector with iterations to check if the operators are
#'  producing results with the needed format. Helps catching errors. Set to `0`
#'  to not check.
#'
#' @details
#' This function is a wrapper for the operator functions, organizing them in the
#'  correct structure of a population-based, two-step, hybrid algorithm. But, it
#'  is the job of the user to implement the operators correctly:
#' * `initializer`: todo.
#' * `optimizer`: todo.
#' * `updater`: todo.
#'
#' @return A list containing:
#' \item{results}{Optimization results for each iteration.}
#' \item{metrics}{Metrics calculated during the optimization process.}
#' \item{duration}{Timing information for initialization, main loop, and total
#'  execution.}
#'
#' @noMd
#' @export
optimize_phy <- function(
  f = \(x, xtil) NA, g = \(x, xtil) 0, gtil = \(xtil) 0,
  x_dom, xtil_dom,
  initializer, optimizer, updater,
  stopper = flow_stopper(), logger = flow_logger(),
  check_samples = NULL, check_op = c(1, 2)
) {

  # Setup tests
  env_main <- current_env()
  test_funs(f, g, gtil, initializer, optimizer, updater, env = env_main)
  test_classes(flow_stopper = stopper, flow_logger = logger, env = env_main)
  #test_domains(x_dom, xtil_dom, env = env_main) not implemented

  # Initialize containers
  results <- vector("list", stopper$iter_upper)
  metrics <- vector("list", stopper$iter_upper)

  # First iteration with `initializer`
  t_init_0 <- Sys.time()
  t <- 1
  results[[t]]$xtil <- try_op(
    initializer(xtil_dom = xtil_dom, gtil = gtil),
    t = t, check_samples = check_samples, check_op = check_op, env = env_main
  )
  metrics[[t]] <- stopper$get_metrics(results, t, Sys.time() - t_init_0)
  t_init_1 <- Sys.time()

  # Main loop
  t_loop_0 <- Sys.time()
  for (t in seq_len(stopper$iter_upper)) {
    xtil_t <- results[[t]]$xtil

    # Optimizing the sample
    results[[t]] <- list_rbind(map(seq_len(nrow(xtil_t)), function(s) {
      xtil_s <- xtil_t[s, ]
      f_s <- \(x) f(x, xtil_s)
      g_s <- \(x) g(x, xtil_s)
      try_op(
        optimizer(f_s = f_s, g_s = g_s, x_dom = x_dom, t = t, xtil_s = xtil_s),
        t = t, check_samples = check_samples, check_op = check_op, s = s, env = env_main
      )
    }))

    # Calculating metrics
    time <- Sys.time() - t_loop_0
    metrics[[t]] <- stopper$get_metrics(results, t, time)

    # Logging and stopping
    logger$log(results[[t]], t, time, metrics[[t]])
    if (stopper$check_metrics(metrics[[t]])) break

    # Updating the sample
    results[[t + 1]]$xtil <- try_op(
      updater(xtil_dom = xtil_dom, gtil = gtil, r_t = results[[t]], t = t),
      t = t, check_samples = check_samples, check_op = check_op, env = env_main
    )
  }
  t_loop_1 <- Sys.time()
  which_stopped <- stopper$check_metrics(metrics[[t]], which = TRUE)

  cli::cli_text()
  cli::cli_alert_success("\nFinished after {t} iterations. Stopping criteria(s) were:")
  cli::cli_ul(which_stopped)

  # Results
  stopper$check_metrics(metrics[[t]], which = TRUE)
  list(
    results = compact(results),
    metrics = compact(metrics),
    metrics_stop = which_stopped,
    duration = list(
      iters = t,
      time_init = t_init_1 - t_init_0,
      time_loop = t_loop_1 - t_loop_0,
      time_total = t_loop_1 - t_init_0
    )
  )
}
