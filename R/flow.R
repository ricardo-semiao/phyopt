#' @title Flow Stopper Constructor
#'
#' @description
#' \loadmathjax
#'
#' This function creates a flow stopper object that calculate metrics, at each
#'  iteration, based on the current results of the algorithm, and checks if the
#'  algorithm should stop based on those metrics.
#'
#' Given the objects in \mjseqn{R} (list of results for every iteration), the
#'  user can define a set of metrics \mjseqn{M_t}, to be calculated at every
#'  iteration \mjseqn{t}. Based on each metric\mjseqn{m}, a stopping criterion
#'  can be defined via any expression on \mjseqn{m} that returns a boolean.
#'
#' Finally, the list of booleans for a given iteration \mjseqn{t} can be
#'  combined with any logic expression to determine if the algorithm should
#'  stop.
#'
#' @param formulas A list of formulas specifying the metrics \mjseqn{m} to be
#'  calculated on the LHS (see details), and the stopping criterion depending on
#'  the metric value `.m`, on the RHS.
#' @param check_expr A RHS-only formula with a logic operation, depending on the
#'  vector of booleans `.ms`.
#' @param iter_upper A technical argument used to initialize containers for
#'  \mjseqn{R} and \mjseqn{M}. A stopping criterion based on \mjseqn{t} should
#'  be defined in the `formulas` argument.
#'
#' @details
#' The LHS of the are evaluated in the context of the algorithm, i.e., it can
#'  depend on the variables `t` (current iteration), `time` (current time in
#'  seconds), and the list of each element in \mjseqn{R} (with length `t`).
#'  For example, `f[[t - 1]]` will evaluate to the vector of objective function
#'  values of all the samples in the previous iteration.
#'
#' Note that you can set unfeasible stopping criteria, `FALSE` to only get
#'  the value of the metric at the end of the algorithm.
#'
#' @return A `"flow_stopper"`-class object to be passed to `optimize_phy()`.
#'
#' @export
flow_stopper <- function(
  formulas = list(get_metric$iter(100), get_metric$f_prop(0.01)),
  check_expr = ~ any(.ms, na.rm = TRUE), iter_upper = 1e4
) {
  env_created <- caller_env()
  check_expr <- f_rhs(check_expr)

  get_metrics <- function(results, t, time) {
    data <- c(list_transpose(compact(results), simplify = FALSE), t = t, time = time)
    map(formulas, \(f) eval(f[[2]], data, env_created))
  }

  check_metrics <- function(metrics_t, which = FALSE) {
    .ms <- map2_lgl(formulas, metrics_t, \(f, m) eval(f[[3]], list(.m = m), enclos = env_created))
    if (which) {
      imap_chr(formulas[.ms], ~ glue("{.y}: {reduce(deparse(.x), paste)}"))
    } else {
      eval(check_expr, list(.ms = .ms), enclos = env_created)
    }
  }

  stopper <- list(
    iter_upper = iter_upper,
    get_metrics = get_metrics,
    check_metrics = check_metrics
  )
  class(stopper) <- "flow_stopper"

  stopper
}


#' Flow Logger Constructor
#'
#' @description
#' \loadmathjax
#'
#' This function creates a flow logger object that calculates metrics and logs
#'  them into the console. The metrics definition are done very similarly to the
#'  [flow_stopper()] function (see its help page), but the `formulas` are
#'  divided into lists, each list being printed in a different line.
#'
#' @param formulas_list A list of `formulas` as in [flow_stopper()] (a list of
#'  lists), except the RHS to be a function that takes the LHS value `.m` and
#'  returns a single string to be printed.
#' @param log_each An integer specifying the frequency of logging.
#' @param sep A character string used to concatenate logs associated with a list
#'  inside `formulas_list`.
#' @param args_alert,args_ul A list of arguments passed to [cli::cli_alert()]
#'  and [cli::cli_ul()], the functions that format the console output.
#'
#' @return A `"flow_logger"`-class object to be passed to `optimize_phy()`.
#'
#' @details
#' The LHS of the formulas are evaluated in the same way as in [flow_stopper()]
#'  (see its help page), but the user only access the current results
#'  (\mjseqn{R_t}). Additionally, any metric of the current iteration
#'  (\mjseqn{M_t}) can be accessed by its name (if `formulas` was named).
#'
#' @export
flow_logger <- function(
  formulas_list = list(list(
    t ~ paste("=> Iter:", .m),
    mean(f) ~ paste("mean(f):", round(.m, 4))
  )),
  log_each = 1, sep = " -- ",
  args_alert = list(), args_ul = list()
) {
  env_created <- caller_env()

  log <- function(results_t, t, time, metrics_t) {
    if (t %% log_each != 0) return(NULL)

    data <- c(results_t, t = t, time = time, metrics_t)

    logs <- map(formulas_list, function(formulas) {
      logs_m <- imap(formulas, \(f, name) {
        .m <- eval(f[[2]], data, env_created)
        eval(f[[3]], list(.m = .m))
      })
      paste0(logs_m, collapse = sep)
    })

    inject(cli::cli_alert(paste0(logs[[1]], collapse = sep), !!!args_alert))
    if (length(logs) > 1) inject(cli::cli_ul(list_c(logs[-1]), !!!args_ul))
  }

  logger <- list(log = log)
  class(logger) <- "flow_logger"

  logger
}


#' Get common metrics' formulas
#' @export
get_metric <- list(
  iter = \(n) {
    expr(t ~ .m >= !!n)
  },
  f_prop = \(n, l = 1) {
    expr(if (t == 1 || max(f[[t - 1]]) == 0) {
      NA
    } else {
      (max(f[[t]]) - max(f[[t - l]])) / abs(max(f[[t - 1]]))
    } ~ .m < !!n)
  }
)
