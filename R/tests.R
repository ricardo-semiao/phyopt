
# Setup testing functions ------------------------------------------------------

#' Test if functions have the correct arguments
#' @noRd
test_funs <- function(f, g, gtil, initializer, optimizer, updater, env) {
  test_data = list(
    fun = list(f, g, gtil, initializer, optimizer, updater),
    fun_name = list("f", "g", "gtil", "initializer", "optimizer", "updater"),
    args_names = list(
      c("x", "xtil"),
      c("x", "xtil"),
      c("xtil"),
      c("xtil_dom", "gtil"),
      c("f_s", "g_s", "x_dom", "t", "xtil_s"),
      c("xtil_dom", "gtil", "r_t", "t")
    )
  )

  pwalk(test_data, function(fun, fun_name, args_names) {
    if(!identical(fn_fmls_names(fun), args_names)) {
      abort(glue("`{fun_name}` must have arguments '{args_names}', in this order."), call = env)
    }
  })
}

#' Test if objects have the correct classes (and only one class)
#' @noRd
test_classes <- function(..., env = caller_env()) {
  objs <- ensyms(...)
  iwalk(objs, function(obj, class_name) {
    if (!inherits_only(eval(obj), class_name)) {
      abort(glue("`{as_name(obj)}` must be of (only) class '{class_name}'."), call = env)
    }
  })
}


# `try_op`` and its helpers ----------------------------------------------------

#' TryCatch operators
#'
#' Catch user created errors on the operators to generate better error messages.
#'  On given iterations, check if the operators are returning the expected object
#'  format.
#'
#' @noRd
try_op <- function(call, t, check_samples, check_op, s = NULL, env) {
  op_name <- call_name(enexpr(call))
  output <- try_abort(call, op_name, t, s, env)

  if (t %in% check_op) {
    switch(op_name,
      "initializer" = test_initializer(output, check_samples, env = env),
      "optimizer" = test_optimizer(output, check_samples, env$x_dom, env$xtil_dom, env = env),
      "updater" = test_updater(output, check_samples, env = env)
    )
  }

  output
}

#' Test if `initializer` returns the expected object format.
#' @noRd
test_initializer <- function(output, check_samples, env) {
  cond_samples <- expr(`if`(is_null(check_samples), TRUE, NROW(output) == check_samples))
  if (! (!is_null(attr(output, "dim"))) && eval(cond_samples)) {
    abort(glue("`initializer` and `optimizer` must return a data frame with the same number of rows as `check_samples`."), call = env)
  }
}

#' Test if `updater` returns the expected object format.
#' @noRd
test_updater <- test_initializer

#' Test if `optimizer` returns the expected object format.
#' @noRd
test_optimizer <- function(output, check_samples, x_dom, xtil_dom, env) {
  if (! inherits_only(output, "data.frame") && NROW(output) == 1 && identical(colnames(output), c("x", "xtil", "f", "i"))) {
    abort(glue("`optimizer` must return 1-row a data frame with columns 'x', 'xtil', 'f' and 'i', in order."), call = env)
  }

  walk2(output[c("x", "xtil")], list(x_dom, xtil_dom), \(x, dom) {
    if (! inherits_only(x, "data.frame") || ncol(x) != length(dom)) {
      abort(glue("`optimizer` must return a data frame for 'x' and 'xtil', with `length(x_dom)` and `length(xtil_dom)` columns respectively."), call = env)
    }
  })

  if (! is_bare_numeric(output$f)) {
    abort(glue("`optimizer` must return a bare numeric for its column 'f'."), call = env)
  }

  walk(output, \(x) {
    cond_samples <- expr(`if`(is_null(check_samples), TRUE, NROW(output) == check_samples))
    if (! eval(cond_samples)) {
      abort(glue("Every `optimizer` output's column must have `NROW(.)` equal to `check_samples`."), call = env)
    }
  })
}

#' TryCatch operator calls and generate better error messages
#' @noRd
try_abort <- function(fun_call, op_name, t, s = NULL, env = caller_env()) {
  in_sample <- if (is.null(s)) "" else paste0(", and sample ", s)

  tryCatch(eval(fun_call),
    error = function(e) {
      abort(glue("Error in iteration {t}, with `{op_name}`{in_sample}: {e$message}"), call = env)
    },
    warning = function(x) {
      abort(glue("Warning in iteration {t}, with `{op_name}`{in_sample}: {w$message}"), call = env)
    },
    message = function(x) {
      abort(glue("Message in iteration {t}, with `{op_name}`{in_sample}: {m$message}"), call = env)
    }
  )
}
