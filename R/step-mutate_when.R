

step_mutate_when <- function(parent, new_vars = list(), nested = FALSE, .when = NULL) {
  vars <- union(parent$vars, names(new_vars))
  vars <- setdiff(vars, names(new_vars)[vapply(new_vars, is_null, lgl(1))])

  new_step(
    parent,
    vars = vars,
    groups = parent$groups,
    arrange = parent$arrange,
    needs_copy = !parent$implicit_copy,
    new_vars = new_vars,
    nested = nested,
    when = .when, #should be a quosure
    class = "dtplyr_step_mutate_when"
  )
}

dt_call.dtplyr_step_mutate_when <- function(x, needs_copy = x$needs_copy) {

  browser()
  if (!x$nested) {
    j <- call2(":=", !!!x$new_vars)
  } else {
    mutate_list <- mutate_nested_vars(x$new_vars)
    j <- call2(":=", call2("c", !!!mutate_list$new_vars), mutate_list$expr)
  }

  if (!quo_is_null(x$when)) {
    i <- x$when
    out <- call2("[", dt_call(x$parent, needs_copy), i, j)
  } else {
    out <- call2("[", dt_call(x$parent, needs_copy), , j)
  }

  add_grouping_param(out, x, arrange = FALSE)
}


#' @export
mutate_when.dtplyr_step <- function(.data, ...,
                                    .when = NULL,
                                    .before = NULL, .after = NULL) {
  browser()
  dots <- capture_dots(.data, ...)
  test <- capture_dot(.data, !!enquo(.when))
  if (is_null(dots)) {
    return(.data)
  }

  nested <- nested_vars(.data, dots, .data$vars)
  .when <- enquo(.when)
  out <- step_mutate_when(.data, dots, nested, .when = .when)

  .before <- enquo(.before)
  .after <- enquo(.after)
  if (!quo_is_null(.before) || !quo_is_null(.after)) {
    # Only change the order of new columns
    new <- setdiff(names(dots), .data$vars)
    out <- relocate(out, !!new, .before = !!.before, .after = !!.after)
  }

  out
}
