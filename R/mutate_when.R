

#' @title Mutate Subset
#' @rdname mutate_when
#' @inheritParams dplyr::mutate
#' @param when logical vector
#' @export
#'
#'
#'
mutate_when <- function(.data, ..., .when = NULL) {
  UseMethod("mutate_when")
}

#' @export
mutate_when.data.frame <- function(.data, ..., .when = NULL) {

  subset_rows <- rlang::eval_tidy(enquo(.when), data = .data)
  dots_quo <- rlang::enquos(...)
  if (is.null(subset_rows)) {
    res <- mutate(.data, !!!dots_quo)
    return(res)
  } else {
    res <- mutate(.data[subset_rows,], !!!dots_quo)
    new_cols <- setdiff(names(res), names(.data))
    n <- length(new_cols)
    if (n>0) {
      .data <- mutate(.data,
                      !!!rlang::pairlist2(
                        !!!setNames(rep(NA, n),
                                    new_cols)
                        )
                      )
    }
    .data[subset_rows,] <- res
    return(.data)
  }

}
