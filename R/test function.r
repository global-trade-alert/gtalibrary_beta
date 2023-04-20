#' @export
test_function <- function(tbl) {
    tbl <- tibble::tibble(a = c(1:10), b = c(10:19))

    filter_statement <- substitute(a %in% c(1, 5, 8))

    # append to filter_statement (for lack of a better solution with dtplyr...)
    out <- dtplyr::lazy_dt(tbl) |>
        dplyr::filter(eval(filter_statement, enclos = parent.frame()))

    return(out)
}
