#' @export
test_function <- function(tbl) {
    # tbl <- data.table::data.table(a = c(1:10), b = c(10:19))

    filter_statement <- substitute(a %in% c(1, 5, 8))

    # append to filter_statement (for lack of a better solution with dtplyr...)
    filter_statement <- substitute(filter_statement | b == 15, list(filter_statement = filter_statement))
    out <- dtplyr::lazy_dt(tbl) |>
        dplyr::filter(!!rlang::enquo(filter_statement))

    return(out)
}
