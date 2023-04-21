#' @export
test_function <- function(tbl) {
    tbl <- tibble::tibble(a = c(1:10), b = c(10:19))

    filter_statement <- quote(a %in% c(1, 5, 8))

    # append to filter_statement (for lack of a better solution with dtplyr...)
    data <- dtplyr::lazy_dt(tbl)
    out <- data |>
        dplyr::filter(!!filter_statement)

    return(out)
}

test_function_2 <- function(tbl) {
    tbl <- tibble::tibble(a = c(1:10), b = c(10:19))
    library(data.table)
    # append to filter_statement (for lack of a better solution with dtplyr...)
    data <- dtplyr::lazy_dt(tbl)
    out <- data |>
        dplyr::filter(a %in% c(1, 5, 8))

    return(out)
}
