#' @export
test_function <- function(tbl) {
    # tbl <- data.table::data.table(a = c(1:10), b = c(10:19))

    filter_statement <- parse(text = "a %in% c(1, 5, 8) | b == 15")
    out <- dtplyr::lazy_dt(tbl) |>
        dplyr::filter(eval(filter_statement, envir = as.environment(list(tbl))))

    return(out)
}
