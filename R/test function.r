#' @export
test_function <- function(tbl) {
    # tbl <- tibble::tibble(a = c(1:10), b = c(10:19))

    filter_statement <- ("a %in% c(1, 5, 8) | b == 15")
    out <- tbl |>
        dplyr::filter(!!rlang::parse_expr(filter_statement))

    return(out)
}
