#' @export
gta_data_slicer_test <- function(data = NULL,
                                 gta_evaluation = NULL) {
    filter_statement <- vector("character")

    gta_evaluation_filter <- stringr::str_to_title(gta_evaluation) # convert gta.evaluation to format used in dataset
    filter_statement <- append(filter_statement, "gta.evaluation %in% gta_evaluation_filter")

    # filter the data frame for the first time
    filter_statement <- paste(filter_statement, collapse = " & ")
    force(
        data <- dtplyr::lazy_dt(data) |>
            dplyr::filter(rlang::eval_tidy(rlang::parse_expr(filter_statement))) |>
            tibble::as_tibble()
    )
    return(data)
}
