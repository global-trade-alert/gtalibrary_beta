#' @export
gta_data_slicer_test <- function(data = NULL,
                                 gta_evaluation = NULL) {
    filter_statement <- vector("character")

    gta_evaluation_filter <- stringr::str_to_title(gta_evaluation) # convert gta.evaluation to format used in dataset
    filter_statement <- append(filter_statement, "gta.evaluation %in% gta_evaluation_filter")

    # filter the data frame for the first time
    filter_statement <- paste(filter_statement, collapse = " & ")
    tibble::as_tibble(data) |>
        dplyr::filter(eval(parse(text = filter_statement)))

    return(data)
}

test <- data.table::as.data.table(mtcars)

filter_test <- "cyl == 6"

test[eval(parse(text = filter_test))]


a <- deparse(10)
eval(parse(text = a))
substitute(a)
data <- as.data.table(master)

master |> gta_data_slicer_test(gta_evaluation = "amber")
