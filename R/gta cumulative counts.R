# Roxygen documentation

#' GTA in force interventions counter
#'
#' Computes number of interventions in force.
#'
#' @param data requires a data frame obtained by running the \code{gta_data_slicer()}
#' @param count_by Specify whether to count by month, quarter or year.
#' @usage gta_cumulative_count(
#'      data,
#'      count_by = c("quarter", "year", "month")
#' )
#' @examples
#' # first, obtain a master dataset (eg. by running the data slicer)
#' master <- gta_data_slicer(affected.jurisdiction = "Switzerland", keep.affected = TRUE, keep.others = FALSE)
#' master |>
#'     gta_cumulative_count(count_by = "year")
#' @return Outputs in force interventions by different given periods.
#' @export
gta_cumulative_count <- function(data, count_by = "quarter") {
    # check parameters
    gta_parameter_check(tolower(count_by), c("quarter", "month", "year"), arg_name = "count_by")

    data <- data |>
        dplyr::select(intervention.id, date.implemented, date.removed) |>
        tidyr::replace_na(list(date.removed = Sys.Date())) |>
        tidyr::drop_na() |>
        dplyr::distinct() |>
        dplyr::mutate(date.removed = dplyr::case_when(
            date.removed > Sys.Date() ~ Sys.Date(),
            TRUE ~ date.removed
        )) |>
        dplyr::filter(date.implemented <= date.removed)

    # calculate results
    results <- count_interventions_cpp(
        start = data$date.implemented,
        end = data$date.removed,
        count_by = count_by
    )

    # modify result output (easier to write in R than in c++)
    if (count_by %in% c("month", "quarter")) {
        results <- results |>
            tidyr::separate(2, into = c("year", count_by), sep = "-", convert = TRUE) |>
            dplyr::arrange(dplyr::desc(year), dplyr::desc(count_by))
    } else {
        results <- results |>
            dplyr::mutate(year = as.numeric(year)) |>
            dplyr::arrange(dplyr::desc(year))
    }

    return(results)
}
