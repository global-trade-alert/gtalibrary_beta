# Roxygen documentation

#' GTA in force interventions counter
#'
#' Computes number of interventions in force.
#'
#' @param data requires a data frame obtained by running the \code{gta_data_slicer()}
#' @param counts.by Specify whether to count by month, quarter or year.'.
#' @details all gta_data_slicer parameters are permissible.'.
#' @return Outputs in force interventions by different given periods.

#' @export
gta_cumulative_counts <- function(data, counts.by = "quarter") {
    # check parameters
    gta_parameter_check(tolower(counts.by), c("quarter", "month", "year"), arg_name = "count.by")

    data <- data |>
        dplyr::select(intervention.id, date.implemented, date.removed) |>
        tidyr::replace_na(list(date.removed = Sys.Date())) |>
        tidyr::drop_na() |> # done after filter statement ?
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
        count_by = counts.by
    )
    # should be by intervention id, no ? --> Possible error in this code!
    # this only gives the unique implemented removed combos, but if two interventions
    # have the same combo, the result will be incorrect...
    # base <- unique(subset(master.sliced, select = c("date.implemented", "date.removed")))

    return(results)
}
