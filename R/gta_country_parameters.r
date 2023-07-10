#' convert between different country parameters
#'
#' Use this function to convert between different country attributes such as from name to un code or from un code to iso code.
#' Conversion is possible between all parameters recorded in the \code{gtalibrary::country.correspondence} table.

#' @param input a vector which contains the parameters to be converted. The type of parameters recorded in the vector
#' must be specified in the from argument
#' @param to un_code, iso.code.2letter, iso_code, name (+ all other colnames from the dataset gtalibrary::country.correspondence)
#' @param from Same parameters as in to
#' @param keep_input if \code{TRUE}, the function returns a tibble where one column corresponds to the input vector and the second
#' column to the matching parameter specified in \code{to}
#' @param message Specify whether you want to receive a version about the success of conversion
#' @usage
#' gta_country_parameters(
#'      c("Switzerland", "Norway", "Slovenia"),
#'      from = c(name, un_code, iso_code, iso.code.2letter, ...),
#'      to = c(name, un_code, iso_code, iso.code.2letter, ...),
#'      message = TRUE,
#'      keep_input = FALSE
#' )

#' @export
gta_country_parameters <- function(input = NULL, from = NULL, to = NULL, message = TRUE, keep_input = FALSE) {
    # check wheter input is numeric or character (only permissible types)
    # gta_parameter_check(from, colnames(gtalibrary::country.correspondence))
    # gta_parameter_check(to, colnames(gtalibrary::country.correspondence))
    # gta_logical_check(keep_input, is.logical, error_msg = "keep_input must be TRUE/FALSE")

    # from cannot be the same as to
    if (substitute(from) == substitute(to)) {
        cli::cli_abort("{.var from} cannot be the same as {.var to}")
    }

    # conversion matrix (discard Macedonia --> Same un_code as North Macedonia)
    conversion_matrix <- gtalibrary::country.names |>
        dplyr::filter(!name == "Macedonia") |>
        dplyr::select({{ from }}, {{ to }}) |>
        tibble::as_tibble()

    conversion_matrix |>
        dplyr::pull(un_code) |>
        duplicated()

    nrow(conversion_matrix)
    # join and pull converted values
    out <- dplyr::tibble(input) |>
        dplyr::left_join(conversion_matrix, by = dplyr::join_by(input == {{ from }}))

    # non-converted results
    non_converted <- out |>
        dplyr::filter(is.na({{ to }})) |>
        dplyr::pull({{ to }})

    # message
    if (message) {
        if (length(non_converted) == 0) {
            cli::cli_alert_success("All inputs were successfully converted")
        } else if (length(non_converted) != length(input)) {
            cli::cli_alert_danger("Could not convert {(length(non_converted) / length(input)) * 100}% of inputs: {unique(non_converted)}")
        } else {
            cli::cli_alert_warning("WARNING: Could not convert any inputs!")
        }
    }

    # return output
    if (!keep_input) {
        converted <- out |>
            dplyr::pull({{ to }})
    } else {
        converted <- out
    }

    return(converted)
}
