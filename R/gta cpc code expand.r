# Roxygen documentation

#' Expands 2-digit level CPC codes into 3-digit level CPC codes.
#'
#' This function takes in 2-digit level CPC codes and returns all corresponding 3-digit level CPC codes.
#' Input can be either numeric or character. Results are returned as characters
#' @usage gta_cpc_code_expand(
#'      codes,
#'      as_list = FALSE,
#'      message = TRUE
#' )
#' @param codes Supply the 2-digit level CPC codes you want to expand.
#' @param as_list indicate whether the converted results are returned as a list or as a vector of unique results (default = FALSE)
#' @param message indicate whether conversion results are printed to the console or not (default = TRUE)

#' @export
gta_cpc_code_expand <- function(codes, as_list = FALSE, message = TRUE) {
    # prepare 3 digit codes
    cpc_names <- gtalibrary::cpc.names |>
        dplyr::filter(cpc.digit.level == 3) |>
        dplyr::select(cpc) |>
        dplyr::mutate(cpc = stringr::str_pad(cpc, width = 3, pad = "0", side = "left"))

    # pass data frame to c++ function for vectorization
    codes_converted <- cpc_code_expand_cpp(cpc_2_digit = codes, cpc_3_digit = cpc_names$cpc)

    # print message if message = TRUE
    if (message) {
        # extract non-converted codes
        not_converted <- codes_converted$unconverted

        if (length(not_converted) == 0) {
            cli::cli_alert_success(cli::style_bold("All supplied codes were matched."))
        } else {
            cli::cli_alert_warning(cli::style_bold("Could not match {length(unique(not_converted)) / length(unique(codes))}% of unique supplied codes: {unique(not_converted)}"), wrap = TRUE)
        }
    }

    # return results
    if (as_list) {
        return(codes_converted)
    } else {
        codes_converted <- codes_converted |>
            unlist() |>
            unique()
        return(codes_converted)
    }
}
