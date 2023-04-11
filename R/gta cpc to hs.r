# Roxygen documentation
#' Convert CPC 2.1 codes into HS 2012 (6-digit level).
#'
#' This function converts vectors of CPC 2.1 codes (3 level) into a vector of 6-digit HS 2012 codes.
#' 2 and 3 digit codes cannot be mixed as interpreting which level is ment can be ambiguous (eg. input of 17 could be "017" or "17" respectively)
#' @usage gta_cpc_to_hs(
#'      codes,
#'      cpc_digits = c(2, 3),
#'      as_list = FALSE,
#'      message = TRUE
#' )
#' @param codes Supply the CPC codes you want to convert. Character and numeric is accepted.
#' @param as_list Indicate whether the output is a vector of uniquely converted codes (False) or as a list with each ... (copy from hs_vintage_converter())
#' @param message If TRUE, print conversion results, including the codes that could not be converted
#' @param cpc_digits If a vector of 2 digit cpc codes is supplied, specify \code{cpc_digits = 2}.
#' @export
gta_cpc_to_hs <- function(codes, as_list = FALSE, message = TRUE, cpc_digits = 3) {
    # check arguments
    gta_logical_check(as_list, is.logical, error_msg = "as_list must be logical (TRUE/FALSE)")
    gta_logical_check(message, is.logical, error_msg = "message must be logical (TRUE/FALSE)")
    gta_parameter_check(cpc_digits, c(2, 3))

    # check that length of cpc codes supplied match the length indicated in cpc_digits
    if (cpc_digits == 3) {
        gta_logical_check(codes, \(x) nchar(x) %in% c(2, 3), error_msg = "{.var codes} must be 2 or 3 digits/characters long")
    } else {
        gta_logical_check(codes, \(x) nchar(x) %in% c(1, 2), error_msg = "{.var codes} must be 1 or 2 digits/characters long")
    }

    # convert cpc codes to string and add potentially leading 0s
    codes <- stringr::str_pad(codes, cpc_digits, pad = "0", side = "left")

    # unique cpc (3 digit) hs code pairs
    cpc_to_hs <- gtalibrary::cpc.to.hs |>
        dplyr::mutate(
            across(everything(), as.character),
            cpc = stringr::str_pad(cpc, width = 3, pad = "0", side = "left")
        ) |>
        dplyr::distinct()

    # adjust cpc hs code pairs for 2 digit cpc codes if cpc_digits = 2 specified in function
    if (cpc_digits == 2) {
        cpc_to_hs <- cpc_to_hs |>
            dplyr::mutate(cpc = stringr::str_sub(cpc, 1, 2))
    }

    # pass to c++ function for conversion
    codes_converted <- cpc_to_hs_cpp(codes = codes, hs_codes = cpc_to_hs$hs, cpc_codes = cpc_to_hs$cpc)
    # print message if message = TRUE
    if (message) {
        # extract non-converted codes
        not_converted <- codes_converted$unconverted

        if (length(not_converted) == 0) {
            cli::cli_alert_success(cli::style_bold("All supplied codes were matched."))
        } else {
            cli::cli_alert_warning(cli::style_bold("Could not match {(length(unique(not_converted)) / length(unique(codes)))*100}% of unique supplied codes: {unique(not_converted)}"), wrap = TRUE)
        }
    }

    # return output
    if (as_list) {
        return(codes_converted$converted)
    } else {
        converted_codes <- codes_converted$converted |>
            unlist() |>
            unique()
        return(converted_codes)
    }
}

# us this?

# cpc_hs_matrix <- readr::read_csv("https://unstats.un.org/unsd/classifications/Econ/tables/CPC/CPCv21_HS12/cpc21-hs2012.txt")
#
# cpc_hs_matrix <- cpc_hs_matrix |>
#     dplyr::select(CPC21code, HS12code) |>
#     dplyr::mutate(
#         HS12code = str_remove(HS12code, "\\."),
#         CPC21code = str_extract(CPC21code, pattern = "\\d{3}")
#     ) |>
#     dplyr::rename(HS_2012 = HS12code, CPC_21 = CPC21code) |>
#     dplyr::distinct()

# Rcpp::sourceCpp("c:\\Users\\sveng\\OneDrive\\Dokumente\\GitHub\\GTA\\gtalibrary_beta\\src\\cpc_to_hs_cpp.cpp")

# microbenchmark::microbenchmark(
#     times = 10,
#     gta_cpc_to_hs(rep(c(10, 12, 13, 14), 1000), as_list = TRUE, cpc_digits = 2, message = TRUE)
# )
