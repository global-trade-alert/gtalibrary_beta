# Roxygen documentation

#' Create a vector of UN country codes

#' Use this function to get the UN codes of countries or to check whether UN codes are consistent with the UN codes
#' used by the GTA. The funciton returns a vector of codes based on country or group names. Per default, a vector of all UN codes is returned
#'
#' @usage gta_un_code_vector(countries = "all")
#' @param countries Vector with either country names, country group names or UN codes. Do not mix UN codes with country or group names. Please use function also for UN codes to ensure the ones you supply are consistent with the ones used by the GTA.
#' @return A vector of UN country codes that is consistent with those used by the GTA.
#' @examples 
#' # retreive the UN codes for Switzerland and the United States of America
#' gta_un_code_vector(countries = c("Switzerland", "United States of America"))
#' @export
gta_un_code_vector <- function(countries = "all") {
    # check wheter input is numeric or character (only permissible types)
    gta_logical_check(
        countries,
        \(x) is.numeric(x) | is.character(x),
        error_msg = "countries must either be vector of type character (names) or numeric (un codes)"
    )

    # un-code country name matrix
    country_correspondence <- gtalibrary::country.correspondence

    # if countries are not specified, return all the un codes
    if (all(countries == "all")) {
        countries <- country_correspondence$un_code |>
            unique()
    }

    # check if either (all numeric (un codes) or all string (country names))
    if (is.numeric(countries)) {
        permissible_values <- country_correspondence$un_code |>
            unique()
        gta_parameter_check(countries, permissible_values)
        un_codes <- countries
    } else {
        countries <- countries |>
            tolower()
        permissible_values <- country_correspondence$name |>
            unique() |>
            tolower()

        gta_parameter_check(countries, permissible_values)
        un_codes <- country_correspondence |>
            dplyr::filter(tolower(name) %in% countries) |>
            dplyr::distinct() |>
            dplyr::pull(un_code)
    }

    # return output
    return(un_codes)
}
