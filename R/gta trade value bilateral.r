#' @export
gta_trade_value_bilateral <- function(data = NULL,
                                      importing.country = NULL,
                                      keep.importer = NULL,
                                      exporting.country = NULL,
                                      keep.exporter = NULL,
                                      cpc.sectors = NULL,
                                      keep.cpc = TRUE,
                                      hs.codes = NULL,
                                      keep.hs = TRUE,
                                      trade.data = "base",
                                      trade.data.path = "data/support tables/good support table for gtalibrary.rds") {
    # check validity of arguments
    gta_parameter_check(trade.data, c("base", as.character(2005:2023)))

    # initialize vector which stores conditional filter statement
    filter_statement <- vector("character")

    # read trade.base data table and filter year of interest if not "base"
    if (trade.data == "base") {
        trade_base <- gtalibrary::trade.base |>
            dplyr::mutate(trade.value = trade.value / 3) |>
            data.table::as.data.table()
    } else if (is.null(data)) {
        trade_base <- readRDS(trade.data.path) |>
            dtplyr::lazy_dt() |>
            dplyr::filter(year == as.numeric(trade.data))
    } else {
        trade_base <- data |>
            dtplyr::lazy_dt() |>
            dplyr::filter(year == as.numeric(trade.data))
    }

    # importer
    if (!is.null(importing.country)) {
        # if importing.country is specified, user must select if countries are kept or deleted in parameter keep.importer
        gta_logical_check(keep.importer, is.logical)
        importers_filter <- gta_un_code_vector(importing.country)

        # append filter statement
        if (keep.importer) {
            filter_statement <- append(filter_statement, "i.un %in% importers_filter")
        } else {
            filter_statement <- append(filter_statement, "!i.un %in% importers_filter")
        }
    }

    # exporter
    if (!is.null(exporting.country)) {
        # if exporting.country is specified, user must select if countries are kept or deleted in parameter keep.exporter
        gta_logical_check(keep.exporter, is.logical)

        # transform specified exporters to UN code (in case country names are provided)
        exporters_filter <- gta_un_code_vector(exporting.country)

        # append filter statement
        if (keep.exporter) {
            filter_statement <- append(filter_statement, "a.un %in% exporters_filter")
        } else {
            filter_statement <- append(filter_statement, "a.un %in% exporters_filter")
        }
    }

    # hs codes
    if (!is.null(hs.codes)) {
        # if hs.codes is specified, user must select if hs codes are kept or deleted in parameter keep.hs
        gta_logical_check(keep.hs, is.logical)
        hs_codes_filter <- gta_hs_code_check(codes = hs.codes, message = FALSE)

        # append filter statement
        if (keep.hs) {
            filter_statement <- append(filter_statement, "hs6 %in% hs_codes_filter") ## check if we need to pad the codes or make them numeric!!!
        } else {
            filter_statement <- append(filter_statement, "!hs6 %in% hs_codes_filter")
        }
    }

    # cpc codes
    if (!is.null(cpc.sectors)) {
        # if cpc.sectors is specified, user must select if cpc codes are kept or deleted in parameter keep.cpc
        gta_logical_check(keep.cpc, is.logical)
        cpc_codes_filter <- gta_cpc_to_hs(cpc.sectors) ## check function!!!

        # append filter statement
        if (is.null(keep.cpc)) {
            filter_statement <- append(filter_statement, "hs6 %in% cpc_codes_filter")
        } else {
            filter_statement <- append(filter_statement, "!hs6 %in% cpc_codes_filter")
        }
    }

    # filter trade base based on filter statement & return reults as data.table
    if (!length(filter_statement) == 0) {
        # filter dataset
        filter_statement <- paste(filter_statement, collapse = " & ")
        out <- trade_base |>
            dplyr::filter(eval(parse(text = filter_statement))) |>
            tibble::as_tibble()
        return(out)
    } else {
        # return unfilterd trade.base if filter statement is empty
        trade_base <- trade_base |>
            tibble::as_tibble()
        return(trade_base)
    }
}
