// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// datefunction
List datefunction(const DateVector& start, const DateVector& end, const Date& current_date, const bool current_year_todate);
RcppExport SEXP _gtalibrary_datefunction(SEXP startSEXP, SEXP endSEXP, SEXP current_dateSEXP, SEXP current_year_todateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DateVector& >::type start(startSEXP);
    Rcpp::traits::input_parameter< const DateVector& >::type end(endSEXP);
    Rcpp::traits::input_parameter< const Date& >::type current_date(current_dateSEXP);
    Rcpp::traits::input_parameter< const bool >::type current_year_todate(current_year_todateSEXP);
    rcpp_result_gen = Rcpp::wrap(datefunction(start, end, current_date, current_year_todate));
    return rcpp_result_gen;
END_RCPP
}
// gta_code_converter_cpp
List gta_code_converter_cpp(const std::vector<std::string>& codes_2012, const std::vector<std::string>& codes_vintage, const std::vector<std::string>& codes);
RcppExport SEXP _gtalibrary_gta_code_converter_cpp(SEXP codes_2012SEXP, SEXP codes_vintageSEXP, SEXP codesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::string>& >::type codes_2012(codes_2012SEXP);
    Rcpp::traits::input_parameter< const std::vector<std::string>& >::type codes_vintage(codes_vintageSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::string>& >::type codes(codesSEXP);
    rcpp_result_gen = Rcpp::wrap(gta_code_converter_cpp(codes_2012, codes_vintage, codes));
    return rcpp_result_gen;
END_RCPP
}
// count_interventions_cpp
DataFrame count_interventions_cpp(const DateVector& start, const DateVector& end, const std::string count_by);
RcppExport SEXP _gtalibrary_count_interventions_cpp(SEXP startSEXP, SEXP endSEXP, SEXP count_bySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DateVector& >::type start(startSEXP);
    Rcpp::traits::input_parameter< const DateVector& >::type end(endSEXP);
    Rcpp::traits::input_parameter< const std::string >::type count_by(count_bySEXP);
    rcpp_result_gen = Rcpp::wrap(count_interventions_cpp(start, end, count_by));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_gtalibrary_datefunction", (DL_FUNC) &_gtalibrary_datefunction, 4},
    {"_gtalibrary_gta_code_converter_cpp", (DL_FUNC) &_gtalibrary_gta_code_converter_cpp, 3},
    {"_gtalibrary_count_interventions_cpp", (DL_FUNC) &_gtalibrary_count_interventions_cpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_gtalibrary(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
