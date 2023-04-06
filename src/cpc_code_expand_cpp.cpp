#include <Rcpp.h>
using namespace Rcpp;

/*
gta_code_converter_cpp() is used in gta_hs_vintage_converter() and XX
to speed up the function. Instead of looping over every code that must be converted,
checking if it matches with a vintage hs code and then appending the results to a list in R,
this loop is implemented in c++.
Arguments:
    - ...
Warning:
    The function does not check if the aruments satisfy all necessary conditions (eg. type, size etc.).
    This is implemented in R in gta_hs_vintage_converter() before the arguments are passed to gta_code_converter_cpp()
    Thus, it is not recommended to use this function outside of gta_hs_vintage_converter()
*/

// [[Rcpp::export]]
List cpc_code_expand_cpp(const std::vector<std::string> &cpc_3_digit,
                         const std::vector<std::string> &cpc_2_digit)
{
    // Define objects
    const int n = cpc_3_digit.size();
    const int m = cpc_2_digit.size();
    List results(m);

    // Create HS2 and HS4 codes from code_vintages // create maps!!!
    std::unordered_multimap<std::string, std::string> cpc_conversion_table(n);
    std::vector<std::string> unconverted_codes;
    unconverted_codes.reserve(50); // reserve some memory
    std::vector<std::string> temp_results;
    temp_results.reserve(100);

    // assign values to maps
    for (int i = 0; i < n; i++)
    {
        cpc_conversion_table.insert({cpc_3_digit[i].substr(0, 2), cpc_3_digit[i]});
    }

    // Loop over each code in codes_convert, check if it matches with 2, 4, 6 digit string of codes_vintage and if yes,
    // return the corresponding value in codes_2012. If no, store value in unconverted_codes
    for (int j = 0; j < m; j++)
    {
        auto range = cpc_conversion_table.equal_range(cpc_2_digit[j]);
        if (range.first != range.second)
        {
            for (auto it = range.first; it != range.second; ++it)
            {
                temp_results.push_back(it->second);
            }
        }
        else
        {
            unconverted_codes.push_back(cpc_2_digit[j]);
        }

        results[j] = wrap(temp_results);
        temp_results.clear(); // clear temporary results after they are appended to output list
    }
    return List::create(Named("converted") = results, Named("unconverted") = unconverted_codes);
}