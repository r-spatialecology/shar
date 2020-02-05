#include <Rcpp.h>
using namespace Rcpp;

//' rcpp_sample
//'
//' @description Rcpp sample function
//'
//' @param x Vector of elements to sample from.
//' @param n Size of the sample.
//' @param replace Sample with replacement.
//'
//' @details
//' \code{Rcpp} implementation of the \code{sample} function.
//'
//' @seealso
//' \code{\link{sample}}
//'
//' @return vector
//'
//' @name rcpp_sample
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_sample(Rcpp::NumericVector x, int n, bool replace = false) {

  return Rcpp::sample(x, n, replace);

}

/*** R
rcpp_sample(x = 1:100, n = 5)
rcpp_sample(x = 1:5, n = 10, replace = TRUE)

bench::mark(rcpp_sample(x = 1:100, n = 5),
            sample(x = 1:100, size = 5),
            check = FALSE, relative = TRUE, iterations = 10000)

bench::mark(rcpp_sample(x = 1:10, n = 20, replace = TRUE),
            sample(x = 1:10, size = 20, replace = TRUE),
            check = FALSE, relative = TRUE, iterations = 10000)
*/
