// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// Ccalc_mROC_stats
std::vector<double> Ccalc_mROC_stats(NumericVector M, NumericVector Y);
RcppExport SEXP _predtools_Ccalc_mROC_stats(SEXP MSEXP, SEXP YSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type M(MSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Y(YSEXP);
    rcpp_result_gen = Rcpp::wrap(Ccalc_mROC_stats(M, Y));
    return rcpp_result_gen;
END_RCPP
}
// Csimulate_null_mROC_stats_unconditional
NumericMatrix Csimulate_null_mROC_stats_unconditional(NumericVector M, int n_sim);
RcppExport SEXP _predtools_Csimulate_null_mROC_stats_unconditional(SEXP MSEXP, SEXP n_simSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type M(MSEXP);
    Rcpp::traits::input_parameter< int >::type n_sim(n_simSEXP);
    rcpp_result_gen = Rcpp::wrap(Csimulate_null_mROC_stats_unconditional(M, n_sim));
    return rcpp_result_gen;
END_RCPP
}