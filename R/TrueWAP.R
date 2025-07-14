#' Title TrueWAP
#'
#' @description Calculates True Range-Weighted Average Price (TrueWAP)
#'
#' @param high Vector of High Values
#' @param low Vector of Low Values
#' @param close Vector of Close Values
#' @param true_range Vector of True Range Values
#' @param period Rolling window length
#'
#' @return Vector of TrueWAP values
#' @export
#' @import TTR
#' @importFrom TTR ATR
#'
#' @examples
#' data(nikkei)
#' TrueWAP(
#' high = nikkei$High
#' , low = nikkei$Low
#' , close = nikkei$Close
#' , true_range = nikkei$tr
#' , period = 50)
TrueWAP <- function(
    high
    , low
    , close
    , true_range
    , period
) {

  lags_1 <- c(NA, lags(close, n = 1)[,2])

  max_val <- ifelse(
    lags_1 > high
    , lags_1
    , high
  )
  min_val <- ifelse(
    lags_1 < low
    , lags_1
    , low
  )
  true_mid <- (max_val + min_val) / 2

  return(runSum(true_mid * true_range, period) / runSum(true_range, period))

}
