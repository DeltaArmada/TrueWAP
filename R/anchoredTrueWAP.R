#' Title anchoredTrueWAP
#'
#' @description Calculates Anchored True Range-Weighted Average Price (TrueWAP)
#'
#' @param high Vector of High Values
#' @param low Vector of Low Values
#' @param close Vector of Close Values
#' @param true_range Vector of True Range Values
#' @param period Vector of bars since start of fixed period
#'
#' @return Vector of Anchored TrueWAP values
#' @export
#' @importFrom zoo rollapply
#'
#' @examples
#' data(nikkei)
#' anchoredTrueWAP(
#' high = nikkei$High
#' , low = nikkei$Low
#' , close = nikkei$Close
#' , true_range = nikkei$tr
#' , period = nikkei$bars_since_segment
#' )
anchoredTrueWAP <- function(
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

  hilo_mid <- (high + low) / 2
  mid_val <- ifelse(
    period == 1
    , hilo_mid
    , true_mid
  )
  range_val <- ifelse(
    period == 1
    , high - low
    , true_range
  )

  truewap_period_numer <- rollapply(
    mid_val*range_val
    , width = period
    , FUN = sum
    , align = "right"
    , fill = NA
  )

  truewap_period_denom <- rollapply(
    range_val
    , width = period
    , FUN = sum
    , align = "right"
    , fill = NA
  )

  TrueWAP_period <- truewap_period_numer / truewap_period_denom

  return(
    TrueWAP_period
  )

}
