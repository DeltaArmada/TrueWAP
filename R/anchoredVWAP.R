#' Title anchoredVWAP
#'
#' @description Calculates Anchored Volume-Weighted Average Price (VWAP)
#'
#' @param HLC3 Vector of High, Low, Close Average Values
#' @param volume Vector of Volume values
#' @param period Vector of bars since start of fixed period
#'
#' @return Vector of Anchored VWAP values
#' @export
#'
#' @examples
#' data(nikkei)
#' anchoredVWAP(
#' HLC3 = nikkei$HLC3
#' , volume = nikkei$Volume
#' , period = nikkei$bars_since_segment
#' )
anchoredVWAP <- function(
    HLC3
    , volume
    , period
) {

  VWAP_period_numer <- rollapply(
    HLC3*volume
    , width = period
    , FUN = sum
    , align = "right"
    , fill = NA
  )

  sum_vol_period <- rollapply(
    volume
    , width = period
    , FUN = sum
    , align = "right"
    , fill = NA
  )

  VWAP_period <- VWAP_period_numer / sum_vol_period

  return(
    VWAP_period
  )

}
