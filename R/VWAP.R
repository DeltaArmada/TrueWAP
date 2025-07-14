#' Title VWAP
#'
#' @description Calculates Volume-Weighted Average Price (VWAP)
#'
#' @param HLC3 Vector of High, Low, Close Average Values
#' @param volume Vector of Volume values
#' @param period Rolling window length
#'
#' @return Vector of VWAP values
#' @export
#'
#' @examples
#' data(nikkei)
#' VWAP(nikkei$HLC3, nikkei$Volume, 50)
VWAP <- function(
    HLC3
    , volume
    , period
) {

  return(runSum(HLC3 * volume, period) / runSum(volume, period))

}
