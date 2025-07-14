#' Title TWAP
#'
#' @description Calculates Time-Weighted Average Price (TWAP)
#'
#' @param OHLC Data frame object with Open, High, Low, & Close fields
#' @param period Rolling window length
#'
#' @return Vector of TWAP values
#' @export
#'
#' @examples
#' data(nikkei)
#' TWAP(nikkei$OHLC, 50)
TWAP <- function(
    OHLC
    , period
) {

  return(SMA(OHLC, n = period))

}
