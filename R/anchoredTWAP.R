#' Title anchoredTWAP
#'
#' @description Calculates Anchored Time-Weighted Average Price (TWAP)
#'
#' @param OHLC Data frame object with Open, High, Low, & Close fields
#' @param period Vector of bars since start of fixed period
#'
#' @return Vector of Anchored TWAP values
#' @export
#'
#' @examples
#' data(nikkei)
#' anchoredTWAP(
#' OHLC = nikkei$OHLC
#' , period = nikkei$bars_since_segment
#' )
anchoredTWAP <- function(
    OHLC
    , period
) {

  return(
    rollapply(
      OHLC
      , width = period
      , FUN = mean
      , align = "right"
      , fill = NA
    )
  )

}
