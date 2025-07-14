\name{nikkei}
\alias{nikkei}
\docType{data}
\title{
nikkei
}
\description{
An example data set of OHLCV data for Nikkei 225 (Osaka), Active Daily Continuation
}
\usage{data("nikkei")}
\format{
  A data frame with 4411 observations on the following 27 variables.
  \describe{
    \item{\code{Open}}{a numeric vector}
    \item{\code{High}}{a numeric vector}
    \item{\code{Low}}{a numeric vector}
    \item{\code{Close}}{a numeric vector}
    \item{\code{Volume}}{a numeric vector}
    \item{\code{Adjusted}}{a numeric vector}
    \item{\code{OHLC}}{a numeric vector}
    \item{\code{HLC3}}{a numeric vector}
    \item{\code{tr}}{a numeric vector}
    \item{\code{atr}}{a numeric vector}
    \item{\code{trueHigh}}{a numeric vector}
    \item{\code{trueLow}}{a numeric vector}
    \item{\code{segment}}{a Date}
    \item{\code{Date}}{a Date}
    \item{\code{FirstRowNumSegment}}{a numeric vector}
    \item{\code{RowNum}}{a numeric vector}
    \item{\code{bars_since_segment}}{a numeric vector}
    \item{\code{current_std}}{a numeric vector}
    \item{\code{Mature_Days}}{a numeric vector}
    \item{\code{Mature_STD}}{a numeric vector}
    \item{\code{lags_mature_days}}{a numeric vector}
    \item{\code{lags_mature_std}}{a numeric vector}
    \item{\code{current_sma}}{a numeric vector}
    \item{\code{current_adiv}}{a numeric vector}
    \item{\code{Mature_ADIV}}{a numeric vector}
    \item{\code{Current_IV}}{a numeric vector}
    \item{\code{lags_mature_adiv}}{a numeric vector}
  }
}
\examples{
data(nikkei)
## maybe str(nikkei) ; plot(nikkei) ...
}
\keyword{datasets}
