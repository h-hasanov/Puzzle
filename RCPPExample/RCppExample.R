library(Rcpp)
dyn.load("D:/RQuantLib.dll")

advance1 <- function(calendar, amount, unit, bdcVal, emr, dates) {
  
  .Call("_RQuantLib_advance1", calendar, amount, unit, bdcVal, emr, dates)
  
}

advance <- function(calendar="TARGET", dates=Sys.Date(),
                    n, timeUnit, # call 1
                    period,      # call 2
                    bdc = 0, emr = 0) {
  stopifnot(is.character(calendar))
  stopifnot(class(dates)=="Date")
  call1 <- missing(period) && !missing(n) && !missing(timeUnit)  
  call2 <- !missing(period) && missing(n) && missing(timeUnit)
  stopifnot(call1 | call2)
  val <- NULL
  if (call1) {
    val <- advance1(calendar, n, timeUnit, bdc, emr, dates)
  }
  if (call2) {
    val <- advance2(calendar, period, bdc, emr, dates)
  }
  stopifnot( !is.null(val) )
  val
}

advance("UnitedKingdom", Sys.Date(), 3, 0)