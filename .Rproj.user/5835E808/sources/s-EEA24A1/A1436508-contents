#' Find start date of permanent snow in a winter
#'
#' This function uses a time series of snow depth to find the beginning
#' of the permanent snow in a given winter. This is based on calculating the
#' minimum amount of snow in a window of a given temporal length to check if,
#' after a given snow event, there was melting and the snow vanished.
#'
#' @param date vector. Date of the weather records.
#' @param snow_depth vector. Snow depth values.
#' @param plot logical. Whether the plot of the start date of permanent snow should be plotted
#' or not. Default is `TRUE`.
#' @param temp.window.size numeric. Size of the temporal window to calculate the minimum
#' amount of snow, in days. Default is 14 days.
#' @param endrule character. String indicating how the values at the beginning and
#' the end, of the array, should be treated. Default is "min". For more options,
#' see the function \code{\link[caTools]{runmin}}).
#' @param align character. How should the temporal window be aligned?
#' Default is "left". For more information, see \code{\link[caTools]{runmin}}).
#' @param set_start POSIX object. Date and time of the start of the winter.
#' Used mainly for debugging, to test other dates. Default is NULL.
#' @param debug logical. If debugging, plot also the snow depth values after
#' minimum filtering. Default is FALSE.
#' @param ...
#'
#' @return Date object. Start date of the permanent snow in a winter.
#'
#' @example examples/find_date_permanent_snow_example.R
#'
#' @export
find_date_permanent_snow <- function(date, snow_depth, plot = TRUE,
                                     temp.window.size = 14,
                                     endrule = c("min", "NA", "trim", "keep", "constant", "func")[1],
                                     align = c("left", "center", "right")[1],
                                     set_start = NULL, debug = FALSE, ...) {

  if(is.null(set_start)) {
    # calculate minimum snow depth in a temporal window of defined size
    min.given.window <- caTools::runmin(snow_depth, k = temp.window.size,
                                        endrule = endrule, align = align, ...)

    # get the date of the first day before permanent snow
    start <- date[first(which(cumsum(min.given.window) > 0))]
  } else {
    start <- set_start
  }

  if(plot == TRUE) {
    # plot
    plot(date, snow_depth, type = "l",
         xlab = "Date", ylab = "Snow depth (mm)")
    if(debug) lines(date, min.given.window, type = "l", col = 2)
    abline(v = start, lty = 2)
    legend("topleft", legend = "start of permanent snow", lty = 2)
  }

  # return the start of the first snow
  return(start)
}
