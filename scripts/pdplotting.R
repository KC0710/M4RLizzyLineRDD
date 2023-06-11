plotPD <- function(dat, variable, ylim = NULL, plotit = TRUE,
                   intervals = 40,
                   auto.text = TRUE, ...) {

  ## make sure variable is character
  variable <- as.character(variable)
  
  ## variables to be treated specifically
  special <- c("trend", "weekday")
  
  ## title for plot
  title <- variable
  
  ## Args setup
  Args <- list(...)
  
  Args$ylab <- if ("ylab" %in% names(Args)) {
    openair::quickText(Args$ylab, auto.text)
  } else {
    latex2exp::TeX("Conc. ($ \\mu g {m}^{-3}$)")
  }
  
  ## check if variable present
  if (!variable %in% dat$var) stop("Variable not present in data.")
  
  ## select variable of interest
  dat <- dat[dat$var == variable, ]
  
  # if type is numeric
  if (dat$var_type[1] == "numeric") {
    dat$x <- as.numeric(dat$x)
    gap <- prettyGap(dat$x, intervals)
    dat$x <- round_any(dat$x, gap)
  }
  
  dat <- dplyr::group_by(dat, .data$var, .data$var_type, .data$x) %>%
    dplyr::summarise(
      mean = mean(.data$mean),
      lower = min(.data$lower),
      upper = max(.data$upper)
    )
  
  if (is.null(ylim)) ylim <- rng(dat)
  

  if (!variable %in% special && is.numeric(dat$x)) {
    #quants <- data.frame(x = stats::quantile(dat[[as.character(variable)]],
                                            # probs = 0:10 / 10, na.rm = TRUE
    #))
    
    plt <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$mean,
                                             ymin = .data$lower, ymax = .data$upper
    )) +
      ggplot2::geom_line(size = 1, col = "tomato") +
      ggplot2::geom_ribbon(alpha = 0.3, fill = "tomato") +
      ggplot2::xlab(openair::quickText(variable)) +
      ggplot2::ylab(openair::quickText(Args$ylab)) +
      ggplot2::coord_cartesian(ylim = ylim) +
      ggplot2::ggtitle(title) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 0.8, face = "bold")) 
      #ggplot2::geom_rug(ggplot2::aes(x = .data$x),
                        #data = quants, sides = "b",
                        #inherit.aes = FALSE, size = 1)
    
    ## better hour x-scaling
    if (variable %in% c("hour", "hour.local")) {
      plt <- plt + ggplot2::scale_x_continuous(breaks = c(0, 6, 12, 18))
    }
  }
  
  if (variable == "trend") {
    dat <- decimalDate(dat, date = "x")
    
    plt <- ggplot2::ggplot(dat, ggplot2::aes(.data$date, .data$mean,
                                             ymin = .data$lower, ymax = .data$upper
    )) +
      ggplot2::geom_line(size = 1, col = "tomato") +
      ggplot2::geom_ribbon(alpha = 0.3, fill = "tomato") +
      ggplot2::xlab(openair::quickText(variable)) +
      ggplot2::ylab(openair::quickText(Args$ylab)) +
      ggplot2::ggtitle(title) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 0.8, face = "bold")) +
      ggplot2::coord_cartesian(ylim = ylim)
  }
  
  ## for factors/character variables
  
  if (!variable %in% special && !is.numeric(dat$x)) {
    dat$x <- factor(dat$x)
    
    plt <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$mean,
                                             ymin = .data$lower, ymax = .data$upper,
                                             xmin = as.numeric(.data$x) - 0.4,
                                             xmax = as.numeric(.data$x) + 0.4
    )) +
      ggplot2::geom_point(size = 2, col = "tomato") +
      ggplot2::geom_rect(alpha = 0.4, fill = "tomato") +
      ggplot2::xlab(openair::quickText(variable)) +
      ggplot2::ylab(openair::quickText(Args$ylab)) +
      ggplot2::ggtitle(title) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 0.8, face = "bold")) +
      ggplot2::ylim(ylim)
  }
  
  
  if (variable == "weekday") {
    ## change to weekday names
    dat$x <- factor(dat$x)
    weekday.names <- format(ISOdate(2000, 1, 2:8), "%a")
    levels(dat$x) <- sort(weekday.names)
    
    dat$x <- ordered(dat$x, levels = weekday.names)
    
    plt <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$mean,
                                             ymin = .data$lower,
                                             ymax = .data$upper,
                                             xmin = as.numeric(.data$x) - 0.4,
                                             xmax = as.numeric(.data$x) + 0.4
    )) +
      ggplot2::geom_point(size = 2, col = "tomato") +
      ggplot2::geom_rect(alpha = 0.4, fill = "tomato") +
      ggplot2::xlab(openair::quickText(variable)) +
      ggplot2::ylab(openair::quickText(Args$ylab)) +
      ggplot2::ggtitle(title) +
      ggplot2::theme(plot.title = ggplot2::element_text(lineheight = 0.8, face = "bold")) +
      ggplot2::ylim(ylim)
  }
  
  if (plotit) print(plt)
  
  invisible(list(plot = plt, data = dat))
}


plotAllPD <- function(dat, ylim = NULL, nrow = NULL, ...) {
  
  ## plot everything
  plots <- lapply(unique(dat$var), plotPD,
                  dat = dat,
                  ylim = ylim, plotit = FALSE, ...
  )
  
  # extract first element of list, which is the plot
  thedata <- sapply(plots, "[", 2)
  plots <- sapply(plots, "[", 1)
  
  do.call(gridExtra::grid.arrange, c(plots, nrow = nrow))
  
  invisible(list(plot = plots, data = thedata))
}




#' y range taking account of expanded uncertainties
#' @noRd
rng <- function(x) {
  ## if no CI information, just return
  if (all(is.na(x[, c("lower", "upper")]))) {
    lims <- NULL
    return(lims)
  }
  
  
  lims <- range(c(x$lower, x$upper), na.rm = TRUE)
  inc <- 0.04 * abs(lims[2] - lims[1])
  lims <- c(lims[1] - inc, lims[2] + inc)
  
  lims
}

decimalDate <- function(x, date = "date") {
  thedata <- x
  x <- x[[date]]
  x.year <- floor(x)
  ## fraction of the year
  x.frac <- x - x.year
  ## number of seconds in each year
  x.sec.yr <- unclass(ISOdate(x.year + 1, 1, 1, 0, 0, 0)) -
    unclass(ISOdate(x.year, 1, 1, 0, 0, 0))
  ## now get the actual time
  x.actual <- ISOdate(x.year, 1, 1, 0, 0, 0) + x.frac * x.sec.yr
  x.actual <- as.POSIXct(trunc(x.actual, "hours"), "GMT")
  thedata$date <- x.actual
  thedata
}

#' @noRd
prettyGap <- function(x, n = 100) {
  return(diff(pretty(x, n))[1])
}

#' @noRd
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}