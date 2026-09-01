################################################################################
## The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions #
## Veronica Ballerini, Marina Bottomley, Michelle L. Bell, Francesca Dominici ##
################### Code author: Veronica Ballerini ############################
################### Last modified: January 10, 2026 ############################
## This code modify the Causal ARIMA library to customize the plots ############
################################################################################

library(tidybayes)
plot.cArima <- function(x, type = c("forecast", "impact", "residuals", "custom"), horizon = NULL, ...){
  
  # param checks
  if(class(x) != "cArima") stop ("`x` must be an object of class cArima")
  if(!missing(horizon) && !any(class(horizon) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt")))
    stop("`horizon` must be a Date object")
  if(!all(type %in% c("forecast", "impact", "residuals", "custom")))
    stop("allowed 'type' values are 'forecast', 'impact', 'residuals', and 'custom'")
  if(any(horizon <= x$int.date)) stop("Dates in `horizon` must follow `int.date`")
  
  # Plot "Observed vs Forecast"
  if("forecast" %in% type){
    res <- .forecast(x, horizon = horizon, ...)
    return(res)
  }
  
  # Plot "Causal effect"
  if("impact" %in% type){
    res <- .impact(x, horizon = horizon, ...)
    return(res)
  }
  
  # Residuals plots
  if("residuals" %in% type){
    .residuals(x, ...)
  }
  
  # Residuals plots
  if("custom" %in% type){
    .custom(x, ...)
  }
  
  
}

# -----------------------------------------------------------------------------------------

.impact <- function(cArima, horizon = NULL, alpha = 0.05, color_line="darkblue", color_intervals="slategray2", lines_size=0.6, main=NULL){
  # Settings
  dates <- cArima$dates[!is.na(cArima$causal.effect)]
  int.date <- cArima$int.date
  x <- dates[dates >= int.date]
  y <- na.omit(cArima$causal.effect)
  obs <- cArima$y
  count <- sum(dates < int.date)
  # y.upper <- y + cArima$norm$inf[, "sd.tau"]*qnorm(1-alpha/2)
  # y.lower <- y - cArima$norm$inf[, "sd.tau"]*qnorm(1-alpha/2)
  boot_tau<-matrix(NA,nrow=dim(cArima$boot$boot.distrib)[1],
                   ncol=dim(cArima$boot$boot.distrib)[2])
  for(h in 1:length(x)){
    boot_tau[h,]<-obs[count + h]-cArima$boot$boot.distrib[h,]
  }
  y.upper <- apply(boot_tau,1,function(x) quantile(x,0.975))
  y.lower <- apply(boot_tau,1,function(x) quantile(x,0.025))
  
  # Plot effect
  dat <- data.frame(x = x, y = y, y.upper = y.upper, y.lower = y.lower)
  ylim <- c(min(dat[, "y.lower"]), max(dat[, "y.upper"]))
  
  g <- ggplot(data = dat, aes(x = x)) +  coord_cartesian(ylim = ylim) + labs(title = paste(main, "- Point effect"), y = "", x = "") +
    theme_bw(base_size = 15)+
    geom_ribbon(aes(x = x, ymax = y.upper, ymin = y.lower), fill =color_intervals)+
    geom_hline(yintercept=0, colour = "darkgrey", size = lines_size, linetype = "solid")+
    geom_line(aes(y = y), color = color_line, linetype = "dashed", size = lines_size)
  
  
  # Cumulative plot
  dat_cum<-dat
  dat_cum$y<-cumsum(dat$y)
  dat_cum$y.upper<-cumsum(dat$y.upper)
  dat_cum$y.lower<-cumsum(dat$y.lower)
  
  g_cum <- ggplot(data = dat_cum, aes(x = x))  + labs(title = paste(main, "- Cumulative effect"), y = "", x = "") +
    theme_bw(base_size = 15)+
    geom_ribbon(aes(x = x, ymax = y.upper, ymin = y.lower), fill = color_intervals)+
    geom_hline(yintercept=0, colour = "darkgrey", size = lines_size, linetype = "solid")+
    geom_line(aes(y = y), color =color_line, linetype = "dashed", size = lines_size)
  
  if(!is.null(horizon)){
    g <-g + geom_vline(xintercept = horizon, linetype="dashed", size = lines_size)
    g_cum<-g_cum+ geom_vline(xintercept = horizon, linetype="dashed", size = lines_size)
  }
  
  results<-list(plot=g, cumulative_plot=g_cum)
  return(results)
  
}

# -----------------------------------------------------------------------------------------

.forecast <- function(cArima, horizon = NULL, win = 1, colours=c("darkblue", "black"),
                      fill_colour="slategray2", lines_size=0.6, main = "Forecasted series"){
  
  # Settings
  dates <- cArima$dates[!is.na(cArima$y)]
  int.date <- cArima$int.date
  observed <- na.omit(cArima$y)
  fitted <- cArima$model$fitted
  forecasted <- na.omit(c(fitted, cArima$forecast))
  
  # forecasted_up<-forecasted_inf<-rep(NA, length(na.omit(cArima$model$fitted))) # it breaks with missing values
  forecasted_up<-forecasted_inf<-rep(NA, length(fitted[!is.na(fitted)]))
  forecasted_up<-append(forecasted_up, cArima$forecast_upper)
  forecasted_inf<-append(forecasted_inf, cArima$forecast_lower)
  
  start <- which(dates == int.date) - round(win * sum(dates < int.date))
  end <- length(forecasted)
  x <- dates[start:end]
  
  # Plot
  dat <- data.frame(x = x, forecasted.cut = forecasted[start:end], observed.cut = observed[start:end],
                    forecasted_up=forecasted_up[start:end], forecasted_inf=forecasted_inf[start:end])
  ylim <- c(min(dat[, -1], na.rm = T), max(dat[, -1], na.rm = T))
  
  g <- ggplot(data = dat, aes(x = x, colour = "Legend")) +  coord_cartesian(ylim = ylim) +  theme_bw(base_size = 15)+
    labs(title = main, y = "", x = "") +
    scale_colour_manual(values =colours ) +
    geom_vline(aes(xintercept = int.date, linetype = paste(int.date)), colour = "darkgrey", size = lines_size) +
    scale_linetype_manual(values = "longdash") +
    labs(color="Time series", linetype="Intervention date") +
    guides(colour = guide_legend(order = 1), linetype = guide_legend(order = 2))+
    guides(color=guide_legend(override.aes=list(fill=NA)))+
    geom_lineribbon(aes(y = forecasted.cut, color = "Forecast", ymin = forecasted_inf, ymax = forecasted_up),
                    size = lines_size, linetype ="dashed", fill = fill_colour)+
    geom_line(aes(y = observed.cut, color = "Observed"), size = lines_size)
  
  if(!is.null(horizon)){
    g<-g+ geom_vline(xintercept = horizon, colour = "darkgrey", size = lines_size, linetype = "dotdash")
  }
  
  return(g)
}

# -----------------------------------------------------------------------------------------

.custom <- function(cArima, horizon = as.Date(c("2005-01-01","2009-01-01","2012-01-01","2014-01-01")), 
                    win = 1, colours=c("darkblue", "black"),
                      fill_colour="slategray2", lines_size=0.6, main = "Forecasted series"){
  
  if (is.null(horizon) || length(horizon) < 2 || any(is.na(horizon))) {
    stop("Argument 'horizon' must be a vector of two values (e.g., c(2005, 2012)).")
  }
  # Check int.date
  if (is.null(cArima$int.date) || is.na(cArima$int.date)) {
    stop("cArima$int.date must not be NULL or NA.")
  }
  
  # Settings
  dates <- cArima$dates[!is.na(cArima$y)]
  int.date <- cArima$int.date
  observed <- na.omit(cArima$y)
  fitted <- cArima$model$fitted
  forecasted <- na.omit(c(fitted, cArima$forecast))
  
  # forecasted_up<-forecasted_inf<-rep(NA, length(na.omit(cArima$model$fitted))) 
  # it breaks with missing values
  forecasted_up<-forecasted_inf<-rep(NA, length(fitted[!is.na(fitted)]))
  forecasted_up<-append(forecasted_up, cArima$forecast_upper)
  forecasted_inf<-append(forecasted_inf, cArima$forecast_lower)
  
  start <- which(dates == int.date) - round(win * sum(dates < int.date))
  end <- length(forecasted)
  x <- dates[start:end]
  
  # Plot
  dat <- data.frame(x = x, forecasted.cut = forecasted[start:end], observed.cut = observed[start:end],
                    forecasted_up=forecasted_up[start:end], forecasted_inf=forecasted_inf[start:end])
  ylim <- c(min(dat[, -1], na.rm = T), max(dat[, -1], na.rm = T))
  
  
  dat_long <- tidyr::pivot_longer(
    dat,
    cols = c(observed.cut, forecasted.cut),
    names_to = "Series",
    values_to = "value"
  )
  
  dat_long$Series <- recode(dat_long$Series, "observed.cut" = "Observed", "forecasted.cut" = "Forecasted")
  
  # Pre-evaluation: one row
  periods <- data.frame(
    start = c(int.date, rep(horizon[1], 3)),
    end   = c(horizon[1], horizon[2:4]),
    period = c("Intervention (2002-2004)", "Evaluation (2005-2009)", "Evaluation (2005-2012)", "Evaluation (2005-2014)"),
    type = rep("seg", 4)
  )
  periods$period <- factor(
    periods$period,
    levels = periods$period
  )
  
  # Compute vertical placement for segments
  miny <- min(dat$observed.cut, dat$forecasted_inf, na.rm = TRUE)
  bar_base <- miny - 0.08 * abs(miny)
  bar_sep <- 10#0.25 * abs(miny)
  periods$y <- bar_base - (seq_len(sum(periods$type=="seg"))-1)*bar_sep
  
  period_colors <- c(
    "Intervention (2002-2004)" = "grey80",
    "Evaluation (2005-2009)" = "#E69F00",
    "Evaluation (2005-2012)" = "#56B4E9",
    "Evaluation (2005-2014)" = "#009E73"
  )
  period_fills <- c(
    "Intervention (2002-2004)" = NA,
    "Evaluation (2005-2009)" = NA,
    "Evaluation (2005-2012)" = NA,
    "Evaluation (2005-2014)" = NA
  )
  
  g <- ggplot() +
    # SEGMENTS (periods)
    geom_segment(
      data = periods,
      aes(x = start, xend = end, y = y, yend = y, color = period),
      linewidth = 3.5
    ) +
    scale_color_manual(
      name = "Period",
      values = period_colors
    ) +
    # This is the key - show legend as horizontal segment, not line or box
    guides(
      color = guide_legend(
        override.aes = list(
          x = 1, xend = 2, y = 0, yend = 0,  # short horizontal segment
          linewidth = 3,
          linetype = "solid"
        )
      )
    ) +
    geom_ribbon(
      data = dat,
      aes(x = x, ymin = forecasted_inf, ymax = forecasted_up),
      fill = "blue", alpha = 0.18
    ) +
    ggnewscale::new_scale_color() +
    
    # SERIES (lines)
    geom_line(
      data = dat_long,
      aes(x = x, y = value, color = Series, linetype = Series),
      linewidth = .7
    ) +
    scale_color_manual(
      name = "Series",
      values = c("Observed" = "black", "Forecasted" = "blue")
    ) +
    scale_linetype_manual(
      name = "Series",
      values = c("Observed" = "solid", "Forecasted" = "dashed")
    ) +
    guides(
      color = guide_legend(override.aes = list(linewidth = .7)),
      linetype = guide_legend()
    ) +
    
    labs(x = "", y = "CO2 Emissions (MMT)") +
    theme_bw(base_size = 15) +
    theme(legend.position = "right")
  
  # # Data for shaded periods
  # shade_df <- data.frame(
  #   xmin = c(int.date, horizon[1]),
  #   xmax = c(horizon[1], horizon[2]),
  #   ymin = -Inf,
  #   ymax = Inf,
  #   Shade = factor(
  #     c("Pre-eval period", "Evaluation period"),
  #     levels = c("Pre-eval period", "Evaluation period")
  #   )
  # )
  # 
  # # Build plot
  # g <- ggplot(dat, aes(x = x)) +
  #   # Plot the shaded periods (rectangles at back)
  #   geom_rect(
  #     data = shade_df,
  #     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Shade),
  #     alpha = 0.3, color = NA, inherit.aes = FALSE
  #   ) +
  #   # --- ADD THIS: confidence band for forecast
  #   geom_lineribbon(
  #     aes(
  #       y = forecasted.cut,
  #       ymin = forecasted_inf,
  #       ymax = forecasted_up
  #     ),
  #     fill = fill_colour,              # Your chosen fill color, e.g. "blue"
  #     alpha = 0.4,
  #     color = NA                       # No outline for the ribbon
  #   ) +
  #   # observed series
  #   geom_line(aes(y = observed.cut, color = "Observed series", linetype = "Observed series"), size = .7) +
  #   # forecast series
  #   geom_line(aes(y = forecasted.cut, color = "Forecasted series", linetype = "Forecasted series"), size = .7) +
  #   # Manual scales (for lines)
  #   scale_color_manual(
  #     name = "Series",
  #     values = c(
  #       "Observed series" = "black",
  #       "Forecasted series" = "blue"
  #     )
  #   ) +
  #   scale_linetype_manual(
  #     name = "Series",
  #     values = c(
  #       "Observed series" = "solid",
  #       "Forecasted series" = "dashed"
  #     )
  #   ) +
  #   # Manual fill scale for the rectangles, label for periods
  #   scale_fill_manual(
  #     name = "Period",
  #     values = c(
  #       "Pre-eval period" = "grey70",
  #       "Evaluation period" = "green4"
  #     ),
  #     labels = c(
  #       "Pre-eval period" = "2002-2004",
  #       "Evaluation period" = "Evaluation period: 2005-2009"
  #     )
  #   ) +
  #   labs(
  #     x = "Year",
  #     y = "CO2 Emissions (Mt)"
  #   ) +
  #   theme_bw(base_size = 15) +
  #   # Combine all legends into one, optional ordering
  #   guides(
  #     color = guide_legend(order = 1),
  #     linetype = guide_legend(order = 1),
  #     fill = guide_legend(order = 2)
  #   )

  
  return(g)
}

# -----------------------------------------------------------------------------------------

qqplot.data <- function(vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  colnames(d) <- "resids"
  
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)
  
}

.residuals <- function(cArima, max_lag=30){
  # Standardized residuals
  std.res <- scale(cArima$model$residuals)
  # Acf and Pacf
  ACF<-ggAcf(std.res, lag.max	=max_lag)+ ggtitle("Autocorrelation Function") +theme_bw(base_size = 15)
  PACF<-ggPacf(std.res, lag.max	=max_lag)+ ggtitle("Partial Autocorrelation Function") +  theme_bw(base_size = 15)
  
  # Normal QQ plot
  QQ_plot<-qqplot.data(std.res)+ggtitle("Normal Q-Q Plot") +
    xlab("Theoretical Quantiles") + ylab("Sample Quantiles")+theme_bw(base_size = 15)
  
  # return plots
  results<-list(ACF=ACF,PACF=PACF, QQ_plot=QQ_plot)
  return(results)
}
