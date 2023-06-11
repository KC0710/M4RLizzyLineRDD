research_period <- function(metNorm, bkdates, T0, margin){
  l <- T0 - weeks(margin)
  u <- T0 + weeks(margin)
  
  # selection of c_l and c_u as in report
  valid_lb <- if (sum(bkdates <= (l - weeks(8))) == 0) {min(metNorm$date)} 
  else { max(bkdates[bkdates <= (l - weeks(8))]) }
  valid_ub <- if (sum(bkdates >= (u + weeks(8))) == 0) {max(metNorm$date)} else { min(bkdates[bkdates >= (u + weeks(8))]) }
  return(c(valid_lb, valid_ub))
}


changeInMargin <- function(metNorm, T0, wks=12){
  d <- ymd("2021-07-01") # limit earliest date of time series
  df <- metNorm[metNorm$date >= d, ]
  
  cp <- breakpoints(Value ~ trend, h=30, data=df) # minimum 30 days between segments
  bps <- cp$breakpoints
  bkdates <- df$date[bps]
  
  changeinmargin <- vector(length=wks+1)
  lrp <- vector(length=wks+1)
  urp <- vector(length=wks+1)
  for(m in 0:wks){
    leftmargin <- T0 - weeks(m)
    rightmargin <- T0 + weeks(m)
    window <- as.POSIXct(seq(leftmargin, rightmargin, by="day"))
    changeinmargin[m+1] <- any(bkdates %in% window)
    rp <- research_period(df, bkdates, T0, m)
    lrp[m+1] <- format(rp[1], "%Y-%m-%d")
    urp[m+1] <- format(rp[2], "%Y-%m-%d")
  }
  response <- cbind(df[1:(wks+1),] %>% dplyr::select(Site, Pollutant, Classification),
                    0:wks,
                    changeinmargin,
                    lrp,
                    urp)
  colnames(response)[4:5] <- c("Weeks", "Response", "LB", "UB")
  return(list(response = response, cp=cp))
}

# RDD estimation ----------------------------------------------------------

genmoddat <- function(metNorm, T0, margin, lb, ub, L, donut=TRUE){
  
  leftmargin <- as.POSIXct(T0 - weeks(margin))
  rightmargin <- as.POSIXct(T0 + weeks(margin))
  
  d <- ymd("2021-07-01")
  metNorm <- metNorm[metNorm$date >= d, ]
  
  if(donut){
    df <- metNorm[((metNorm$date <= ub) & (rightmargin <= metNorm$date)) |
                    ((metNorm$date <= leftmargin) & (lb <= metNorm$date)), ]
  }else{
    df <- metNorm[((metNorm$date <= ub) & (lb <= metNorm$date)), ]
  }
  
  Y <- df$Value
  intervention_date <- prepData(data.frame(date=T0), add="trend")[1, 2]
  W <- ifelse(df$trend >= intervention_date, 1, 0)
  
  if(sum(W) == 0){
    return(NULL)
  }
  
  Kpost <- ifelse(df$trend >= intervention_date, df$trend - intervention_date + 1, 0)
  moddat <- cbind(Y, W, df$trend, Kpost)
  colnames(moddat) <- c("Y", "W", "trend", "post_trend") 
  
  for (l in 1:L){
    moddat <- cbind(moddat, lag(df$Value, l))
  }
  
  moddat <- na.omit(moddat)
  moddat.df <- as.data.frame(moddat)
  return(moddat.df)
}

estimate <- function(metNorm, T0, margin, lb, ub, L, donut=TRUE,
                     parametric=TRUE){
  set.seed(0121)
  
  moddat <- genmoddat(metNorm, T0, margin, lb, ub, L, donut)

  if(is.null(moddat)){
    return(list(effect=NULL, Gamma=NULL))
  }
  
  if (parametric){
    mod <- lm(log(Y) ~ ., data=moddat)
    # stepwise variable selection
    mod <- stepAIC(mod, direciton="backward",
                   scope=list(lower = log(Y) ~ W, upper = log(Y) ~ .))
    smry <- coeftest(mod, vcov. = vcovHAC(mod))
    rmse <- NA
  }else{
    bw <- with(moddat, IKbandwidth(trend, Y, cutpoint=2022.392))
    wts <- with(moddat, kernelwts(trend, center = 2022.392, bw=2*bw))
    moddat$wts <- wts
    mod <- lm(log(Y) ~ . -wts, data = subset(moddat, wts > 0), weights=wts)
    smry <- coeftest(mod, vcov. = vcovHAC(mod))
    preds <- mod$fitted.values
    rmse <- sqrt(sum(( log(subset(moddat, wts > 0) %>% dplyr::select(Y)) - preds )^2))
  }
  
  aic <- stats::AIC(mod)
  bic <- stats::BIC(mod)
  adj.r.squared <- summary(mod)$adj.r.squared
  

  delta1 <- smry["W", "Estimate"]
  std.error1 <- smry["W", "Std. Error"]
  
  lags_in_model <- which(rownames(smry) %in% c("V5", "V6", "V7"))
  num_lags <- sum(rownames(smry) %in% c("V5", "V6", "V7"))
  alpha <- smry[lags_in_model, "Estimate"]
  effect_long_run <- delta1 / (1 - sum(alpha))
  
  #delta1 <- smry[2, 1]
  simdelta <- rnorm(10000, delta1, std.error1)
  if (num_lags == 0){
    mceffects <- simdelta
    lbeffect <- quantile(mceffects, 0.1)
    ubeffect <- quantile(mceffects, 0.9)
    effect <- mean(mceffects)
    
    
    if(lbeffect > 0 & ubeffect > 0){
      significant <- 1
    }else if(lbeffect < 0 & ubeffect < 0){
      significant <- -1
    }else{significant <- 0 }
  
    effect <- as.data.frame(cbind(margin, effect, lbeffect, ubeffect, aic, bic, significant, effect_long_run, adj.r.squared, rmse))
    colnames(effect) <- c("margin", "effect", "lci", "uci", "AIC", "BIC", "significant", "effect_long_run", "adj.r.squared", "rmse")
    
    return(list(effect=effect, Gamma=mceffects))
  }
  
  simalpha1 <- tryCatch(rnorm(10000, smry["V5", "Estimate"], smry["V5", "Std. Error"]),
                        error=function(e) rep(0, times=10000))
  simalpha2 <- tryCatch(rnorm(10000, smry["V6", "Estimate"], smry["V6", "Std. Error"]),
                        error=function(e) rep(0, times=10000))
  simalpha3 <- tryCatch(rnorm(10000, smry["V7", "Estimate"], smry["V7", "Std. Error"]),
                        error=function(e) rep(0, times=10000))
  
  mceffects <- simdelta + simalpha1*simdelta + (simalpha1^2 + simalpha1*simalpha2)*simdelta + (simalpha1^3 + 2*simalpha1*simalpha2 + simalpha3)*simdelta
  lbeffect <- quantile(mceffects, 0.1)
  ubeffect <- quantile(mceffects, 0.9)
  effect <- mean(mceffects)

  if(lbeffect > 0 & ubeffect > 0){
    significant <- 1
  }else if(lbeffect < 0 & ubeffect < 0){
    significant <- -1
  }else{significant <- 0 }

  effect <- as.data.frame(cbind(margin, effect, lbeffect, ubeffect, aic, bic, significant, effect_long_run, adj.r.squared, rmse))
  colnames(effect) <- c("margin", "effect", "lci", "uci", "AIC", "BIC", "significant", "effect_long_run", "adj.r.squared", "rmse")
  
  return(list(effect=effect, Gamma=mceffects))
}

getMetNormPol <- function(metNormList, pollutant){
  plyr::compact(lapply(metNormList,
                       FUN = function(df) { if (df$Pollutant[1] == pollutant) {df} }))
}

getMetNormSitePol <- function(metNormList, site, pollutant){
  plyr::compact(lapply(metNormList,
                       FUN = function(df) { if (df$Pollutant[1] == pollutant &
                                                df$Site[1] == site) {df} }))
}


# Aggregate Effects -------------------------------------------------------

cityestimates <- function(metNormList, pollutant, classification, 
                          effects, mceffects, responsemargin, meanresponse=FALSE, B=1000){
  if(meanresponse){
    metNormPolList <- getMetNormPol(metNormList[responsemargin$Response], pollutant)
  }else{
    metNormPolList <- getMetNormPol(metNormList, pollutant)
  }
  n <- length(metNormPolList)
  responsive_sites <- unlist(responsemargin %>% 
                               filter(Pollutant == pollutant,
                                      Classification == classification,
                                      Response == TRUE) %>% 
                               dplyr::select(Site))

  cityeffect <- vector(length = B)
  
  set.seed(0121)
  for (b in 1:B){
    Sp <- metNormPolList[sample(1:n, n, replace=TRUE)]
    tau <- vector(length = n)
    
    for (s in 1:n){
      site <- Sp[[s]]$Site[1]
      if (site %in% responsive_sites){
        ind <- which((effects$Site == site) & 
                       (effects$Pollutant == pollutant) &
                       (effects$Classification == classification))
        tau[s] <- sample(mceffects[[ind]], 1)
      }else{
        tau[s] <- 0
      }
    }
    
    cityeffect[b] <- mean(tau)
  } 
  
  lci <- quantile(cityeffect, 0.1)
  uci <- quantile(cityeffect, 0.9)
  cityeffect <- mean(cityeffect)
  cityeffect <- data.frame(Pollutant=pollutant, Classification=classification, effect=cityeffect, lci=lci, uci=uci)
  return(cityeffect)
}