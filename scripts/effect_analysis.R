# Preprocessing (now complete) -----------------------------------------------------------
library(here)
library(deweather)
source(here("scripts", "loadRData.R"))

load(here("data", "processed", "site_pol_labels.RData"))

for (i in 1:8){
  assign(paste0("metNorm", i),
         loadRData(here("results", paste0("dwoptim_batch", i, ".RData"))))
}

metNormAll <- c(metNorm1, metNorm2, metNorm3, metNorm4, metNorm5, metNorm6, metNorm7, metNorm8)
labels <- site_pol_labels[1:length(metNormAll), ]

metNormLab <- lapply(1:length(metNormAll),
                      FUN = function(i){
                      df <- prepData(metNormAll[[i]], add="trend")
                      n <- NROW(df)
                      sitepolclass <- as.data.frame(t(replicate(n, unlist(labels[i,], use.names=F))))
                      colnames(sitepolclass) <- names(labels[i, ])
                      cbind(df, sitepolclass)
                    })


# Initialise --------------------------------------------------------------


library(here)
library(deweather)
library(openair)
library(ggplot2)
library(dplyr)
library(strucchange)
library(lubridate)
library(data.table)
library(lmtest)
library(SignifReg)
library(rdrobust)
library(tidyverse)
library(forcats)
library(latex2exp)
library(MASS)
library(rdd)


source(here("scripts", "RDDestimation.R"))


lizzydate <- ymd("2022-05-24")
bondstdate <- ymd("2022-10-24")



# Changepoint detection ---------------------------------------------------

response <- lapply(metNormLab, FUN=function(df) tryCatch(changeInMargin(df, lizzydate), error=function(e) NULL))
resp <- lapply(response, FUN=function(x) x$response)
cpdetect <- lapply(response, FUN=function(x) x$cp) 

if (any(lengths(resp) == 0)){
  rmv <- which(lengths(resp) == 0)
  metNormLab_liz <- metNormLab[-rmv]
  labels_liz <- labels[-rmv, ]
  cpdetect_liz <- cpdetect[-rmv]
}

responsefull_liz <- rbindlist(resp[-rmv])

# group together some classifications
responsefull_liz <- responsefull_liz %>%
  mutate(Classification = fct_recode(Classification,
                                     Background = "Suburban Background",
                                     Background = "Urban Background",
                                     Background = "Suburban",
                                     Roadside = "Urban Traffic"))

metNormLab_liz <- lapply(metNormLab_liz,
                         function(df) {
                           df <- df %>%
                             mutate(Classification = fct_recode(Classification,
                                                                Background = "Suburban Background",
                                                                Background = "Urban Background",
                                                                Background = "Suburban",
                                                                Roadside = "Urban Traffic"))})

labels_liz <- labels_liz %>%
  mutate(Classification = fct_recode(Classification,
                                     Background = "Suburban Background",
                                     Background = "Urban Background",
                                     Background = "Suburban",
                                     Roadside = "Urban Traffic"))

save(responsefull_liz, file=here("results", "responsefull_liz.RData"))
save(metNormLab_liz, file=here("results", "metNormLab_liz.RData"))
save(labels_liz, file=here("data", "processed", "labels_liz"))
save(cpdetect_liz, file=here("results", "cp.RData"))






# Site-pol effect estimation -------------------------------------------------------

load(here("results", "responsefull_liz.RData"))
load(here("results", "metNormLab_liz.RData"))
load(here("data", "processed", "labels_liz"))
#load(here("results", "cp.RData"))

response_ratio <- responsefull_liz %>% group_by(Pollutant, Classification, Weeks) %>%
  summarise(ratio = mean(Response))

theme <- theme(axis.text = element_text(size= rel(1.2)),
               axis.title = element_text(size=rel(1.5),
                                         family="CMU Sans Serif Medium"),
               axis.text.x = element_text(family="CMU Sans Serif Medium",
                                          angle=0,
                                          vjust=0.5),
               axis.ticks.length = unit(0.4, "cm"),
               axis.line = element_line(linewidth=0.5),
               panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(linewidth=0.5,
                                                 colour="gray"),
               panel.background = element_rect(fill="white"),
               strip.text.x = element_text(size=rel(1.5),
                                           family = "CMU Sans Serif Medium"),
               legend.text = element_text(size=rel(1.2),
                                          family="CMU Sans Serif Medium"),
               legend.title = element_text(size=rel(1.2),
                                           family="CMU Sans Serif Medium"))

response_ratio_plot <- ggplot(response_ratio %>% filter(Classification %in% c("Background", "Roadside")),
                              aes(x=Weeks, y=ratio, colour=Pollutant)) +
  geom_line() +
  geom_point() +
  facet_wrap( ~ Classification, ncol=1) + 
  scale_x_continuous(breaks=seq(0,12,1),labels=0:12) + 
  scale_colour_discrete(name = "Pollutant",
                        labels = c(TeX("NO$_2$"),
                                   TeX("NO$_x$"),
                                   TeX("O$_3$"),
                                   TeX("PM$_{10}$"),
                                   TeX("PM$_{2.5}$"))) +
  xlab("Margin (Weeks)") +
  ylab("Response ratio") + 
  theme
    
ggsave(here("figures", "response_ratio_plot.jpg"), response_ratio_plot,
       width=10, height=10)



get_effects <- function(margin, responsefull, metNormLab, labels, T0, 
                        donut=T, parametric=T){
  responsemargin <- responsefull %>% filter(Weeks == margin)
 
  rp <- split(responsemargin[, c("lrp", "urp")],
              seq(nrow(responsemargin[, ]))) # research periods
  effects <- mapply(FUN=function(df, rp) 
    { tryCatch(estimate(df, T0, margin, rp$lrp, rp$urp, L=3, donut, parametric),
               error=function(e) {return(list(effect=NULL, Gamma=NULL))}) },
                    metNormLab,
                    rp)
  
  meaneffects <- rbindlist(effects[1, ])
  sitepoleffects <- cbind(labels[lengths(effects[1, ]) > 0, ], meaneffects)
  
  mceffects = effects[2, ][!lengths(effects[2, ]) == 0]
  
  return(list(speffects = sitepoleffects, mceffects = mceffects))
}

speffects_liz <- list()
sp_mceffects_liz <- list()
minmarg <- 3
maxmarg <- 8
for (margin in minmarg:maxmarg){
  eff <- get_effects(margin, responsefull_liz,
                     metNormLab_liz, labels_liz, lizzydate, donut=T)
  speffects_liz[[margin - minmarg + 1]] <- eff$speffects
  sp_mceffects_liz[[margin - minmarg + 1]] <- eff$mceffects
}
speffects <- rbindlist(speffects_liz)
sp_mceffects <- lapply(sp_mceffects_liz, 
                       function(x) x[lengths(x) != 0])
responsefull_liz_upd <- responsefull_liz %>% 
  rename(margin = Weeks) %>%
  merge(speffects, by = c("Site", "Pollutant", "Classification", "margin")) %>%
  dplyr::select(Site, Pollutant, Classification, margin, Response, lrp, urp) %>%
  rename(Weeks = margin)

# Aggregate effects (w/ donut) -------------------------------------------------------

pols <- c("no2","nox", "pm10", "o3", "pm25")
classif <- c("Background", "Kerbside", "Roadside") 

agg_effects <- function(pols, classif, speffects, sp_mceffects,
                        responsefull, metNormLab, minmarg, maxmarg){
  
  cityeffect <- list()
  meanresponse <- list()
  
  for (mp in minmarg:maxmarg){
    speffects_margin <- speffects %>% filter(margin == mp)
    sp_mceffects_margin <- sp_mceffects[[mp-minmarg+1]]
    responsemargin <- responsefull %>% filter(Weeks == mp)
    for (pol in pols){
      for (cl in classif){
        cityeffect <- append(cityeffect,
                             list(cityestimates(metNormLab, pol, cl,
                                           speffects_margin, sp_mceffects_margin, 
                                           responsemargin, B=1000)))
        meanresponse <- append(meanresponse,
                               list(cityestimates(metNormLab, pol, cl,
                                                  speffects_margin, sp_mceffects_margin, 
                                                  responsemargin, meanresponse=TRUE, B=1000)))
      }
    }
  }
  cityeffect_df <- rbindlist(cityeffect)
  cityeffect_df <- cityeffect_df %>% mutate(mp = rep(minmarg:maxmarg,
                                                     times = rep(15, length(minmarg:maxmarg))))
  
  meanresponse_df <- rbindlist(meanresponse)
  meanresponse_df <- meanresponse_df %>% mutate(mp = rep(minmarg:maxmarg,
                                                         times = rep(15, length(minmarg:maxmarg))))
  rownames(cityeffect_df) <- NULL
  cityeffect <- tibble(cityeffect_df)
  rownames(meanresponse_df) <- NULL
  meanresponse <- tibble(meanresponse_df)
  
  return(list(cityeffect=cityeffect, meanresponse=meanresponse))
}

aggeffects <- agg_effects(pols, classif, speffects, sp_mceffects_liz,
                          responsefull_liz_upd, metNormLab_liz, 3, 8)
cityeffect <- aggeffects$cityeffect
meanresponse <- aggeffects$meanresponse

save(cityeffect, file=here("results", "agg_effect.RData"))
save(meanresponse, file=here("results", "agg_responsive_effect.RData"))
save(speffects, file=here("results", "speffects.RData"))

responsefull_liz_upd %>% group_by(Weeks, Classification, Pollutant) %>%
  summarise(num_resp = sum(Response), total = n()) %>% 
  filter(Weeks == 4, Classification %in% c("Background", "Roadside"))


# Donut Size Selection ----------------------------------------------------
library(kableExtra)
library(reprex)
library(gt)
library(gtsummary)

metric_table <- function(speffects, metric){
  if(metric == "AIC"){
    y <- speffects %>% 
      group_by(Site, Pollutant, Classification, margin) %>%
      summarise(AIC = min(AIC), BIC=min(BIC))
    w <- y %>%
      dplyr::select(-BIC) %>% 
      group_by(Site, Pollutant, Classification) %>%
      slice_min(AIC) %>% 
      group_by(Pollutant, Classification) %>% count(margin)
    
    z <- y %>%
      dplyr::select(-AIC) %>% 
      group_by(Site, Pollutant, Classification) %>%
      slice_min(BIC) %>%
      group_by(Pollutant, Classification) %>% count(margin)
    
    best_margin <- merge(w, z, by=c("Pollutant", "Classification", "margin"),
                         suffixes=c(".AIC", ".BIC"))
    best_margin_major_classif <- best_margin %>% 
      filter(Classification %in% c("Background", "Roadside")) %>%
      pivot_longer(cols = c("n.AIC", "n.BIC"),
                   names_to = c("Metric"),
                   values_to = c("count")) %>%
      pivot_wider(names_from = c(margin, Metric),
                  values_from = count)
    
    best_margin_major_classif[is.na(best_margin_major_classif)] <- 0
    
    actual_colnames <- colnames(best_margin_major_classif)
    desired_colnames <- actual_colnames %>%
      str_remove("(3_n.|4_n.|5_n.|6_n.|7_n.|8_n.)") %>%
      str_to_upper()
    names(desired_colnames) <- actual_colnames
    
    
    best_margin_major_classif_gt <- best_margin_major_classif %>% 
      gt(groupname_col = 'Classification', rowname_col = "Pollutant") %>%
      cols_label(.list = desired_colnames) %>%
      tab_spanner(
        label = md("d=3"),
        columns = 3:4
      ) %>%
      tab_spanner(
        label = md("d=4"),
        columns = 5:6
      ) %>%
      tab_spanner(
        label = md("d=5"),
        columns = 7:8
      ) %>%
      tab_spanner(
        label = md("d=6"),
        columns = 9:10
      ) %>%
      tab_spanner(
        label = md("d=7"),
        columns = 11:12
      ) %>%
      tab_spanner(
        label = md("d=8"),
        columns = 13:14
      ) 
  }else{
    y <- speffects %>% 
      group_by(Site, Pollutant, Classification, margin) %>%
      summarise(rmse=min(rmse))
    w <- y %>%
      group_by(Site, Pollutant, Classification) %>%
      slice_min(rmse) %>% 
      group_by(Pollutant, Classification) %>% count(margin)
    
    best_margin_major_classif <- w %>% 
      filter(Classification %in% c("Background", "Roadside")) %>%
      pivot_longer(cols = "n",
                   names_to = c("Metric"),
                   values_to = c("count")) %>%
      pivot_wider(names_from = c(margin, Metric),
                  values_from = count)
    
    best_margin_major_classif[is.na(best_margin_major_classif)] <- 0
    
    actual_colnames <- colnames(best_margin_major_classif)
    desired_colnames <- actual_colnames %>%
      str_remove("(3_n.|4_n.|5_n.|6_n.|7_n.|8_n.)") %>%
      str_to_upper()
    names(desired_colnames) <- actual_colnames
    
    
    best_margin_major_classif_gt <- best_margin_major_classif %>% 
      gt(groupname_col = 'Classification', rowname_col = "Pollutant") %>%
      cols_label(.list = desired_colnames) %>%
      tab_spanner(
        label = md("d=3"),
        columns = 3
      ) %>%
      tab_spanner(
        label = md("d=4"),
        columns = 4
      ) %>%
      tab_spanner(
        label = md("d=5"),
        columns = 5
      ) %>%
      tab_spanner(
        label = md("d=6"),
        columns = 6
      ) %>%
      tab_spanner(
        label = md("d=7"),
        columns = 7
      ) %>%
      tab_spanner(
        label = md("d=8"),
        columns = 8
      ) 
  }
}

donut_size_tbl <- metric_table(speffects, "AIC")

as.character(as_latex(donut_size_tbl))

save(donut_size_tbl, file=here("results", "best_margin_major_classif_gt.RData"))



# No donut ----------------------------------------------------------------

speffects_liz_nodonut <- list()
sp_mceffects_liz_nodonut <- list()

eff <- get_effects(margin=0, responsefull_liz,
                     metNormLab_liz, labels_liz, lizzydate, donut=FALSE)
speffects_liz_nodonut <- eff$speffects
sp_mceffects_liz_nodonut <- eff$mceffects

speffects_nodonut <- tibble(speffects_liz_nodonut)
responsefull_liz_upd_nodonut <- responsefull_liz[responsefull_liz$Site %in% speffects_nodonut$Site, ]                
       


# Nonparametric estimates -------------------------------------------------

speffects_liz_np <- list()
sp_mceffects_liz_np <- list()

minmarg <- 3
maxmarg <- 8

for(margin in minmarg:maxmarg){
  eff <- get_effects(margin, responsefull_liz,
                     metNormLab_liz, labels_liz, lizzydate, donut=T, parametric=F)
  speffects_liz_np[[margin-minmarg+1]] <- eff$speffects
  sp_mceffects_liz_np[[margin-minmarg+1]] <- eff$mceffects
}

speffects_np <- rbindlist(speffects_liz_np)
sp_mceffects_np <- lapply(sp_mceffects_liz_np, 
                       function(x) x[lengths(x) != 0])
responsefull_liz_upd_np <- responsefull_liz %>% 
  rename(margin = Weeks) %>%
  merge(speffects_np, by = c("Site", "Pollutant", "Classification", "margin")) %>%
  dplyr::select(Site, Pollutant, Classification, margin, Response, lrp, urp) %>%
  rename(Weeks = margin)

aggeffects_np <- agg_effects(pols, classif, speffects_np, sp_mceffects_np,
                             responsefull_liz_upd_np, metNormLab_liz, minmarg, maxmarg)

cityeffect_np <- aggeffects_np$cityeffect
meanresponse_np <- aggeffects_np$meanresponse

donut_size_np_tbl <- metric_table(speffects_np, "rmse")

save(cityeffect_np, file=here("results", "agg_effects_np.RData"))
save(meanresponse_np, file=here("results", "agg_responsive_effects_np.RData"))
save(speffects_np, file=here("results", "speffects_np.RData"))
save(donut_size_np_tbl, file=here("results", "best_margin_tbl.RData"))

responsefull_liz_upd_np %>% group_by(Weeks, Classification, Pollutant) %>%
  summarise(num_resp = sum(Response), total = n()) %>% 
  filter(Weeks == 3, Classification %in% c("Background", "Roadside"))



# Bond Street Opening -----------------------------------------------------

response_bondst <- lapply(metNormLab, FUN=function(df) tryCatch(changeInMargin(df, bondstdate), error=function(e) NULL))
resp <- lapply(response_bondst, FUN=function(x) x$response)

if (any(lengths(response_bondst) == 0)){
  rmv <- which(lengths(response_bondst) == 0)
  metNormLab_bondst <- metNormLab[-rmv]
  labels_bondst <- labels[-rmv, ]
}

responsefullbondst <- rbindlist(resp)
save(responsefullbondst, file=here("results", "responsefullbondst.RData"))
save(metNormLab_bondst, file=here("results", "metNormLab_bondst.RData"))

# Response ratio (BondSt) -------------------------------------------------------

load(here("results", "responsefullbondst.RData"))
load(here("results", "metNormLab_bondst.RData"))

response_ratio_bondst <- responsefullbondst %>% group_by(Pollutant, Classification, Weeks) %>%
  summarise(ratio = mean(Response))

response_ratio_bondst_plot <- ggplot(response_ratio_bondst %>% filter(Classification %in% c("Urban Background", "Roadside")),
                                      aes(x=Weeks, y=ratio, colour=Pollutant)) +
  geom_line() +
  geom_point() +
  facet_wrap( ~ Classification, ncol=1) + 
  scale_x_continuous(breaks=seq(0,12,1),labels=0:12) + 
  scale_colour_discrete(name = "Pollutant",
                        labels = c(TeX("NO$_2$"),
                                   TeX("NO$_x$"),
                                   TeX("O$_3$"),
                                   TeX("PM$_{10}$"),
                                   TeX("PM$_{2.5}$"))) +
  xlab("Margin (Weeks)") +
  ylab("Response ratio") + 
  theme

ggsave(here("figures", "response_ratio_bondst_plot.jpg"), response_ratio_bondst_plot,
       width=10, height=10)

