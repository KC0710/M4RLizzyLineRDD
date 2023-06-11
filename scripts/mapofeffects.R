# Add location data -------------------------------------------------------


load(here("results", "speffects.RData"))
load(here("results", "speffects_np.RData"))
site_full <- read_csv("~/Documents/M4R/03_ELZ_data/01_Site/_00_selected_AQE_KCL_sites_valid_coordinates_DROP_DUPLICATE_2023.csv")
site_full <- site_full[!duplicated(site_full), ]

speffects_loc <- merge(speffects,
                       site_full %>% select(Site, Latitude, Longitude),
                       by="Site")
speffects_np_loc <- merge(speffects_np,
                       site_full %>% select(Site, Latitude, Longitude),
                       by="Site")

# Compute shortest distances
stations <- read.csv(here("data", "raw", "Elizabeth line tube stations.csv"))
site_full_relsites <- site_full %>% filter(Site %in% speffects_loc$Site) %>%
  select(Site, Longitude, Latitude)
site_full_relsites_np <- site_full %>% filter(Site %in% speffects_np_loc$Site) %>%
  select(Site, Longitude, Latitude)

site_distances <- list()
for (i in 1:dim(site_full_relsites)[1]){
  co <- cbind(rep(unlist(site_full_relsites[i, c("Longitude")], use.names = F), times=41), 
              rep(unlist(site_full_relsites[i, c("Latitude")], use.names = F), times=41))
  dist <- distGeo(as.matrix(co), stations %>% select(Longitude, Latitude))
  site_distances[[i]] <- data.frame(Site = site_full_relsites$Site[i],
                                    Nearest_Station = stations$Station[which(dist == min(dist))],
                                    dist = min(dist))
}
site_distances <- rbindlist(site_distances)
speffects_loc <- merge(speffects_loc, site_distances, 
                       by=c("Site"), 
                       allow.cartesian=TRUE) %>%
  distinct()

save(speffects_loc, file=here("results", "speffects_loc.RData"))

site_distances_np <- list()
for (i in 1:dim(site_full_relsites_np)[1]){
  co <- cbind(rep(unlist(site_full_relsites_np[i, c("Longitude")], use.names = F), times=41), 
              rep(unlist(site_full_relsites_np[i, c("Latitude")], use.names = F), times=41))
  dist <- distGeo(as.matrix(co), stations %>% select(Longitude, Latitude))
  site_distances_np[[i]] <- data.frame(Site = site_full_relsites_np$Site[i],
                                    Nearest_Station = stations$Station[which(dist == min(dist))],
                                    dist = min(dist))
}
site_distances_np <- rbindlist(site_distances_np)
speffects_np_loc <- merge(speffects_np_loc, site_distances_np, 
                          by=c("Site"), allow.cartesian=TRUE) %>%
  distinct()

save(speffects_np_loc, file=here("results", "speffects_np_loc.RData"))
# Map of effects (load data) ----------------------------------------------------------

library(extrafont)
library(showtext)
library(jjb)
#external_graphs(ext=FALSE)
#font_import(prompt=F)
#loadfonts()


font_add("CMU Sans Serif Medium", "/Users/kc/Library/Fonts/cmunss.ttf")
showtext_auto()
library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(gt)
library(glue)
library(huxtable)
library(psych)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(ggthemes)
library(latex2exp)
library(RColorBrewer)
library(emojifont)
library(geosphere)


load(here("results", "agg_effect.RData"))
load(here("results", "agg_responsive_effect.RData"))
load(here("results", "speffects_loc.RData"))

load(here("results", "agg_effects_np.RData"))
load(here("results", "agg_responsive_effects_np.RData"))
load(here("results", "speffects_np_loc.RData"))

margin <- 4
mp <- 4

theme <- theme(axis.text = element_text(size= rel(1.4)),
               axis.title = element_text(size=rel(1.4),
                                         family="CMU Sans Serif Medium"),
               axis.text.x = element_text(family="CMU Sans Serif Medium",
                                          angle=0,
                                          vjust=0.5,
                                          size=rel(1.4)),
               axis.text.y = element_text(family="CMU Sans Serif Medium",
                                          angle=0,
                                          vjust=0.5,
                                          size=rel(2)),
               axis.ticks.length = unit(0.4, "cm"),
               axis.line = element_line(linewidth=0.5),
               panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_line(linewidth=0.5,
                                                 colour="gray"),
               panel.background = element_rect(fill="white"),
               panel.spacing.x = unit(1, "lines"),
               strip.text.x = element_text(size=rel(3),
                                           family = "CMU Sans Serif Medium"),
               legend.text = element_text(size=rel(2.2),
                                          family="CMU Sans Serif Medium"),
               legend.title = element_text(size=rel(1.5),
                                           family="CMU Sans Serif Medium"),
               title = element_text(size=rel(2.2), family="CMU Sans Serif Medium"))


# Aggregate effects (for tables) ------------------------------------------------

sum_speffects <- speffects_loc %>% 
                 filter(margin == 5,
                       significant != 0,
                       Classification %in% c("Background", "Roadside", "Kerbside")) %>%
                 select(Classification, Pollutant, effect, effect_long_run, adj.r.squared)

desbygroup <- describeBy(sum_speffects, group=c("Classification", "Pollutant"))



effect_tbl_p <- function(cityeffect, meanresponse, speffects, m, pol){
  
  effect_tbl_city <- merge(cityeffect %>% filter(mp==m) %>% select(-mp),
                      meanresponse %>% filter(mp==m) %>% select(-mp),
                      by=c("Classification", "Pollutant"),
                      suffixes = c("_city", "_mean")) %>%
    pivot_longer(cols=-(1:2), names_pattern = "(.*)(_.*)$", names_to = c("limit", "name")) %>% 
    mutate(limit=ifelse(limit=="", "value", limit)) %>%
    pivot_wider(id_cols = c(Classification, Pollutant, name),
                names_from = limit, 
                values_from = value,
                names_repair = "check_unique") %>%
    arrange(Classification, Pollutant) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    mutate(effect = glue("{effect} ({lci}, {uci})")) %>%
    select(-lci, -uci)
  
  
  #hux() %>%
  #merge_repeated_rows(col=1:2)
  
  effect_tbl_pol <- effect_tbl_city %>% filter(Pollutant == pol) %>%
    filter(Classification %in% c("Background", "Roadside")) %>%
    pivot_wider(names_from = "Classification", values_from = "effect") %>%
    rename(effect_Background = Background,
           effect_Roadside = Roadside,
           mes = name) %>%
    select(-Pollutant) %>%
    mutate(adj.r.squared_Background = rep("", times=2),
           adj.r.squared_Roadside = rep("", times=2))
  
  
  tbl <- speffects %>% filter(mp==m) %>% 
    mutate(effect = ifelse(significant == 0, 0, effect)) %>%
    select(Classification, Pollutant, effect, adj.r.squared) %>%
    group_by(Classification, Pollutant) %>%
    summarise(across(c("effect", "adj.r.squared"),
                     list(std = ~sd(.x), min= ~min(.x), max= ~max(.x)))) %>%
    pivot_longer(cols=-(1:2), names_pattern = "(.*)(_.*)$", names_to = c("limit", "name")) %>% 
    mutate(limit=ifelse(limit=="", "value", limit)) %>%
    pivot_wider(id_cols = c(Classification, Pollutant, name),
                names_from = limit, 
                values_from = value,
                names_repair = "check_unique") %>%
    filter(Classification %in% c("Background", "Roadside")) %>%
    mutate(mes = case_when(name == "_fn1" ~ "sd",
                           name == "_fn2" ~ "min",
                           name == "_fn3" ~ "max")) %>%
    select(-name) %>%
    filter(Pollutant == pol) %>%
    pivot_wider(names_from = Classification, values_from = c("effect", "adj.r.squared")) %>%
    select(-Pollutant) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    rbind(effect_tbl_pol) %>%
    dplyr::select(1, 3, 5, 2, 4) %>%
    gt(rowname_col = "mes") %>%
    tab_spanner(
      label = md(glue("Background {pol}")),
      columns = 2:3
    ) %>%
    tab_spanner(
      label = md(glue("Roadside {pol}")),
      columns = 4:5
    ) %>%
    tab_footnote(
      footnote = "The total effect includes the impact from the current period and the stacked impacts from the lagged periods. Interval estimate is
simulated with 10 000 Monte Carlo iterations. Standard errors of coefficients are heteroscedasticity and autocorrelation consistent
(HAC).",
      locations = cells_column_labels(columns = c(effect_Background,
                                                  effect_Roadside))
      ) %>%
    tab_footnote(
      footnote = "The standard deviation, minimum value, and maximum value are provided with statistically insignificant estimates (at the 10% level)
adjusted to zero",
      locations = cells_column_labels(columns = c(effect_Background,
                                                  effect_Roadside))
    ) %>%
    tab_footnote(
      footnote = "The mean response is the aggregated effect across all sites where the concentrations responded to the intervention.",
      locations = cells_stub(rows = "_mean")
    ) %>%
    tab_footnote(
      footnote = "The regional mean is the aggregated effect across all sites",
      locations = cells_stub(rows = "_city")
    ) %>%
    tab_footnote(
      footnote = "The aggregated effect is computed with 1000 bootstrap resampling iterations. The 95% CI of aggregated effect (in bracket) is the
      percentile interval of 1000 bootstrap resampling iterations.",
      locations = cells_stub(rows = "_mean")
    ) %>%
    tab_footnote(
      footnote = "The adjusted R2 indicates the performance of the RDD model. The standard deviation, minimum value, and maximum value are
provided by summarising the model performance across all RDD models.",
      locations = cells_column_labels(columns = c(adj.r.squared_Background,
                                                  adj.r.squared_Roadside))
    ) %>%
    tab_source_note(source_note = md(
      "All figures rounded to two decimal places."
    )) |>
    opt_footnote_marks(marks = "letters")
  
  print(tbl)
  as_latex(tbl)
}
effect_tbl_np <- function(cityeffect, meanresponse, speffects, m, pol){
  
  effect_tbl_city <- merge(cityeffect %>% filter(mp==m) %>% select(-mp),
                           meanresponse %>% filter(mp==m) %>% select(-mp),
                           by=c("Classification", "Pollutant"),
                           suffixes = c("_city", "_mean")) %>%
    pivot_longer(cols=-(1:2), names_pattern = "(.*)(_.*)$", names_to = c("limit", "name")) %>% 
    mutate(limit=ifelse(limit=="", "value", limit)) %>%
    pivot_wider(id_cols = c(Classification, Pollutant, name),
                names_from = limit, 
                values_from = value,
                names_repair = "check_unique") %>%
    arrange(Classification, Pollutant) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    mutate(effect = glue("{effect} ({lci}, {uci})")) %>%
    select(-lci, -uci)
  
  
  #hux() %>%
  #merge_repeated_rows(col=1:2)
  
  effect_tbl_pol <- effect_tbl_city %>% filter(Pollutant == pol) %>%
    filter(Classification %in% c("Background", "Roadside")) %>%
    pivot_wider(names_from = "Classification", values_from = "effect") %>%
    rename(effect_Background = Background,
           effect_Roadside = Roadside,
           mes = name) %>%
    select(-Pollutant) %>%
    mutate(rmse_Background = rep("", times=2),
           rmse_Roadside = rep("", times=2))
  
  
  tbl <- speffects %>% filter(margin == m) %>% 
    mutate(effect = ifelse(significant == 0, 0, effect)) %>%
    filter(Pollutant == pol) %>%
    select(Classification, Pollutant, effect, rmse) %>%
    group_by(Classification, Pollutant) %>%
    summarise(across(c("effect", "rmse"),
                 list(std = ~sd(.x), min= ~min(.x), max= ~max(.x)))) %>%
    pivot_longer(cols=-(1:2), names_pattern = "(.*)(_.*)$", names_to = c("limit", "name")) %>% 
    mutate(limit=ifelse(limit=="", "value", limit)) %>%
    pivot_wider(id_cols = c(Classification, Pollutant, name),
                names_from = limit, 
                values_from = value,
                names_repair = "check_unique") %>%
    filter(Classification %in% c("Background", "Roadside")) %>%
    mutate(mes = case_when(name == "_fn1" ~ "sd",
                           name == "_fn2" ~ "min",
                           name == "_fn3" ~ "max")) %>%
    select(-name) %>%
    filter(Pollutant == pol) %>%
    pivot_wider(names_from = Classification, values_from = c("effect", "rmse")) %>%
    select(-Pollutant) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    rbind(effect_tbl_pol) %>%
    dplyr::select(1, 3, 5, 2, 4) %>%
    gt(rowname_col = "mes") %>%
    tab_spanner(
      label = md(glue("Background {pol}")),
      columns = 2:3
    ) %>%
    tab_spanner(
      label = md(glue("Roadside {pol}")),
      columns = 4:5
    ) %>%
    tab_footnote(
      footnote = "The total effect includes the impact from the current period and the stacked impacts from the lagged periods. Interval estimate is
simulated with 10 000 Monte Carlo iterations. Standard errors of coefficients are heteroscedasticity and autocorrelation consistent
(HAC).",
      locations = cells_column_labels(columns = c(effect_Background,
                                                  effect_Roadside))
    ) %>%
    tab_footnote(
      footnote = "The standard deviation, minimum value, and maximum value are provided with statistically insignificant estimates (at the 10% level)
adjusted to zero",
      locations = cells_column_labels(columns = c(effect_Background,
                                                  effect_Roadside))
    ) %>%
    tab_footnote(
      footnote = "The mean response is the aggregated effect across all sites where the concentrations responded to the intervention.",
      locations = cells_stub(rows = "_mean")
    ) %>%
    tab_footnote(
      footnote = "The regional mean is the aggregated effect across all sites",
      locations = cells_stub(rows = "_city")
    ) %>%
    tab_footnote(
      footnote = "The aggregated effect is computed with 1000 bootstrap resampling iterations. The 95% CI of aggregated effect (in bracket) is the
      percentile interval of 1000 bootstrap resampling iterations.",
      locations = cells_stub(rows = "_mean")
    ) %>%
    tab_footnote(
      footnote = "The RMSE indicates the performance of the RDD model. The standard deviation, minimum value, and maximum value are
provided by summarising the model performance across all RDD models.",
      locations = cells_column_labels(columns = c(rmse_Background,
                                                  rmse_Roadside))
    ) %>%
    tab_source_note(source_note = md(
      "All figures rounded to two decimal places."
    )) |>
    opt_footnote_marks(marks = "letters")
  
  print(tbl)
  as_latex(tbl)
}

effect_tbl_p(cityeffect, meanresponse, speffects_loc, 4, "nox")
effect_tbl_p(cityeffect, meanresponse, speffects_loc, 4, "no2")
effect_tbl_p(cityeffect, meanresponse, speffects_loc, 4, "o3")
effect_tbl_p(cityeffect, meanresponse, speffects_loc, 4, "pm25")
effect_tbl_p(cityeffect, meanresponse, speffects_loc, 4, "pm10")
effect_tbl_np(cityeffect_np, meanresponse_np, speffects_np_loc, 3, "no2")
effect_tbl_np(cityeffect_np, meanresponse_np, speffects_np_loc, 3, "nox")
effect_tbl_np(cityeffect_np, meanresponse_np, speffects_np_loc, 3, "o3")
effect_tbl_np(cityeffect_np, meanresponse_np, speffects_np_loc, 3, "pm25")
effect_tbl_np(cityeffect_np, meanresponse_np, speffects_np_loc, 3, "pm10")


# Aggregate effects (plots) -------------------------------------------------------
margin <- 4

ggplot(data=cityeffect %>% filter(mp == margin, Classification %in% c("Background", "Roadside")),
       aes(x = effect, y = Pollutant)) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci), linewidth=0.2, height=0.2) + 
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  scale_y_discrete(labels=c(TeX("NO$_2$"), TeX("NO$_X$"),
                            TeX("O$_3$"), TeX("PM$_{10}$"), 
                            TeX("PM$_{2.5}$"))) +
  xlab("Total effect (%)") + 
  ggtitle("City-wide effect by pollutant and classification (linear model)") +
  theme

ggsave(here("figures", "agg_effect.jpg"), height=3, width=5)

ggplot(data=meanresponse %>% filter(mp == margin, Classification %in% c("Background", "Roadside")),
       aes(x = effect, y = Pollutant)) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci), linewidth=0.2, height=0.2) + 
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  scale_y_discrete(labels=c(TeX("NO$_2$"), TeX("NO$_X$"),
                            TeX("O$_3$"), TeX("PM$_{10}$"), 
                            TeX("PM$_{2.5}$"))) +
  xlab("Total effect (%)") + 
  ggtitle("Mean response by pollutant and classification (linear model)") +
  theme

ggsave(here("figures", "agg_responsive_effect.jpg"), height=3, width=5)

# Site Pol effects --------------------------------------------------------
mp <- 4

speffects_mp <- speffects_loc %>%
  filter(margin == mp, Classification %in% c("Background", "Roadside"))

ggplot(data=speffects_mp %>% filter(Pollutant == "no2") %>% slice(1:25),
       aes(x = effect, y = reorder(Site, desc(dist)))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci, colour = factor(significant)),
                 linewidth=0.7, 
                 height=0.2) +
  scale_color_manual(values = c("green", "#808080", "#FF0000"),
                     name = "Effect direction",
                     labels = c("Decrease", "No effect", "Increase")) +
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  xlab("Total Effect (%)") +
  ylab("Site Code") +
  ggtitle(TeX("NO$_2$ total effects by site (linear model)")) +
  theme

ggsave(here("figures", "speffects_no2.jpg"), width=8, height=7)

ggplot(data=speffects_mp %>% filter(Pollutant == "nox") %>% slice(1:25),
       aes(x = effect, y = reorder(Site, desc(dist)))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci, colour = factor(significant)),
                 linewidth=0.7, 
                 height=0.2) +
  scale_color_manual(values = c("green", "#808080", "#FF0000"),
                     name = "Effect direction",
                     labels = c("Decrease", "No effect", "Increase")) +
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  xlab("Total Effect (%)") + 
  ylab("Site Code") +
  ggtitle(TeX("NO$_x$ total effects by site (linear model)")) +
  theme

ggsave(here("figures", "speffects_nox.jpg"), width=8, height = 7)

ggplot(data=speffects_mp %>% filter(Pollutant == "pm10") %>% slice(1:25),
       aes(x = effect, y = reorder(Site, dist))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci, colour = factor(significant)),
                 linewidth=0.7, 
                 height=0.2) +
  scale_color_manual(values = c("green", "#808080", "#FF0000"),
                     name = "Effect direction",
                     labels = c("Decrease", "No effect", "Increase")) +
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange") +
  xlab("Total Effect (%)") + 
  ylab("Site Code") +
  ggtitle(TeX("PM$_{10}$ total effects by site (linear model)")) +
  theme

ggsave(here("figures", "speffects_pm10.jpg"), width=8, height=7)

ggplot(data=speffects_mp %>% filter(Pollutant == "pm25") %>% slice(1:25),
       aes(x = effect, y = reorder(Site, desc(dist)))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci, colour = factor(significant)),
                 linewidth=0.7, 
                 height=0.2) +
  scale_color_manual(values = c("green", "#808080", "#FF0000"),
                     name = "Effect direction",
                     labels = c("Decrease", "No effect", "Increase")) +
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  xlab("Total Effect (%)") + 
  ylab("Site Code") +
  ggtitle(TeX("PM$_{2.5}$ total effects by site (linear model)")) +
  theme

ggsave(here("figures", "speffects_pm25.jpg"), width=8, height=7)

ggplot(data=speffects_mp %>% filter(Pollutant == "o3") %>% slice(1:25),
       aes(x = effect, y = reorder(Site, desc(dist)))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci, colour = factor(significant)),
                 linewidth=0.7, 
                 height=0.2) +
  scale_color_manual(values = c("green", "#808080", "#FF0000"),
                     name = "Effect direction",
                     labels = c("Decrease", "No effect", "Increase")) +
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  xlab("Total Effect (%)") + 
  ylab("Site Code") +
  ggtitle(TeX("O$_3$ total effects by site (linear model)")) +
  theme

ggsave(here("figures", "speffects_o3.jpg"), width=8, height=7)


# Aggregate NP effects (plots) -------------------------------------------------------
margin_np <- 3


ggplot(data=cityeffect_np %>% filter(mp == margin_np, Classification %in% c("Background", "Roadside")),
       aes(x = effect, y = Pollutant)) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci), linewidth=0.2, height=0.2) + 
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  scale_y_discrete(labels=c(TeX("NO$_2$"), TeX("NO$_X$"),
                            TeX("O$_3$"), TeX("PM$_{10}$"), 
                            TeX("PM$_{2.5}$"))) +
  xlab("Total effect (%)") + 
  ggtitle("City-wide effect by pollutant and classification (kernel weighted)") +
  theme

ggsave(here("figures", "agg_effect_np.jpg"), height=3, width=5)

ggplot(data=meanresponse_np %>% filter(mp == margin_np, Classification %in% c("Background", "Roadside")),
       aes(x = effect, y = Pollutant)) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci), linewidth=0.2, height=0.2) + 
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  scale_y_discrete(labels=c(TeX("NO$_2$"), TeX("NO$_X$"),
                            TeX("O$_3$"), TeX("PM$_{10}$"), 
                            TeX("PM$_{2.5}$"))) +
  xlab("Total effect (%)") + 
  ggtitle("Mean response by pollutant and classification (kernel weighted)") +
  theme

ggsave(here("figures", "agg_responsive_effect_np.jpg"), height=3, width=5)

# Site Pol NP effects --------------------------------------------------------
mp_np <- 3

speffects_np_mp <- speffects_np_loc %>%
  filter(margin == mp_np, Classification %in% c("Background", "Roadside"))

ggplot(data=speffects_np_mp %>% filter(Pollutant == "no2") %>% slice(1:25),
       aes(x = effect, y = reorder(Site, desc(dist)))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci, colour = factor(significant)),
                 linewidth=0.7, 
                 height=0.2) +
  scale_color_manual(values = c("green", "#808080", "#FF0000"),
                     name = "Effect direction",
                     labels = c("Decrease", "No effect", "Increase")) +
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  xlab("Total Effect (%)") +
  ylab("Site Code") +
  ggtitle(TeX("NO$_2$ total effects by site (kernel weighted)")) +
  theme

ggsave(here("figures", "speffects_no2_np.jpg"), width=8, height=7)

ggplot(data=speffects_np_mp %>% filter(Pollutant == "nox") %>% slice(1:25),
       aes(x = effect, y = reorder(Site, desc(dist)))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci, colour = factor(significant)),
                 linewidth=0.7, 
                 height=0.2) +
  scale_color_manual(values = c("green", "#808080", "#FF0000"),
                     name = "Effect direction",
                     labels = c("Decrease", "No effect", "Increase")) +
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  xlab("Total Effect (%)") + 
  ylab("Site Code") +
  ggtitle(TeX("NO$_x$ total effects by site (kernel weighted)")) +
  theme

ggsave(here("figures", "speffects_nox_np.jpg"), width=8, height = 7)

ggplot(data=speffects_np_mp %>% filter(Pollutant == "pm10") %>% slice(1:25),
       aes(x = effect, y = reorder(Site, dist))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci, colour = factor(significant)),
                 linewidth=0.7, 
                 height=0.2) +
  scale_color_manual(values = c("green", "#808080", "#FF0000"),
                     name = "Effect direction",
                     labels = c("Decrease", "No effect", "Increase")) +
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange") +
  xlab("Total Effect (%)") + 
  ylab("Site Code") +
  ggtitle(TeX("PM$_{10}$ total effects by site (kernel weighted)")) +
  theme

ggsave(here("figures", "speffects_pm10_np.jpg"), width=8, height=7)

ggplot(data=speffects_np_mp %>% filter(Pollutant == "pm25") %>% slice(1:25),
       aes(x = effect, y = reorder(Site, desc(dist)))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci, colour = factor(significant)),
                 linewidth=0.7, 
                 height=0.2) +
  scale_color_manual(values = c("green", "#808080", "#FF0000"),
                     name = "Effect direction",
                     labels = c("Decrease", "No effect", "Increase")) +
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  xlab("Total Effect (%)") + 
  ylab("Site Code") +
  ggtitle(TeX("PM$_{2.5}$ total effects by site (kernel weighted)")) +
  theme

ggsave(here("figures", "speffects_pm25_np.jpg"), height=7)

ggplot(data=speffects_np_mp %>% filter(Pollutant == "o3") %>% slice(1:25),
       aes(x = effect, y = reorder(Site, desc(dist)))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lci, xmax=uci, colour = factor(significant)),
                 linewidth=0.7, 
                 height=0.2) +
  scale_color_manual(values = c("#808080", "#FF0000"),
                     name = "Effect direction",
                     labels = c("No effect", "Increase")) +
  facet_wrap( ~ Classification) +
  geom_vline(xintercept = 0, colour="orange", linetype=4) +
  xlab("Total Effect (%)") + 
  ylab("Site Code") +
  ggtitle(TeX("O$_3$ total effects by site (kernel weighted)")) +
  theme

ggsave(here("figures", "speffects_o3_np.jpg"), width=8, height=7)


# Long-term effects -------------------------------------------------------

max(abs(speffects_np_loc$effect - speffects_np_loc$effect_long_run)) # 0.0592
max(abs(speffects_loc$effect - speffects_loc$effect_long_run)) # 0.0405

speffects_loc %>% 
  mutate(diff = effect - effect_long_run) %>% 
  select(diff) %>%
  summarise(mean = mean(diff), median=median(diff))
       

# Map plot ----------------------------------------------------------------

stations <- stations %>% mutate(id = 1:dim(stations)[1])
from <- c(28, 37, 24, 5, 32, 22,21, 38, 16, 33, 14,
          39, 9, 2, 27, 3, 36, 10, 23, 40, 34, 26, 11, 25,
          20, 30, 13, 7, 29, 12, 15, 4, 40, 6, 8, 41, 16, 19, 19)
          
to <- c(37, 24, 5, 32, 22, 21, 38, 16, 33, 14,39, 9, 2, 27,
        3, 36, 10, 23, 40, 34, 26, 11, 25, 20, 30, 13, 7, 29,
        12, 15, 4, 31, 6, 8, 41, 1, 19, 18, 17)

edges <- data.frame(from=from,
                    to=to,
                    line=rep("Elizabeth Line", times=length(from)),
                    linecolor=rep("#6950a1", times=length(from)))

vertices <- stations %>% select(-OS.X, -OS.Y, -Zone, -Postcode) %>%
  rename(name=Station,
         latitude=Latitude,
         longitude=Longitude)


new_edgelist <- edges |> 
  dplyr::inner_join(vertices |> 
                      dplyr::select(id, latitude, longitude), 
                    by = c("from" = "id")) |> 
  dplyr::rename(lat_from = latitude, lon_from = longitude) |> 
  dplyr::inner_join(vertices |> 
                      dplyr::select(id, latitude, longitude), 
                    by = c("to" = "id")) |> 
  dplyr::rename(lat_to = latitude, lon_to = longitude)


lizgraph <- igraph::graph_from_data_frame(
  d = new_edgelist, 
  vertices = vertices[, c(4,1,2,3)],
  directed = FALSE
)

londonmap <- get_map(location = "London, UK", 
                     source = "google",
                     zoom = 10,
                     maptype = c("roadmap"))
                     
# layer a London map
base_liz <- ggmap(londonmap, 
                  base_layer = ggraph(lizgraph, layout="linear"),
                  darken = c(0.4, "white"))
base_map <- base_liz + 
  geom_node_point(aes(x = longitude, y = latitude), color = "black") +
  geom_edge_link(aes(x = lon_from, y = lat_from,
                     xend = lon_to, yend = lat_to,
                     color = line), width = 1) +
  scale_edge_color_manual(name = "",
                          values = "#6950a1")

base_map

mp <- 4
myPalette <- colorRampPalette(rev(brewer.pal(9, "YlOrRd")))
effect_map <- function(m, speffects, classif, pol){
  dat <- speffects %>% filter(margin==m,
                              Classification %in% classif,
                              Pollutant %in% pol)
  #print(dat)
  breaks <- 10^(seq(-4, -1, by=1))
  effect_map <- base_map +
    geom_point(data=dat,
               aes(x=Longitude, y=Latitude,
                   shape=factor(significant)), 
               size=20) +
    scale_shape_manual(values=c("\u25BC", "\u25EF", "\u25B2"),
                       name = "Effect Direction",
                       labels = c("Decrease", "No effect", "Increase")) 
    theme(legend.text=element_text(size=40, family="CMU Sans Serif Medium"),
          legend.title=element_text(size=45, family="CMU Sans Serif Medium"),
          legend.position = "right",
          title = element_text(size=40, family="CMU Sans Serif Medium"))
    
  effect_map
}

classif <- c("Background", "Roadside", "Kerbside")
pols <- c("no2", "nox", "o3", "pm10", "pm25")
tex_pols <- c("NO$_2$", "NO$_x$", "O$_3$", "PM$_{10}$", "PM$_{2.5}$")

for (cl in classif){
  for (i in 1:length(pols)) {
    effect_map(4, speffects_loc, cl, pols[i]) +
      ggtitle(TeX(glue("{cl} {tex_pols[i]} effect by proximity to Elizabeth Line")))
    ggsave(here("figures", paste0("map_", pols[i], "_", cl, ".jpeg")), height=10, width=10)
  }
}
