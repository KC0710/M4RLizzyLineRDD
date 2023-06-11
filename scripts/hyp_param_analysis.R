library(here)
library(dplyr)
library(tidyverse)
source(here("scripts", "loadRData.R"))
load(here("data", "processed", "site_pol_labels.RData"))

for (i in 1:8){
  assign(paste0("rmse_batch", i),
         loadRData(here("results", paste0("gbm_rmse", i, ".RData"))))
}

load(here("results", "dwdefault_rmse.RData"))

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


dwdefault_rmse <- unlist(dwdefault, use.names=FALSE)

rmse_all <- c(rmse_batch1,
              rmse_batch2,
              rmse_batch3,
              rmse_batch4,
              rmse_batch5,
              rmse_batch6,
              rmse_batch7,
              rmse_batch8)
rmse_vec <- unlist(rmse_all, use.names = FALSE)
labels <- site_pol_labels[1:length(rmse_vec), ]
rmse_sp <- labels %>% mutate(rmse = rmse_vec,
                             def_rmse = dwdefault_rmse) %>%
  rename(BayesOpt = rmse,
         Default = def_rmse) %>%
  mutate(diff = BayesOpt - Default)

variable_names <- list(
  "no2" = TeX("NO$_2$"),
  "nox" = TeX("NO$_x$"),
  "pm10" = TeX("PM$_{10}$"),
  "pm25" = TeX("PM$_{2.5}$"),
  "o3" = TeX("O$_3$")
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

ggplot(rmse_sp, aes(y=diff)) + 
  geom_boxplot() +
  facet_wrap(. ~ Classification + Pollutant) +
  theme
  

rmse_sp_wider <- rmse_sp %>% select(Pollutant, BayesOpt, Default) %>%
  pivot_longer(., c("BayesOpt", "Default"), names_to="Method", values_to="RMSE")

ggplot(rmse_sp_wider, aes(x=Method, y=RMSE)) + 
  geom_boxplot() + 
  facet_wrap(.~Pollutant, labeller=variable_labeller) +
  theme

ggsave(here("figures", "hyp_param_perf.jpg"), width=10, height=10)



