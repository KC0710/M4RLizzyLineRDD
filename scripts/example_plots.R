load(here("data", "processed", "all_validated_rr.RData"))
load(here("results", "example_cp_BG2.RData"))
load(here("results", "metNormLab_liz.RData"))
load(here("results", "pd1.RData"))

library(ggfortify)
library(scales)  
library(extrafont)
library(showtext)
library(jjb)
#external_graphs(ext=FALSE)
#font_import(prompt=F)
#loadfonts()
font_add("CMU Sans Serif Medium", "/Users/kc/Library/Fonts/cmunss.ttf")
showtext_auto()

df_orig <- all_validated_rr[[1]] %>% rename(date=DateTime)
d <- ymd("2021-07-01")
df_orig_dat <- openair::timeAverage(df_orig) %>% filter(date >= d)
df_metNorm <- metNormLab_liz[[1]][metNormLab_liz[[1]]$date >= d, ]
cp_metNorm <- BG2_no2_cp_metNorm
dates_metNorm <- df_metNorm$date
bkdates <- dates_metNorm[cp_metNorm$breakpoints]
bkground <- data.frame(start=c(dates_metNorm[1], bkdates),
                       end=c(bkdates, tail(dates_metNorm, 1)),
                       colours=rep(c("cyan","red"),
                                   times=(length(bkdates)+1)))

dateSeq <- as.POSIXct(seq.Date(as.Date(dates_metNorm[1]),
                      as.Date(tail(dates_metNorm, 1)),
                      by = "month"))
metNormValueSeq <- seq(floor(sort(df_metNorm$Value)[1]),
                       ceiling(sort(df_metNorm$Value, decreasing=T)[1]),
                       by = 2)

theme <- theme(axis.text = element_text(size= rel(1.2)),
               axis.title = element_text(size=rel(1.5),
                                         family="CMU Sans Serif Medium"),
               axis.text.x = element_text(family="CMU Sans Serif Medium",
                                          angle=45,
                                          vjust=0.5),
               axis.ticks.length = unit(0.4, "cm"),
               axis.line = element_line(linewidth=0.5),
               panel.background=element_blank())

p_orig <- ggplot(df_orig_dat, aes(x=date, y=Value)) +
  geom_line() + 
  scale_y_continuous() +
  scale_x_datetime(breaks=dateSeq, date_labels="%Y-%m", expand=c(0,0)) +
  ylab(TeX("Concentration ($ \\mu g {m}^{-3}$)")) + 
  xlab("Date") + 
  theme

ggsave(filename = here("figures", "BG2_orig_ex.jpg"), p_orig)

p_metNorm <- ggplot(df_metNorm, aes(x=date, y=Value)) + 
  geom_line() +
  scale_y_continuous(breaks=metNormValueSeq, expand=c(0.1, 0)) +
  scale_x_datetime(breaks = dateSeq,
                   date_labels="%Y-%m",
                   expand=c(0,0.05)) +
  ylab(TeX("Concentration ($ \\mu g {m}^{-3}$)")) +  
  xlab("Date") +
  theme

ggsave(filename = here("figures", "metNorm_BG2_ex.jpg"), p_metNorm)

p_cp <- ggplot(df_metNorm, aes(date, Value)) + 
  geom_line() +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24")),
             linetype=4) +
  scale_y_continuous(breaks=metNormValueSeq, expand=c(0.1, 0)) +
  scale_x_datetime(date_labels="%Y-%m", 
                   breaks=dateSeq,
                   expand=c(0,0)) +
  ylab(TeX("Concentration ($ \\mu g {m}^{-3}$)")) + 
  xlab("Date") +
  geom_rect(data = bkground,
            aes(x=NULL,
                y=NULL,
                xmin = start,
                xmax = end,
                ymin = - Inf,
                ymax = Inf,
                fill = colours),
            show.legend=FALSE,
            alpha = 0.5) +
  theme
        
ggsave(filename = here("figures", "cp_plot_BG2_ex.jpeg"), p_cp)

source(here("scripts", "pdplotting.R"))
pdp <- plotAllPD(pd[[1]])
pdpfacet <- gridExtra::grid.arrange(pdp$plot[[1]],
                              pdp$plot[[2]],
                              pdp$plot[[3]],
                              pdp$plot[[4]],
                              pdp$plot[[5]],
                              pdp$plot[[6]],
                              pdp$plot[[7]],
                              pdp$plot[[8]],
                              pdp$plot[[9]],
                              pdp$plot[[10]],
                              pdp$plot[[11]],
                              ncol=3, nrow=4)
ggsave(pdpfacet, filename=here("figures", "pdplot_BG2_ex.jpg"))



