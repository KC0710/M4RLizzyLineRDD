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


BN2 <- getMetNormSitePol(metNormLab_liz, "BN2", "pm10")[[1]]
d <- ymd("2021-07-01")
ggplot(BN2[BN2$date > d, ], aes(x = date, y=Value)) + geom_line() +
  geom_line(aes(x=date, y=exp(preds)), data = datp, colour="green") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24")),
             linetype=4) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24") - weeks(4)),
             linetype=4, color = "red") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24") + weeks(4)),
             linetype=4, color="blue") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-08-22")),
             linetype=4, color="orange") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2021-09-09")),
             linetype=4, color="orange") +
  ggtitle("Site BN2 PM10") 
  
ggsave(here("figures", "BN2_PM10.jpg"))

BQ9 <- getMetNormSitePol(metNormLab_liz, "BQ9", "pm10")[[1]]
d <- ymd("2021-07-01")
ggplot(BQ9[BQ9$date > d, ], aes(x = date, y=Value)) + geom_line() +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24")),
             linetype=4) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24") - weeks(4)),
             linetype=4, color = "red") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24") + weeks(4)),
             linetype=4, color="blue") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-08-21")),
             linetype=4, color="orange") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2021-12-19")),
             linetype=4, color="orange") +
  ggtitle(TeX("Site BQ9, pollutant $PM_{10}$")) +
  xlab("Date") + 
  ylab(TeX("Concentration ($ \\mu g /m^3$)")) +
  theme

ggsave(here("figures", "BQ9_PM10.jpg"))

HI3 <- getMetNormSitePol(metNormLab_liz, "HI3", "pm10")[[1]]
d <- ymd("2021-07-01")
ggplot(HI3[HI3$date > d, ], aes(x = date, y=Value)) + geom_line() +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24")),
             linetype=4) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24") - weeks(4)),
             linetype=4, color = "red") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24") + weeks(4)),
             linetype=4, color="blue") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-08-22")),
             linetype=4, color="orange") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2021-12-02")),
             linetype=4, color="orange") +
  ggtitle("Site HI3 PM10") 

ggsave(here("figures", "HI3_PM10.jpg"))

EI3 <- getMetNormSitePol(metNormLab_liz, "EI3", "no2")[[1]]
d <- ymd("2021-07-01")
ggplot(EI3[EI3$date > d, ], aes(x = date, y=Value)) + geom_line() +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24")),
             linetype=4) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24") - weeks(3)),
             linetype=4, color = "red") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24") + weeks(3)),
             linetype=4, color="blue") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-01-26")),
             linetype=4, color="orange") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-06-16")),
             linetype=4, color="orange") +
  ggtitle("Site EI3 NO2") 

EN5 <- getMetNormSitePol(metNormLab_liz, "EN5", "no2")[[1]]
d <- ymd("2021-07-01")
ggplot(EN5[EN5$date > d, ], aes(x = date, y=Value)) + geom_line() +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24")),
             linetype=4) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24") - weeks(4)),
             linetype=4, color = "red") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-05-24") + weeks(4)),
             linetype=4, color="blue") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-02-20")),
             linetype=4, color="orange") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2022-08-21")),
             linetype=4, color="orange") +
  ggtitle("Site EN5 NO2") 
  
  
  
