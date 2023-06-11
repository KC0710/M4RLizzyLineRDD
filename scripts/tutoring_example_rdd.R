tutoring <- read_csv(here("tutoring_program.csv"))

theme <- theme(axis.text = element_text(size= rel(1.4)),
               axis.title = element_text(size=rel(2.5),
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
               legend.text = element_text(size=rel(3),
                                          family="CMU Sans Serif Medium"),
               legend.title = element_text(size=rel(2.5),
                                           family="CMU Sans Serif Medium"),
               title = element_text(size=rel(2.2), family="CMU Sans Serif Medium"))

ggplot(tutoring, aes(x = entrance_exam, y = exit_exam, color = tutoring)) +
  geom_point(size = 0.5) +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "Entrance exam score", y = "Exit exam score", color = "Used tutoring") +
  theme

ggsave(here("figures", "tutoring_scatter.jpeg"))
