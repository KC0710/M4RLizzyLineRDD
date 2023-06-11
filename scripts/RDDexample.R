library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(broom)  # Convert models to data frames
library(rdrobust)  # For robust nonparametric regression discontinuity
library(rddensity)  # For nonparametric regression discontinuity density tests
library(modelsummary)  # Create side-by-side regression tables

tutoring <- read_csv(here("tutoring_program.csv"))

ggplot(tutoring, aes(x = entrance_exam, y = tutoring, color = tutoring)) +
  # Make points small and semi-transparent since there are lots of them
  geom_point(size = 0.5, alpha = 0.5, 
             position = position_jitter(width = 0, height = 0.25, seed = 1234)) + 
  # Add vertical line
  geom_vline(xintercept = 70) + 
  # Add labels
  labs(x = "Entrance exam score", y = "Participated in tutoring program") + 
  # Turn off the color legend, since it's redundant
  guides(color = FALSE)

ggplot(tutoring, aes(x = entrance_exam, y = exit_exam, color = tutoring)) +
  geom_point(size = 0.5, alpha = 0.5) + 
  geom_vline(xintercept = 70) +
  labs(x = "Entrance exam score", y = "Exit exam score", color = "Used tutoring") + 
  theme_light()
