install.packages(c(
  'tidyverse',
  'rmarkdown',
  'tinytex',
  'knitr',
  'patchwork',
  'scales',
  'dplyr'
))
install.packages("xfun")



getwd()
setwd("C:/Users/nicho/Downloads")
setwd("C:/Users/nicho/OneDrive/Documents/SRP Files/SRP Data")

library(xfun)
library(tidyverse)
library(dplyr)
library(ggplot2)
Miami <- read.csv("Miami Rain.csv")

Miami <- Miami %>%
  mutate(COOPID = if_else(COOPID == 85663, "Miami", as.character(COOPID)))
Miami <- Miami %>%
  filter(PRECIPITATION >= 0)

Miami1 <- Miami %>%
  group_by(YEAR) %>%
  summarize(
    total_precip = sum(PRECIPITATION, na.rm = TRUE),
    mean_temp = mean(MEAN.TEMP, na.rm = TRUE),
    .groups = "drop"
  )

Miami1 <- Miami1 %>%
  filter(YEAR != 2024)

ggplot(Miami1, aes(x = YEAR)) +
  geom_line(aes(y = total_precip, color = "Total Precip (in)")) +
  geom_smooth(aes(y = total_precip, color = "Total Precip (in)"), 
              method = "lm", se = FALSE, linetype = "dashed") +
  geom_line(aes(y = mean_temp, color = "Mean Temp (F)")) +
  scale_y_continuous(
    limits = c(0, 100),
    name = "Inches and Fahrenheit",
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Total Precip (in)" = "blue", "Mean Temp (F)" = "red")
  ) +
  labs(
    title = "Annual Total Precipitation and Mean Temperature for Miami",
    x = "Year"
  ) +
  theme_minimal()

Jax <- read.csv("Jax Rain.csv")

Jax <- Jax %>%
  mutate(COOPID = if_else(COOPID == 84358, "Jax", as.character(COOPID)))
Jax <- Jax %>%
  filter(PRECIPITATION >= 0)

Jax <- Jax %>%
  group_by(YEAR) %>%
  summarize(
    total_precip = sum(PRECIPITATION, na.rm = TRUE),
    mean_temp = mean(MEAN.TEMP, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(Jax, aes(x = YEAR)) +
  geom_line(aes(y = total_precip, color = "Total Precip (in)")) +
  geom_smooth(aes(y = total_precip, color = "Total Precip (in)"), 
              method = "lm", se = FALSE, linetype = "dashed") +
  geom_line(aes(y = mean_temp, color = "Mean Temp (F)")) +
  scale_y_continuous(
    limits = c(0, 100),
    name = "Inches and Fahrenheit",
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Total Precip (in)" = "blue", "Mean Temp (F)" = "red")
  ) +
  labs(
    title = "Annual Total Precipitation and Mean Temperature for Miami",
    x = "Year"
  ) +
  theme_minimal()

FtM <- read.csv("Ft Myers Rain.csv")
FtM <- FtM %>%
  filter(PRECIPITATION >= 0) %>%
  mutate(COOPID = if_else(COOPID == 'Jax', "FtM", as.character(COOPID)))
FtM <- FtM %>%
  group_by(YEAR) %>%
  summarize(
    total_precip = sum(PRECIPITATION, na.rm = TRUE),
    mean_temp = mean(MEAN.TEMP, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(FtM, aes(x = YEAR)) +
  geom_line(aes(y = total_precip, color = "Total Precip (in)")) +
  geom_smooth(aes(y = total_precip, color = "Total Precip (in)"), 
              method = "lm", se = FALSE, linetype = "dashed") +
  geom_line(aes(y = mean_temp, color = "Mean Temp (F)")) +
  scale_x_continuous(
    limits = c(1915, 2022)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    name = "Inches and Fahrenheit",
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Total Precip (in)" = "blue", "Mean Temp (F)" = "red")
  ) +
  labs(
    title = "Annual Total Precipitation and Mean Temperature for Ft Myers",
    x = "Year"
  ) +
  theme_minimal()

Pens <- read.csv("Pens Rain.csv")
Pens <- Pens %>%
  filter(PRECIPITATION >= 0) %>%
  mutate(COOPID = if_else(COOPID == 86997, "Pens", as.character(COOPID)))
Pens <- Pens %>%
  group_by(YEAR) %>%
  summarize(
    total_precip = sum(PRECIPITATION, na.rm = TRUE),
    mean_temp = mean(MEAN.TEMP, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(Pens, aes(x = YEAR)) +
  geom_line(aes(y = total_precip, color = "Total Precip (in)")) +
  geom_smooth(aes(y = total_precip, color = "Total Precip (in)"), 
              method = "lm", se = FALSE, linetype = "dashed") +
  geom_line(aes(y = mean_temp, color = "Mean Temp (F)")) +
  scale_y_continuous(
    limits = c(0, 100),
    name = "Inches and Fahrenheit",
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Total Precip (in)" = "blue", "Mean Temp (F)" = "red")
  ) +
  labs(
    title = "Annual Total Precip and Mean Temperature for Pensacola",
    x = "Year"
  ) +
  theme_minimal()

Orl <- read.csv("Orlando Rain.csv")
Orl <- Orl %>%
  filter(PRECIPITATION >= 0) %>%
  mutate(COOPID = if_else(COOPID == 'Pens', "Orl", as.character(COOPID)))
Orl <- Orl %>%
  group_by(YEAR) %>%
  summarize(
    total_precip = sum(PRECIPITATION, na.rm = TRUE),
    mean_temp = mean(MEAN.TEMP, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(Orl, aes(x = YEAR)) +
  geom_line(aes(y = total_precip, color = "Total Precip (in)")) +
  geom_smooth(aes(y = total_precip, color = "Total Precip (in)"), 
              method = "lm", se = FALSE, linetype = "dashed") +
  geom_line(aes(y = mean_temp, color = "Mean Temp (F)")) +
  scale_x_continuous(
    limits = c(1953, 2022)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    name = "Inches and Fahrenheit",
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Total Precip (in)" = "blue", "Mean Temp (F)" = "red")
  ) +
  labs(
    title = "Annual Total Precip and Mean Temperature for Orlando",
    x = "Year"
  ) +
  theme_minimal()

Tampa <- read.csv("Tampa Rain.csv")
Tampa <- Tampa %>%
  filter(PRECIPITATION >= 0) %>%
  mutate(COOPID = if_else(COOPID == 88788, "Tampa", as.character(COOPID)))
Tampa <- Tampa %>%
  group_by(YEAR) %>%
  summarize(
    total_precip = sum(PRECIPITATION, na.rm = TRUE),
    mean_temp = mean(MEAN.TEMP, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(Tampa, aes(x = YEAR)) +
  geom_line(aes(y = total_precip, color = "Total Precip (in)")) +
  geom_smooth(aes(y = total_precip, color = "Total Precip (in)"), 
              method = "lm", se = FALSE, linetype = "dashed") +
  geom_line(aes(y = mean_temp, color = "Mean Temp (F)")) +
  scale_x_continuous(
    limits = c(1933, 2025)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    name = "Inches and Fahrenheit",
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Total Precip (in)" = "blue", "Mean Temp (F)" = "red")
  ) +
  labs(
    title = "Annual Total Precip and Mean Temperature for Tampa",
    x = "Year"
  ) +
  theme_minimal()

Naples <- read.csv("Naples Rain.csv")
Naples <- Naples %>%
  filter(PRECIPITATION >= 0, MEAN.TEMP >= 0) %>%
  mutate(COOPID = if_else(COOPID == 86078, "Naples", as.character(COOPID)))
Naples <- Naples %>%
  group_by(YEAR) %>%
  summarize(
    total_precip = sum(PRECIPITATION, na.rm = TRUE),
    mean_temp = mean(MEAN.TEMP, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(Naples, aes(x = YEAR)) +
  geom_line(aes(y = total_precip, color = "Total Precip (in)")) +
  geom_smooth(aes(y = total_precip, color = "Total Precip (in)"), 
              method = "lm", se = FALSE, linetype = "dashed") +
  geom_line(aes(y = mean_temp, color = "Mean Temp (F)")) +
  scale_x_continuous(
    limits = c(1942, 2022)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    name = "Inches and Fahrenheit",
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Total Precip (in)" = "blue", "Mean Temp (F)" = "red")
  ) +
  labs(
    title = "Annual Total Precip and Mean Temperature for Naples",
    x = "Year"
  ) +
  theme_minimal()


city_data <- list(
  miami = Miami1,
  naples = Naples,
  orlando = Orl,
  pensacola = Pens
)
models <- lapply(city_data, function(df) {
  lm(total_precip ~ mean_temp, data = df)
})
lapply(models, summary)


