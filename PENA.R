library(tidyverse)
library(ggplot2)
library(lubridate)
library(EnvStats)

Sys.setlocale("LC_ALL","English")

ipf_lifts <- read_csv("D:/tyokansio/datavisu/powerlift/ipf_lifts.csv")
ipf_lifts_BenchMale <- filter(ipf_lifts, sex == 'M' & event == 'B')


#  Get months
ipf_lifts_BenchMale$Month <- month(ipf_lifts_BenchMale$date, label = TRUE, abbr = FALSE)

#  Get years
ipf_lifts_BenchMale$Year <- format(ipf_lifts_BenchMale$date,format="%y")

# sample size
sample_size = ipf_lifts_BenchMale %>% group_by(Month) %>% summarize(num=n())

ipf_lifts_BenchMale %>%
  left_join(sample_size) %>%
  ggplot( aes(x = Month, y=best3bench_kg, alpha=0.8)) +
  geom_jitter(color="#C5CBE3", size=0.4, alpha=0.5) +
  geom_violin(width=1,  fill="#D79922", colour="#D79922") +
  geom_boxplot(width=0.2, color="#F13C20", fill="#4056A1", alpha=0.8) +
  stat_n_text(size = 3, y.pos = -10, color = "#F13C20") +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11, colour = "#F13C20"),
    plot.subtitle = element_text(size=8, colour = "#F13C20"),
    plot.background = element_rect(fill = "#EFE2BA"),
    axis.text = element_text(colour = "#F13C20"),
    plot.caption = element_text(size=8, colour = "#F13C20"),
    axis.title = element_text(colour = "#F13C20")
  ) +
  ggtitle("Bench press results by month (men), 1990-2019") +
  xlab("") +
  labs(caption = "Source: Open Powerlifting",
       subtitle = "Maximum of the first three successful attempts for the lift.",
             y = "Result (kg)") 

