setwd("/Users/saliahsoares/Desktop")
library(readr)
library(tidyverse)
covid_03_2023 <- read.csv("US_covid_cases_March_2023 - Copy.csv")

install.packages("zoo")
library(zoo)

library(lubridate)
covid_03_2023$date <- ymd(covid_03_2023$date)

covid_03_2023 <-mutate(covid_03_2023,new_cases =cases - lag(cases))
covid_03_2023$ma.7 <- rollmean(covid_03_2023$new_cases, k=7 , fill=NA , align = "right")
covid_03_2023 <- covid_03_2023 %>%
  rename(ma_7 = ma.7)

covid_03_2023 <- covid_03_2023 %>%
  arrange(date)

end_date <- max(covid_03_2023$date, na.rm = TRUE)
start_date <- end_date - 90

covid_90 <- covid_03_2023 %>%
  filter(date >= start_date)

C1 <- ggplot(covid_90, aes(x = date)) +
  
  geom_col(
    aes(y = new_cases),
    fill = "#f4a6a6",
    alpha = 0.6
  ) +
  
  geom_line(
    aes(y = ma_7),
    color = "#b22222",
    linewidth = 1.5
  ) +
  
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  
  labs(
    title = "New reported cases",
    subtitle = "Last 90 days",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = "gray30")
  )
C1

ggsave(
  filename = "covid_replication.png",
  plot = C1,
  width = 10,
  height = 5,
  dpi = 300
)