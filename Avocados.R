#Load Libraries
# Load packages
library(tidyverse)

library(lubridate)


library(scales)

library(janitor)
library(skimr)
library(gt)
library(DT)
library(highcharter)
library(viridis)
library(ggplot2)

# Read CSV file into tibble
head(avocados)

# Identify data organization
glimpse(avocado)

# Clean data
avocado_v2 <- avocado %>% # Transform data
  avocado_v2 <- avocado_v2 %>% 
  select(-x1) %>%
  relocate(year, .after = date) %>% 
  mutate(year = as.character(year)) %>% 
  mutate(month = month(date, label = TRUE, abbr = FALSE), .after = year) %>% 
  mutate(week = week(date), .after = month) %>% 
  rename(plu_4046 = x4046,
         plu_4225 = x4225,
         plu_4770 = x4770) %>% 
  mutate(type = str_to_title(type)) %>% 
  mutate(type = as_factor(type)) %>%
  mutate(region = gsub("([a-z])([A-Z])", "\\1 \\2", region)) %>% 
  mutate(region = case_when(
    region == "Baltimore Washington" ~ "Baltimore/Washington",
    region == "Buffalo Rochester" ~ "Buffalo/Rochester",
    region == "Cincinnati Dayton" ~ "Cincinnati/Dayton",
    region == "Dallas Ft Worth" ~ "Dallas/Ft. Worth",
    region == "Harrisburg Scranton" ~ "Harrisburg/Scranton",
    region == "Hartford Springfield" ~ "Hartford/Springfield",
    region == "Miami Ft Lauderdale" ~ "Miami/Ft. Lauderdale",
    region == "New Orleans Mobile" ~ "New Orleans/Mobile",
    region == "Phoenix Tucson" ~ "Phoenix/Tucson",
    region == "Raleigh Greensboro" ~ "Raleigh/Greensboro",
    region == "Richmond Norfolk" ~ "Richmond/Norfolk",
    region == "St Louis" ~ "St. Louis",
    region == "Total US" ~ "Total U.S.",
    region == "West Tex New Mexico" ~ "West Tex/New Mexico",
    TRUE ~ as.character(region)
  ))
  clean_names() %>% 
  drop_na() %>% 
  distinct()
  
  # Skim for summary statistics
  avocado_v2 %>% 
    skim_without_charts()
  
  # Count observations by year
  avocado_v2 %>% 
    count(year) %>% 
    ggplot(aes(x = year, y = n, fill = n)) +
    geom_col(width = 0.75) +
    scale_y_continuous(labels = label_comma()) +
    scale_fill_viridis_c(option = "turbo") +
    labs(title = "Observation Totals by Year",
         caption = "Source: Hass Avocado Board",
         x = "",
         y = "Observations") +
    guides(fill = "none") +
    geom_text(aes(label = n),
              vjust = 1.5,
              color = "white")


# Find min and max date by year
avocado_v2 %>% 
  group_by(year) %>% 
  summarize(min_date = min(date),
            max_date = max(date))

# Create box plot average price by region for conventional avocados
avocado_v2 %>%
  filter(type == "Conventional") %>% 
  ggplot(aes(x = reorder(region, average_price),
             y = average_price)) +
  geom_boxplot(fill = viridis(n = 1, begin = 0.5, end = 0.5,
                              option = "turbo")) +
  guides(fill = "none") +
  stat_summary(fun = "mean",
               geom = "point",
               shape = 23,
               size = 1.5,
               fill = "white") +
  coord_flip() +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Conventional Hass Avocado Prices",
       subtitle = "Top Reporting Markets (2015 to 2017)",
       caption = "Means are shown with a diamond",
       x = "",
       y = "Average Selling Price ($)")


# Create table mean price for conventional avocados
avocado_v2 %>% 
  filter(type == "Conventional") %>% 
  group_by(region) %>% 
  summarise(mean_price = mean(average_price) %>% 
              round(2)) %>% 
  arrange(-mean_price) %>% 
  mutate(rank = 1:54,
         .before = region) %>% 
  gt() %>% 
  tab_header(title = md("**Conventional Hass Avocado Prices**"),
             subtitle = "Top Reporting Markets (2015 to 2017)") %>% 
  fmt_currency(columns = mean_price,
               currency = "USD") %>% 
  data_color(
    columns = mean_price,
    colors = col_numeric(
      palette = turbo(n = 4),
      domain = NULL
    )
  ) %>% 
  tab_options(
    column_labels.hidden = TRUE
  )

# Create box plot average price by year for conventional avocados in Total U.S.
avocado_v2 %>% 
  filter(type == "Conventional" & region == "Total U.S.") %>% 
  ggplot(aes(x = year,
             y = average_price)) +
  geom_boxplot(fill = viridis(n = 1, begin = 0.5, end = 0.5,
                              option = "turbo")) +
  stat_summary(fun = "mean",
               geom = "point",
               shape = 23,
               size = 3,
               fill = "white") +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Conventional Hass Avocado Prices",
       subtitle = "Total U.S. (2015 to 2017)",
       caption = "Means are shown with a diamond",
       x = "",
       y = "Average Selling Price ($)") +
  guides(fill = "none")

# Create box plot average price by year for organic avocados in Total U.S.
avocado_v2 %>% 
  filter(type == "Organic" & region == "Total U.S.") %>% 
  ggplot(aes(x = year,
             y = average_price)) +
  geom_boxplot(fill = viridis(n = 1, begin = 0.55, end = 0.55,
                              option = "turbo")) +
  stat_summary(fun = "mean",
               geom = "point",
               shape = 23,
               size = 3,
               fill = "white") +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Organic Hass Avocado Prices",
       subtitle = "Total U.S. (2015 to 2017)",
       caption = "Means are shown with a diamond",
       x = "",
       y = "Average Selling Price ($)") +
  guides(fill = "none")

# Create table mean price by type and year
avocado_v2 %>% 
  filter(region == "Total U.S.") %>% 
  group_by(type, year) %>% 
  summarise(mean_price = mean(average_price)) %>% 
  mutate(change = ((mean_price - lag(mean_price)))) %>% 
  rename(Year = year,
         Price = mean_price,
         Change = change) %>% 
  gt() %>% 
  tab_header(title = md("**Hass Avocado Mean Average Prices**"),
             subtitle = "Total U.S. (2015 to 2017)") %>% 
  fmt_currency(columns = Price,
               currency = "USD") %>% 
  fmt_percent(columns = Change) %>% 
  data_color(columns = Change,
             colors = col_numeric(
               palette = turbo(n = 2, begin = 0.3, end = 0.5),
               domain = NULL
             ))
mean(avocados)


library("dplyr")
library("rcompanion")
library("avocados")

plotNormalHistogram(avocados$AveragePrice)

avocadosANOVA<- aov(avocados$AveragePrice ~ avocados$region)
summary(avocadosANOVA)

avocadosMeans <- avocados %>% group_by(Category) %>% summarize(Mean = mean(Price))

# Plot grand total volume by year
avocado %>% 
  filter(region == "Total U.S.") %>% 
  group_by(year) %>% 
  summarise(grand_total_volume = (sum(total_volume))) %>% 
  ggplot(aes(x = year,
             y = grand_total_volume,
             group = year,
             fill = grand_total_volume)) +
  geom_col() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_c(begin = 0.45, end = 0.55, option = "turbo") +
  labs(title = "Hass Avocado Unit Sales",
       subtitle = "Total U.S. (2015 to 2017)",
       x = "",
       y = "Units sold") +
  guides(fill = "none") +
  geom_text(aes(label = round(grand_total_volume)),
            vjust = 1.5,
            color = "gray25")


