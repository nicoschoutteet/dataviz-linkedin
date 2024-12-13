library(tidyverse)
library(httr)

#https://www.color-hex.com/color-palette/5050


df_solar <- httr::GET(url = "https://opendata.elia.be/api/explore/v2.1/catalog/datasets/ods032/exports/csv?refine=datetime%3A%222024%22&refine=region%3A%22Belgium%22&timezone=Europe%2FBrussels") %>% 
  httr::content(as = "text") %>% 
  read_delim(delim = ";") %>% 
  mutate(DateTime = with_tz(datetime, "Europe/Brussels")) %>%
  select(DateTime, everything(), -datetime) %>% 
  write_csv("data/20241206 - solar injection Elia ods032.csv")

df_solar_max <- df_solar %>% 
  group_by(Date = as.Date(DateTime, "Europe/Brussels")) %>% 
  mutate(Total = sum(dayahead11hforecast, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Total == max(Total, na.rm = TRUE)) %>% 
  select(-Total)

df_solar_max_area <- df_solar_max %>% 
  mutate(xmin = DateTime,
         xmax = DateTime + minutes(15),
         ymin = case_when(dayahead11hforecast < 0 ~ 0,
                          dayahead11hforecast == 0 ~ 0,
                          dayahead11hforecast > 0 ~ dayahead11hforecast),
         ymax = case_when(dayahead11hforecast < 0 ~ dayahead11hforecast,
                          dayahead11hforecast == 0 ~ 0,
                          dayahead11hforecast > 0 ~ 0))

ggplot(data = df_solar_max,
       mapping = aes(x = DateTime, y = dayahead11hforecast)) +
  geom_step(colour = "#FFEB3B") +
  geom_rect(data = df_solar_max_area,
            mapping = aes(xmin = xmin,
                          xmax = xmax,
                          ymin = ymin,
                          ymax = ymax),
            fill = "#FFEB3B", alpha = .2) +
  scale_x_datetime(breaks = c(seq.POSIXt(min(df_solar_max$DateTime, na.rm = TRUE),
                                         min(df_solar_max$DateTime, na.rm =TRUE) + hours(18),
                                         by = "6 hours"),
                              max(df_solar_max$DateTime, na.rm = TRUE)),
                   labels = scales::time_format("%H:%M", tz = "Europe/Brussels")) +
  scale_y_continuous(name = element_blank(),
                     breaks = scales::breaks_width(2000),
                     labels = c(format(seq(0, 6000, 2000), big.mark = ".", decimal.mark = ","),
                                "8.000 MW")) +
  coord_cartesian(ylim = c(0, 8000),
                  xlim = c(min(df_solar_max$DateTime),
                           max(df_solar_max$DateTime)),
                  expand = FALSE) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = .25, linetype = "dashed"),
        plot.margin =  margin(20, 20, 20, 10, "pt"),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("output/20240729 - solar day ahead forecast.png",
       width = 16, height = 10, units = "cm", dpi = 200)

ggplot(data = df_solar_max,
       mapping = aes(x = DateTime, y = dayahead11hforecast)) +
  geom_step(colour = "#FFEB3B") +
  geom_rect(data = df_solar_max_area,
            mapping = aes(xmin = xmin,
                          xmax = xmax,
                          ymin = ymin,
                          ymax = ymax),
            fill = "#FFEB3B", alpha = .2) +
  geom_step(data = df_solar_max %>% group_by(DateTime = floor_date(DateTime, "1 hour")) %>% summarize(dayahead11hforecast = mean(dayahead11hforecast, na.rm = TRUE)),
            colour = "#dd5d5d") +
  scale_x_datetime(breaks = c(seq.POSIXt(min(df_solar_max$DateTime, na.rm = TRUE),
                                         min(df_solar_max$DateTime, na.rm =TRUE) + hours(18),
                                         by = "6 hours"),
                              max(df_solar_max$DateTime, na.rm = TRUE)),
                   labels = scales::time_format("%H:%M", tz = "Europe/Brussels")) +
  scale_y_continuous(name = element_blank(),
                     breaks = scales::breaks_width(2000),
                     labels = c(format(seq(0, 6000, 2000), big.mark = ".", decimal.mark = ","),
                                "8.000 MW")) +
  coord_cartesian(ylim = c(0, 8000),
                  xlim = c(min(df_solar_max$DateTime),
                           max(df_solar_max$DateTime)),
                  expand = FALSE) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = .25, linetype = "dashed"),
        plot.margin =  margin(20, 20, 20, 10, "pt"),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("output/20240729 - solar day ahead forecast with DA bidding strategy.png",
       width = 16, height = 10, units = "cm", dpi = 200)

rm(list = ls())


# auction prices ----------------------------------------------------------
df_DA <- read_delim("data/Nord Pool/2024 DA.csv", delim = ";") %>% 
  mutate(DateTime = as.POSIXct(`Delivery Start (CET)`, format = "%d.%m.%Y %H:%M:%S", tz = "Europe/Brussels"),
         Auction = "DA") %>% 
  select(DateTime, Auction, Price = `BE Price (EUR)`)

expand_to_quarter_hour <- function(row) {
  # Generate quarter-hour intervals (0, 15, 30, 45 minutes)
  intervals <- seq(from = 0, to = 45, by = 15)
  
  # Create a new DateTime for each quarter-hour and replicate the Price
  new_rows <- data.frame(
    DateTime = row$DateTime + minutes(intervals),
    Auction = rep(row$Auction, length(intervals)),
    Price = rep(row$Price, length(intervals))
  )
  
  return(new_rows)
}


df_IDA1 <- read_delim("data/Nord Pool/2024 IDA1.csv", delim = ";") %>% 
  mutate(DateTime = as.POSIXct(`Delivery Start (CET)`, format = "%d.%m.%Y %H:%M:%S", tz = "Europe/Brussels"),
         Auction = "IDA1") %>% 
  select(DateTime, Auction, Price = `BE Price (EUR)`) %>% 
  na.omit()

df_IDA2 <- read_delim("data/Nord Pool/2024 IDA2.csv", delim = ";") %>% 
  mutate(DateTime = as.POSIXct(`Delivery Start (CET)`, format = "%d.%m.%Y %H:%M:%S", tz = "Europe/Brussels"),
         Auction = "IDA2") %>% 
  select(DateTime, Auction, Price = `BE Price (EUR)`) %>% 
  na.omit()

df_IDA3 <- read_delim("data/Nord Pool/2024 IDA3.csv", delim = ";") %>% 
  mutate(DateTime = as.POSIXct(`Delivery Start (CET)`, format = "%d.%m.%Y %H:%M:%S", tz = "Europe/Brussels"),
         Auction = "IDA3") %>% 
  select(DateTime, Auction, Price = `BE Price (EUR)`) %>% 
  na.omit()

df <- rbind(df_DA %>% 
                    rowwise() %>% 
                    do(expand_to_quarter_hour(.)), df_IDA1, df_IDA2, df_IDA3) %>% 
  write_csv("data/2024 DA IDA123 auction prices.csv")

rm(list = setdiff(ls(), "df"))

df_monthly <- df %>% 
  # filter(DateTime >= as.POSIXct("2024-06-15 00:00")) %>% 
  group_by(DateTime = floor_date(DateTime, "1 month"),
           Auction) %>% 
  summarize(Price = mean(Price, na.rm = TRUE))

ggplot(data = df_monthly,
       mapping = aes(x = DateTime,y = Price, colour = Auction, fill = Auction, label = Auction)) +
  geom_point(colour = "white", shape = 21) +
  geom_line() +
  geom_text(data = filter(df_monthly, month(DateTime) == 12),
            mapping = aes(x = DateTime + days(5)),
            size = 2.5, hjust = 0, nudge_y = c(0, -3, 5, 0)) +
  scale_colour_manual(name = element_blank(),
                      values = c("DA" = "#64b5f6", 
                                 "IDA1" = "#ef5350",
                                 "IDA2" = "#ffeb3b",
                                 "IDA3" = "#66bb6a")) +
  scale_fill_manual(name = element_blank(),
                      values = c("DA" = "#64b5f6", 
                                 "IDA1" = "#ef5350",
                                 "IDA2" = "#ffeb3b",
                                 "IDA3" = "#66bb6a")) +
  scale_x_datetime(name = element_blank(),
                   breaks = scales::breaks_width("1 month"),
                   labels = scales::label_date_short()) +
  scale_y_continuous(name = element_blank(),
                     breaks = seq(0, 150, 50),
                     labels = c(seq(0, 100, 50), "150 €/MWh")) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 150),
                  clip = "off") +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = .25, linetype = "dashed"),
        plot.margin =  margin(20, 20, 20, 10, "pt"),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("output/20240729 - monthly clearing prices 2024.png",
       width = 16, height = 10, units = "cm", dpi = 200)

rm(list = setdiff(ls(), "df"))

# histograms --------------------------------------------------------------
df <- read_csv("data/2024 DA IDA123 auction prices.csv")

df_delta <- df %>% 
  filter(DateTime >= as.POSIXct("2024-06-15 00:00")) %>% 
  group_by(DateTime) %>% 
  summarize(Delta = max(Price, na.rm = TRUE) - min(Price, na.rm = TRUE))

ggplot(data = df_delta,
       mapping = aes(x = Delta, y = after_stat(count/sum(count)))) +
  geom_bar(fill = "#64b5f6") +
  scale_x_binned(name = "Distribution of price differences between DA, IDA1, IDA2 and IDA3 for each quarter-hour",
                 breaks = seq(0, 10000, 20)) +
  scale_y_continuous(name = element_blank(),
                     breaks = seq(0, 1, .1),
                     labels = scales::percent_format(),
                     position = "right",
                     expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 200),
                  ylim = c(0, .4)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = .25, linetype = "dashed"),
        plot.margin =  margin(20, 20, 20, 10, "pt"),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("output/20240729 - histogram price deltas.2024.png",
       width = 16, height = 10, units = "cm", dpi = 200)



# daily prices ------------------------------------------------------------
Date = as.Date("2024-07-29", tz = "Europe/Brussels")

df_filtered <-  filter(df, as.Date(DateTime, tz = "Europe/Brussels") == Date)

df_filtered %>% group_by(Auction) %>% summarize(Price = mean(Price, na.rm = TRUE))

ggplot(data = df_filtered,
       mapping = aes(x = DateTime, y = Price, colour = Auction)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             linewidth = .25,
             colour = "black") +
  geom_step(direction = "mid") +
  geom_step(data = filter(df_filtered, Auction == "DA"),
            colour = "#64b5f6",
            direction = "mid") +
  scale_y_continuous(name = element_blank(),
                     breaks = seq(-100, 200, 100),
                     labels = c(seq(-100, 100, 100), "200 €/MWh")) +
  scale_x_datetime(breaks = c(seq.POSIXt(min(df_filtered$DateTime, na.rm = TRUE),
                                         min(df_filtered$DateTime, na.rm =TRUE) + hours(18),
                                         by = "6 hours"),
                              max(df_filtered$DateTime, na.rm = TRUE)),
                   labels = scales::time_format("%H:%M", tz = "Europe/Brussels")) +
  scale_colour_manual(name = element_blank(),
                      values = c("DA" = "#64b5f6", 
                                 "IDA1" = "#ef5350",
                                 "IDA2" = "#ffeb3b",
                                 "IDA3" = "#66bb6a")) +
  theme_minimal() +
  coord_cartesian(clip = "off",
                  ylim = c(-100, 200)) +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = .25, linetype = "dashed"),
        plot.margin =  margin(20, 20, 20, 10, "pt"),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("output/20240729 - DA IDA123 prices.png",
       width = 16, height = 12, units = "cm", dpi = 200)


# quantification of effect ------------------------------------------------
df_solar <- read_csv("data/20241206 - solar injection Elia ods032.csv")

df_solar_daily <- df_solar %>% 
  filter(DateTime >= as.POSIXct("2024-06-15 00:00")) %>% 
  group_by(Date = as.Date(DateTime, tz = "Europe/Brussels")) %>% 
  summarize(Injection = mean(dayahead11hforecast, na.rm = TRUE))

median(df_solar_daily$Injection)

low_solar_days <- df_solar_daily$Date[df_solar_daily$Injection < 1170]
high_solar_days <- df_solar_daily$Date[df_solar_daily$Injection >= 1170]

df_prices <- read_csv("data/2024 DA IDA123 auction prices.csv") %>% 
  mutate(DateTime = with_tz(DateTime, "Europe/Brussels")) %>% 
  filter(DateTime >= as.POSIXct("2024-06-15 00:00"),
         !between(DateTime, as.POSIXct("2024-10-27 02:00:00", "Europe/Brussels"), as.POSIXct("2024-10-27 02:45:00", "Europe/Brussels"))) %>% 
  pivot_wider(names_from = Auction,
              values_from = Price) %>% 
  mutate(IDA1_delta = DA - IDA1,
         IDA2_delta = DA - IDA2,
         IDA3_delta = DA - IDA3,
         DateType = case_when(as.Date(DateTime, "Europe/Brussels")
                               %in% low_solar_days ~ "Low solar",
                               as.Date(DateTime, "Europe/Brussels")
                               %in% high_solar_days ~ "High solar"))

df_averagedelta <- df_prices %>% 
  filter(!is.na(DateType)) %>% 
  group_by(DateType,
           QuarterHour = as.numeric(hour(DateTime)) * 4 + as.numeric(minute(DateTime)) / 15 + 1) %>% 
  summarize(IDA1 = mean(IDA1_delta, na.rm = TRUE),
            IDA2 = mean(IDA2_delta, na.rm = TRUE),
            IDA3 = mean(IDA3_delta, na.rm = TRUE)) %>% 
  pivot_longer(cols = -c(DateType, QuarterHour),
               names_to = "Auction",
               values_to = "Delta")

ggplot(data = df_averagedelta,
       mapping = aes(x = QuarterHour,
                     y = Delta,
                     fill = Auction)) +
  geom_col() +
  scale_fill_manual(values = c("IDA1" = "#ef5350",
                    "IDA2" = "#ffeb3b",
                    "IDA3" = "#66bb6a")) +
  scale_y_continuous(name = element_blank(),
                     position = "right",
                     breaks = seq(-200, 200, 100),
                     labels = c(seq(-200, 100, 100), "200 €/MWh")) +
  scale_x_continuous(name = element_blank(),
                     breaks = c(1, 25, 49, 73, 96),
                     labels = c("00:00", "06:00", "12:00", "18:00", "23:45")) +
  facet_grid(Auction ~ DateType,
             switch = "y") +
  theme_minimal() +
  coord_cartesian(clip = "off",
                  ylim = c(-200, 200)) +
  theme(axis.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = rel(.9)),
        strip.text.y.left = element_text(angle = 0),
        panel.grid.major.x = element_line(linewidth = .25, linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = .25, linetype = "dashed"),
        plot.margin =  margin(20, 20, 20, 10, "pt"),
        plot.background = element_rect(fill = "white", colour = NA))

        
ggsave("output/2024 - DA IDA123 deltas high low solar.png",
       width = 16, height = 16, units = "cm", dpi = 200)
