library(tidyverse)

setwd(".")

# df <- plyr::ldply(.data = list.files("data/Nord Pool",
#                                      pattern = "AuctionPrice",
#                                      full.names = TRUE),
#                   .fun = read_delim,
#                   delim = ";") %>% 
#   mutate(DateTime = as.POSIXct(`Delivery Start (CET)`,
#                                format = "%d.%m.%Y %H:%M:%S",
#                                tz = "Europe/Brussels")) %>% 
#   select(DateTime, Price = `BE Price (EUR)`) %>% 
#   write_csv("data/2022-024 - Belgium day ahead prices.csv")

df <- read_csv("data/2022-024 - Belgium day ahead prices.csv") %>% 
  mutate(DateTime = with_tz(DateTime, "Europe/Brussels")) %>% 
  rbind(c(as.POSIXct("2024-12-31 23:00", "Europe/Brussels"), NA))

camcorder::gg_record(
  dir = "LinkedIn/temp",
  device = "png", 
  width = 1280,     
  height = 1080,    
  units = "px",   
  dpi = 300      
)

ggplot(data = df,
       mapping = aes(x = DateTime,
                     y = Price)) +
  geom_point(size = .15,
             shape = 16,  
             colour = "#ffbf00") +
  geom_hline(yintercept = 0,
             linewidth = .2,
             linetype = "dashed",
             colour = "#214c58") +
  geom_hline(data = filter(df, year(DateTime) == 2022),
             mapping = aes(yintercept = mean(df$Price[year(df$DateTime) == 2022], na.rm = TRUE)),
             linewidth = .2,
             linetype = "dashed",
             colour = "#c85250") +
  geom_hline(data = filter(df, year(DateTime) == 2024),
             mapping = aes(yintercept = mean(df$Price[year(df$DateTime) == 2024], na.rm = TRUE)),
             linewidth = .2,
             linetype = "dashed",
             colour = "#c85250") +
  shadowtext::geom_shadowtext(data = filter(df, year(DateTime) == 2022) %>% slice(which.max(DateTime)),
                              mapping = aes(x = DateTime, y = mean(df$Price[year(df$DateTime) == 2022], na.rm = TRUE),
                                            label = paste0("Average:\n",
                                                           format(round(mean(df$Price[year(df$DateTime) == 2022], na.rm = TRUE), 1),
                                                                  decimal.mark = ","),
                                                           " €/MWh")),
                              hjust = 0, size = rel(1.5),
                              colour = "#c85250", bg.colour = "#F0F6F7") +
  shadowtext::geom_shadowtext(data = filter(df, year(DateTime) == 2024) %>% slice(which.max(DateTime)),
                              mapping = aes(x = DateTime, y = mean(df$Price[year(df$DateTime) == 2024], na.rm = TRUE),
                                            label = paste0("Average:\n",
                                                           format(round(mean(df$Price[year(df$DateTime) == 2024], na.rm = TRUE), 1),
                                                                  decimal.mark = ","),
                                                           " €/MWh")),
                              hjust = 0, size = rel(1.5),
                              colour = "#c85250", bg.colour = "#F0F6F7") +
  scale_x_datetime(name = element_blank(),
                   breaks = scales::breaks_width("1 month"),
                   labels = scales::label_date_short()) +
  scale_y_continuous(name = element_blank(),
                     breaks = seq(-200, 1000, 200),
                     labels = c(seq(-200, 800, 200), "1.000\n€/MWh")) +
  labs(title = "Putting electricity prices in perspective.",
       subtitle = "Comparison of hourly day-ahead clearing prices for delivery in Belgium, in €/MWh",
       caption = "Figure by Nico Schoutteet | Data by Nord Pool") +
  facet_wrap(~paste0("Belgian day-ahead prices in ", year(DateTime), " ↴"),
             scales = "free_x",
             ncol = 1) +
  coord_cartesian(ylim = c(-200, 1000),
                  expand = FALSE, clip = "off") +
  theme_minimal(base_size = 5) +
  theme(legend.position = "none",
        axis.text = element_text(size = rel(.9)),
        strip.text = element_text(size = rel(1.35),
                                  family = "Calibri", colour= "#3b3b3b",
                                  margin = margin(0, 0, 10, 0, "pt")),
        panel.spacing.y = unit(10, "pt"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#ececec", linetype = "dashed"),
        plot.margin = margin(t = 10, r = 30, b = 0, l = 10, unit = "pt"),
        plot.title = element_text(size = rel(2),
                                  family = "Calibri", colour = "#214c58", face = "bold"),
        plot.subtitle = element_text(size = rel(1.4),
                                     margin = margin(t = 2, b = 10, unit = "pt"),
                                     family = "Calibri", colour = "#3b3b3b"),
        plot.caption = element_text(size = rel(1),
                                    hjust = 0, margin = margin(t = 5, b = 5, unit = "pt"),
                                    family = "Calibri", colour = "#3b3b3b"),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#F0F6F7", colour = NA),
        panel.background = element_rect(fill = "#F0F6F7", colour = NA))

ggsave("output/20241213 - comparison 2022 - 2024 DA prices Belgium.png",
       width = 1280, height = 1080, units = "px", dpi = 300)
