library(tidyverse)
#devtools::install_github("nicoschoutteet/JAOPuTo")
library(JAOPuTo)


# Use input data from Entso-E (dataset 12.1.D_r3 on energy prices, available via sftp)
# df_prices <- plyr::ldply(.data = list.files("U:/Electricity/EM Monitoring/EMTP/energy prices",
#                                             full.names = TRUE, pattern = "202[2-4]"),
#                          .fun = read_delim,
#                          delim = "\t",
#                          .progress = "tk") %>% 
#   mutate(DateTime = with_tz(`DateTime(UTC)`, "Europe/Brussels"),
#          BiddingZoneAbb = case_match(MapCode,
#                                      "DE_LU" ~ "DE",
#                                      .default = MapCode)) %>% 
#   filter(ResolutionCode == "PT60M",
#          BiddingZoneAbb %in% c("AT", "BE", "CZ", "DE", "HR", "HU", "FR", "NL", "RO", "SI", "SK", "PL")) %>% 
#   select(DateTime, BiddingZoneAbb, Price = `Price[Currency/MWh]`)

# Download data via JAOPuTo package
# df_netpositions <- JAOPuTo::JAOPuTo_Core_netpositions(as.POSIXct("2023-01-01 00:00", "CET"),
#                                                       as.POSIXct("2024-10-31 23:00", "CET"))

# Merge datasets
# df <- df_netpositions %>% 
#   left_join(df_prices,
#             by = c("DateTime", "BiddingZoneAbb")) %>%
#   filter(!(BiddingZoneAbb %in% c("ALBE", "ALDE"))) %>% 
#   unique() %>% 
#   write_csv("data/20241106 - Core prices and net positions.csv")

df <- read_csv("data/20241106 - Core prices and net positions.csv")

df_rank <- df %>% 
  group_by(Date = as.Date(floor_date(DateTime, "1 month"), tz = "Europe/Brussels"),
           BiddingZoneAbb) %>% 
  summarize(NetPosition = mean(NetPosition, na.rm = TRUE),
            Price = mean(Price, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(year(Date) == 2024) %>% 
  group_by(Date) %>% 
  mutate(PriceRank = rank(Price),
         NetPositionRank = rank(-NetPosition))

camcorder::gg_record(
  dir = "LinkedIn/temp",
  device = "png", 
  width = 1280,     
  height = 1080,    
  units = "px",   
  dpi = 300      
)

scale = c("#053a8d", "#0b9dce", "#098945", "#83bf1c", "#ec000b",
          "#ff872e", "#7b45b5", "#d883fc", "#a45700", "#ffbf00",
          "#808080", "#c5c5c5")

ggplot(data = df_rank,
       mapping = aes(x = NetPositionRank, y = PriceRank, colour = BiddingZoneAbb, fill = BiddingZoneAbb)) +
  geom_abline(slope = 1, intercept = 0, linewidth = .25, colour =  "#c85250", linetype = "dashed") +
  geom_point(colour = "#F0F6F7", shape = 21, size = 2) +
  ggforce::geom_mark_ellipse(data = filter(df_rank, BiddingZoneAbb == "BE" | 
                                             (BiddingZoneAbb == "DE" & NetPositionRank >= 9 & PriceRank <= 5)),
                             alpha = .1, linewidth = .2, linetype = "dashed", expand = .01) +
  ggrepel::geom_text_repel(mapping = aes(label = BiddingZoneAbb),  
                           max.overlaps = 20,
                           size = 1.5, segment.size = .1, segment.linetype = 2) +
  scale_x_continuous(name = element_blank(),
                     breaks = seq(1, 12, 1)) +
  scale_y_continuous(name = element_blank(),
                     breaks = seq(1, 12, 1),
                     position = "right") +
  scale_color_manual(values = scale) +
  scale_fill_manual(values = scale) +
  annotate(geom = "segment",
           x = c(13.5, 13.5, 1.2 , 11.8),
           xend = c(13.5, 13.5, .8 , 12.2),
           y = c(11.8, 1.2, -.5, -.5),
           yend = c(12.2, 0.8, -.5, -.5),
           arrow = arrow(length = unit(2, "pt"), type = "closed"),
           linewidth = .05) +
  annotate(geom = "text",
           x = c(13.5, 13.5, 13.5, 6.5, 1.3, 11.7),
           y = c(11.7, 1.3, 6.5, -.5, -.5, -.5),
           vjust = c(1, 0, .5, .5, .5, .5),
           hjust = c(.5, .5, .5, .5,  0, 1),
           fontface = c("plain", "plain", "bold", "bold", "plain", "plain"),
           size = 1.5,
           font = "Calibri", colour = "#3b3b3b",
           label = c("Higher\nprice", "Lower\nprice", "Relative\nprice\nranking", "Relative net position ranking","Higher export", "Higher import")) +
  coord_cartesian(ylim = c(1, 12),
                  xlim = c(1, 12),
                  clip = "off") +
  labs(title = "Low prices, yet high imports? Non-intuitive exchanges can maximize socio-economic welfare.",
       subtitle = "Non-intuitive market coupling results occurred frequently in Belgium and Germany, during the first 10 months of 2024.\nThese results are observed when electricity flows from high- to low-priced zones (instead of vice versa), because\nthey free up capacity to increase exchanges elsewhere in the region, contributing to regional welfare maximization.",
       caption = "Figure by Nico Schoutteet | Data by JAO Publication Tool and Entso-E Transparency Platform") +
  theme_minimal(base_size = 5) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#ececec", linetype = "dashed"),
        plot.margin = margin(t = 10, r = 30, b = 0, l = 10, unit = "pt"),
        plot.title = element_text(size = rel(1.5),
                                  family = "Calibri", colour = "#214c58", face = "bold"),
        plot.subtitle = element_text(size = rel(1.1),
                                     margin = margin(t = 2, b = 10, unit = "pt"),
                                     family = "Calibri", colour = "#3b3b3b"),
        plot.caption = element_text(size = rel(1),
                                    hjust = 0, margin = margin(t = 20, b = 5, unit = "pt"),
                                    family = "Calibri", colour = "#3b3b3b"),
        plot.caption.position = "plot",
        plot.title.position = "panel",
        plot.background = element_rect(fill = "#F0F6F7", colour = NA),
        panel.background = element_rect(fill = "#F0F6F7", colour = NA))

ggsave("output/20241106 - Core prices and net positions monthly ranking.png",
       width = 1280, height = 1080, units = "px", dpi = 300)
