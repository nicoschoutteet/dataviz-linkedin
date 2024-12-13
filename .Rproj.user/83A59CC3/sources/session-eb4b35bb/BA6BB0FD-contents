library(tidyverse)
library(cowplot)
library(sf)

# read and filter data
df_prices <- plyr::ldply(
  .data = list.files("U:/Electricity/EM Monitoring/EMTP/energy prices/",
                     full.names = TRUE, pattern = "2023_12|2024"),
  .fun = read_delim,
  delim = "\t",
  .progress = "tk") %>% 
  filter(ResolutionCode == "PT60M",
         Currency == "EUR",
         year(as.Date(`DateTime(UTC)`, tz = "Europe/Brussels")) == 2024) %>% 
  group_by("BiddingZone" = MapCode) %>% 
  summarize(Price = mean(`Price[Currency/MWh]`, na.rm = TRUE))

df_netpositions <- plyr::ldply(
  .data = list.files("U:/Electricity/EM Monitoring/EMTP/implicit allocations net position/",
                     full.names = TRUE, pattern = "2023_12|2024"),
  .fun = read_delim,
  delim = "\t",
  .progress = "tk") %>% 
  filter(ResolutionCode %in% c("PT30M", "PT60M"),
         ContractType == "Daily",
         year(as.Date(`DateTime(UTC)`, tz = "Europe/Brussels")) == 2024) %>% 
  
  mutate(NetPosition = case_when(Direction == "Import" ~ -`NetPosition[MW]`,
                                 TRUE ~ `NetPosition[MW]`)) %>%
  group_by("BiddingZone" = MapCode) %>% 
  summarize(NetPosition = mean(NetPosition, na.rm = TRUE)) 

df <- left_join(df_prices, df_netpositions) %>% 
  filter(!(BiddingZone %in% c("IT-SACOAC", "IT-SACODC", "IE_SEM", "CH", "ME", "MK", "NO2NSL", "RS"))) %>% 
  mutate(Region = case_when(BiddingZone %in% c("AT", "BE", "CZ", "DE_LU", "FR", "HR", "HU", "NL", "PL", "RO", "SI", "SK") ~ "Core",
                            substr(BiddingZone, 1, 2) %in% c("NO", "SE", "DK", "FI") ~ "Nordic",
                            substr(BiddingZone, 1, 2) == "IT" ~ "Italy",
                            BiddingZone %in% c("EE", "LT", "LV") ~ "Baltic",
                            BiddingZone %in% c("ES", "PT") ~ "SWE",
                            BiddingZone %in% c("BG", "GR") ~"SEE")) %>% 
  rbind(c("GB", NA, NA, NA),
        c("IE_SEM", NA, NA, NA),
        c("CH", NA, NA, NA),
        c("GB-NIR", NA, NA, NA),
        c("RS", NA, NA, NA),
        c("ME", NA, NA, NA),
        c("MK", NA, NA, NA),
        c("XK", NA, NA, NA),
        c("AL", NA, NA, NA),
        c("BA", NA, NA, NA)) %>% 
  write_csv("data/20240101 - 20241129 European prices and net positions day ahead.csv")

# calculate average price and load in geojson files
df <- read_csv("data/20240101 - 20241129 European prices and net positions day ahead.csv")  %>%
  rowwise() %>%
  mutate(geometry = list(st_read(paste0("data/bidding zones geojson/", BiddingZone, ".geojson"), quiet = TRUE) %>%
                           st_geometry() %>% .[[1]])) %>%
  ungroup() %>%
  filter(!sapply(geometry, is.null)) %>%
  st_as_sf(sf_column_name = "geometry")

bivariate_color_scale <- tibble(
  PriceCategory = rep(c("High", "Medium", "Low"), 3),
  NetPositionCategory = rep(c("Export", "Balanced", "Import"), each = 3),
  Fill = c("#3F2949",
           "#435786",
           "#4885C1",
           "#77324C",
           "#806A8A",
           "#89A1c8",
           "#AE3A4E",
           "#BC7C8F",
           "#CABED0")) %>% 
  mutate(PriceCategory = factor(PriceCategory, 
                                levels = c("Low", "Medium", "High")),
         NetPositionCategory = factor(NetPositionCategory,
                                      levels = c("Import", "Balanced", "Export")))

df_map <- df %>% 
  mutate(NetPositionCategory = cut(NetPosition,
                                   breaks = 3,
                                   labels = c("Import", "Balanced", "Export")),
         PriceCategory = cut(Price,
                             breaks = 3,
                             labels = c("Low", "Medium", "High"))) %>% 
  left_join(bivariate_color_scale) 

# visualization -----------------------------------------------------------
camcorder::gg_record(
  dir = "LinkedIn/temp",
  device = "png", 
  width = 1280,     
  height = 1080,    
  units = "px",   
  dpi = 300      
)

legend <-
ggplot() +
  geom_tile(data = bivariate_color_scale,
            mapping = aes(x = PriceCategory, 
                          y = NetPositionCategory,
                          fill = Fill),
            colour = "#F0F6F7") +
  scale_fill_identity() +
  labs(x = "Higher price ⟶️",
       y = "Higher net export ⟶️") +
  theme_minimal() +
  coord_fixed(expand = FALSE,
              clip = "off") +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = rel(.3)))

map <- ggplot(mapping = aes(fill = Fill)) +
  geom_sf(data = filter(df_map, is.na(Price)),
          fill = "lightgrey", colour = "lightgrey") +
  geom_sf(data = filter(df_map, !is.na(Price)),
          colour = "#F0F6F7") +
  annotate(geom = "text",
           label = c("France is a low-priced,\nhigh-exporting bidding zone",
                     "North- and Central-Italian bidding zones\nare relatively high-priced and import a lot",
                     "Despite lower prices (compared to the continent),\nsouthern Swedish and Norwegian bidding zones\nimport electricity (from their northern neighbours)"),
           x = c(-12, 8, -1),
           y = c(47, 35, 68),
           hjust = c(1, .5, .5),
           size = 1.5) +
  annotate(geom = "curve",
           x = c(-11.5),
           xend = c(-4),
           y = c(46.2),
           yend = c(47),
           curvature = .2,
           arrow = arrow(length = unit(2, "pt"), type = "closed"),
           linewidth = .15) +
  annotate(geom = "curve",
           x = c(8.5),
           xend = c(12),
           y = c(68),
           yend = c(62.5),
           curvature = -.2,
           arrow = arrow(length = unit(2, "pt"), type = "closed"),
           linewidth = .15) +
  coord_sf(clip = "off",
           xlim = c(-20, 30)) +
  scale_fill_identity() +
  labs(title = "Single day-ahead market coupling: calculating prices and net positions to achieve socio-economic optimum",
       subtitle = "Average prices and net positions resulting from the SDAC (calculated by Euphemia) in 2024",
       caption = "Figure by Nico Schoutteet | Data by Entso-E Transparency Platform (Energy prices: 12.1.D & Day-Ahead implicit net positions: 12.1.E") +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 0, l = 10, unit = "pt"),
        plot.title = element_text(size = rel(.55),
                                  hjust = 0,
                                  family = "Calibri", colour = "#214c58", face = "bold"),
        plot.subtitle = element_text(size = rel(.4),
                                     margin = margin(t = 0, b = 10, unit = "pt"),
                                     family = "Calibri", colour = "#3b3b3b"),
        plot.caption = element_text(size = rel(.4),
                                    hjust = 0, margin = margin(t = 0, b = 5, unit = "pt"),
                                    family = "Calibri", colour = "#3b3b3b"),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#F0F6F7", colour = NA),
        panel.background = element_rect(fill = "#F0F6F7", colour = NA))

cowplot::ggdraw() +
  cowplot::draw_plot(map, 0, 0, 1, 1) +
  cowplot::draw_plot(legend, 0, .5, 0.25, 0.2)

ggsave("output/20241129 - SDAC bivariate choropleth.png",
       width = 1280, height = 1080, units = "px", dpi = 300)                
