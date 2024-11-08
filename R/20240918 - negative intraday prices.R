# libraries ---------------------------------------------------------------
library(tidyverse)
library(camcorder)
library(patchwork)
library(ggtext)

# data analysis -----------------------------------------------------------
df <- read_csv("data/20240918 - CWE DA and IDA123 prices.csv") %>% 
  mutate(DateTime = with_tz(DateTime, "Europe/Brussels")) %>% 
  filter(DateTime <= as.POSIXct("2024-09-15 00:00", "Europe/Brussels"))

df_correlation <- tibble()

for (country in c("Belgium", "France", "Netherlands", "Germany", "Austria")) {
  
  df_correlation <- df %>% 
    filter(Country == country) %>% 
    pivot_wider(names_from = "Auction",
                values_from = "Price") %>% 
    filter(`DA` < 0) %>% 
    summarize(DA_share = sum(`DA` < 0, na.rm = TRUE) / sum(`DA` < 0, na.rm = TRUE),
              IDA1_share = sum(`IDA1` < 0, na.rm = TRUE) / sum(`DA` < 0, na.rm = TRUE),
              IDA2_share = sum(`IDA2` < 0, na.rm = TRUE) / sum(`DA` < 0, na.rm = TRUE),
              IDA3_share = sum(`IDA3` < 0, na.rm = TRUE) / sum(`DA` < 0 & between(hour(DateTime), 12, 23), na.rm = TRUE)) %>% 
    rbind(df %>% 
            filter(Country == country) %>% 
            pivot_wider(names_from = "Auction",
                        values_from = "Price") %>% 
            filter(`IDA1` < 0) %>% 
            summarize(DA_share = sum(`DA` < 0, na.rm = TRUE) / sum(`IDA1` < 0, na.rm = TRUE),
                      IDA1_share = sum(`IDA1` < 0, na.rm = TRUE) / sum(`IDA1` < 0, na.rm = TRUE),
                      IDA2_share = sum(`IDA2` < 0, na.rm = TRUE) / sum(`IDA1` < 0, na.rm = TRUE),
                      IDA3_share = sum(`IDA3` < 0, na.rm = TRUE) / sum(`IDA1` < 0 & between(hour(DateTime), 12, 23), na.rm = TRUE)),
          df %>% 
            filter(Country == country) %>% 
            pivot_wider(names_from = "Auction",
                        values_from = "Price") %>% 
            filter(`IDA2` < 0) %>% 
            summarize(DA_share = sum(`DA` < 0, na.rm = TRUE) / sum(`IDA2` < 0, na.rm = TRUE),
                      IDA1_share = sum(`IDA1` < 0, na.rm = TRUE) / sum(`IDA2` < 0, na.rm = TRUE),
                      IDA2_share = sum(`IDA2` < 0, na.rm = TRUE) / sum(`IDA2` < 0, na.rm = TRUE),
                      IDA3_share = sum(`IDA3` < 0, na.rm = TRUE) / sum(`IDA2` < 0 & between(hour(DateTime), 12, 23), na.rm = TRUE)),
          df %>% 
            filter(Country == country) %>% 
            pivot_wider(names_from = "Auction",
                        values_from = "Price") %>% 
            filter(`IDA3` < 0) %>% 
            summarize(DA_share = sum(`DA` < 0, na.rm = TRUE) / sum(`IDA3` < 0, na.rm = TRUE),
                      IDA1_share = sum(`IDA1` < 0, na.rm = TRUE) / sum(`IDA3` < 0, na.rm = TRUE),
                      IDA2_share = sum(`IDA2` < 0, na.rm = TRUE) / sum(`IDA3` < 0, na.rm = TRUE),
                      IDA3_share = sum(`IDA3` < 0, na.rm = TRUE) / sum(`IDA3` < 0 & between(hour(DateTime), 12, 23), na.rm = TRUE))) %>% 
    mutate(From = case_when(`DA_share` == 1 ~ "DA",
                            `IDA1_share` == 1 ~ "IDA1",
                            `IDA2_share` == 1 ~ "IDA2",
                            `IDA3_share` == 1 ~ "IDA3")) %>% 
    pivot_longer(-From,
                 names_to = "To",
                 values_to = "Correlation") %>% 
    mutate(To = factor(str_remove(To, "_share"), levels = c("DA", "IDA1", "IDA2", "IDA3")),
           From = factor(From, levels = c("IDA3", "IDA2", "IDA1", "DA")),
           Country = country) %>% 
    rbind(df_correlation)
  
} 

# data viz ----------------------------------------------------------------

camcorder::gg_record(
  dir = "LinkedIn/temp",
  device = "png", 
  width = 1920,     
  height = 1080,    
  units = "px",   
  dpi = 300      
)

top <- ggplot(data = filter(df_correlation, Country == "Belgium"),
              mapping = aes(x = To, y = From, fill = Correlation, label = scales::percent(Correlation, accuracy = .1, decimal.mark = ","))) +
  geom_tile(colour = "#F0F6F7", linewidth = .25) +
  geom_tile(inherit.aes = FALSE,
            data = filter(df_correlation, Country == "Belgium", From == "IDA1", To == "IDA3"),
            mapping = aes(x = To, y = From),
            colour = "#214c58", fill = NA, linetype = "dashed", linewidth = .25) +
  geom_text(colour = "black",
            size = 2) +
  annotate(geom = "text", size = 2, colour = "#214c58", 
           x = 2.5, y = 5,
           hjust = .5, label = "Belgium") +
  annotate(geom = "text", size = 2, colour = "#214c58",
           x = 5.5, y = 3, label = "How to read this chart:\n\nDuring 68,8% of all quarter-hours with negative prices in IDA1,\nprices cleared negative in IDA3 as well.\n\nHigher values indicate that prices clear negative in different auctions more often.", hjust = 0) +
  annotate(geom = "segment", colour = "#214c58",
           x = 5.4, xend = 4.6,
           y = 3, yend = 3,
           arrow = arrow(length = unit(.1, "cm"), type = "closed"),
           linewidth = .1) +
  scale_y_discrete(name = element_blank()) +
  scale_x_discrete(name = element_blank()) +
  scale_fill_gradientn(name = element_blank(),
                       colours = c("#E3856B", "white", "#80c4b7"),
                       values = c(0, .5, .75, .99),
                       limits = c(0, 1)) +
  coord_cartesian(xlim = c(1, 15), ylim = c(1, 4.25),
                  clip = "off") +
  theme(legend.position = "none",
        axis.text = element_text(size = rel(.6), colour = "#214c58"),
        panel.grid = element_blank())

bottom <- ggplot(data = filter(df_correlation, Country != "Belgium"),
                 mapping = aes(x = To, y = From, fill = Correlation, label = scales::percent(Correlation, accuracy = .1, decimal.mark = ","))) +
  geom_tile(colour = "#F0F6F7", linewidth = .25) +
  geom_text(colour = "black",
            size = 1.3) +
  scale_y_discrete(name = element_blank()) +
  scale_x_discrete(name = element_blank()) +
  scale_fill_gradientn(name = element_blank(),
                       colours = c("#E3856B", "white", "#80c4b7"),
                       values = c(0, .5, .75, .99),
                       limits = c(0, 1)) +
  facet_wrap(~Country, nrow = 1) +
  theme(legend.position = "none",
        axis.text = element_text(size = rel(.4), colour = "#214c58"),
        strip.text = element_text(size = rel(1.6)),    
        panel.spacing.x = unit(1.2, "cm"),
        panel.grid = element_blank())

( top  / bottom )  +
  plot_layout(heights = c(4, 3)) +
  plot_annotation(title = "Negative electricity prices are not limited to day-ahead markets",
                  subtitle = "Concurrence of negative prices in different auctions (DA, IDA1, IDA2 and IDA3) for selected bidding zones",
                  caption = "Figure by Nico Schoutteet | Data by EPEX SPOT | Considered period: 16.06.2024 - 15.09.2024") &
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.margin = margin(l = 3, t = 5, r = 5, unit = "pt"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 5, family = "Calibry", colour = "#214c58"),
        plot.title = element_text(size = rel(1.3),
                                  family = "Calibri", colour = "#214c58", face = "bold"),
        plot.subtitle = element_markdown(size = rel(.8),
                                         margin = margin(t = 0, b = 10, unit = "pt"),
                                         family = "Calibri", colour = "#3b3b3b"),
        plot.caption = element_text(size = rel(.5),
                                    hjust = 0, margin = margin(t = 5, b = 5, unit = "pt"),
                                    family = "Calibri", colour = "#3b3b3b"),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#F0F6F7", colour = NA),
        panel.background = element_rect(fill = "#F0F6F7", colour = NA),
        panel.spacing.y = unit(0, "cm"))


ggsave("output/20240918 - concurrence of negative prices.png",
       width = 1920,     
       height = 1080,    
       units = "px",   
       dpi = 300)

# occurrence --------------------------------------------------------------
df %>% 
  group_by(Country = case_match(Country,
                                "Austria" ~ "AT",
                                "Belgium" ~ "BE",
                                "Germany" ~ "DE",
                                "France" ~ "FR",
                                "Netherlands" ~ "NL"), Auction) %>% 
  summarize(Count = sum(Price < 0, na.rm = TRUE) / n_distinct(DateTime)) %>% 
  ggplot(mapping = aes(x = Auction, y = Count, fill = Country, group = Country)) +
  geom_col(position = position_dodge(),
           alpha = .8) +
  geom_text(mapping = aes(x = Auction, y = Count + .01, label = scales::percent(Count, accuracy = .1, decimal.mark = ",")),
            position = position_dodge(width = .9), vjust = 1, size = 2, colour = "#3b3b3b") +
  geom_text(mapping = aes(x = Auction, y = .01, label = Country),
            position = position_dodge(width = .9), vjust = 1, size = 2, colour = "#F0F6F7") +
  scale_x_discrete(name = element_blank(),
                   labels = c("Day-Ahead\n(H0 - H23)", "Intraday 1\n(H0 - H23)", "Intraday 2\n(H0 - H23)", "Intraday 3\n(H12 - H23)")) +
  scale_y_continuous(name = element_blank(),
                     breaks = seq(0, 1, .05),
                     labels = scales::percent_format(accuracy = 1),
                     expand = c(0, 0)) +
  scale_fill_manual(name = element_blank(),
                    values = c("#E3856B", "#53ad27", "#214c58", "#EEC95C", "#80C4B7")) +
  labs(title = "Negative electricity prices are not limited to day-ahead markets",
       subtitle = "Share of quarter-hours with negative prices in different auctions (DA, IDA1, IDA2 and IDA3) for selected bidding zones",
       caption = "Figure by Nico Schoutteet | Data by EPEX SPOT | Considered period: 16.06.2024 - 15.09.2024") +
  coord_cartesian(ylim = c(0, .2), clip = "off") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = .25, linetype = "dashed"),
        plot.margin = margin(l = 5, t = 5, r = 5, unit = "pt"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = rel(.7)),
        strip.background = element_blank(),
        strip.text = element_text(size = 5, family = "Calibry", colour = "#214c58"),
        plot.title = element_text(size = rel(1.3),
                                  family = "Calibri", colour = "#214c58", face = "bold"),
        plot.subtitle = element_markdown(size = rel(.8),
                                         margin = margin(t = 0, b = 20, unit = "pt"),
                                         family = "Calibri", colour = "#3b3b3b"),
        plot.caption = element_text(size = rel(.5),
                                    hjust = 0, margin = margin(t = 5, b = 5, unit = "pt"),
                                    family = "Calibri", colour = "#3b3b3b"),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#F0F6F7", colour = NA),
        panel.background = element_rect(fill = "#F0F6F7", colour = NA),
        panel.spacing.y = unit(0, "cm"))

ggsave("output/20240918 - occurrence of negative prices.png",
       width = 1920,     
       height = 1080,    
       units = "px",   
       dpi = 300)
