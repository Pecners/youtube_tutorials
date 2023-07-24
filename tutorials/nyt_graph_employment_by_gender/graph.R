library(tidyverse)
library(colorspace)
library(ggtext)
library(glue)
library(showtext)

setwd("tutorials/nyt_graph_employment_by_gender")

m <- read_csv("data/men.csv") |> 
  mutate(group = "men")
w <- read_csv("data/women.csv") |> 
  mutate(group = "women")

both <- bind_rows(m, w) |> 
  mutate(Label = ym(Label))

mean_2019 <- both |> 
  group_by(year = year(Label), group) |> 
  summarise(avg = mean(Value)) |> 
  filter(year == "2019")

end <- both |> 
  group_by(group) |> 
  filter(Label == max(Label))

nyt_cols <- c("#80000F", "#FFA89A")
dotted_cols <- darken(nyt_cols, .25)
  
both |> 
  ggplot(aes(Label, Value, 
             color = ifelse(group == "men", nyt_cols[1], nyt_cols[2]))) +
  geom_line(linewidth = 1) +
  geom_segment(data = mean_2019, 
               x = min(both$Label), xend = max(both$Label),
               aes(y = avg, yend = avg,
                 color = ifelse(group == "men", dotted_cols[1], dotted_cols[2])), 
               linetype = 3, size = .75) +
  geom_text(data = mean_2019, label = "2019 average",
            x = ymd("2020-11-01"), fontface = "bold",
            aes(y = avg + 1,
                color = ifelse(group == "men", dotted_cols[1], dotted_cols[2]))) +
  annotate(geom = "text", color = "grey50",
            x = ymd("2018-09-01"), y = seq(60, 90, by = 10), 
            label = c(seq(60, 80, by = 10), "90%"),
           vjust = -1, hjust = 0) +
  geom_text(data = end,
            label = c("Men", "Women"),
            aes(color = ifelse(group == "men", dotted_cols[1], dotted_cols[2]),
                x = Label + 50),
            hjust = 0, vjust = .25, fontface = "bold") +
  geom_textbox(y = 69, x = ymd("2023-08-01"), 
               label = glue("The employment rate for ",
                            "<strong style='color:{dotted_cols[2]}'>women</strong> is now well above<br>its 2019 average."),
               color = "grey30", box.size = 0, halign = 1, width = unit(2, "in"),
               box.padding = unit(0, "pt"), fontface = "bold") +
  annotate(geom = "curve", x = ymd("2023-08-01"), xend = ymd("2023-06-15"),
           y = 71.5, yend = 74.5, arrow = arrow(length = unit(.25, "cm")), 
           curvature = .15, color = "grey30") +
  scale_y_continuous(breaks = seq(from = 60, to = 90, by = 10),
                     limits = c(60, 91.5), expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0), limits = c(ymd("2018-09-01"), ymd("2024-04-01")),
               breaks = seq.Date(from = ymd("2019-01-01"),
                                 to = ymd("2023-01-01"), by = "year"),
               labels = 2019:2023) +
  scale_color_identity() +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(color = "grey50"),
        axis.ticks.length.x = unit(6, "pt"),
        axis.text.x = element_text(color = "grey50", size = 10,
                                   margin = margin(t = 5)),
        plot.title = element_text(hjust = .5, face = "bold",
                                  margin = margin(b = 20)),
        plot.caption = element_textbox(color = "grey50", hjust = 0,
                                    family = "serif", size = 11,
                                    width = unit(6, "in"), lineheight = 1.1, face = "bold")) +
  labs(x = "",
       y = "",
       title = "Share of people ages 25 to 54 who are employed, by gender",
       caption = glue("Note: Data is as of June 2023 and is seasonally adjusted. ",
                      "<span style='color:{'white'};font-size:8pt;'>\u2022</span> ",
                      "<span style='color:{'grey80'};font-size:10pt;'>\u2022</span> ",
                      "<span style='color:{'white'};font-size:8pt;'>\u2022</span> ",
                  "Source: Bureau of Labor Statistics. ",
                  "<span style='color:{'white'};font-size:8pt;'>\u2022</span> ",
                  "<span style='color:{'grey80'};font-size:10pt;'>\u2022</span>  ",
                  "<span style='color:{'white'};font-size:8pt;'>\u2022</span> ",
                  "Originally by NYTimes, recreated in R by Spencer Schien (@MrPecners)"))

ggsave("imgs/recreation.png", bg = "white", w = unit(9, "in"), h = 6)

