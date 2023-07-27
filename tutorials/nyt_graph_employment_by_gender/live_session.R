library(tidyverse)
library(glue)
library(ggtext)

# load the data

f <- list.files("data")
ff <- paste0("data/", f)

dd <- map_df(ff, function(i) {
  if (str_detect(i, "women")) {
    f_name <- "women"
  } else {
    f_name <- "men"
  }
  d <- read_csv(i) |> 
    mutate(group = f_name)
  
})


to_plot <- dd |> 
  mutate(d_date = ym(Label))

# get 2019 avg
avg_df <- to_plot |> 
  filter(year(d_date) == "2019") |> 
  group_by(group) |> 
  summarise(avg = mean(Value))

# get end labels
end_labs <- to_plot |> 
  group_by(group) |> 
  filter(d_date == max(d_date))
  
colors <- c(
  "#80000f", # dark men
  "#BC3938", # light men
  "#ce514d", # dark women
  "#FFA89A" # light women
)

long_anno <- glue("The employment rate for ",
                  "<span style='color:{colors[3]}'>women</span> ",
                  "is now well above its 2019 average.")

to_plot |> 
  ggplot(aes(x = d_date, y = Value, 
             color = ifelse(group == "men", colors[2], colors[4]))) +
  geom_line(linewidth = 1) +
  scale_color_identity() +
  # y axis text
  annotate(geom = "text", 
           x = ymd("2018-10-01"),
           y = c(60, 70, 80, 90),
           label = c(60, 70, 80, "90%"),
           vjust = -.5, hjust = 0, color = "grey50") +
  # set up 2019 avg line
  geom_segment(data = avg_df,
               aes(x = min(to_plot$d_date),
                   xend = max(to_plot$d_date),
                   y = avg,
                   yend = avg,
                   color = ifelse(group == "men", colors[1], colors[3])),
               linetype = 3, size = .75) +
  # annotate '2019 avg'
  geom_text(data = avg_df, label = "2019 average",
            aes(y = avg + 1, x = ymd("2020-10-01"),
                color = ifelse(group == "men", colors[1], colors[3])), 
            inherit.aes = FALSE) +
  # label men & women
  geom_text(data = end_labs,
            aes(label = str_to_title(group), 
                x = d_date,
                y = Value,
                color = ifelse(group == "men", colors[1], colors[3])),
            fontface = "bold", hjust = -.25) +
  # add long annotation
  geom_textbox(x = ymd("2023-06-01"), y = 70, 
               label = long_anno, color = "grey20", box.size = 0,
               halign = 1, box.padding = unit(0, "cm")) +
  # add arrow
  annotate(geom = "curve", x = ymd("2023-08-01"), xend = ymd("2023-06-15"),
           y = 72, yend = 74.5, arrow = arrow(length = unit(.25, "cm")),
           color = "grey20", curvature = .2, angle = 60) +
  scale_y_continuous(limits = c(60, 91),
                     labels = c(60, 70, 80, "90%"),
                     expand = c(0, 0)) +
  scale_color_identity() +
  scale_x_date(expand = c(0, 0),
               limits = c(ymd("2018-10-01"), ymd("2024-06-01")),
               breaks = seq.Date(from = ymd("2019-01-01"), 
                                 to = ymd("2023-01-01"),
                                 by = "year"),
               labels = 2019:2023) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, 
                                   color = "grey50",
                                   margin = margin(t = 10)),
        axis.ticks.x = element_line(color = "grey20"),
        axis.ticks.length = unit(.15, "cm"),
        plot.title = element_text(hjust = .5, face = "bold",
                                  margin = margin(b = 20)),
        plot.caption = element_textbox(w = unit(7, "in"),
                                       color = "grey50", 
                                       size = 10, halign = 0,
                                       hjust = 0, lineheight = 1.2)) +
  labs(x = "", y = "",
       title = "Share of people ages 25 to 54 who are employed, by gender",
       caption = glue("Note: Data is as of June 2023 and is seasonally adjusted. ",
                      "<span style='color:{'grey80'}'>\u2022</span> ",
                      "Source: Bureau of Labor Statistics ", 
                      "<span style='color:{'grey80'}'>\u2022</span> ",
                      "Originally by The New York Times, recreated by Spencer Schien (@MrPecners)"))

# save file
ggsave(filename = "live_plot.png", bg = "white", w = 9, h = 6)
