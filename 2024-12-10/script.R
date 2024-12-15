# shout-out to Steven Ponce, Nicola Rennie, Georgios Karamanis, and other 
# who I learned alot from them



library(tidytuesdayR)
library(tidyverse)
library(camcorder)
library(sysfonts)
library(showtext)
library(ggtext)
library(glue)
library(gghighlight)

tt <- tt_load(x = "2024-12-10")

data_raw <- 
  tt$parfumo_data_clean %>% 
  janitor::clean_names()

data_weighted <- 
  data_raw %>% 
  filter(!is.na(rating_count)) %>% 
  mutate(
    weighted_rate = 
      (
        (rating_value * rating_count) +
          (10 * mean(rating_value))
        # minimum votes required to be considered is set to 10
      ) /
      (
        10 * mean(rating_value)
      )
  ) %>% 
  select(name:release_year, rating_value, rating_count, weighted_rate) %>% 
  arrange(-weighted_rate)


top_ten_brands <- 
  data_weighted %>% 
  slice_head(n = 10) %>% 
  pull(brand)


data_top_ten <- 
  data_weighted %>% 
  filter(brand %in% top_ten_brands) %>% 
  drop_na()


gg_record(
  dir = "2024-12-10/temp_files/", 
  device = "png", 
  width = 40, 
  height = 20, 
  units = "cm", 
  dpi = 300
  )


font_add_google(name = "Raleway", family = "raleway", regular.wt = 400)

showtext_auto()


p_title <- 
  "Average performance of top ten brands perfume since 1709 over decades" %>% 
  str_wrap(width = 100)

p_subtitle <- 
  "A weighted rate was calculated from average rating and rating count using Bayesian average.\n
Arithmetic mean was used to estimate yearly performance\n
Top ten brands: the brands of the ten fragrances with highest weighted rate"


p_cap <- 
  "#TidyTuesday: 2024 week 50 &bull; Source: parfumo.com<br>by Alireza Sadeghi"



data_top_ten %>% 
  
  group_by(brand , release_year) %>% 
  
  summarise(mean_score = mean(weighted_rate)) %>% 
  
  ungroup() %>% 
  
  ggplot(aes(x = release_year, y = mean_score, group = brand)) +
  
  geom_line(
    color = "#4E5283", 
    linewidth = 1) +
  
  gghighlight(
    use_direct_label = FALSE, 
    unhighlighted_params = 
      list(
        # color = "#FFFFE8", 
        # alpha = .8,
        linewidth = .3
        )
    ) +
  
  # geom_point(color = "#88B7B5", size = 2.5) +
  
  facet_wrap(~ brand, nrow = 2) +
  
  theme_minimal(base_size = 50) + 
  
  labs(title = p_title, subtitle = p_subtitle, caption = p_cap) + 
  
  xlab("Release Year") + 
  ylab("Weighted Rating") +
  
  theme(
    
    text = element_text(family = "raleway"), 
    
    # strip.text = element_text(face = "bold"),
    
    axis.title = element_text(face = "bold"),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(linewidth = .8),
    
    panel.grid.major.x = element_blank(),
    
    plot.subtitle = element_text(
      family = "raleway",
      size = 50, lineheight = .15,
      face = "plain", 
      color = "gray15",
      ),
    
    plot.title = element_text(
      face = "bold",
      size = 60,
      lineheight = .3, 
      vjust = 0,
      hjust = 0),
    
    plot.background = element_rect(fill = "#F7F3E6", colour = NA),
    
    strip.text = element_text(
      size = 50,
      face = "bold.italic",
      vjust = -3),
    
    plot.caption = element_markdown(
      family = "raleway",
      lineheight = .1,
      size   = 35,
      color  = "gray35",
      hjust  = 1,
      margin = margin(t = 5)
    ), 
    
    axis.text.x = element_text(vjust = 5),
    axis.text.y = element_text(hjust = 1.5)
    
    
    )

ggsave( 
  path = "2024-12-10/", 
  filename = "plot.png",
  device = "png", 
  width = 40, 
  height = 20, 
  units = "cm", 
  dpi = 300
  
)
  