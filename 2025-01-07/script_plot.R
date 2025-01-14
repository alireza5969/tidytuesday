

library(tidyverse)
library(jalcal)
library(camcorder)
library(ggtext)

# raw data handling
{
  folders <- list.files("2025-01-07/data_raw/")
  
  
  raw_tbl <- 
    readxl::read_excel(
      "2025-01-07/data_raw/41_Fars/41 Fars/Hourly Data/1.xlsx",
      skip = 1) %>%
    .[0,]
  
  
  for (i in 1:length(folders)) {
    
    cat(i %>% crayon::red())
    
    base_path <- 
      paste0(
        "2025-01-07/data_raw/", folders[i], "/41 Fars/Hourly Data/"
      )
    
    excel_names <- 
      list.files(base_path)
    
    for (j in 1:length(excel_names)) {
      
      print(j)
      
      raw_tbl <- 
        raw_tbl %>% 
        bind_rows(
          readxl::read_excel(
            path = paste0(base_path, excel_names[j]), skip = 1
          )
        )
      
    }
    
  }
}




raw_tbl <- 
  raw_tbl %>% 
  janitor::clean_names() 

raw_tbl %>% write_csv("2025-01-07/data_temp/clean_data.csv", quote = "all")

raw_tbl %>% write_rds(file = "2025-01-07/data_temp/clean_data.rds")


greg_date_tbl <- 
  
  raw_tbl %>%
  
  separate_wider_delim(
    cols = c(zman_payan, zman_shrwʿ), 
    delim = " ", 
    names_sep = "_",
    names = c("date", "time")
    ) %>%
  
  separate_wider_delim(
    cols = c(zman_payan_date, zman_shrwʿ_date),
    delim = "/", 
    names_sep = "_",
    names = c("year", "month", "day")
    ) %>%
  
  rowwise() %>%
  
  mutate(
    end_date = jal2greg(
      year = zman_payan_date_year, 
      month = zman_payan_date_month, 
      day = zman_payan_date_day
      ),
    start_date = jal2greg(
      year = zman_shrwʿ_date_year, 
      month = zman_shrwʿ_date_month, 
      day = zman_shrwʿ_date_day
      )
    ) %>% 
  
  ungroup() %>% 
  
  select(-contains("zman_payan_date_"), -contains("zman_shrwʿ_date_"))


greg_date_tbl %>% write_rds("2025-01-07/data_temp/greg_data.rds")


greg_date_time_tbl <- 
  
  greg_date_tbl %>% 
  
  select(-nam_mhwr) %>% 
  
  mutate(
    start_date_time = ymd_hms(paste(start_date, zman_shrwʿ_time), tz = "Asia/Tehran"),
    end_date_time = ymd_hms(paste(end_date, zman_payan_time), tz = "Asia/Tehran")
    ) %>% 
  
  select(-c(start_date, end_date, zman_shrwʿ_time, zman_payan_time))


greg_date_time_tbl <- 
  greg_date_time_tbl %>% 
  rename(
    road_code = kd_mhwr,
    active_cam_duration = mdt_zman_karkrd_dqyqh,
    vehicle_number_all = tʿdad_kl_wsylh_nqlyh,
    vehicle_number_class1 = tʿdad_wsylh_nqlyh_klas_1,
    vehicle_number_class2 = tʿdad_wsylh_nqlyh_klas_2,
    vehicle_number_class3 = tʿdad_wsylh_nqlyh_klas_3,
    vehicle_number_class4 = tʿdad_wsylh_nqlyh_klas_4,
    vehicle_number_class5 = tʿdad_wsylh_nqlyh_klas_5,
    avg_speed = srʿt_mtwst,
    speed_violation_number = tʿdad_tkhlf_srʿt_ghyr_mjaz,
    distance_violation_number = tʿdad_tkhlf_faslh_ghyr_mjaz,
    overtaking_violation_number = tʿdad_tkhlf_sbqt_ghyr_mjaz,
    estimated_number = tʿdad_brawrd_shdh
  )


plot_data <- 
  
  greg_date_time_tbl %>% 
  
  mutate(
    month = month(start_date_time),
    day = day(start_date_time),
    hour = hour(start_date_time)
    ) %>% 
  
  group_by(month, day, hour) %>% 
  
  summarise(
    avg_vehicle_per_minute = mean(vehicle_number_all / active_cam_duration),
    avg_speed = sum(avg_speed * vehicle_number_all) / sum(vehicle_number_all),
    avg_violation_per_minute = mean((speed_violation_number + distance_violation_number + overtaking_violation_number) / active_cam_duration), 
    active_cam = mean(active_cam_duration),
    .groups = "drop"
    ) %>% 
  
  ungroup %>% 
  
  pivot_longer(
    cols = c(avg_vehicle_per_minute, avg_speed, avg_violation_per_minute), 
    values_to = "value",
    names_to = "measure"
    ) %>% 
  
  arrange(month, day, hour) %>% 
  
  mutate(month = factor(month)) %>%
  
  group_by(measure) %>% 
  
  mutate(z_score = scale(value) %>% as.vector()) %>% 
  
  ungroup()
  

scale_to_range <- function(x, new_min, new_max) {
  min_x <- min(x)
  max_x <- max(x)
  scaled_x <- ((x - min_x) / (max_x - min_x)) * (new_max - new_min) + new_min
  return(scaled_x)
}


plot_data <- 
  plot_data %>% 
  mutate(active_cam = scale_to_range(active_cam, new_min = 0, new_max = 23)
)

  
gg_record(
  dir = "2025-01-07/plot_temp",
  device = "jpeg",
  width = 12, 
  height = 6, 
  units = "in",
  dpi = 320
  )




plot_data %>%
  
  mutate(
    month = as.integer(month),
    datetime = make_date(year = 2024, month = month, day = day)
    ) %>%
  
  
  ggplot(data = ., aes(x = datetime, y = hour, fill = z_score)) + 
  
  geom_tile() + 
    scale_fill_gradient(
    low = "#00BFFF", 
    high = "#FF6347",
    na.value = "white") +
  
  geom_line(aes(y = active_cam), color = "#FFD700", alpha = .45) +
  
  scale_x_date(date_breaks = "2 months", date_labels =  "%b") +
  
  labs(
    title = "On the Road Again: Reckless Driving Trends",
    subtitle = str_wrap(
      "This heatmap visualizes road traffic in **Fars, Iran**, highlighting periods of increased driver activity, higher speeds, and more frequent violations over the past year. <br>Notably, traffic surges during the Persian New Year (**Nowrooz**) correlate with an uptick in violations, although average speeds remain consistent with other times of <br>the year. Early mornings tend to show lighter traffic but higher average speeds, particularly around dawn. Yellow lines denote the duration of active traffic control <br>camera monitoring. Interestingly, the heatmap suggests that times with heavy vehicle presence and violation rates, as well as peak speeds, often coincide with periods <br>of reduced camera activity.",
      width = 130
    ),
    caption = "#tidytuesday · Source: Traffic Police of Iran · Graphic: Alireza Sadeghi"
  ) +
  
  facet_wrap(
    ~ measure, 
    labeller = labeller(
      measure = c( 
        avg_speed = "Average Speed",
        avg_vehicle_per_minute = "Average Number of Vehicles / Minute",
        avg_violation_per_minute = "Average Number of Violation Occurrences / Minute"
  ))) +
  
  ylab(label = "Hour") +
  
  coord_cartesian(expand = FALSE) +
  
  
  theme_minimal() +
  
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_text(face = "bold"),
    legend.position = "none", 
    plot.title = element_text(face = "bold"), 
    plot.subtitle = element_markdown(
      linewidth = 1, 
      lineheight = 1.2, 
      colour = "gray20"
      ), 
    panel.grid = element_blank(), 
    axis.text.x = element_text(vjust = 0), 
    plot.background = element_blank(), 
    panel.background = element_blank(),
    strip.background = element_blank(), 
    legend.background = element_blank(),
    panel.spacing.y = unit(0, "lines"),
    strip.text = element_text(face = "bold", vjust = 0),
    plot.margin = margin(10, 10, 5, 10), 
    panel.spacing = unit(1, units = "cm"),
    # strip.placement = "inside", 
    strip.clip = "off", 
    plot.caption = element_text(margin = margin(10, 0, 0, 0), colour = "gray50")
    )


ggsave(
  filename = "2025-01-07/plot.jpeg", 
  device = "jpeg",
  width = 12, 
  height = 6, 
  units = "in",
  dpi = 320
  )
