#function - create a bee map


bee_map_y <-function(data_year) {
  
   colony_y <- colony |> 
    mutate(id = US_name_to_abb(state)) |> 
    filter(year == data_year) 
  
   us_map <- fortify(us, region="iso3166_2") |> 
    left_join(colony_y, by = "id") |> 
    group_by(id) %>% 
    mutate(long_m = mean(long), 
           lat_m = mean(lat),
           colony_lost_pct_m = mean(colony_lost_pct, na.rm = T)) |> 
    mutate(percent_display = round(colony_lost_pct_m/100, 3)) |> 
    mutate(percent_display = scales::percent(percent_display, accuracy = 1))
  
  ggplot(data = us_map,
         mapping = aes(map_id = id,
                       x = long,
                       y = lat,
                       fill = colony_lost_pct_m)) +
    geom_map(map = us_map,
             colour = my_palette$gray,
             linewidth = 0.6) +
    geom_text(data = us_map,
              aes(label = id,
                  x = long_m,
                  y = lat_m),
              size = 8) +
    geom_text(data = us_map,
              inherit.aes = T,
              aes(label = percent_display,
                  x = long_m,
                  y = lat_m),
              size = 6,
              vjust = 1.9) +
    scale_fill_gradientn(colours = c("#ffffb2", 
                                     my_palette$yellow, 
                                     my_palette$orange),
                         na.value = my_palette$gray) +
    ggtitle(paste(#"Bee Colony Loss in the USA in",
                  us_map$year[8])) +
    coord_map() +
    theme_void() +
    theme(legend.position = "", 
          legend.title = element_text(),
          plot.title = element_text(hjust = 0.5,
                                    vjust = -12,
                                    margin = margin(t = 10), 
                                    size = 24))
}



bee_map_y(2021)
