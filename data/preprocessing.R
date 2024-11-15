library(tidyverse)
library(plotly)

xmas_billboard <- read_csv("data/christmas_billboard_data.csv")

# DATA PREPROCESSING STEPS:
# 1) FORMAT DATES/VARS PROPERLY
# 2) REFORMAT BY HOLIDAY SEASON (I.E. December '08/'09 count as same year)
# 3) SWAP NAs with 0s
# 4) AGGREGATE BY HOLIDAY SEASON

# Format date
xmas_billboard <- xmas_billboard |>
  mutate(weekid = mdy(weekid))

# Aggregate by calendar year (keeping distinct song/artist)
# @AF: year won't work because of new years, so need to make new holiday season var
# @AF: limit to since 1997





xmas_billboard_agg <- xmas_billboard |>
  group_by(song, year) |> 
  reframe(total_weeks_on_chart = sum(weeks_on_chart),
          peak_position = (100 - min(peak_position)))

xmas_billboard_agg

# Limit to only the 'staple' popular Christmas songs
# ...


song_colors <- setNames(rainbow(length(unique(xmas_billboard_agg$song))),
                        unique(xmas_billboard_agg$song))

plot_ly(data = xmas_billboard_agg, 
        x = ~year, 
        y = ~peak_position, 
        color = ~song, 
        type = 'scatter', 
        mode = 'lines+markers', 
        colors = song_colors,
        text = ~song,
        hoverinfo = 'text') %>%
  layout(showlegend = FALSE, 
         xaxis = list(title = "Year", range = c(1999, 2017)), 
         yaxis = list(title = "Total Weeks on Chart"))


