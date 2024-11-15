library(tidyverse)
library(plotly)

xmas_billboard <- read_csv("data/christmas_billboard_data.csv")

# Format dates and reformat years by holiday season
xmas_billboard <- xmas_billboard |>
  mutate(weekid = mdy(weekid)) |>
  mutate(season_yr = case_when(
    month(weekid) < 2 ~ year - 1, # if Jan/Feb of new year, assign to prev year
    TRUE ~ year
    )
  )

View(xmas_billboard)

# TEST OUT PLOT
# View Billboard chart battle among Christmas songs each year since 1958
# by weeks on chart or by chart position


# TEST OUT INTERACTIVE TABLE
# View all-time rankings or rankings within a specified time range




