library(tidyverse)
library(plotly)

# TEST OUT PLOT
# View Billboard chart battle among Christmas songs each year since 1958
# by weeks on chart or by chart position
plot_ly(starwars, 
        x = ~name, 
        y = ~height, 
        type = 'bar', 
        name = 'Height',
        marker = list(color = 'blue')) |>
  layout(
    title = "Comparison of Height",
    xaxis = list(title = "Character",
                 categoryorder = "total ascending"),
    yaxis = list(title = "Height"),
    barmode = 'group' # Use 'stack' for stacked bars
  )

# TEST OUT INTERACTIVE TABLE
# View all-time rankings or rankings within a specified time range




