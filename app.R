library(dash)
library(readr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(ggplot2)
library(ggthemes)
library(plotly)

raw_data <- read_csv("data/olympics_data.csv") 
  
app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

app$layout(
  div(
    list(
      dccRadioItems(
        id = 'season',
        options = list(list(label = 'Summer', value = 'summer'),
                       list(label = 'Winter', value = 'winter'),
                       list(label = "All", value = "all")),
        value = 'all'),
      dccChecklist(
        id = 'medal_type',
        options = list(list(label = 'Gold', value = 'Gold'),
                       list(label = 'Silver', value = 'Silver'),
                       list(label = 'Bronze', value = 'Bronze')),
        value = list('Gold', 'Silver', 'Bronze')),
      dccGraph(id='bubble'),
      dccSlider(
        id = 'medals_by_country',
        value = 2000,
        min = 1896,
        max = 2016,
        step = 2,
        mark = NULL,
        tooltip = list(placement = 'bottom', always_visible = TRUE),
        included = FALSE),
      dccStore(id='filter_data')
      )
    )
)

app$callback(
  output('filter_data', 'data'),
  list(input('season', 'value'),
       input('medal_type', 'value')),
  function(sel_season, medal_type) {
    temp <- raw_data
    
    if (sel_season != 'all') {
      temp <- temp %>%
        filter(season == sel_season)
    }
    
    if (length(medal_type) > 0) {
      temp <- drop_na(temp) %>%
        filter(medal %in% medal_type)
    } else {
      temp <- drop_na(temp)
    }
    
    temp %>% toJSON()
  }
)

app$callback(
  output('bubble', 'figure'),
  list(input('filter_data', 'data'),
       input('medals_by_country', 'value')),
  function(data, sel_year){
    temp <- fromJSON(data, simplifyDataFrame = TRUE)
    sel_year <- as.integer(sel_year)
    
    temp <- temp %>%
      filter(year == sel_year)
    
    athletes <- raw_data %>%
      filter(year == sel_year)
    
    athletes <- athletes %>%
      group_by(noc) %>%
      summarise(athletes = n_distinct(id))
    
    medals <- temp %>%
      group_by(noc) %>%
      summarise(metal_count = n())
    
    graph_data <- merge(athletes, medals)
    
    graph_data <- graph_data %>%
      mutate(ave_metals = metal_count / athletes)
    
    p <- ggplot(graph_data) +
      geom_point(aes(x = athletes,
                     y = ave_metals,
                     size = metal_count),
                 stat = "identity") +
      scale_color_tableau()
    
    ggplotly(p + aes(text = noc), tooltip = list('noc', 'size'))
  }
)

app$run_server(debug = T, host = '0.0.0.0')
