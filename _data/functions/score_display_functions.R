## get Index or goal score (area weighted average)
# x = "Index"

score <- function(x, ohi_scores){round(weighted.mean(get(x, ohi_scores), ohi_scores$are_km2), 0)}

dial <- function(score, colors){
  
  color_stops <- lapply(1:100, function(i) {
    list(from = i-1, to = i, color = colors[i])
  })
  
  # Create a Highcharter object with gauge type
  hc <- highchart() %>%
    hc_chart(type = "gauge", plotBackgroundColor = NULL,
             plotBackgroundImage = NULL, plotBorderWidth = 0,
             plotShadow = FALSE) %>%
    hc_title(text = "") %>%
    hc_pane(startAngle = -140, endAngle = 140) %>%
    hc_yAxis(min = 0, max = 100,
             title = list(text = ""),
             lineWidth = 0,
             minorTickInterval = NULL,
             minorTickLength = 0,
             tickPixelInterval = 30,
             tickWidth = 2,
             labels = list(step = 2, rotation = "auto"),
             plotBands = color_stops) %>%
    hc_add_series(name = "", data = list(score),
                  tooltip = list(valueSuffix = " units"))
  # Render the chart
  hc
}

map_figure <- function(ohi_scores, goal="Index"){
  
  getColor <- function(score) {
    colors[score + 1]  # Adding 1 because R indexing starts at 1
  }
  
  # HTML content for the legend
  color0 <- getColor(0)
  color25 <- getColor(25)
  color50 <- getColor(50)
  color75 <- getColor(75)
  color100 <- getColor(100)
  
  # Create the CSS for the color gradient with actual color values
  gradient_css <- paste0("background: linear-gradient(to right, ",
                         color0, ", ",
                         color25, ", ",
                         color50, ", ",
                         color75, ", ",
                         color100, ");")
  
  # HTML content for the legend
  legend_html <- htmltools::HTML(
    paste0(
      "<div style='padding: 6px; background-color: white; border-radius: 4px; border: 1px solid #ccc;'>",
      "<div style='width: 100%; height: 10px; border-radius: 2px; ", gradient_css, "'></div>",
      "<div style='margin-top: 6px; text-align: center;'>0&nbsp;&nbsp;25&nbsp;&nbsp;50&nbsp;&nbsp;75&nbsp;&nbsp;100</div>",
      "</div>"
    )
  )
  
  
  ## data
  scores_map <- ohi_scores %>%
    select(country, iso3c, wtr_bdy, goal_value=all_of(goal))
  
  # leaf maps: https://leaflet-extras.github.io/leaflet-providers/preview/index.html
  #st_bbox(eez)
  
  map <-  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = scores_map, 
                fillColor = ~getColor(goal_value), 
                fillOpacity = 1,
                weight = 1, 
                color = "brown", 
                opacity = 0.8,
                popup = ~paste(country, "<br>",  wtr_bdy, "<br>",
                               goal, "score = ", round(goal_value, 0))) %>% 
    addControl(html = legend_html, position = "bottomright") %>%    
    addPolygons(data = land,
                fillColor = "lightgray",
                weight = 2, 
                fillOpacity = 0.5, 
                color="darkgray",
                popup = ~paste(formal_en)) %>%
    setView(lng=41, lat=20, zoom = 4)
  
  map
  
}