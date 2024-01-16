## get Index or goal score (area weighted average)
# x = "Index"

score <- function(x, ohi_scores){round(weighted.mean(get(x, ohi_scores), ohi_scores$are_km2, na.rm=TRUE), 0)}


# Dial
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


# Map
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


# dimension plot
score_dimensions <- function(goal_tmp){
  # goal_tmp <- "AO"
  dims <- read_csv(here("_data/output/score_dimensions.csv")) %>%
    filter(goal == goal_tmp)
  
  tmp_pre <- data.frame(
    goal = goal_tmp,
    current_status = weighted.mean(dims$status, dims$are_km2),
    trend = weighted.mean(dims$trend, dims$are_km2),
    pressures = weighted.mean(dims$pressures, dims$are_km2),
    resilience = weighted.mean(dims$resilience, dims$are_km2),
    resilience_change = weighted.mean(dims$future, dims$are_km2), 
    score = weighted.mean(dims$score, dims$are_km2))
  
  tmp <- tmp_pre %>%
    mutate(trend_change = current_status*(1 + (2/3)*trend),
           pressure_change = current_status*(1 + (2/3)*trend +                       (1/3)*(0-(pressures)/100))) %>%
    select(c(goal, current_status, trend_change, pressure_change, resilience_change, score))
  
  tmp <- tidyr::pivot_longer(tmp, cols=-goal, names_to="dimension") 
  tmp$x <- c(1, 2, 3, 4, 5)
  tmp$point_labels <- c("current \nstatus", NA, NA, "future \nstatus", "final \nscore")
  tmp$color <- c("#FA9646", "darkgray", "darkgray", "#EE7E23", "#E0585C")
  tmp$size <- c(4, 3, 3, 4, 5)
  
  # Base plot setup
  
  p <- ggplot(tmp, aes(x = x, y = value)) +
    geom_point(aes(color = color, size = size)) +
    theme_minimal() +
    ylim(c(0, 100)) +
    scale_color_identity() +  # Use the color specified in the dataframe
    scale_size_identity() +    # Use the size specified in the dataframe
    theme(
      axis.text.x = element_blank(),  # Remove x-axis text
      axis.ticks.x = element_blank()  # Remove x-axis ticks
    ) +
    ylab("score") + 
    xlab("getting to the final score")
  
  rectangle1 <- c(xmin = 1, xmax = 2, ymin = 0, ymax = 100) 
  rectangle2 <- c(xmin = 2, xmax = 3, ymin = 0, ymax = 100) 
  rectangle3 <- c(xmin = 3, xmax = 4, ymin = 0, ymax = 100) 
  
  p <- p + annotate("rect", xmin = rectangle1["xmin"], xmax = rectangle1["xmax"], 
                    ymin = rectangle1["ymin"], ymax = rectangle1["ymax"], 
                    fill = "#CDCCCD", alpha = 0.2) +
    annotate("rect", xmin = rectangle2["xmin"], xmax = rectangle2["xmax"], 
             ymin = rectangle2["ymin"], ymax = rectangle2["ymax"], 
             fill = "#9CACAF", alpha = 0.2) +
    annotate("rect", xmin = rectangle3["xmin"], xmax = rectangle3["xmax"], 
             ymin = rectangle3["ymin"], ymax = rectangle3["ymax"], 
             fill = "#668B88", alpha = 0.2)
  
  
  p <- p + geom_segment(y=tmp_pre$current_status, yend=tmp_pre$current_status, x = 1, xend= 4.9, color = "orange", size = 0.4, alpha=0.75, linetype="dashed", arrow = arrow(type = "closed", length = unit(0.05, "inches")))
  
  p <- p + geom_segment(y=tmp_pre$resilience_change, yend=tmp_pre$resilience_change, x = 4, xend = 4.9, color = "red", size = 0.4, alpha=0.75, linetype="dashed", arrow = arrow(type = "closed", length = unit(0.05, "inches")))
  
  # Adding arrows and labels
  for(i in 1:(nrow(tmp)-2)) { # i=1
    x_start <- tmp$x[i]+0.1
    y_start <- tmp$value[i]
    x_end <- tmp$x[i+1]-0.1
    y_end <- tmp$value[i+1]
    
    # Add arrow
    p <- p + geom_segment(x = x_start, y = y_start, xend = x_end, yend = y_end,
                          arrow = arrow(type = "closed", length = unit(0.05, "inches")), color="darkgray")
    
    # Calculate midpoint for label
    mid_x <- (x_start + x_end) / 2
    mid_y <- 10
    # Add label at midpoint of the arrow
    arrow_names <- c("+ trend", "+ pressure", "+ resilience")
    
    p <- p + geom_text(x = mid_x, y = mid_y, label = arrow_names[i], vjust = -1, size=4, color="darkgray")
  }
  
  # Add labels for first and last point
  p <- p + geom_text(aes(label = point_labels), vjust = 1.5, size = 3)
  
  # add description of score line
  p <- p + annotate("text", x = 4.1, y = 10, vjust = -1, label = paste("Index score = ", round(tmp_pre$score, 0)), size = 3, hjust=0, color="red") 
  
  p <- p + geom_segment(x = 1.25, y = 0, xend = 4.75, yend = 0,
                        arrow = arrow(type = "closed", length = unit(0.1, "inches")), color="darkgray", size=2.5)
  
  # Print the plot
  print(p)
}