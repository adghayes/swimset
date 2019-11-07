library(tidyverse)
library(shiny)
library(shinythemes)
library(ggrepel)
load("../data/fina.rda")
ws <- fina_join() %>% 
  filter(!relay) %>%
  unite("event", distance, style, sep = " ", remove = FALSE) %>%
  select(result_id, ioc_code, time, family_name, first_name, 
         competition, gender, event, year, distance,
         series, pool) 

ws_splits <- ws %>%
  left_join(splits, by = "result_id") %>%
  filter(split_distance == 50) %>%
  select(-split_distance)

min_year <- ws$year %>% min()
max_year <- ws$year %>% max()
genders <- ws$gender %>% unique()
events <- ws %>% 
  filter(series == "Olympic Games") %>%
  arrange(distance, event) %>% 
  pull(event) %>% unique()
pools <- ws$pool %>% unique() %>% sort()
series <- ws$series %>% unique()


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "event", label = "Event", choices = events, selected = "400 Freestyle"),
      checkboxGroupInput(inputId = "gender", label = "Division", choices = genders, selected = "Women"),
      checkboxGroupInput(inputId = "pool", label = "Pool Length", choices = pools, selected = "50m"),
      checkboxGroupInput(inputId = "series", label = "Competitions Included", choices = series,
                        selected = c("Olympic Games", "Championships (50m)")),
      sliderInput(inputId = "year_range", label = "Year Range",
                 min = min_year, max = max_year, value = c(min_year, max_year),
                 step = 1, sep = "")
    ),
    mainPanel(
      plotOutput(outputId = "time_plot"),
      plotOutput(outputId = "split_plot"),
      plotOutput(outputId = "orecord_plot")
    )
  )
)

server <- function(input, output) {
  
  selected_results <- reactive({
    ws %>% 
      filter(event == input$event,
             gender %in% input$gender,
             pool %in% input$pool,
             series %in% input$series,
             year > input$year_range[1],
             year < input$year_range[2],
             !is.na(time)
      )
  })
  
  selected_splits <- reactive({
    ws_splits %>% 
      filter(event == input$event,
             gender %in% input$gender,
             pool %in% input$pool,
             series %in% input$series,
             year > input$year_range[1],
             year < input$year_range[2] 
             )
  })
  
  olympic_bests <- reactive({
    ws %>%
      filter(series == "Olympic Games",
             event == input$event,
             gender %in% input$gender, 
             !is.na(time)) %>%
      group_by(year, gender) %>%
      summarise(best = min(time), name = family_name[which.min(time)]) %>%
      group_by(gender) %>%
      arrange(year) %>%
      mutate(
        or = cummin(best),
        or_diff = best - lag(or),
        `New OR?` = if_else(or_diff < 0 | is.na(or_diff), "Yes", "No"),
        name = if_else(`New OR?` == "Yes", name, "")
      ) %>%
      ungroup()
  })

  output$time_plot <- renderPlot({
    selected_results() %>%
      ggplot(aes(time, stat(count))) + 
      geom_density() + 
      theme_light() + 
      labs(x = "Time (s)", y = "Frequency") +
      facet_grid(pool ~ gender)
  })
  
  output$split_plot <- renderPlot({
    selected_splits() %>%
      ggplot(aes(leg, split)) + 
      geom_point(alpha = .10, position = "jitter") + 
      theme_light() + 
      labs(x = "Length", y = "Split Time (s)") +
      facet_grid(pool ~ gender)
    })
  
  output$orecord_plot <- renderPlot({
    olympic_bests() %>% ggplot(aes(x = year, y = best, label = name)) + 
      geom_line() + 
      geom_point(mapping = aes(color = `New OR?`)) +
      theme_light() +
      facet_grid(. ~ gender) +
      labs(x = "Year", y = "Olympic Best Time (s)") +
      geom_label_repel(rot = 45)
      
  })
}

shinyApp(ui = ui, server = server)