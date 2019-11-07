library(tidyverse)
library(shiny)
library(shinythemes)
library(ggrepel)
load("./odata.rda")
oresults <- oresults %>% 
  filter(!relay) %>%
  unite("event", distance, style, sep = " ", remove = FALSE) %>%
  select(result_id, ioc_code, time, family_name, first_name, 
         competition, gender, event, year, distance,
         series, pool) 

osplits <- osplits %>%
  semi_join(ws, by = "result_id") %>%
  filter(split_distance == 50) %>%
  select(-split_distance)

min_year <- ws$year %>% min()
max_year <- ws$year %>% max()
divisions <- c("Women" = "Women", 
                 "Men" = "Men", 
                 "Both" = "Both")
events <- ws %>% 
  arrange(distance, event) %>% 
  pull(event) %>% unique()

ui <- fluidPage(theme = shinytheme("darkly"),
  titlePanel("Olympic Historic Results Dashboard"),
  wellPanel(
    fluidRow(
      column(5,
        selectInput(inputId = "event", label = "Event:", 
                    choices = events, selected = "400 Freestyle")
      ),
      column(5, offset = 1,
        selectInput(inputId = "division", label = "Division:", 
                    choices = divisions)
      )
    )
  ),
  tabsetPanel(
    tabPanel("Olympic Records",plotOutput(outputId = "orecord_plot")),
    tabPanel("Final Times", plotOutput(outputId = "time_plot", width = "100%")),
    tabPanel("Splits", width = "100%",
             plotOutput(outputId = "split_plot"),
             br(),
             "Show",
             fluidRow(
               column(3, offset = 1,
                 selectInput("split_plot_overlay", label = "Overlay:",
                             choices = c("Boxplot" = "boxplot",
                                         "Line" = "line",
                                         "None" = "none"), selected = "none")
               ),
               conditionalPanel(condition = "input.split_plot_overlay == 'line'",
                 column(2, offset = 1,
                        sliderInput("num_lines", label = "Quantiles:",
                                    min = 1, max = 4, value = 1, step = 1)
                        )
               )
             )
          
    )
  )
  
)

server <- function(input, output, session) {
  
  # Set Division Options to Only ones which exist for the event
  reactive({
    possible_genders <- oresults %>%
      filter(event == input$event) %>%
      pull(gender)
    if(length(possible_genders) > 1){
      updateSelectInput(session, "division",
                        choices = divisions)
    } else {
      updateSelectInput(session, "division",
                        choices = c(possible_genders = possible_genders))
    }
  })
  
  # Translate divisions in vector of genders
  genders <- reactive({
    if(input$division == "Both"){
      c("Men","Women")
    } else {
      input$division
    }
  })
  
  selected_results <- reactive({
    data <- oresults %>% 
      filter(event == input$event,
             gender %in% genders(),
             !is.na(time)
      )
    data
  })
  
  selected_splits <- reactive({
    osplits %>% 
      inner_join(selected_results(), by = "result_id")
  })
  
  olympic_bests <- reactive({
    selected_results() %>%
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
    p <- selected_results() %>%
      ggplot(aes(time, stat(count))) + 
      geom_density() + 
      theme_light() + 
      labs(x = "Time (s)", y = "Frequency")
    
    if(length(genders()) > 1){
      p <- p + facet_grid(. ~ gender)
    }
    
    p
    
  })
  
  output$split_plot <- renderPlot({
    p <- selected_splits() %>%
      ggplot(aes(leg, split)) + 
      geom_point(alpha = .5, position = "jitter") + 
      theme_light() + 
      labs(x = "Length", y = "Split Time (s)")
    
    if(length(genders()) > 1){
      p <- p + facet_grid(. ~ gender)
    }
    
    if(input$split_plot_overlay == "boxplot"){
      p <- p + geom_boxplot(aes(group = leg), outlier.shape = NA)
    }
    
    if(input$split_plot_overlay == "line"){
      f <- ecdf(selected_splits()$time)
      split_quantiles <- selected_splits() %>% 
        group_by(gender) %>%
        mutate(quantile = {
          f <- ecdf(time)
          ceiling(f(time)*input$num_lines)
        }) %>%
        ungroup()
        
      p <- p + 
        suppressWarnings(geom_smooth(data = split_quantiles, 
                    mapping = aes(group = quantile),
                    method = "loess",
                    se = FALSE))
    }
    
    p
    
    })
  
  output$orecord_plot <- renderPlot({
    p <- olympic_bests() %>% ggplot(aes(x = year, y = best, label = name)) + 
      geom_line() + 
      geom_point(mapping = aes(color = `New OR?`)) +
      theme_light() +
      scale_color_manual(values=c("#999999", "#E69F00")) +
      labs(x = "Year", y = "Olympic Best Time (s)") +
      geom_label_repel()
    
    if(length(genders()) > 1){
      p <- p + facet_grid(. ~ gender)
    }
    
    p
      
  })
}

shinyApp(ui = ui, server = server)