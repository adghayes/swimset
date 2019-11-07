library(tidyverse)
library(shiny)
library(shinythemes)
library(ggrepel)
load("./odata.rda")
oresults <- oresults %>% 
  filter(!relay) %>%
  unite("event", distance, style, sep = " ", remove = FALSE)

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
  titlePanel("Olympic Results Dashboard"),
  HTML(paste("<h6>data from <a href = \"fina.org\">fina.org</a>,",
            "code on <a href = \"https://github.com/adghayes/swimset\">",
            "github</a><h5>")),
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
    tabPanel("Times", 
             plotOutput(outputId = "time_plot", width = "100%"),
             br(),
             textOutput("time_info")
    ),
    tabPanel("Splits", width = "100%",
             plotOutput(outputId = "split_plot"),
             br(),
             textOutput("split_info"),
             br(),
             fluidRow(
               column(3, offset = 1,
                 selectInput("split_plot_overlay", label = "Overlay:",
                             choices = c("Boxplot" = "boxplot",
                                         "Line" = "line",
                                         "None" = "none"), selected = "boxplot")
               ),
               conditionalPanel(condition = "input.split_plot_overlay == 'line'",
                 column(2, offset = 0,
                        sliderInput("num_lines", label = "Quantiles:",
                                    min = 1, max = 4, value = 1, step = 1)
                        )
               ),
               conditionalPanel(condition = "input.split_plot_overlay == 'boxplot'",
                                column(2, offset = 0,
                                       checkboxInput("add_notch", label = "Add Notches:")
                                )
               )
             )
    )   
  )
)

server <- function(input, output, session) {
  
  # Translate divisions in vector of genders
  genders <- reactive({
    if(input$division == "Both"){
      c("Men","Women")
    } else {
      input$division
    }
  })
  
  selected_results <- reactive({
    
    # Check for non events
    req(input$event != "800 Freestyle" | input$division == "Women")
    req(input$event != "1500 Freestyle" | input$division == "Men")
    
    data <- oresults %>% 
      filter(event == input$event,
             gender %in% genders()
      )
    data
  })
  
  selected_splits <- reactive({
    osplits %>% 
      inner_join(selected_results(), by = "result_id")
  })
  
  olympic_bests <- reactive({
    selected_results() %>%
      filter(!is.na(time)) %>%
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
  
  output$time_info <- renderText({
    n_results <- selected_results() %>%
      filter(!is.na(time)) %>%
      nrow()
    n_non_times <- selected_results() %>%
      filter(is.na(time)) %>%
      nrow()
    first_year <- selected_results() %>%
      pull(year) %>% min()
    paste("Times for ", n_results,
          " distinct races are shown above, ",
          "starting as early as ", 
          first_year, ". ", 
          "There were also ", n_non_times,
          " races without times, either disqualifications, ",
          "did not finish, did not start, or missing data.", sep = "")
  })

  output$time_plot <- renderPlot({
    distance <- input$event %>%
      str_extract("\\d*") %>%
      as.integer()
    
    p <- selected_results() %>%
      filter(!is.na(time)) %>%
      ggplot(aes(time, stat(count)))  + 
      scale_x_continuous(breaks = seq(1,10000,distance/50)) +
      scale_y_continuous(breaks = seq(0,500,5)) +
      theme_light() + 
      labs(x = "Time (s)", y = "Counnt")
    
    if(length(genders()) < 2){
      p <- p + geom_histogram(binwidth = distance/100, boundary = 0,
                     fill = "#00bc8c", color = "black")
    } else {
      p <- p +
        geom_histogram(aes(fill = gender), binwidth = distance/100, 
                       boundary = 0, color = "black") +
        scale_fill_manual(values = c("#00bc8c","#375a7f"))
    }
    
    p
    
  })
  
  output$split_info <- renderText({
    n_results <- selected_splits() %>%
      pull(result_id) %>% 
      unique() %>%
      length()
    first_year <- selected_splits() %>%
      pull(year) %>% min()
    paste("Splits for ", n_results,
          " distinct races are shown above. ",
          "Splits appear for this event beginning ", 
          first_year, ".", sep = "")
  })
  
  
  output$split_plot <- renderPlot({
    set.seed(1)

    p <- selected_splits() %>%
      ggplot(aes(leg, split)) + 
      geom_point(alpha = .5, position = position_jitter(width = .2)) + 
      theme_light() + 
      labs(x = "Length", y = "Split Time (s)") + 
      scale_y_continuous(breaks = seq(1,1000,1)) + 
      scale_x_continuous(breaks = seq(1,30,1))
    
    if(length(genders()) > 1){
      p <- p + facet_grid(. ~ gender)
    }
    
    if(input$split_plot_overlay == "boxplot"){
      p <- p + geom_boxplot(aes(group = leg), outlier.shape = NA, 
                            notch = input$add_notch, fill = "#00bc8c")
    }
    
    distance <- input$event %>%
      str_extract("\\d*") %>%
      as.integer()
      
    if(input$split_plot_overlay == "line" && distance > 100){
      f <- ecdf(selected_splits()$time)
      split_quantiles <- selected_splits() %>% 
        group_by(gender) %>%
        mutate(quantile = {
          f <- ecdf(time)
          ceiling(f(time)*input$num_lines)
        }) %>%
        ungroup()
      
      n_splines <- max(split_quantiles$leg)/2
        
      p <- p + 
        suppressWarnings(geom_smooth(data = split_quantiles, 
                    mapping = aes(group = quantile),
                    method = lm, formula = y ~ splines::bs(x, n_splines),
                    se = FALSE, color = "#00bc8c"))
    }
    
    p
    
    })
  
  output$orecord_plot <- renderPlot({
    min_best <- olympic_bests()$best %>% min()
    max_best <- olympic_bests()$best %>% max()
    range_best = round(min_best, max_best)
    p <- olympic_bests() %>% ggplot(aes(x = year, y = best, label = name)) + 
      geom_line() + 
      geom_point(mapping = aes(color = `New OR?`)) +
      scale_x_continuous(breaks = seq(1900,2100,8)) +
      scale_y_continuous(breaks = 
                           seq(0, 2000, 
                               round(range_best/30*length(genders())))) +
      theme_light() +
      scale_color_manual(values=c("#375a7f","#00bc8c")) +
      labs(x = "Year", y = "Olympic Best Time (s)") +
      geom_label_repel()
    
    if(length(genders()) > 1){
      p <- p + facet_grid(gender ~ .)
    }
    
    p
  })
}

shinyApp(ui = ui, server = server)