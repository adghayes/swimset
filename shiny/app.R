library(tidyverse)
library(shiny)
library(shinythemes)
library(ggrepel)
load("./odata.rda") # Olympic subset of fina.org data

# Limiting scope to individual results
oresults <- oresults %>% 
  filter(!relay) %>%
  unite("event", distance, style, sep = " ", remove = FALSE)

# Limiting scope to individual results
osplits <- osplits %>%
  semi_join(oresults, by = "result_id")

# Defining options for inputs
divisions <- c("Women" = "Women", 
                 "Men" = "Men", 
                 "Both" = "Both")

events <- oresults %>% 
  arrange(distance, event) %>% 
  pull(event) %>% unique()

# Client Definition
ui <- fluidPage(theme = shinytheme("darkly"),
  titlePanel("Olympic Swimming Results Dashboard"),
  HTML(paste("<h6>data from <a href = \"fina.org\">fina.org</a>,",
            "code on <a href = \"https://github.com/adghayes/swimset\">",
            "github</a>. data is imperfect. only individual events<h6>")),
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
  htmlOutput("warning"),
  tabsetPanel(type = "tabs",
    tabPanel(
      title = "Olympic Records", 
      plotOutput(outputId = "orecord_plot"),
      br(),
      hr(),
      tags$p(paste("*Note that there are other instances in which",
                   "an Olympic Record was broken during a Games",
                   "but then broken again at the same Games, either",
                   "in the same heat or a later heat. These instances",
                   "are not shown here. The records shown here are only",
                   "those that were held between Games.")),
      conditionalPanel(
        condition = "input.division == 'Men' || input.division == 'Both'",
        column(
          width = 5, offset = .5,
          h6("Men's Olympic Records"),
          tableOutput("orecord_table_men"))
      ),
      conditionalPanel(
        condition = "input.division == 'Women' || input.division == 'Both'",
        column(
          width = 5, offset = .5,
          h6("Women's Olympic Records"),
          tableOutput("orecord_table_women"))
      )
    ),
    tabPanel(
      title = "Times", 
      plotOutput(outputId = "time_plot", width = "100%"),
      br(),
      hr(),
      textOutput("time_info")
    ),
    tabPanel(
      title = "Splits", width = "100%",
      plotOutput(outputId = "split_plot"),
      br(),
      hr(),
      textOutput("split_info"),
      br(),
      fluidRow(
        column(
          width = 3, offset = 1,
          selectInput("split_plot_overlay", label = "Overlay:",
                     choices = c("Boxplot" = "boxplot",
                                 "Line" = "line",
                                 "None" = "none"), selected = "boxplot")
       ),
       conditionalPanel(condition = "input.split_plot_overlay == 'line'",
         column(
           width = 2, offset = 0,
           sliderInput("num_lines", label = "Quantiles:",
                       min = 1, max = 4, value = 1, step = 1))
       ),
       conditionalPanel(condition = "input.split_plot_overlay == 'boxplot'",
          column(
            width = 2, offset = 0,
                checkboxInput("add_notch", label = "Add Notches:")
        )
       )
     )
    )   
  )
)

server <- function(input, output, session) {
  
  # Translate division selection to vector
  genders_selected <- reactive({
    if(input$division == "Both"){
      c("Men","Women")
    } else {
      input$division
    }
  })
  
  # Parse event distance for plot, axes settings
  distance <- reactive({
    as.integer(str_extract(input$event,"\\d*"))
  })
  
  # Filter results for dashboard
  selected_results <- reactive({
    
    # Halt data the selected event is a missing event
    isw1500 <- input$division %in% c("Women") && input$event == "1500 Freestyle" 
    ism800 <- input$division %in% c("Men") && input$event == "800 Freestyle"
    req(!(isw1500 || ism800))
    
    oresults %>% 
      filter(event == input$event,
             gender %in% genders_selected()
      )
  })
  
  # Filtered splits for dashboard
  selected_splits <- reactive({
    osplits %>% 
      inner_join(selected_results(), by = "result_id")
  })
  
  # Formatted time series for record history
  olympic_bests <- reactive({
    selected_results() %>% 
      filter(!is.na(time)) %>%
      group_by(year, gender) %>%
      top_n(n = 1, desc(time)) %>%
      ungroup() %>%
      arrange(year) %>%
      mutate(
        or = cummin(time),
        or_change = time - lag(or),
        year_incr = year - lag(year),
        or_change = if_else(year_incr == 0, lag(or_change), or_change),
        is_or = if_else(or_change < 0 | is.na(or_change), "Yes", "No")
      )
  })
  
  # Text message about how many times in time plot
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

  # Time histogram
  output$time_plot <- renderPlot({

    p <- selected_results() %>%
      filter(!is.na(time)) %>%
      ggplot(aes(time, stat(count)))  + 
      scale_x_continuous(breaks = seq(1,10000,distance()/50)) +
      scale_y_continuous(breaks = seq(0,500,5)) +
      theme_light() + 
      labs(x = "Time (s)", y = "Counnt")
    
    # Both genders --> fill represents gender
    if(length(genders_selected()) < 2){
      p <- p + geom_histogram(binwidth = distance()/100, boundary = 0,
                     fill = "#00bc8c", color = "black")
    } else {
      p <- p +
        geom_histogram(aes(fill = gender), binwidth = distance()/100, 
                       boundary = 0, color = "black") +
        scale_fill_manual(values = c("#00bc8c","#375a7f"))
    }
    
    p
    
  })
  
  # Text message about number of splits in split plot
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
  
  # Split plot
  output$split_plot <- renderPlot({
    set.seed(1) # So points remain constant as parameters change
    p <- selected_splits() %>%
      ggplot(aes(leg, split)) + 
      geom_point(alpha = .5, position = position_jitter(width = .2)) + 
      theme_light() + 
      labs(x = "Length", y = "Split Time (s)") + 
      scale_y_continuous(breaks = seq(1,1000,1)) + 
      scale_x_continuous(breaks = seq(1,30,1))
    
    # Both genders --> faceted plot
    if(length(genders_selected()) > 1){
      p <- p + facet_grid(. ~ gender)
    }
    
    # Boxplot overlay
    if(input$split_plot_overlay == "boxplot"){
      p <- p + geom_boxplot(aes(group = leg), outlier.shape = NA, 
                            notch = input$add_notch, fill = "#00bc8c")
    }
    
    
    # Smoothed line overlay  
    if(input$split_plot_overlay == "line"){
      # Only do line for races longer than 100
      if(distance() > 100){
        # Calculate wuantile groups
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
    }
    
    p
    
    })
  
  # Olympic Record Time Series Plot
  output$orecord_plot <- renderPlot({
    min_best <- olympic_bests()$time %>% min()
    max_best <- olympic_bests()$time %>% max()
    range_best = round(min_best, max_best)
    p <- olympic_bests() %>% ggplot(aes(x = year, y = time, label = family_name)) + 
      geom_line() + 
      geom_point(mapping = aes(color = is_or)) +
      scale_x_continuous(breaks = seq(1900,2100,8)) +
      scale_y_continuous(breaks = 
                           seq(0, 2000, 
                               round(range_best/30*length(genders_selected())))) +
      theme_light() +
      scale_color_manual(values=c("#375a7f","#00bc8c")) +
      labs(x = "Year", y = "Olympic Best Time (s)", color = "Olympic Record") +
      geom_label_repel(size = 3, force = 10)
    
    # Both genders -> vertical faceting
    if(length(genders_selected()) > 1){
      p <- p + facet_grid(gender ~ .)
    }
    
    p
  })
  
  output$orecord_table_men <- renderTable(expr = {
    olympic_bests() %>%
      filter(is_or == "Yes", gender == "Men") %>%
      arrange(desc(year)) %>% 
      mutate(name = tools::toTitleCase(paste(first_name,
                          tolower(family_name)))) %>%
      select(Year = year, Name = name, Time = str_time, Country = ioc_code)
  }, 
  bordered = TRUE, hover = TRUE)
  
  output$orecord_table_women <- renderTable(expr = {
    olympic_bests() %>%
      filter(is_or == "Yes", gender == "Women") %>%
      arrange(desc(year)) %>% 
      mutate(name = tools::toTitleCase(paste(first_name,
                                             tolower(family_name)))) %>%
      select(Year = year, Name = name, Time = str_time, Country = ioc_code)
  }, 
  bordered = TRUE, hover = TRUE)
  
  output$warning <- renderText({
    warning_html <- ""
    if((input$division %in% c("Women", "Both") && input$event == "1500 Freestyle") ||
       (input$division %in% c("Men", "Both") && input$event == "800 Freestyle")){
      warning_text <- paste("Historically, the longest men's distance ",
                            "event at the Olympics has been the 1500m ",
                            "Freestyle while the longest women's distance ",
                            "event has been the 800m Freestyle. At Tokyo ",
                            "in 2020 the Olympics will, for the first time, ",
                            "have a Men's 800m Freestyle and a Women's 1500m",
                            "Freestyle like other modern swim competitions.",
                            sep = "")
      warning_html <- paste("<p><font color=\"#F39C12\">",
                            warning_text,
                            "</font></p>")
    }
  warning_html
  })
  
}

shinyApp(ui = ui, server = server)
