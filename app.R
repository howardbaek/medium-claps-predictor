library(tidyverse)
library(tidytext)
library(shiny)
library(shinyWidgets)
library(DT)



# NOTE: Model Estimate has no duplicate terms
model_estimate <- read_csv("model_estimate.csv") %>% 
  select(term, estimate)

# Available Data Science Terms 
estimate_terms <- model_estimate %>% 
  select(term) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = str_to_title(term))


# Define UI for application
ui <- fluidPage(
  
   # Application title
  titlePanel(tagList(
    span("Medium", style = "font-size: 32px;font-family: Noe Display",
         img(src = "claps.png", height = 30, width = 30),
         span("Predictor App", style = "font-size: 32px;font-family: Noe Display",
              span(
                actionButton("Twitter", 
                             label = "Twitter",
                             icon = icon("twitter"),
                             width = "80px",
                             onclick ="window.open(`https://twitter.com/howiebaik`, '_blank')",
                             style="color: #fff; background-color: #00acee; border-color: #00acee"),
                actionButton("github",
                             label = "Code",
                             icon = icon("github"),
                             width = "77px",
                             onclick ="window.open(`https://github.com/howardbaik`, '_blank')",
                             style="color: #fff; background-color: #767676; border-color: #767676"),
                style = "position:absolute;right:2em;"
              )
         )
    )
  ),
  windowTitle = "Medium Claps Predictor App"
  ),
  
  hr(),
  
  fluidRow(
    column(5,
           searchInput(
             inputId = "post_title",
             label = "Blog Post Title",
             placeholder = "Done? Click `Search` icon or Hit `Enter`",
             btnSearch = icon("search"),
             btnReset = icon("remove"),
             width = "490px"
           ),
           
           br(),
           
           div(dataTableOutput("term_dt"), style = "width: 80%"),
           
           br(),
           
           h4(div(img(src="information.png", width = 25), "App Information")),
           
           textOutput("app_intro"),
           uiOutput("app_info"),
           uiOutput("app_motivation"),
           textOutput("model_info")
           
    ),
    
    column(7, 
           mainPanel(
             span(
             h2("Prediction:"), style = "font-family: Noe Display",
             span(
             h4("By default, you start with 8 claps"), style = "font-family: Noe Display")),
             textOutput("search_result"),
             tags$head(tags$style("#search_result{font-size: 40px;color: cornflowerblue;
                                  font-style: italic;font-family: Noe Display
                                  }"
                         )
             ),
             
             br(),
             br(),
             
             imageOutput("network_graph")
           )
    )
  )
)



server <- function(input, output) {
  
  # Step 1: Get rid of stop words (and, or, the, a, an)
  # Step 2: Lowercase everything (unnest_tokens does this)
  claps <- reactive({
    
    title_df <- tibble(title = as.character(input$post_title)) %>% 
      unnest_tokens(processed_title, title) %>% 
      anti_join(stop_words, by = c("processed_title" = "word"))
    
    term_estimates <- title_df %>% 
      inner_join(model_estimate, by = c("processed_title" = "term"))
    
    estimates <- term_estimates %>% 
      summarize(sum_estimate = sum(estimate)) %>% 
      pull(sum_estimate)
    
    exp(estimates + 2.227) - 1
    
  })
  
  output$search_result <- renderText({
    
   # Print message
    paste0(floor(claps()), " Claps")
    
  })
  
  
  output$term_dt <- renderDataTable({
    
    datatable(estimate_terms,
              rownames = FALSE,
              colnames = "Data Science Terms",
              class = "compact",
              options = list(dom = "ftp",
                             language = list(searchPlaceholder = "Find words for your title"),
                             pageLength = 10))
    
  })
  
  output$network_graph <- renderImage({
    
    list(src = "./images/network-graph.png",
         width = "450px",
         height = "450px")
    
  }, deleteFile = FALSE)
  
  
  
  # App Info
  howard_baik <- a("Howard Baik", href="https://insidethetv.netlify.com/")
  drob <- a("David Robinson's Tidy Tuesday Screencast", href="https://youtu.be/C69QyycHsgE?t=3530")
  
  output$app_intro <- renderText({
    
    "- Predicts # of claps that your data science post will receive on Medium"
    
  })
  
  output$app_info <- renderUI({
    
    tagList("- Developed by", howard_baik)
  })
  
  output$app_motivation <- renderUI({
    
    tagList("- Motivated by", drob)
  })
  
  output$model_info <- renderText({
    
    "- Fit a LASSO Model on dataset using glmnet R package"
    
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

