
# Version 3.3

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(plotly)
library(readxl)
library(shinyjs)
library(ggplot2)

# Load your data
loaded_data <- read_excel("Data/HPP_MH_review_1127_combined_v3.2.xlsx", sheet = "All_clean")
headDes_data <- read_excel("Data/HPP_MH_review_1127_combined_v3.2.xlsx", sheet = "headDes")

# Define helptexts
helptext_article <- "This group includes information about articles such as title, DOI, and year of the original literature. see details in the Description tab."
helptext_food <- "This group includes information about food compositions such as pH, water activity,and fat percentage. see details in the Description tab."
helptext_pathogen <- "This group includes information about pathogens such as strain,serotype and their surrogates"
helptext_inored <- "This group includes information about inocculum level and total log reduction achieved post HPP treatment. see details in the Description tab."

# Define UI
ui <- fluidPage(
  
  useShinyjs(),  # Initialize shinyjs
  
  div(
    img(src = "logo.png", width = "70%"),
    p("Hazard High Pressure Processing Inactivation Database", style = "font-size: 28px;font-family: Open Sans;"),
    style = "background-color: #4CAF50; color: white; padding: 10px; text-align: center;"
  ),
  
  theme = shinythemes::shinytheme("flatly"),  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Category", 
                  choices = c("Pathogen", "Food Item", "Food Category"), 
                  selected = "Pathogen"),
      
      # Conditional panel for Pathogen
      conditionalPanel(
        condition = "input.category == 'Pathogen'",
        selectInput("pathogen_select", "Select Pathogen", 
                    choices = unique(loaded_data$pathogen),
                    multiple = TRUE),
        
# Master checkbox for Article information group
        div(
          style = "display: flex; align-items: center;",
          checkboxInput("master_checkbox_article", "Master Article Checkbox", FALSE),
          
          # Help button for Article information group
          actionButton("help_article", label = icon("info-circle"), 
                       style = "margin-left: 5px; font-size: 15px; background-color: #4CAF50; color: white; border-radius: 30%; border: none; padding: 2px 3px;")
        ),
        
        # Helptext for Article information group
        conditionalPanel(
          condition = "input.help_article % 2 == 1", ### this is to reset the argument of the action button to display info>once
          helpText(helptext_article)
        ),
        
        
        # Checkbox group for Article information group
        shinyWidgets::checkboxGroupButtons(
          inputId = "article_checkboxes_pathogen",
          label = "Article information group",
          choices = c("title", "DOI", "year"),
          selected = c("title", "DOI", "year"),
          justified = TRUE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
        
        
# Master checkbox for Food information group
        div(
          style = "display: flex; align-items: center;",
          checkboxInput("master_checkbox_food", "Master Food Checkbox", FALSE),
          
          # Help button for Food information group
          actionButton("help_food", label = icon("info-circle"), 
                       style = "margin-left: 5px; font-size: 15px; background-color: #4CAF50; color: white; border-radius: 30%; border: none; padding: 2px 3px;")
        ),
        
        # Helptext for Food information group
        conditionalPanel(
          condition = "input.help_food % 2 == 1", ### this is to reset the argument of the action button to display info>once
          helpText(helptext_food)
        ),
        
        # Checkbox group for Food information group
        shinyWidgets::checkboxGroupButtons(
          inputId = "food_checkboxes_pathogen",
          label = "Food information group",
          choices = c("pH", "pH_cat", "aw", "aw_cat", "fat_percentage"),
          selected = c("pH", "pH_cat", "aw", "aw_cat", "fat_percentage"),
          justified = TRUE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
        
# Master checkbox for Pathogen information group
        div(
          style = "display: flex; align-items: center;",
          checkboxInput("master_checkbox_pathogen", "Master Pathogen Checkbox", FALSE),
          
          # Help button for Pathogen information group
          actionButton("help_pathogen", label = icon("info-circle"), 
                       style = "margin-left: 5px; font-size: 15px; background-color: #4CAF50; color: white; border-radius: 30%; border: none; padding: 2px 3px;")
        ),
        
        # Helptext for Pathogen information group
        conditionalPanel(
          condition = "input.help_pathogen % 2 == 1", ### this is to reset the argument of the action button to display info>once
          helpText(helptext_pathogen)
        ),
        
        
        # Checkbox group for Pathogen information group
        shinyWidgets::checkboxGroupButtons(
          inputId = "pathogen_checkboxes_pathogen",
          label = "Pathogen information group",
          choices = c("pathogen_adj", "strain", "cocktail", "surrogate", "serotype"),
          selected = c("pathogen_adj", "strain", "cocktail", "surrogate", "serotype"),
          justified = TRUE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),
        
        
# Master checkbox for Inored information group
        div(
          style = "display: flex; align-items: center;",
          checkboxInput("master_checkbox_inored", "Master Inored Checkbox", FALSE),
          
          # Help button for Inored information group
          actionButton("help_inored", label = icon("info-circle"), 
                       style = "margin-left: 5px; font-size: 15px; background-color: #4CAF50; color: white; border-radius: 30%; border: none; padding: 2px 3px;")
        ),
        
        # Helptext for Inored information group
        conditionalPanel(
          condition = "input.help_inored % 2 == 1", ### this is to reset the argument of the action button to display info>once
          helpText(helptext_inored)
        ),
        
        # Checkbox group for Inored information group
        shinyWidgets::checkboxGroupButtons(
          inputId = "inored_checkboxes_pathogen",
          label = "Inored information group",
          choices = c("estimated_logN0", "logN0_CUTin", "logN0_CUTex", "log_red_byCUT","log_red_adj", "Enumeration_postHPP_h"),
          selected = c("estimated_logN0", "logN0_CUTin", "logN0_CUTex", "log_red_byCUT","log_red_adj", "Enumeration_postHPP_h"),
          justified = TRUE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        )

      ),  # Remove the comma here
      
      # Conditional panel for Food Item
      conditionalPanel(
        condition = "input.category == 'Food Item'",
        selectInput("food_item_select", "Select Food Item", 
                    choices = unique(loaded_data$food_item),
                    multiple = TRUE)
      ),
      
      # Conditional panel for Food Category
      conditionalPanel(
        condition = "input.category == 'Food Category'",
        selectInput("SAFFI_FC_select", "Select Food Category", 
                    choices = unique(loaded_data$SAFFI_FC),
                    multiple = TRUE)
      ),
      
      # Placeholder for additional content in the sidebar if needed
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Pathogen Tab", DTOutput("data_table_pathogen")),  # Responsive data table
        tabPanel("Description", DTOutput("data_table_other"))  # Placeholder for another table
      )
    )
  )
)



# Define Server Logic
server <- function(input, output, session) {
  # Reactive values for selected checkboxes in each information group
  rv <- reactiveValues(
    article_checkboxes_pathogen = c("title", "DOI", "year"),
    food_checkboxes_pathogen = c("pH", "pH_cat", "aw", "aw_cat", "fat_percentage"),
    pathogen_checkboxes_pathogen = c("pathogen_adj", "strain", "cocktail", "surrogate", "serotype"),
    inored_checkboxes_pathogen = c("estimated_logN0", "logN0_CUTin", "logN0_CUTex", "log_red_byCUT", "log_red", "log_red_CUTin", "log_red_adj", "Enumeration_postHPP_h")
  )
  
  # Initialize checkbox states when a pathogen is selected
  observeEvent(input$pathogen_select, {
    if (!is.null(input$pathogen_select) && length(input$pathogen_select) > 0) {
      # Update the state of the master checkboxes based on the current reactive values
      updateCheckboxInput(session, "master_checkbox_article", value = length(rv$article_checkboxes_pathogen) > 0)
      updateCheckboxInput(session, "master_checkbox_food", value = length(rv$food_checkboxes_pathogen) > 0)
      updateCheckboxInput(session, "master_checkbox_pathogen", value = length(rv$pathogen_checkboxes_pathogen) > 0)
      updateCheckboxInput(session, "master_checkbox_inored", value = length(rv$inored_checkboxes_pathogen) > 0)
      
      # Update the state of the checkbox groups based on the current reactive values
      updateCheckboxGroupInput(session, "article_checkboxes_pathogen", selected = rv$article_checkboxes_pathogen)
      updateCheckboxGroupInput(session, "food_checkboxes_pathogen", selected = rv$food_checkboxes_pathogen)
      updateCheckboxGroupInput(session, "pathogen_checkboxes_pathogen", selected = rv$pathogen_checkboxes_pathogen)
      updateCheckboxGroupInput(session, "inored_checkboxes_pathogen", selected = rv$inored_checkboxes_pathogen)
    }
  })
  
  
  # Filtered data reactive for Description tab
  filtered_data_other <- reactive({
    data <- headDes_data
    # Additional processing or filtering logic as needed
    data
  })
  
  # Filtered data reactive for Pathogen category
  
  filtered_data_pathogen <- reactive({
    data <- loaded_data
    selected_category <- input$category
    
    # Filter based on selected category and subcategories
    if (selected_category == "Pathogen") {
      pathogen_filter <- data$pathogen %in% input$pathogen_select
      data <- data[pathogen_filter, c("pathogen", rv$article_checkboxes_pathogen, rv$food_checkboxes_pathogen, rv$pathogen_checkboxes_pathogen, rv$inored_checkboxes_pathogen), drop = FALSE]
    } else {
      data <- data[1, 1]  # Placeholder, modify accordingly for other categories
    }
    
    data
  })
  
  # Handle changes in master checkboxes
  observeEvent(input$master_checkbox_article, {
    if (input$master_checkbox_article) {
      rv$article_checkboxes_pathogen <- c("title", "DOI", "year")
    } else {
      rv$article_checkboxes_pathogen <- character(0)
    }
    
    # Update the corresponding checkbox group input
    updateCheckboxGroupInput(session, "article_checkboxes_pathogen", selected = rv$article_checkboxes_pathogen)
  })
  
  observeEvent(input$master_checkbox_food, {
    if (input$master_checkbox_food) {
      rv$food_checkboxes_pathogen <- c("pH", "pH_cat", "aw", "aw_cat", "fat_percentage")
    } else {
      rv$food_checkboxes_pathogen <- character(0)
    }
    
    # Update the corresponding checkbox group input
    updateCheckboxGroupInput(session, "food_checkboxes_pathogen", selected = rv$food_checkboxes_pathogen)
  })
  
  observeEvent(input$master_checkbox_pathogen, {
    if (input$master_checkbox_pathogen) {
      rv$pathogen_checkboxes_pathogen <- c("pathogen_adj", "strain", "cocktail", "surrogate", "serotype")
    } else {
      rv$pathogen_checkboxes_pathogen <- character(0)
    }
    
    # Update the corresponding checkbox group input
    updateCheckboxGroupInput(session, "pathogen_checkboxes_pathogen", selected = rv$pathogen_checkboxes_pathogen)
  })
  
  observeEvent(input$master_checkbox_inored, {
    if (input$master_checkbox_inored) {
      rv$inored_checkboxes_pathogen <- c("estimated_logN0", "logN0_CUTin", "logN0_CUTex", "log_red_byCUT", "log_red", "log_red_CUTin", "log_red_adj","Enumeration_postHPP_h")
    } else {
      rv$inored_checkboxes_pathogen <- character(0)
    }
    
    # Update the corresponding checkbox group input
    updateCheckboxGroupInput(session, "inored_checkboxes_pathogen", selected = rv$inored_checkboxes_pathogen)
  })
  
  # Handle changes in individual checkboxes using observeEvent
  observeEvent(input$article_checkboxes_pathogen, {
    rv$article_checkboxes_pathogen <- input$article_checkboxes_pathogen
  })
  
  observeEvent(input$food_checkboxes_pathogen, {
    rv$food_checkboxes_pathogen <- input$food_checkboxes_pathogen
  })
  
  observeEvent(input$pathogen_checkboxes_pathogen, {
    rv$pathogen_checkboxes_pathogen <- input$pathogen_checkboxes_pathogen
  })
  
  observeEvent(input$inored_checkboxes_pathogen, {
    rv$inored_checkboxes_pathogen <- input$inored_checkboxes_pathogen
  })
  
  # Show/hide helptext based on button clicks
  observe({
    shinyjs::enable("help_article")
    shinyjs::enable("help_food")
    shinyjs::enable("help_pathogen")
    shinyjs::enable("help_inored")
    
    if (input$master_checkbox_article) {
      shinyjs::disable("help_article")
    }
    if (input$master_checkbox_food) {
      shinyjs::disable("help_food")
    }
    if (input$master_checkbox_pathogen) {
      shinyjs::disable("help_pathogen")
    }
    if (input$master_checkbox_inored) {
      shinyjs::disable("help_inored")
    }
  })
  
  
  # Render responsive data table for Pathogen category
  output$data_table_pathogen <- renderDT({
    datatable(
      filtered_data_pathogen(), 
      options = list(
        lengthMenu = c(25, 50, 100),
        pageLength = 25,
        responsive = TRUE
      ),
      escape = FALSE  # To allow HTML rendering
    )
  })
  
  # Render responsive data table for Description tab
  output$data_table_other <- renderDT({
    datatable(
      filtered_data_other(), 
      options = list(
        lengthMenu = c(25, 50, 100),
        pageLength = 25,
        responsive = TRUE
      ),
      escape = FALSE  # To allow HTML rendering
    )
  })
  
}

# Run the App
shinyApp(ui, server)