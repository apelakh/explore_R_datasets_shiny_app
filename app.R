# Global script ----
# Code added here will be executed one time when you run the app
# This should be everything that you only need to run once


## Load libraries ----
library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(ggpubr)
library(thematic)

source("plot_functions.R") # Source R script

thematic_shiny() # This function from {thematic} will update ggplot defaults based on your current app theme

theme_set(theme_bw(base_size = 20)) # You can still specify some defaults using theme_set(). I find that text is usually too small on default plots.

## Data ----
# data() creates a list of all the available datasets from loaded packages
data_info <- data()$results %>% 
  as_tibble %>% 
  mutate(across(Item, ~ str_remove(.x, "\\s\\(.*\\)")))

## Custom functions ----

get_variable_info <- function(my_df){
  # gets basic column-level information about the dataset
  # this is useful later when creating dynamic dropdown lists
  
  tibble(
    variable = my_df %>% names(), # variable names
    data_type = my_df %>% map_chr( ~ class(.) %>% paste(., collapse = ' ')), # data type
    n_unique = my_df %>% map_dbl(n_distinct), # number of unique values
    n_missing = my_df %>% map_dbl( ~ sum(is.na(.))) # number of missing values
  ) 
}

# User Interface ----
ui <- fluidPage(
  
  ## Theme / Styling ----
  # {bslib} has a lot of themes to choose from. You can preview the different options in the dropdown menu on the upper right when you run the app.
  theme = bslib::bs_theme(
    bootswatch = "shiny"
  ),
  ## Page Title ----
  titlePanel("Explore Available Data Sets in R"),
  
  sidebarLayout(
    ## Dataset selection -----
    sidebarPanel(
      # Inputs will always have the inputId as the first argument and the label as the second argument.
      selectInput( 
        "side_select_data", # inputId
        "Select a dataset", # label
        choices = data_info$Item,
        selected = "mtcars"
      )
    ),
    ## Dataset title/package ----
    mainPanel(
      # display the descriptive title, package, and data dimensions
      # NOTE: You can wrap any content inside an HTML tag by calling it like a function. For example h3("Heading 3") will be printed as <h3>Heading 3</h3>
      h4(textOutput("main_title")),
      h5(textOutput("main_package")),
      h5(textOutput("main_class")),
      textOutput("main_dim")
    )
  ),
  
  ## Tabs ----
  fluidRow(
    style = "margin-top: 20px;", # you can add custom CSS styling to any of the layout functions.
    column(
      width = 12, 
      tabsetPanel(
        ### View data ----
        # Display the data frame using {DT}
        tabPanel(
          "View data",
          br(), # add a little spacer
          # display the dataset
          DTOutput("view_data")
        ),
        ### Data descriptives ----
        # display some descriptive statistics about the dataset
        tabPanel(
          "Data descriptives",
          br(),
          # layout_column_wrap() and card() are handy layout options from {bslib}
          # https://rstudio.github.io/bslib/articles/cards/index.html
          layout_column_wrap(
            fillable = TRUE,
            card(
              card_header("Variable Information"),
              full_screen = TRUE,
              max_height = "500px",
              card_body(
                class = "align-items-center",
                tableOutput("describe_var_info") 
              )
            ),
            card(
              card_header("Missing Values"),
              full_screen = TRUE,
              plotOutput(("describe_plot_missing"))
            ),
            card(
              card_header("Data Types"),
              full_screen = TRUE,
              plotOutput(("describe_plot_datatypes"))
            )
          )
          # splitLayout( 
          #   tableOutput("describe_var_info"),
          #   plotOutput(("describe_plot_missing")),
          #   plotOutput(("describe_plot_datatypes"))
          # )
        ),
        ### Visualize ----
        # allow the user to create some basic visualizations
        tabPanel(
          "Visualize",
          br(),
          navlistPanel(
          # Here, we'll create a multi-page layout inside of our tabPanel()
            id = "vis_type", # <- setting the `id` argument so that I can access the value of the active panel in the server
            header = h3(textOutput("vis_header")), # This line displays which tab is currently active
            widths = c(2,10), # I can set the widths for the sidebar and main panels. The numbers just have to add up to 12
            well = FALSE, # `well` refers to the default sidebar styling (gray container). There is actually a CSS style called .well, so you can add it to any element you want. Set this value to TRUE if you want to see what it does.
            #### Bar plots ----
            tabPanel(
              "Bar Plots",
              sidebarLayout(
                position = "right",
                ##### sidebar -----
                sidebarPanel(
                  uiOutput("vis_bar_select_var1"),
                  uiOutput("vis_bar_select_fill"),
                  checkboxInput(
                    "vis_bar_flip",
                    "Flip Coordinates"
                  )
                ),
                ##### main -----
                mainPanel(
                  plotOutput("vis_bar_plot")
                )
              )
            ),
            #### Histograms -----
            tabPanel(
              "Histograms",
              sidebarLayout(
                position = "right",
                ##### sidebar -----
                sidebarPanel(
                  uiOutput("vis_hist_select_var1"),
                  sliderInput(
                    "vis_hist_bins",
                    "Select the number of bins",
                    min = 1,
                    max = 20,
                    value = 9
                  )
                ),
                ##### main -----
                mainPanel(
                  plotOutput("vis_hist_plot"),
                  textOutput("vis_hist_test")
                )
              )
            ),
            #### Scatter plots ----
            tabPanel(
              "Scatter Plots",
              sidebarLayout(
                position = "right",
                ##### sidebar -----
                sidebarPanel(
                  uiOutput("vis_scatter_select_var1"),
                  uiOutput("vis_scatter_select_var2"),
                  radioButtons(
                    "vis_scatter_smooth_method",
                    "Select smoothing method",
                    choices = c("lm", "loess")
                  ),
                  radioButtons(
                    "vis_scatter_corr_method",
                    "Select correlation method",
                    choices = c("pearson", "kendall", "spearman")
                  ),
                  sliderInput(
                    "vis_scatter_lbl_x",
                    "Adjust label x position",
                    min = 0,
                    max = 1,
                    value = 0,
                    ticks = FALSE
                  ),
                  sliderInput(
                    "vis_scatter_lbl_y",
                    "Adjust label y position",
                    min = 0,
                    max = 1,
                    value = 1,
                    ticks = FALSE
                  )
                ),
                ##### main -----
                mainPanel(
                  plotOutput("vis_scatter_plot")
                )
              )
            )
          )
        ),
        ### Shiny Resources ----
        tabPanel(
          "Extensions and Resources",
          style = "padding: 20px;",
          includeMarkdown("resources.md")
        )
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  bslib::bs_themer() # This is the code that generates the theme customizer when you run the app
  
  ## Reactive expressions ----
  
  # Selected data info: filter `data_info` so it only contains information about our selected dataset. This will update every time the user selects a new dataset from the dropdown
  selected <- reactive({
    data_info %>% 
      filter(Item == input$side_select_data)
  })
  
  # Now we'll actually search the global environment for the data object and save it as a reactive expression
  df <- reactive({
    obj <- get(selected()$Item)
    
    # I need to convert all the data types to data.frame but I want to be able to display their original data type. I'm going to use the comment attribute to do this
    
    # save the class to a variable
    raw_class <- paste(class(obj), collapse = ", ")
    
    # if the object is not a data frame, convert it to one
    if(!is.data.frame(obj)){
      
      if(is.ts(obj)){
        # code for time series data
        obj <- data.frame(time = time(obj), data = obj)
      } else if(is.atomic(obj) & !is.matrix(obj)){
        # `enframe()` worked better for most atomic vectors
        obj <- enframe(obj)
      } else {
        obj <- as.data.frame(obj)
      }
    }
    
    # save the original class to the comment attribute
    comment(obj) <- raw_class 
    
    return(obj)
  })
  
  # Here, we'll use the custom function defined at the top of the script to create a data frame with some key information about the variables in the data.
  var_info <- reactive({
    df() %>% get_variable_info()
  })
  
  # Now we can use `var_info()` to generate a list of categorical and numeric variables that we'll use to generate the dropdown menus in the data visualizations
  
  
  # list of categorical variables
  var_cat <- reactive({
    var_info() %>% 
      filter(n_unique <= 10) %>% # I'm allowing users to select any variable with up to 10 unique values as a categorical variable
      pull(variable) %>% 
      as.character()
  })
  
  # list of numeric variables
  var_num <- reactive({
    var_info() %>% 
      filter(data_type == "numeric") %>% 
      pull(variable) %>% 
      as.character()
  })
  
  
  ## Dataset title/package ----
  
  ### main_title 
  output$main_title <- renderText(selected()$Title)
  ### main_package 
  output$main_package <- renderText({
    paste("Package:", selected()$Package)
  })
  ### main_class
  output$main_class <- renderText({
    paste("Class:", comment(df()))
  })
  ### main_dim 
  output$main_dim <- renderText({
    rows <- df() %>% nrow()
    cols <- df() %>% ncol()
    paste(rows, "observations of", cols, "variables")
  })
  
  ## Tabs ----
  
  ### View data ----
  output$view_data <- renderDT(df(), filter = "top") # You don't need to use {} if the function is on a single line
  
  ### Data descriptives ----
  
  #### describe_var_info 
  output$describe_var_info <- renderTable({
    var_info()
  })
  #### describe_plot_missing 
  output$describe_plot_missing <- renderPlot({
    df() %>% plot_missing_values() # this function is defined in plot_functions.R
  })
  #### describe_plot_datatypes
  output$describe_plot_datatypes <- renderPlot({
    df() %>% plot_data_types() # this function is defined in plot_functions.R
  })
  
  ### Visualize ----
  
  output$vis_header <- renderText(paste("Active Tab:", input$vis_type))
  
  #### Bar Plots -----
  
  ##### Input controls ----
  
  output$vis_bar_select_var1 <- renderUI({
    selectInput(
      "vis_bar_var1",
      "Select a variable",
      choices = var_cat() # We can use a reactive expression to populate the dropdown menu in the server
    )
  })
  
  output$vis_bar_select_fill <- renderUI({
    selectInput(
      "vis_bar_fill",
      "Fill by:",
      choices = var_cat()
    )
  })
  
  ##### Plot output ----
  
  output$vis_bar_plot <- renderPlot({
    
    # Set of conditions on which to try to render the plot
    # These are not totally necessary but will save computation and avoid displaying error messages. Basically, since code does not run from top to bottom, Shiny will sometimes try to render the plot before the other variables have been defined. These statements tell Shiny to abort the function if the necessary variables don't exist yet.
    if(is.null(input$vis_bar_var1)){return(NULL)}
    if(length(var_cat()) == 0){return(NULL)}
    
    # I like to define which inputs I'm using at the top of the function so that they are all listed in one place. It helps keep track of which inputs you're using and makes it easier to make changes down the road.
    var_axis <- input$vis_bar_var1
    var_fill <- input$vis_bar_fill
    flip_coords <- input$vis_bar_flip
    
    # start defining plot
    p <- df() %>% 
      mutate(
        # because we allowed some of our categorical variables to be numeric, we need to convert them to factors before sending them to ggplot
        across(all_of(c(var_axis, var_fill)), as.factor),
        across(all_of(var_axis), fct_infreq)
      ) %>% 
      ggplot(mapping = aes(x = .data[[var_axis]])) +
      geom_bar(mapping = aes(fill = .data[[var_fill]])) 
    
    if(flip_coords){
      p <- p + coord_flip()
    }
  
    return(p)
    
  }, height = 500) # You can specify height and width. Default width is 100%, height is 400px.
  
  #### Histograms -----
    
    ##### Input controls ----
    
    output$vis_hist_select_var1 <- renderUI({
      selectInput(
        "vis_hist_var1",
        "Select a variable",
        choices = var_num()
      )
    })
    
    ##### Plot output ----
    
    output$vis_hist_plot <- renderPlot({
      
      # Set of conditions on which to try to render the plot
      # These are not totally necessary but will save computation and avoid displaying error messages
      if(is.null(input$vis_hist_var1)){return(NULL)}
      if(length(var_num()) == 0){return(NULL)}
      
      # Specify inputs
      var_axis <- input$vis_hist_var1
      num_bins <- input$vis_hist_bins
      
      # Start defining plot
      p <- df() %>%
        ggplot(mapping = aes(x = .data[[var_axis]])) +
        geom_histogram(bins = num_bins)

      return(p)
      
    }, height = 500)
    
    
  #### Scatter plots -----
  
    ##### Input controls ----
    
    output$vis_scatter_select_var1 <- renderUI({
      selectInput(
        "vis_scatter_var1",
        "Select x axis variable",
        choices = var_num(),
        selected = var_num()[1]
      )
    })

    output$vis_scatter_select_var2 <- renderUI({
      selectInput(
        "vis_scatter_var2",
        "Select y axis variable",
        choices = var_num(),
        selected = var_num()[2]
      )
    })
    
    ##### Plot output ----
    
    output$vis_scatter_plot <- renderPlot({

      # Set of conditions on which to try to render the plot
      # These are not totally necessary but will save computation and avoid displaying error messages
      if(is.null(input$vis_scatter_var1)){return(NULL)}
      if(length(var_num()) == 0){return(NULL)}
      
      # Specify inputs
      var_axis_x <- input$vis_scatter_var1
      var_axis_y <- input$vis_scatter_var2
      smooth_method <- input$vis_scatter_smooth_method
      corr_method <- input$vis_scatter_corr_method
      corr_coef_name <- case_match(
        corr_method,
        "pearson" ~ "R",
        "spearman" ~ "rho",
        "kendall" ~ "tau")
      lbl_x <- input$vis_scatter_lbl_x
      lbl_y <- input$vis_scatter_lbl_y
      
      # start defining plot
      p <- df() %>%
        ggplot(mapping = aes(x = .data[[var_axis_x]], y = .data[[var_axis_y]])) +
        geom_point() +
        geom_smooth(method = smooth_method, formula = "y~x") +
        ggpubr::stat_cor(
          method = corr_method, 
          cor.coef.name = corr_coef_name,
          label.x.npc = lbl_x,
          label.y.npc = lbl_y,
          size = 6
        )

      return(p)

    }, height = 500)
    
}

# Run App ----
# Function to run the application 
shinyApp(ui = ui, server = server)
