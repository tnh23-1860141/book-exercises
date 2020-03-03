# Exercise 3: interactive Shiny apps

# Load the shiny, ggplot2, and dplyr libraries
library("shiny")
library("ggplot2")
library("dplyr")

# You will again be working with the `diamonds` data set provided by ggplot2
# Use dplyr's `sample_n()` function to get a random 1000 rows from the data set
# Store this sample in a variable `diamonds_sample`
diamonds_sample <- sample_n(diamonds, 1000)

# For convenience store the `range()` of values for the `price` column
# (of your sample)
price_range <- range(diamonds_sample$price)

# For convenience, get a vector of column names from the `diamonds` data set to
# use as select inputs
features <- colnames(diamonds)


# To help keep the code organized, we'll store some UI elements in variables
# _before_ defining the UI.


# Define a variable `price_input` that is a `sliderInput()` with the following
# properties:
# - an inputId of `price_choice`
# - a label of "Price (in dollars)"
# - min and max valuesvalue based on the `price_range` calculated above
# - a current value equal to the price range
price_input <- sliderInput(
  inputId = "price_choice", 
  label = "Price (in dollars", 
  min = price_range[1],
  max = price_range[2], 
  value = price_range
)


# Define a variable `feature_input` that is a `selectInput()` with the
# label "Feature of Interest". This dropdown should let the user pick one of
# the columns of the diamond data set. Use the `carat` column as a default
# Make sure to set an inputId to reference in your server!
feature_input <- selectInput(
  inputId = "feature_choice",
  label = "Feature of Interest",
  choices = features,
  selected = "carat"
)

trendline_input <- checkboxInput(
  inputId = "show_trend",
  label = "Show Trendline ",
  value = TRUE
)

plot_output <- plotOutput(outputId = "plot")

# Define a UI using a `fluidPage()` layout with the following content:
my_ui <- fluidPage(
  titlePanel("Diamond Viewer"),
  price_input,
  feature_input,
  trendline_input,
  plot_output
)


# Define a `server` function (with appropriate arguments)
# This function should perform the following:
my_server <- function(input_list, output_list){
  
  output_list$plot <- renderPlot({
    
    selected_range <- input_list$price_choice
    
    filter_data <- diamonds_sample %>% 
      filter(price > selected_range[1], price < selected_range[2])  
      
   the_plot <-  ggplot(data = filter_data, 
                      mapping = aes_string(x = input_list$feature_choice, y = "price", color = "cut")) + 
                      geom_point()
      
      if(input_list$show_trend == TRUE) {
        the_plot <- the_plot + geom_smooth(se = FALSE)
      }
    
   return(the_plot)
   
  })
  
}

# Create a new `shinyApp()` using the above ui and server
shinyApp(ui = my_ui, server = my_server)
