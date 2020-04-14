#### Load packages ----
library(shiny)
library(tidyverse)

#### Load data ----
nutrient_data <- read_csv("Data/NTL-LTER_Lake_Nutrients_PeterPaul_Processed.csv")
nutrient_data$sampledate <- as.Date(nutrient_data$sampledate, format = "%Y-%m-%d")
nutrient_data <- nutrient_data %>%
  filter(depth_id > 0) %>%
  select(lakename, sampledate:po4)

#### Define UI ----(user interface)
#a fluid page will include multiple elements that fit the
#size of the window no matter what size
ui <- fluidPage(
  titlePanel("Nutrients in Peter Lake and Paul Lake"), 
  sidebarLayout(
    sidebarPanel( 
      
      # Select nutrient to plot #adding widget where we can select an input
      selectInput(inputId = "y", #selecting the input of what is on the y axis
                  label = "Nutrient",
                  choices = c("tn_ug", "tp_ug", "nh34", "no23", "po4"), 
                  selected = "tp_ug"), 
      #this is whichever one is up when we open the app
  
      ),#closing out sidebar panel, sidebar layout is still open

    # Output
    mainPanel(
      plotOutput("scatterplot")
    ))) #closing sidebar layout and fluid page

#### Define server  ----
server <- function(input, output) {
     
    # Create a ggplot object for the type of plot you have defined in the UI  
       output$scatterplot <- renderPlot({
        ggplot(nutrient_data, #this is the data frame we are pulling from
               aes_string(x = "sampledate", y = input$y, #y is interactive so pulling from input designated above
                          fill = "depth_id", shape = "lakename")) +
          geom_point(alpha = 0.8, size = 2) +
          theme_classic(base_size = 14) +
          scale_shape_manual(values = c(21, 24)) +
          labs(x = "Date", y = expression(Concentration ~ (mu*g / L)), shape = "Lake", fill = "Depth ID") +
          scale_fill_distiller(palette = "YlOrBr", guide = "colorbar", direction = 1)
          #scale_fill_viridis_c(option = "viridis", begin = 0, end = 0.8, direction = -1)
      })
       
       
  }


#### Create the Shiny app object ----
shinyApp(ui = ui, server = server)


