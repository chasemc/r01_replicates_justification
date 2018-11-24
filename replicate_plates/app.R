#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

aa<-readRDS("data/replicates_plate.rds")

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Which plate"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         
        radioButtons("radio", label = h3("Radio buttons"),
                     choices = as.list(unique(aa[!is.na(aa)])), 
                     selected = "172-6_1")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tableOutput("ktable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$ktable <- renderPlot({
      # generate bins based on input$bins from ui.R
     aa %>% 
       kable_styling(c("striped","bordered"), full_width = F) %>% 
       mutate_all(ifelse(. == "172-6_4",
                         cell_spec(., color = "red", bold = T),
                         cell_spec(., color = "green", italic = T))
       ) %>% 
       kable(escape = F, align = "c") %>% 
       kable_styling(c("striped","bordered"), full_width = F) 
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

