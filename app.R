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
library(magrittr)
library(kableExtra)
library(dplyr)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(""),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width=3,
         
        radioButtons("radio", label = h3("Radio buttons"),
                     choices = as.list(sort(unique(aa[!is.na(aa)]))), 
                     selected = "172-6_1")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        htmlOutput("location"),
        tableOutput("ktable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$ktable <- function(){
      # generate bins based on input$bins from ui.R
     aa2 <-(aa)
     aa2[is.na(aa2)] <- " "
     aa2<-as.data.frame(aa2)
     
     aa2 %>% 
       mutate_all(funs(ifelse(. == input$radio,
                         cell_spec(., color = "red", bold = T),
                         cell_spec(., color = "green", italic = T))
       )) -> p
     
     rownames(p) <- LETTERS[1:16]
     p %>% 
       kable(escape = F, align = "c", row.names = T) %>% 
       kable_styling(c("striped","bordered"), full_width = F,  font_size = 10) 
      
     
     
   }
   
   
 output$location <- renderText({  
   lets <- LETTERS[1:16]
   nums <- 1:24
   
   b <- sapply(nums, function(x) paste0(lets, "-", x))
   b <- matrix(b, nrow = 16, ncol = 24)
   
b <-    b[which(aa %in% input$radio)]   

HTML(paste0(b , sep=" <br/> "))


 })

}
 

# Run the application 
shinyApp(ui = ui, server = server)

