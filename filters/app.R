#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
library(spatstat)
library(magrittr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Processamento interattivo immagini con filtri"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(
          "tipo_filtro",
          "Seleziona il filtro da applicare:",
          choices = c("Gaussiano", "Sobel", "Box", "Manuale")
        ),
        # sliderInput(
        #   "width_filter",
        #   "Larghezza del filtro (in pixel):",
        #   min = 3, max = 9, value = 3, step = 2,
        #   ticks = FALSE
        # ),
        sliderInput(
          "gauss_sigma",
          "\u03c3",
          min = 0.1, max = 3, value = 1, step = .05
        ),
        rHandsontableOutput(
          "view_filter"
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         imageOutput("img_originale"),
         imageOutput("img_filtrata")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  calculate_gaussian_filter = function(sigma, px_from_zero=3){
    # 2d centered dnorm
    gauss = function(x, y, sigma){return(1/(2*pi*sigma*sigma)*exp(-(x*x + y*y)/(2*sigma*sigma)))}
    
    kernel = matrix(0.0, nrow=px_from_zero*2 + 1, ncol=px_from_zero*2 + 1)
    
    for(i in 0:px_from_zero){ # TODO
      gaus_dens = gauss(0:px_from_zero, i, sigma)
      kernel[px_from_zero+1 : px_from_zero*2 + 1] = gaus_dens
      kernel[px_from_zero+1 : px_from_zero*2 + 1] = gaus_dens
      kernel[px_from_zero+1 : px_from_zero*2 + 1] = gaus_dens
      kernel[px_from_zero+1 : px_from_zero*2 + 1] = gaus_dens
    }
    
  }
  
  load_img_and_convert = function(path){
    img = imager::load.image(path)
    img = imager::grayscale(img)
    return(img)
  }
  
  output$img_originale = renderImage(
    {img = load_img_and_convert("img/cat.jpg");
      return(list(
      src = img,
      contentType = "image/jpg",
      alt = "cat"
    ))},
    deleteFile = FALSE)
  
  #output$img_filtrata = 
   
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)

