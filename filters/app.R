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
          choices = c("Gaussiano", "Sobel", "Media (Box)", "Manuale")
        ),
        sliderInput(
          "gauss_sigma",
          "\u03c3",
          min = 0.1, max = 5, value = 1, step = .05
        ),
        sliderInput(
          "filter_len",
          "Dimensione del filtro:",
          min = 3, max = 9, value = 3, step = 2
        ),
        rHandsontableOutput(
          "view_filter"
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("img_originale"),
         plotOutput("img_filtrata")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  calculate_gaussian_filter = function(sigma, px_from_zero=3){
    # 2d centered dnorm
    gauss = function(x, y, sigma){return(1/(2*pi*sigma*sigma)*exp(-(x*x + y*y)/(2*sigma*sigma)))}
    
    kernel = matrix(0.0, nrow=px_from_zero*2 + 1, ncol=px_from_zero*2 + 1)
    
    for(i in 0:px_from_zero){
      if(i==0){
        gauss_dens = gauss(0:px_from_zero, i, sigma)
        kernel[(px_from_zero+1) : (px_from_zero*2 + 1), px_from_zero+1] = gauss_dens
        kernel[px_from_zero+1, (px_from_zero+2) : (px_from_zero*2 + 1)] = gauss_dens[2:length(gauss_dens)]
        kernel[1:px_from_zero, px_from_zero+1] = rev(gauss_dens[2:length(gauss_dens)])
        kernel[px_from_zero+1, 1:px_from_zero] = rev(gauss_dens[2:length(gauss_dens)])
      }else{
        gauss_dens = gauss(1:px_from_zero, i, sigma)
        kernel[px_from_zero+1 - i, (px_from_zero+2) : (px_from_zero*2 + 1)] = gauss_dens
        kernel[px_from_zero+1 + i, (px_from_zero+2) : (px_from_zero*2 + 1)] = gauss_dens
        kernel[px_from_zero+1 - i, 1 : px_from_zero] = rev(gauss_dens)
        kernel[px_from_zero+1 + i, 1 : px_from_zero] = rev(gauss_dens)
      }
      
    }
    return(kernel)
    
  }
  
  load_img_and_convert = function(path){
    img = imager::load.image(path)
    img = imager::grayscale(img)
    return(img)
  }
  
  get_kernel = function(method="box", kernel_size=NULL){
    if(method == "gaussian"){
      
    }else if(method == "box"){
      return(matrix(1/(kernel_size*kernel_size), kernel_size, kernel_size))
    }else if(method == "sobel"){
      sobel_nos = c(1,2,1,0,0,0,-1,-2,-1)
      ker_x = matrix(sobel_nos, 3, 3, byrow = FALSE)
      ker_y = matrix(sobel_nos, 3, 3, byrow = TRUE)
      return(c(ker_x, ker_y))
    }
  }
  
  filter_image = function(image, method="gaussian", kernel=NULL){
    if(method == "gaussian"){
      return(imager::isoblur(image, input$gauss_sigma))
    }else if(method == "box"){
      kernel = imager::as.cimg(get_kernel(method, kernel_size=input$filter_len))
      return(imager::convolve(img, imager::as.cimg(kernel)))
    }else if(method == "sobel"){
      kernels = get_kernel(method)
      ker_x = imager::as.cimg(kernels[1])
      ker_y = imager::as.cimg(kernels[2])
      G_x = imager::convolve(img, ker_x)
      G_y = imager::convolve(img, ker_y)
      return(sqrt((G_x*G_x) * (G_y*G_y)))
    }
    else{
      stop(paste("Unexpected filtering method specified:", method))
    }
  }
  
  plot_img = function(image, title=""){
    plot(image, main=title, axes=FALSE, xlim=c(0,dim(image)[1]), ylim=c(dim(image)[2], 0))
  }
  
  output$img_originale = renderPlot(
    {img = load_img_and_convert("img/cat.jpg");
     plot_img(img, "Immagine originale")
    })
  
  output$img_filtrata = renderPlot(
    {if(input$tipo_filtro == "Gaussiano"){method="gaussian"}
     else if(input$tipo_filtro == "Media (Box)"){method="box"}
     else if(input$tipo_filtro == "Sobel"){method="sobel"};
     img_mod = filter_image(img, method=method);
     plot_img(img_mod, "Immagine con filtro")}
  )
   
  output$view_filter = renderRHandsontable(
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

