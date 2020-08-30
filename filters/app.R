#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(rhandsontable)
library(magrittr)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   useShinyjs(),
   # Application title
   titlePanel("Processamento interattivo immagini con filtri"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(
          "image",
          "Seleziona un'immagine:",
          choices = c("Gatto"="cat.jpg",
                      "Fiore"="flower.jpg",
                      "Statua"="anaximenes.jpg",
                      "Cinciallegra"="cincia.jpg")
        ),
        selectInput(
          "filter_type",
          "Seleziona il filtro da applicare:",
          choices = c("Gaussiano"="gaussian",
                      "Sobel"="sobel",
                      "Media (Box)"="box",
                      "Mediana"="median",
                      "Manuale"="manual")
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
        h3("Filtro"),
        textOutput("table_1_title"),
        rHandsontableOutput(
          "view_filter"
        ),
        actionButton("update_filter", "Modifica filtro"),
        checkboxInput("normalize_manual", "Normalizza filtro (somma valori filtro = 1)"),
        textOutput("table_2_title"),
        rHandsontableOutput(
          "y_sobel"
        ),
        width = 5
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("img_originale"),
         plotOutput("img_filtrata"),
         dataTableOutput("debug"),
         width = 5
      )
   )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  prev_type = NULL
  
  kernel_reactive = reactiveValues(
    kern = NULL
  )
  
  image_reactive = reactiveValues(
    img = NULL
  )
  
  
  update_kernel_reactive = observe({
    input$filter_len
    input$gauss_sigma
    input$filter_type
    if(input$filter_type == "gaussian"){
      kernel_reactive$kern = calculate_gaussian_filter(input$gauss_sigma, 4)
    }else if(input$filter_type == "box"){
      kernel_reactive$kern = matrix(1/(input$filter_len*input$filter_len), input$filter_len, input$filter_len)
    }else if(input$filter_type == "sobel"){
      sobel_nos = c(-1,-2,-1,0,0,0,1,2,1)
      ker_x = matrix(sobel_nos, 3, 3, byrow = FALSE)
      ker_y = matrix(sobel_nos, 3, 3, byrow = TRUE)
      kernel_reactive$kern = list("x"=ker_x, "y"=ker_y)
    }else if(input$filter_type == "manual"){
      kernel_reactive$kern = matrix(1, input$filter_len, input$filter_len)
    }else{
      kernel_reactive$kern = NULL
    }
  })

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
  
  load_img_and_convert = function(path, grayscale=TRUE){
    img = imager::load.image(path)
    if(grayscale){
      img = imager::grayscale(img)
    }
    return(img)
  }
  
  
  kernel_to_df = function(ker){
    ker_df = ker %>% as.data.frame
    names(ker_df) = 1:ncol(ker_df)
    return(ker_df)
  }
  
  filter_img = function(orig_image, method, gauss_sigma, kern, len){
    if(method == "gaussian"){
      return(imager::isoblur(orig_image, gauss_sigma))
    }else if(method == "box"){
      return(imager::correlate(orig_image, imager::as.cimg(kern)))
    }else if(method == "sobel"){
      ker_x = imager::as.cimg(kern[["x"]])
      ker_y = imager::as.cimg(kern[["y"]])
      G_x = imager::correlate(orig_image, ker_x)
      G_y = imager::correlate(orig_image, ker_y)
      return(sqrt((G_x*G_x) * (G_y*G_y)))
    }else if(method == "manual"){
      return(imager::correlate(orig_image, imager::as.cimg(kern)))
    }else if(method == "median"){
      return(imager::medianblur(orig_image, len))
    }
    else{
      stop(paste("Unexpected filtering method specified:", method))
    }
  }
  
  plot_img = function(img, title=""){
    plot(img, main=title, axes=FALSE, xlim=c(0,dim(img)[1]), ylim=c(dim(img)[2], 0))
  }
  
  output$img_originale = renderPlot(
    {img_name = paste0("img/", input$image)
     grayscale = img_name!="img/cincia.jpg"
     image_reactive$img <<- load_img_and_convert(img_name, grayscale = grayscale)
     plot_img(image_reactive$img, "Immagine originale")
    })
  
  apply_filter = reactive({
    img_mod = filter_img(image_reactive$img, method=input$filter_type,
                         gauss_sigma=input$gauss_sigma, kern=kernel_reactive$kern,
                         len=input$filter_len)
    plot_img(img_mod, "Immagine con filtro")
  })
  
  output$img_filtrata = renderPlot(
    {
      apply_filter()
    }
  )
  
  get_formatted_table = function(df, readOnly){
    tab = rhandsontable(df, useTypes=TRUE, stretchH="all", readOnly=readOnly)
    for (c in 1:ncol(df)){
      tab = tab %>% hot_col(as.character(c), format = "0.0000")
    }
    return(tab)
  }
   
  output$view_filter = renderRHandsontable(
    {
      input$filter_len
      input$gauss_sigma
      if(!is.null(kernel_reactive$kern)){
        if(input$filter_type == "sobel"){
          kernel_display = kernel_reactive$kern[["x"]]
        }else{
          kernel_display = kernel_reactive$kern
        }
        ker_df = kernel_to_df(kernel_display)
        readOnlyTable = input$filter_type != "manual"
        get_formatted_table(ker_df, readOnlyTable)
      }
    }
    
    
  )
  
  output$y_sobel = renderRHandsontable(
    {
      if(input$filter_type == "sobel"){
        ker_df = kernel_reactive$kern[["y"]] %>% kernel_to_df
        get_formatted_table(ker_df, FALSE)
      }
    }
  )
  
  output$table_1_title = renderText(
      {if(input$filter_type == "sobel"){
        "Componente x:"
      }else if(input$filter_type == "median"){
        "Non disponibile per tipo filtro = 'Mediana'"
      }
    }
  )
  
  output$table_2_title = renderText(
    { if(input$filter_type == "sobel"){
        "Componente y:"
      }else if(input$filter_type == "gaussian"){
        paste("Nota: larghezza filtro approssimata. La larghezza del filtro utilizzato dal computer",
              "dipende da quanto velocemente i valori della gaussiana tendono a 0 all'allontanarsi dal centro",
              "del filtro. A sua volta, ciò dipende da sigma: più alto è, più lentamente i valori tendono",
              "allo zero, più grande sarà il filtro utilizzato dal computer.")
      }
    }
  )
  
  observeEvent(input$filter_type,
               {
                 if (input$filter_type == "gaussian"){
                   shinyjs::hide("filter_len")
                   shinyjs::show("gauss_sigma")
                   shinyjs::hide("update_filter")
                   shinyjs::hide("normalize_manual")
                 }else if(input$filter_type %in% c("box","manual","median")){
                   shinyjs::show("filter_len")
                   shinyjs::hide("gauss_sigma")
                   if(input$filter_type == "manual"){
                     shinyjs::show("update_filter")
                     shinyjs::hide("normalize_manual")
                   }else{
                     shinyjs::hide("update_filter")
                     shinyjs::hide("normalize_manual")
                   }
                 }else if(input$filter_type == "sobel"){
                   shinyjs::hide("filter_len")
                   shinyjs::hide("gauss_sigma")
                   shinyjs::hide("update_filter")
                   shinyjs::hide("normalize_manual")
                 }else{
                   shinyjs::hide("filter_len")
                   shinyjs::hide("gauss_sigma")
                   shinyjs::hide("update_filter")
                   shinyjs::hide("normalize_manual")
                 }
                 
                 if(input$filter_type == "median"){
                   shinyjs::hide("view_filter")
                 }else{
                   shinyjs::show("view_filter")
                 }
               })
  
  
  observeEvent(input$update_filter, {
    kernel_temp = as.matrix(hot_to_r(input$view_filter))
    if (input$normalize_manual){
      s = sum(kernel_temp)
      kernel_temp = kernel_temp / s
    }
    kernel_reactive$kern <<- kernel_temp
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

