
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(kgschart)

shinyServer(function(input, output) {

  
  RV <- reactiveValues(
    current_file = NULL
  )
  
  # when new input file is loaded, then update the source image
  observeEvent(input$input_file, {
    RV$current_file <- isolate(input$input_file$datapath)
    output$src_image <- renderImage({
      list(alt='source image', height='400px', width='auto',
           src=isolate(input$input_file$datapath))
    }) 
  })
  
  # when load/fetch button is clicked, then retrieve image file from server
  # if file is successfully loaded, then update the source image
  observeEvent(input$load_btn, {
    id <- input$input_id
    print(id)
    # is this valid ID?
    if (!(nchar(id)>0 && grepl('^[a-zA-Z][a-zA-Z0-9]+$', id))) {
      showNotification('invalid ID!')
      return()
    }
    fn <- download_graph(id)
    print(fn)
    if (!file.exists(fn)) {
      showNotification('failed to retrieve image!')
      return()
    }
    
    RV$current_file <- fn
    output$src_image <- renderImage({
      list(alt='source image', height='400px', width='auto', src=fn)
    }) 
  })
  
  observeEvent(input$parse_btn, {
    if (!file.exists(RV$current_file)) return(NULL)  
    output$parsed_plot <- renderPlot({
      print("hey")
      x <- kgschart(isolate(RV$current_file))
      plot(x)
    })
  })
  
})
