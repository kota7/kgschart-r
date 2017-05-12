
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(kgschart)


shinyServer(function(input, output) {

  
  RV <- reactiveValues(
    object = NULL # store parsed object here
  )
  
  # when new input file is loaded, then update the source image
  observeEvent(input$input_file, {
    
    # show image 
    output$src_image <- renderImage({
      list(alt='source image', height='400px', width='auto',
           src=isolate(input$input_file$datapath))
    }) 
    
    # parse and store the result
    RV$object <- kgschart(isolate(input$input_file$datapath))
      
  })
  
  # when load/fetch button is clicked, then retrieve image file from server
  # if file is successfully loaded, then update the source image
  observeEvent(input$load_btn, {
    id <- input$input_id
    print(id)
    # is this valid ID?
    if (!(nchar(id)>0 && grepl('^[a-zA-Z][a-zA-Z0-9]*$', id))) {
      showNotification('invalid ID!')
      return()
    }
    fn <- download_graph(id)
    print(fn)
    if (!file.exists(fn)) {
      showNotification('failed to retrieve image!')
      return()
    }
    
    # show image
    output$src_image <- renderImage({
      list(alt='source image', height='400px', width='auto', src=fn)
    }) 
    
    # parse and store object
    RV$object <- kgschart(fn)
  })
  
  
  observeEvent(input$parse_btn, {
    if (is.null(isolate(RV$object))) return()
    
    output$parsed_plot <- renderPlot({ plot(isolate(RV$object)) })
  })
  
})
