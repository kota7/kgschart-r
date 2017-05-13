
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#if (!require(kgschart)) devtools::install_github('kota7/kgschart-r')
#if (packageVersion('kgschart') < '1.2.2') devtools::install_github('kota7/kgschart-r')


library(shiny)
library(kgschart)


shinyServer(function(input, output) {


  RV <- reactiveValues(
    object = NULL, # store parsed object here
    id = ''
  )

  # when new input file is loaded, then update the source image
  observeEvent(input$input_file, {

    # show image
    output$src_image <- renderImage({
      list(alt='source image', width='95%', align='right',
           src=isolate(input$input_file$datapath))
    })

    # parse and store the result
    RV$object <- kgschart(isolate(input$input_file$datapath))

    # keep id
    RV$id <- sub('\\-.*', '', basename(input$input_file$name))
  })

  # when load/fetch button is clicked, then retrieve image file from server
  # if file is successfully loaded, then update the source image
  observeEvent(input$load_btn, {
    id <- input$input_id
    #print(id)
    # is this valid ID?
    if (!(nchar(id)>0 && grepl('^[a-zA-Z][a-zA-Z0-9]*$', id))) {
      showNotification('invalid ID!')
      return()
    }
    fn <- download_graph(id)
    #print(fn)
    if (!file.exists(fn)) {
      showNotification('failed to retrieve image!')
      return()
    }

    # show image
    output$src_image <- renderImage({
      list(alt='source image', width='95%', align='right',
           src=fn)
    })

    # parse and store object
    RV$object <- kgschart(fn)

    # keep id
    RV$id <- id
  })


  observeEvent(RV$object, {
    if (is.null(isolate(RV$object))) return()

    output$parsed_plot <- renderPlot({ plot(isolate(RV$object)) })

    output$time_range <- renderText({
      tr <- isolate(RV$object$time_range)
      if (is.null(tr) || length(tr) != 2) return("N/A")
      tr <- strftime(tr, format='%Y-%m-%d')
      sprintf("%s\n~ %s", tr[1], tr[2])
    })

    output$rank_range <- renderText({
      rr <- isolate(RV$object$rank_range)
      if (is.null(rr) || length(rr) != 2) return("N/A")
      sprintf("%s ~ %s", rr[1], rr[2])
    })
  })


  output$dl_btn <- downloadHandler(
    filename = function() paste(isolate(RV$id), '.csv', sep=''),
    content = function(file) write.csv(isolate(RV$object$data), file, row.names=FALSE)
  )
})
