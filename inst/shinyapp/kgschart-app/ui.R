
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css",href="busy.css"),
      tags$script(type="text/javascript", src = "busy.js")
    )
  ),
  div(class = "busy",  
      p("Please wait..."), 
      img(src="ajaxloaderq.gif")
  ),
  
  # Application title
  titlePanel("KGS Rank Graph Parser"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput('input_file', 'Choose PNG File',
                accept=c('image/png')),
      
      textInput('input_id', 'or type in a player name'),
      
      actionButton('load_btn', 'load')
      
      
    ),
    

    # Show a plot of the generated distribution
    mainPanel(
      imageOutput('src_image'),
      actionButton('parse_btn', 'Parse!'),
      plotOutput("parsed_plot")
    )
  )
))
