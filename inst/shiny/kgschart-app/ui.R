
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)
library(shinyjs)
library(kgschart)


shinyUI(fluidPage(

  # reference
  # https://gist.github.com/withr/8799489
  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="busy.css"),
      tags$script(type="text/javascript", src = "busy.js")
    )
  ),
  div(class = "busy",
      p("Please wait..."),
      img(src="ajaxloaderq.gif")
  ),
  useShinyjs(),

  # Application title
  titlePanel("KGS Rank Graph Parser"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput('input_file', 'Choose an image file (PNG format)', accept=c('image/png')),

      div(style="display: inline-block; vertical-align:middle",
          textInput('input_id', 'Or type in a player name', placeholder="CrazyStone")),
      div(style="display: inline-block;vertical-align:middle; width:10px",
          shiny::br()),
      div(style="display: inline-block; vertical-align:-30%;",
          actionButton('load_btn', 'Load')),

      shiny::hr(),

      shiny::strong('Rank range:'),
      shiny::tags$ul(
        shiny::tags$li(textOutput('rank_range')),
        style='list-style: none;'
      ),

      shiny::strong('Period:'),
      shiny::tags$ul(
        shiny::tags$li(textOutput('time_range')),
        style='list-style: none;'
      ),

      downloadButton('dl_btn', 'Download Data'),

      shiny::hr(),
      shiny::strong('Links'),
      shiny::tags$ul(
        shiny::tags$li(shiny::a('KGS Archives', target='_blank',
                           href='http://www.gokgs.com/archives.jsp')),
        shiny::tags$li(shiny::a('Bug Report', target='_blank',
                           href='https://github.com/kota7/kgschart-r/issues')),
        shiny::tags$li(shiny::a('kgschart package', target='_blank',
                                href='https://kota7.github.io/kgschart-r/'))
      ),


      shiny::hr(),
      textOutput('version_label'),
      shiny::p('Copyright \ua9 2017 by ',
               shiny::a('Kota Mori.', href='https://github.com/kota7/',
                        target='_blank'),
               shiny::br(),
               'All rights reserved.')
    ),


    # Show a plot of the generated distribution
    mainPanel(
      imageOutput('src_image', width='504px', height='378px'),
      shiny::br(),
      #actionButton('parse_btn', 'Parse!'),
      plotOutput("parsed_plot", width='525px', height='336px')
    )
  )
))
