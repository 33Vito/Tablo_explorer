
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(navbarPage(title = "Tablo explorer 1.0", inverse = F,
  tabPanel("Run har2csv", 
          column(2, 
                 withTags({
                   div(align="center",
                       a(href='https://www2.deloitte.com/au/en/pages/finance/topics/deloitte-access-economics.html', 
                         img(src='logo.png',height='50',width='210'))
                   )
                 }),
                 
                 hr(), 
                 br(),
                 
                 tags$blockquote("Most economic fallacies derive from the tendency to assume that there is a fixed pie.", 
                                 style = "font-size:90%;"), 
                 p("- Milton Friedman", align="right", style = "font-size:90%;"), 
                 br(),
                 
                 tags$blockquote("Successful investing is anticipating the anticipations of others.", 
                                 style = "font-size:90%;"), 
                 p("- John Maynard Keynes", align="right", style = "font-size:90%;"),
                 br(),
                 
                 tags$blockquote("Everything should be made as simple as possible, but no simpler.", 
                                 style = "font-size:90%;"), 
                 p("- Albert Einstein", align="right", style = "font-size:90%;"),
                 br(), 
                 
                 tags$blockquote("Most economic fallacies derive from successful investments that are simpler than otherwise.", 
                                 style = "font-size:90%;"), 
                 p("- John Milton Einstein", align="right", style = "font-size:90%;"),
                 br()
                 ),
          column(
            width = 10,
      
            # Application title
            # titlePanel("Directory Input Demo"),
            directoryInput('directory', label = 'selected directory', value = '~'),
            # textOutput("directory"),
            div(style="display: inline-block;width: 250px;",
                # textInput("tab_file", "Tablo file: ", "expand1.har")
                uiOutput("tab_file_control")
            ),
            div(style="display: inline-block;width: 130px;",
                textInput("csv_target", "Table: ", "VOA2")
                ),
            div(style="display: inline-block;width: 150px;",
                actionButton("b.har2csv", "Run batch har2csv")
                ),
            div(style="display: inline-block;width: 150px;",
                actionButton("b.table", "Read existing table")
            ),
            br(),
            h4("Converted table: "), 
            DT::dataTableOutput("dt.har2csv"),
      
            br(),
            h4("All files: "), 
            DT::dataTableOutput('files')
          )
          # column(1)
  ), 
  
  tabPanel("VOA2", 
           column(2),
           column(8, 
           h4("Value added in raw $ value"),
           selectizeInput("Ex_REG", "Excluding region: ", c("ROA", "ROW"), selected = "ROW", multiple = T),
           chorddiagOutput("cd.voa2", height = 800),
           div(style="display: inline-block;vertical-align:right; width: 100px;",
               textInput("REG", "Enter region: ", "ROA")
           ),
           plotlyOutput("py.s.voa2"),
           br(), 
           plotlyOutput("py.f.voa2"),
           br(), 
           DT::dataTableOutput("dt.voa2")
           ), 
           column(2)
           ),
  
  navbarMenu("TRAD", 
             tabPanel("$ raw Value", 
                      column(2), 
                      column(8,
                      h4("Flow of trade in $ value (excluding ROW)"),
                      chorddiagOutput("cd", height = 800),
                      DT::dataTableOutput("dt.cd")
                      ),
                      column(2)
                      ),
                      
             tabPanel("% by REG", 
                      column(2), 
                      column(8,
                      h4("Flow of trade as % of regional total"),
                      chorddiagOutput("pc.cd", height = 800),
                      DT::dataTableOutput("dt.pc.cd")
                      ),
                      column(2)
             ), 
             tabPanel("% by COM", 
                      column(2), 
                      column(8,
                      h4("Flow of trade as % of commodity total (excluding ROW)"),
                      chorddiagOutput("pc2.cd", height = 800),
                      DT::dataTableOutput("dt.pc2.cd")
                      ),
                      column(2)
             )
           ), 
  tabPanel("About", 
           column(1), 
           column(10,
           htmlOutput("wiki")
           ),
           column(1)
           )
))





