
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output, session) {

#------------------------------------------Directory set up--------------------------------------------------------
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch

        path = choose.dir(default = readDirectoryInput(session, 'directory'))
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  )

  output$directory <- renderText({
    readDirectoryInput(session, 'directory')
  })

  output$files <- DT::renderDataTable({
    files = list.files(readDirectoryInput(session, 'directory'), full.names = T)
    data.frame(name = basename(files), file.info(files)[,c("size", "mtime", "ctime", "atime")]) %>% 
      datatable(rownames = F, 
                class = "hover", 
                extensions = 'Buttons',
                filter = "top", 
                selection = "single",
                options = list(dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               autoWidth = F))
  })
  
  output$tab_file_control <- renderUI({
    files = list.files(readDirectoryInput(session, 'directory'), full.names = T)
    ss <- basename(files)[input$files_rows_selected]
    textInput("tab_file", label = "Tablo file: ", value = ss)
  })

#------------------------------------------run har2csv--------------------------------------------------------
  observeEvent(input$b.har2csv, {
    # setwd(readDirectoryInput(session, 'directory'))
    batch_command <- paste0("har2csv ", input$tab_file, " ", input$csv_target, ".csv ", input$csv_target)
    shell(paste0("cd ", str_replace_all(readDirectoryInput(session, 'directory'), "/", "\\\\"), " && ", 
                 batch_command))
                  # "har2csv expand1.har trade_R.csv TRAD"))
    # shell("har2csv expand1.har trade_R.csv TRAD")
  })
  
  har2csv <- reactive({
    validate(
      need(input$b.har2csv | input$b.table, "Please select a table from the .har file to export")
    )
    read.csv(paste0(readDirectoryInput(session, 'directory'),"/", input$csv_target,".csv"))
  })
  
  output$dt.har2csv <- DT::renderDataTable({
    # req(input$b.har2csv | input$b.table)
    # validate(
    #   need(input$b.har2csv | input$b.table, "Please select a table from the .har file to export")
    # )
    datatable(har2csv(), 
              rownames = F, 
              class = "hover", 
              extensions = 'Buttons',
              filter = "top", 
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             autoWidth = F))
  })

#------------------------------------------TRAD chorddiag by $ value--------------------------------------------------------
  output$dt.cd <- DT::renderDataTable({
    dd <- har2csv() %>% 
      filter(TRADES == "BASIC") %>% 
      group_by(COM, REG1) %>% 
      summarise(Value = sum(Value, na.rm=T)) %>% 
      spread(REG1, Value) %>% 
      datatable(rownames = F, 
                class = "hover compact", 
                extensions = 'Buttons',
                filter = "top", 
                options = list(dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               autoWidth = F))
    dd
  })
  
  output$cd <- renderChorddiag({
    dd <- har2csv() %>% 
      filter(TRADES == "BASIC") %>% 
      filter(REG1 != "ROW") %>% 
      group_by(COM, REG1) %>% 
      summarise(Value = sum(Value, na.rm=T)) %>% 
      spread(REG1, Value)
    
    mm <- dd[,names(dd) != "COM"] %>% as.matrix()
    rownames(mm) <- dd$COM
    
    chorddiag(t(mm), 
              type = "bipartite", 
              palette = "Paired",
              ticklabelFontsize = 8,
              groupPadding = 1,
              groupnamePadding = 50, 
              groupnameFontsize = 14)
  })
  
#------------------------------------------TRAD chorddiag % by REG--------------------------------------------------------
  output$dt.pc.cd <- DT::renderDataTable({
    dd <- har2csv() %>% 
      filter(TRADES == "BASIC") %>% 
      group_by(COM, REG1) %>% 
      summarise(Value = sum(Value, na.rm=T)) %>% 
      group_by(REG1) %>% 
      mutate(Value = Value/sum(Value)) %>% 
      spread(REG1, Value) %>% 
      datatable(rownames = F, 
                class = "hover compact", 
                extensions = 'Buttons',
                filter = "top", 
                options = list(dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               autoWidth = F))
    dd
  })
  
  output$pc.cd <- renderChorddiag({
    dd <- har2csv() %>% 
      filter(TRADES == "BASIC") %>% 
      group_by(COM, REG1) %>% 
      summarise(Value = sum(Value, na.rm=T)) %>% 
      group_by(REG1) %>% 
      mutate(Value = Value/sum(Value)) %>% 
      spread(REG1, Value)
    
    mm <- dd[,names(dd) != "COM"] %>% as.matrix()
    rownames(mm) <- dd$COM
    
    chorddiag(t(mm), 
              type = "bipartite", 
              palette = "Paired",
              ticklabelFontsize = 8,
              groupPadding = 1,
              groupnamePadding = 50, 
              groupnameFontsize = 14)
  })
  
#------------------------------------------TRAD chorddiag % by COM--------------------------------------------------------
  output$dt.pc2.cd <- DT::renderDataTable({
    dd <- har2csv() %>% 
      filter(TRADES == "BASIC", REG1 != "ROW") %>% 
      group_by(COM, REG1) %>% 
      summarise(Value = sum(Value, na.rm=T)) %>% 
      group_by(COM) %>% 
      mutate(Value = Value/sum(Value)) %>% 
      spread(REG1, Value) %>% 
      datatable(rownames = F, 
                class = "hover compact", 
                extensions = 'Buttons',
                filter = "top", 
                options = list(dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               autoWidth = F))
    dd
  })
  
  output$pc2.cd <- renderChorddiag({
    dd <- har2csv() %>% 
      filter(TRADES == "BASIC", REG1 != "ROW") %>% 
      group_by(COM, REG1) %>% 
      summarise(Value = sum(Value, na.rm=T)) %>% 
      group_by(COM) %>% 
      mutate(Value = Value/sum(Value)) %>% 
      spread(REG1, Value)
    
    mm <- dd[,!(names(dd) %in% c("COM", "ROW"))] %>% as.matrix()
    rownames(mm) <- dd$COM
    
    chorddiag((mm), 
              type = "bipartite", 
              palette = "Paired",
              ticklabelFontsize = 8,
              groupPadding = 1,
              groupnamePadding = 50, 
              groupnameFontsize = 14)
  })

#------------------------------------------VOA2 dg & cd by $ value--------------------------------------------------------
  output$dt.voa2 <- DT::renderDataTable({
    dd <- har2csv() %>% 
      # mutate(Year = ymd(paste0(str_sub(COMBINED, 9,12), "-1-1"))) %>% 
      # select(-COMBINED) %>% 
      datatable(rownames = F, 
                class = "hover compact", 
                extensions = 'Buttons',
                filter = "top", 
                options = list(dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               autoWidth = F))
    dd
  })
  
  output$py.s.voa2 <- renderPlotly({
    # sd <- ymd(paste0(str_sub(har2csv()$COMBINED, 9,12), "-1-1")) %>% min
    # ed <- ymd(paste0(str_sub(har2csv()$COMBINED, 9,12), "-1-1")) %>% max
    
    har2csv() %>% 
      filter(REG == input$REG) %>% 
      mutate(Year = ymd(paste0(str_sub(COMBINED, 9,12), "-1-1"))) %>% 
      select(-REG, -COMBINED) %>% 
      # spread(NSAV_COMM, Value) %>%
      mutate(NSAV_COMM = fct_rev(NSAV_COMM)) %>% 
      plot_ly(x=~Year, y=~Value, color=~NSAV_COMM, type = 'bar', 
              colors = rev(brewer.pal(12, "Paired"))) %>%
      layout(yaxis = list(title = "Value added"), barmode = 'stack')
      
  })
  
  output$py.f.voa2 <- renderPlotly({
    # sd <- ymd(paste0(str_sub(har2csv()$COMBINED, 9,12), "-1-1")) %>% min
    # ed <- ymd(paste0(str_sub(har2csv()$COMBINED, 9,12), "-1-1")) %>% max
    
    har2csv() %>% 
      filter(REG == input$REG) %>% 
      mutate(Year = ymd(paste0(str_sub(COMBINED, 9,12), "-1-1"))) %>% 
      select(-REG, -COMBINED) %>% 
      # spread(NSAV_COMM, Value) %>%
      # mutate(NSAV_COMM = fct_rev(NSAV_COMM)) %>% 
      plot_ly(x=~Year, y=~Value, color=~NSAV_COMM, type = 'bar', 
              colors = (brewer.pal(12, "Paired"))) %>%
      layout(yaxis = list(title = "Value added"), barmode = 'group')
    
  })
  
  output$cd.voa2 <- renderChorddiag({
    
    dd <- har2csv() %>% 
      filter(!(REG %in% input$Ex_REG)) %>% 
      group_by(REG, NSAV_COMM) %>% 
      summarise(Value = mean(Value, na.rm=T)) %>% 
      spread(REG, Value)
    
    mm <- dd[,!(names(dd) %in% c("NSAV_COMM"))] %>% as.matrix()
    rownames(mm) <- dd$NSAV_COMM
    
    chorddiag(t(mm), 
              type = "bipartite", 
              palette = "Paired", 
              showTicks = F, 
              ticklabelFontsize = 6,
              groupPadding = 1,
              groupnamePadding = 10, 
              groupnameFontsize = 14)
  })

  
#------------------------------------------wikipedia--------------------------------------------------------
  output$wiki <- renderUI({
    page <- tags$iframe(src="https://en.wikipedia.org/wiki/Computable_general_equilibrium", 
                        height=900, width=1500)
    page
  })
  
})











