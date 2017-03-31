

serverAppCode <- function() {
  root.directory <- getwd()
  
  rootDirReactive <- reactive({
    root.directory
    
  })
  
  
  #   #==============================================================================================================================================
  #   ##
  #   obsB <- observe({
  #     fileInput()
  #     csvFileUploaded()
  #   })
  
  
  #==================================================================================================================================================
  ##
  fileInput <- reactive({
    if (is.null(input$file1)) {
      return(NULL)
    }
    input$file1
  })
  
  
  observeEvent(input$loadDataButton, {
    print("********888888888888888888888 cliqued")
    
    #session$sendCustomMessage(type = 'testmessage',
    #                        message = 'Thank you for clicking')
  })
  
  #==============================================================================================================================================
  #
  datasetInput <- reactive({
    print("in datasetInput <- reactive({")
    
    if (is.null(input$dataset)) {
      print("+++++++++++ null!!!!")
      return(invisible())
    } else
    {
      print("+++++++++++ not null!!!!")
    }
    
    if (is.null(input$loadDataButton)) {
      print("is.null(input$loadDataButton)")
      return(invisible())
    }
    
    if (input$loadDataButton == 0) {
      print("input$loadDataButton == 0")
      return()
    }
    
    isolate({
      print("in datasetInput: button clicked")
      
      dataset <- selectedOrUploadedDataSet()
      
      if (input$filterdataset != "FALSE") {
        dataset <- dataset.filter(
          dataset,
          distance.labels = c("A", "B", "C", "D"),
          distance.midpoints = c(25, 75, 150, 250),
          transect.id = input$transect.id.value.select,
          distance.field = input$distance.field.value.select,
          effort.field = input$effort.field.value.select,
          lat.field = input$lat.field.value.select,
          long.field = input$long.field.value.select,
          sp.field = input$sp.field.value.select,
          date.field = input$date.field.value.select
        )
      }
      
      return(dataset)
      
    })
  })
  
  
  #
  #==============================================================================================================================================
  ##
  reactiveEmbeddedDataset <- reactive({
    if (isInvalid(input$embeddedDataset))
      return(invisible())
    return(input$embeddedDataset)
  })
  
  #   observeEvent(input$embeddedDataset, {
  #
  #     print('embeddedDataset')
  #
  #   })
  
  #==============================================================================================================================================
  ##
  reactiveDataVisualizeContent <- reactive({
    if (input$visualizeDataButton == 0) {
      return(
        #invisible()
        
        data.frame(
          Date = as.Date(character()),
          File = character(),
          User = character(),
          stringsAsFactors = FALSE
        )
      )
    }
    
    isolate({
      sendLoadingMessageToClient(type = 'displayLoading',
                                 text =  "Loading data...",
                                 action = 'start')
      data.tmp <- selectedOrUploadedDataSet()
      sendLoadingMessageToClient(type = 'displayLoading',
                                 text =  "Loading data...",
                                 action = 'end')
      return(data.tmp)
      
    })
    
    #     return(data.frame(Date=as.Date(character()),
    #                       File=character(),
    #                       User=character(),
    #                       stringsAsFactors=FALSE) )
    
  })
  
  
  # #==============================================================================================================================================
  # ##
  # output$uploadedFileContent <- renderDataTable(
  #
  #   reactiveDataVisualizeContent(),
  #
  #   options = list(
  #     pageLength = 10,
  #     scrollX = "100%",
  #     scrollCollapse = TRUE)
  # )
  #
  #
  #==============================================================================================================================================
  ##
  output$uploadedFileContent <- DT::renderDataTable(DT::datatable(
    reactiveDataVisualizeContent(),
    options = list(
      orderClasses = TRUE,
      lengthMenu = c(5, 10, 20, 30, 50),
      pageLength = 5
    )
  ))
  
  
  #
  #==============================================================================================================================================
  #
  output$frequencyTable <- DT::renderDataTable(DT::datatable(
    data.frame.frequencies(),
    options = list(
      orderClasses = TRUE,
      lengthMenu = c(5, 10, 20, 30, 50),
      pageLength = 5
    )
  ))
  
  
  #==============================================================================================================================================
  #
  data.frame.frequencies  <- reactive({
    if (input$freqButton == 0) {
      return()
    }
    
    input$freqButton
    
    isolate({
      sendLoadingMessageToClient(type = 'displayLoading',
                                 text =  "Loading data...",
                                 action = 'start')
      df <- as.data.frame(table(dataset()$Alpha))
      sendLoadingMessageToClient(type = 'displayLoading',
                                 text =  "Loading data...",
                                 action = 'end')
      return(df)
      
    })
  })
  
  
  #==============================================================================================================================================
  #
  selectedOrUploadedDataSet <- reactive({
    print('in selectedOrUploadedDataSet')
    
    input$loadDataButton
    
    #input$dataset
    
    print("44444input$dataset")
    print(input$dataset)
    
    if (is.null(input$dataset)) {
      print('is.null(input$dataset)')
    }
    
    if (is.null(input$loadDataButton)) {
      print('is.null(input$loadDataButton)')
    }
    
    
    if (is.null(input$dataset) ||
        is.null(input$loadDataButton)) {
      return(invisible())
    }
    
    if (input$loadDataButton == 0) {
      return()
    }
    
    isolate({
      print('selectedOrUploadedDataSet: dans isolate')
      #sendLoadingMessageToClient(type = 'displayLoading', text =  "Loading data...", action = 'start')
      
      datasetFile <- fileInput()
      
      if (isInvalid(input$dataset) ||
          input$dataset == 0) {
        return(invisible())
      }
      
      if (input$dataset == "FALSE") {
        print('------ input$dataset=="FALSE"')
        if (is.null(datasetFile)) {
          return(invisible())
        }
        csvData <-  csvFileUploaded()
        
      }
      else{
        print('------ input$dataset=="TRUE"')
        if (isInvalid(reactiveEmbeddedDataset())) {
          return(invisible())
        }
        print('------ 1')
        data(list = reactiveEmbeddedDataset())
        print('------ 2')
        csvData <- get(reactiveEmbeddedDataset())
        print('------ 3')
      }
      
      # sendLoadingMessageToClient(type = 'displayLoading', text =  "Loading data...", action = 'end')
      
      return(csvData)
      
    })
  })
  
  
  #==============================================================================================================================================
  ##
  dataset                <- reactive({
    ds <- datasetInput()
    if (is.null(ds)) {
      return(invisible())
    }
    return(ds)
  })
  
  
  #==============================================================================================================================================
  ##
  dataset.names          <- reactive({
    names(dataset())
  })
  
  
  #==============================================================================================================================================
  ##
  dataset.names.nbr      <- reactive({
    length(dataset.names())
  })
  
  
  #==============================================================================================================================================
  ##
  dataset.non.numerical.names          <- reactive({
    non.nums <- !sapply(dataset(), is.numeric)
    dataset.names()[non.nums]
  })
  
  
  #==============================================================================================================================================
  ##
  dataset.numerical.names          <- reactive({
    var.nums <- sapply(dataset(), is.numeric)
    dataset.names()[var.nums]
  })
  
  
  #==============================================================================================================================================
  ##
  dataset.character.names          <- reactive({
    var.chars <- sapply(dataset(), is.character)
    dataset.names()[var.chars]
  })
  
  
  #==============================================================================================================================================
  ##
  dataset.non.numerical.names.nbr      <- reactive({
    length(dataset.non.numerical.names())
  })
  
  
  #==============================================================================================================================================
  ##
  transect <- reactive({
    shp.file.input <- shapeFileInput()
    return(shp.file.input)
  })
  
  
  #==============================================================================================================================================
  #
  shapeFileInput <- reactive({
    if (is.null(dataset()) || isInvalid(dataset())) {
      return(invisible())
    }
    
    shapefile.data <- dataset.observations.shp(dataset(),
                                               projection.string = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                                               count.field = count.field())
    
    return (shapefile.data)
  })
  
  
  #==============================================================================================================================================
  #
  pathInput <- reactive({
    output.dir <<- "./output"
    
    return(output.dir)
  })
  
  
  #==============================================================================================================================================
  #
  pathMCDSInput <- reactive({
    input$mcdspath
  })
  
  
  #==============================================================================================================================================
  #
  breaksInput     <- reactive({
    breaksr <- breakClasses()
    #breaksr <- c(0,50,100,200,300) #c(input$break1,input$break2,input$break3,input$break4,input$break5) ... it's now dynamic
    return(breaksr)
  })
  
  
  #==============================================================================================================================================
  #
  STR_LABELInput  <- reactive({
    strlabelr <- "STR_LABEL" #input$strlabel
    return(strlabelr)
  })
  
  
  #==============================================================================================================================================
  #
  SMP_EFFORTInput <- reactive({
    smpeffortr <- "WatchLenKm" # input$smpeffort
    return(smpeffortr)
  })
  
  
  #==============================================================================================================================================
  #
  STR_AREAInput   <- reactive({
    strarear <- "STR_AREA" #input$strarea
    return(strarear)
  })
  
  
  #==============================================================================================================================================
  #
  SMP_LABELInput  <- reactive({
    smplabelr <- "WatchID" #input$smplabel
    return(smplabelr)
  })
  
  
  #==============================================================================================================================================
  #
  DISTANCEInput   <- reactive({
    distancer <- "Distance" #input$distance
    return(distancer)
  })
  
  
  #==============================================================================================================================================
  #
  SIZEInput       <- reactive({
    sizer <- count.field() #"Count" #input$size
    return(sizer)
  })
  
  #==============================================================================================================================================
  #
  SamplingUnitInput       <- reactive({
    if (is.null(input$uniteEchantillonnage))
      return("Date")
    
    return(input$uniteEchantillonnage)  #"Date" #"Count" #input$sampling?
  })
  
  
  #==============================================================================================================================================
  ##
  output$samplingUnitSection <- renderUI({
    htmlTag.prefixe <-
      "<label class='control-label col-sm-3 col-md-3' for='uniteEchantillonnage'>
    <a href='#' class='tooltips tooltipsgen' title='' data-original-title='"
    
    htmlTag.field.text <-
      ifelse(input$language == "fr",
             "Unité d’échantillonnage",
             "Sampling unit")
    
    htmlTag.sufixe <-
      paste0("'>",
             htmlTag.field.text,
             ":</a></label> <div class='col-sm-1 col-md-1'>")
    
    htmlTag.data <-
      ifelse(
        input$language == "fr",
        "Choisir la variable qui définit l’unité d’échantillonnage qui sera utilisé pour les calculs de densité.",
        "Select the variable that will be used to define the sampling unit in the density computation."
      )
    
    selectTag <-
      selectInputWithoutLabel(
        inputId = "uniteEchantillonnage",
        choices = list("Date"),
        selected = "Date",
        multiple = FALSE,
        selectize = TRUE
      )
    
    HTML(paste(
      paste0(
        htmlTag.prefixe,
        htmlTag.data,
        htmlTag.sufixe,
        selectTag,
        "</div>"
      ),
      collapse = "\n"
    ))
    
  })
  #==============================================================================================================================================
  #
  factorInput     <- reactive({
    a <- NULL #input$factor
    return(a)
  })
  
  
  #==============================================================================================================================================
  #
  stratumInput    <- reactive({
    a <- input$stratum
    
    if (input$stratum == "NULL")
      a <- NULL
    
    return(a)
  })
  
  
  #==============================================================================================================================================
  #
  splitInput      <- reactive({
    if (is.null(input$lsubnbrvars))
      return(invisible())
    
    if (isInvalid(input$lsubnbrvars))
      return(invisible())
    
    if (allSpecies())
      return(FALSE)
    else
      return(TRUE)
    
  })
  
  
  #==============================================================================================================================================
  rareInput       <- reactive({
    if (isInvalid(input$sub_group_variable_values_for_rare))
      return(NULL)
    
    if (input$sub_group_variable_values_for_rare == "NULL")
      return(NULL)
    
    args <-
      setNames(c(input[['sub_group_variable_values_for_rare']]), c(input[['datasetVariables1']]))
    
    return(args)
  })
  
  
  #==============================================================================================================================================
  #
  periodInput     <- reactive({
    if (input$period == "NULL") {
      a <- NULL
    }
    else{
      info <- input$period
      dates <- unlist(strsplit(info, ' - ')) # dates[1], dates[2]
      #betterDates <- as.Date(dates,format = "%m/%d/%Y")
      betterDates <- as.Date(dates, format = "%Y-%m-%d")
      a <- betterDates
    }
    return(a)
  })
  
  
  #==============================================================================================================================================
  #
  estimatorInput  <- reactive({
    if (is.null(input$distanceAnalysis)) {
      return(invisible())
    }
    
    if (input$distanceAnalysis == "FALSE") {
      a <- list(c("UN", "CO"))
    }
    else{
      if (isInvalid(input$selectedEstimators)) {
        a <- NULL
      }
      else {
        selected.estimators <- input$selectedEstimators
        
        list.estimator <- list()
        
        if ("UN-CO" %in% selected.estimators) {
          list.estimator[[length(list.estimator) + 1]] <- c("UN", "CO")
        }
        if ("HN-CO" %in% selected.estimators) {
          list.estimator[[length(list.estimator) + 1]] <- c("HN", "CO")
        }
        if ("UN-PO" %in% selected.estimators) {
          list.estimator[[length(list.estimator) + 1]] <- c("UN", "PO")
        }
        if ("HN-HE" %in% selected.estimators) {
          list.estimator[[length(list.estimator) + 1]] <- c("HN", "HE")
        }
        if ("HA-CO" %in% selected.estimators) {
          list.estimator[[length(list.estimator) + 1]] <- c("HA", "CO")
        }
        if ("HA-PO" %in% selected.estimators) {
          list.estimator[[length(list.estimator) + 1]] <- c("HA", "PO")
        }
        
        a <- list.estimator
      }
    }
    return(a)
  })
  
  
  #==============================================================================================================================================
  #
  emptyInput      <- reactive({
    a <- NULL #input$empty
    return(a)
  })
  
  
  #==============================================================================================================================================
  #
  detectionInput  <- reactive({
    if (is.null(input$detection))
      # pas d'analyse de distance
      return("All")
    
    input$detection
  })
  
  
  #==============================================================================================================================================
  #
  monotoneInput   <- reactive({
    "Strict" #input$monotone
  })
  
  
  #==============================================================================================================================================
  #
  multiplierInput <- reactive({
    as.numeric(input$multiplier)
  })
  verboseInput    <- reactive({
    FALSE #as.logical(input$verbose)
  })
  
  
  #==============================================================================================================================================
  #
  titleInput      <- reactive({
    input$plottitle
  })
  
  
  #==============================================================================================================================================
  #
  subTitleInput   <- reactive({
    input$plotsubtitle
  })
  
  
  
  #==============================================================================================================================================
  #
  output$nbVarsLsubSectionFixe <- renderUI({
    selectInputWithoutLabel(
      inputId = "lsubnbrvars",
      choices = as.list(c("All/Tout", 1)),
      selected = "All/Tout",
      multiple = FALSE,
      selectize = TRUE
    )
    
  })
  
  
  #==============================================================================================================================================
  #
  output$nbVarsLsubSection <- renderUI({
    datasetvariablesnbr <- dataset.non.numerical.names.nbr()
    
    selectInput(
      inputId = "lsubnbrvars",
      "",
      choices = as.list(c("All/Tout", c(
        as.numeric(1:datasetvariablesnbr)
      ))),
      selected = "All/Tout",
      multiple = FALSE,
      selectize = TRUE
    )
    
  })
  
  
  #==============================================================================================================================================
  #
  lsubnbrvars <- reactive({
    if (is.null(input$lsubnbrvars))
      return(invisible())
    
    if (isInvalid(input$lsubnbrvars) || allSpecies())
      return(0)
    
    return(as.numeric(input$lsubnbrvars))
  })
  
  
  #==============================================================================================================================================
  #
  output$lsubs <- renderUI({
    if (is.null(input$lsubnbrvars))
      return(invisible())
    
    if (lsubnbrvars() == 0)
      return(tags$span("All dataset / toutes les donnees"))
    
    lsubs_output_list <- lapply(1:lsubnbrvars(), function(i) {
      selectvariablename <- paste("datasetVariables", i, sep = "")
      outTag <- div(
        class = "row lsubvar",
        style = "height: 120px;min-height: 120px;",
        div(
          class = "col-md-4",
          style = "width: 220px;",
          
          selectInput(
            inputId = selectvariablename,
            "",
            choices = as.list(dataset.non.numerical.names()[i:dataset.non.numerical.names.nbr()]),
            multiple = FALSE,
            selectize = TRUE
          )
        )
      )
    })
    
    do.call(tagList, lsubs_output_list)
    
  })
  
  
  #==============================================================================================================================================
  #
  output$lsubsValues <- renderUI({
    especesSelectionnesReactive()
  })
  
  
  #==============================================================================================================================================
  #
  especesSelectionnesReactive <- reactive({
    if (isInvalid(lsubnbrvars()))
      return()
    
    if (lsubnbrvars() == 0)
      return()
    
    lsubs_output_list <- lapply(1:lsubnbrvars(), function(i) {
      selectvariablename <- paste("datasetVariables", i, sep = "")
      selectvaluesname <-
        paste("sub_group_variable_values", i, sep = "")
      
      if (isInvalid(input[[selectvariablename]]))
        return (invisible())
      
      data.column      <-
        unique(dataset()[[input[[selectvariablename]]]], incomparables <-
                 FALSE)
      
      if (is.factor(data.column)) {
        data.column <-
          as.character(data.column)
      } #data.column <- levels(data.column)
      
      data.column <-
        data.column[!is.null(data.column) &
                      !is.na(data.column) & data.column != ""]
      
      outTag <-
        div(
          class = "row",
          style = "margin-bottom:0px; margin-top:20px;",
          div(
            class = "col-sm-4 col-md-4",
            selectInputWithoutLabel(
              inputId = selectvaluesname,
              choices = as.list(data.column),
              multiple = TRUE,
              selectize = TRUE
            )
          ),
          div(class = "col-sm-4 col-md-4",
              div(
                class = "alert alert-info",
                strong('Note:'),
                ifelse(
                  input$language == "fr",
                  'Choisir les classes des sous-groupes à utiliser.',
                  'Select the classes within subgroups to use.'
                )
              ))
        )
      
      list(outTag, br(), br())
      
    })
    
    do.call(tagList, lsubs_output_list)
    
  })
  
  
  #==============================================================================================================================================
  #
  output$rareValues <- renderUI({
    especesSelectionnesPourRareReactive()
  })
  
  
  #==============================================================================================================================================
  #
  especesSelectionnesPourRareReactive <- reactive({
    selectvariablename <- "datasetVariables1"
    selectvaluesname   <- "sub_group_variable_values_for_rare"
    
    if (isInvalid(lsubnbrvars()) ||
        lsubnbrvars() == "All/Tout" || lsubnbrvars() == 0) {
      outTag <- div(
        selectInputWithoutLabel(
          inputId = selectvaluesname,
          choices = "NULL",
          multiple = FALSE
        )
      )
    }
    else {
      if (isInvalid(input[[selectvariablename]]))
        return (invisible())
      
      data.column <- reactiveSubsetSelectedVariableValue()
      
      outTag <-
        div(
          selectInputWithoutLabel(
            inputId = selectvaluesname,
            choices = as.list(c("NULL", data.column)),
            multiple = FALSE
          )
        )
      
    }
    
    liste.selection <- list(outTag, br())
    
    do.call(tagList, liste.selection)
  })
  
  
  #==============================================================================================================================================
  #
  lsubRead <- function (prefix)
    function (i) {
      return(input[[paste0(prefix, i)]])
    }
  
  
  #==============================================================================================================================================
  #
  lsubInput       <- reactive({
    if (is.null(input$lsubnbrvars))
      return(invisible())
    
    if (isInvalid(input$lsubnbrvars) || isInvalid(lsubnbrvars()))
      return(invisible())
    
    if (allSpecies()) {
      return(NULL)
    }
    
    args <- setNames(lapply(1:lsubnbrvars(),
                            lsubRead('sub_group_variable_values')),
                     lapply(1:lsubnbrvars(), lsubRead('datasetVariables')))
    
    return(args)
  })
  
  
  #   #==============================================================================================================================================
  #   #
  #   output$select2ProvincesValues <- renderUI({
  #
  #     if(is.null(input$loadDataButton))
  #       return()
  #
  #     if(input$loadDataButton==0)
  #       return()
  #
  #     datasetInput()
  #
  #     isolate({
  #
  #       can2 <- canadaAdmLimits()
  #
  #       select2Tag <- div(class="controls-group",
  #                         div(class="controls",
  #                             selectInput(inputId="selectedProvinces", "",
  #                                         choices = can2@data[,"Province"] ,
  #                                         selected=c("Quebec", "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick"),
  #                                         multiple=TRUE, selectize=TRUE)
  #                         )
  #       )
  #
  #       buttonTelechargerTag <- div(class="row",
  #                                   div(class="col-md-4", downloadButton('downloadButton',
  #                                                                     label=ifelse(input$language=="fr",'La carte en PDF','Download the PDF'),
  #                                                                     class='btn btn-inverse btn-block shiny-bound-input')
  #                                   )
  #       )
  #
  #
  #       buttonVisualiserTag <- div(class="row",
  #                                  div(class="col-md-4", actionButton2("visualizeDataOnWorldMap",
  #                                                                   # style="display:none;  background: transparent;border: none !important;font-size:0;",
  #                                                                   class='btn btn-primary btn-block shiny-bound-input',
  #                                                                   label=ifelse(input$language=="fr","Visualiser la carte", "Visualize the map")
  #                                  )
  #                                  )
  #       )
  #
  #       brTag <- br()
  #
  #       do.call(tagList, list(select2Tag, brTag, buttonVisualiserTag, brTag, buttonTelechargerTag))
  #
  #     })
  #   })
  
  
  
  #==============================================================================================================================================
  #
  output$plotProvinces <- renderPlot({
    if (is.null(input$visualizeDataOnWorldMap))
      return(NULL)
    
    if (input$visualizeDataOnWorldMap == 0)
      return(NULL)
    
    #         x    <- faithful[, 2]  # Old Faithful Geyser data
    #         bins <- seq(min(x), max(x), length.out = 10 + 1)
    #
    #         # draw the histogram with the specified number of bins
    #         hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    input$visualizeDataOnWorldMap
    
    isolate({
      makeProvincesPlotReactive()
    })
    
  }, height = 400, width = 800)
  
  
  #==============================================================================================================================================
  #
  makeProvincesPlotReactive <- function() {
    if (is.null(shpmData()) || is.null(transect())) {
      return(invisible())
    }
    
    plot(transect(), col = "red", axes = T)
    return(plot(shpmData(), add = T, axes = T))
  }
  
  
  #==============================================================================================================================================
  #
  list.subset.variable.names <- reactive({
    l <-
      dataset.non.numerical.names()[1:dataset.non.numerical.names.nbr()]
    
    return(l)
  })
  
  
  #==============================================================================================================================================
  #
  reactiveSubsetSelectedVariableValue <- reactive({
    if (isInvalid(input$sub_group_variable_values1) ||
        input$sub_group_variable_values1 == "")
      return()
    
    selected.subsets <-
      strsplit(gsub(
        '"',
        "",
        gsub('"( )*"', ",", input$sub_group_variable_values1)
      ), split = ",")
    
    return(selected.subsets)
  })
  
  
  #==============================================================================================================================================
  #
  reactiveByStratum  <- reactive({
    ifelse(
      is.null(input$byStratum) ||
        is.na(input$byStratum) ||
        input$byStratum == "" ||
        input$byStratum == "FALSE",
      return(FALSE),
      return(TRUE)
    )
  })
  
  
  #==============================================================================================================================================
  #
  output$embeddedDatasetDiv <- renderUI({
    selectInput(
      inputId = "embeddedDataset",
      label = '',
      choices = as.list(c("quebec", "alcidae")),
      selected = "quebec",
      multiple = FALSE,
      selectize = TRUE
    )
    #selectInputWithoutLabel(inputId="embeddedDataset",choices=as.list(c("quebec", "alcidae")), selected = "quebec", multiple=FALSE, selectize=TRUE)
    
  })
  
  #   #
  #   output$embeddedDatasetDiv2 <- renderUI({
  #     selectInput(inputId="embeddedDataset2", label= '', choices=as.list(c("quebec", "alcidae")), selected = "quebec", multiple= FALSE, selectize=TRUE)
  #   })
  #
  #   observeEvent(input$embeddedDataset2, {
  #
  #     print('embeddedDataset2')
  #
  #   })
  #
  #   output$summary <- renderPrint({
  #     input$inputId
  #   })
  #
  #   observeEvent(input$inputId, {
  #     cat("Showing", input$inputId, "rows\n")
  #   })
  
  #==============================================================================================================================================
  #
  earthNatureMap <- reactive({
    if (isInvalid(rootDirReactive()))
      return(invisible())
    
    earthSh <-
      earth.map.shp(
        file.with.path = file.path(rootDirReactive(), "input", "Terreshp"),
        layer = "ne_10m_land"
      )
    return(earthSh)
  })
  
  
  #==============================================================================================================================================
  #
  shpmData <- reactive({
    temp.shpm <- earthNatureMap()
    
    if (is.null(transect()) || is.null(temp.shpm)) {
      return(invisible())
    }
    
    shpm <- dataset.and.earth.map.shp(transect(), temp.shpm)
    
    return(shpm)
  })
  
  #==============================================================================================================================================
  #
  makePlot1 <- function() {
    mcdspath <- file.path(rootDirReactive(), "input")
    
    shpm <- shpmData()
    plot(shpm, axes = T)
    plot(transect(),
         add = T,
         col = "red",
         axes = T)
  }
  
  #==============================================================================================================================================
  #
  makePlot1Reactive <- reactive({
    mcdspath <- file.path(rootDirReactive(), "input")
    
    shpm <- shpmData()
    
    plot(shpm, axes = T)
    plot(transect(),
         add = T,
         col = "red",
         axes = T)
  })
  
  
  #==============================================================================================================================================
  #
  makePlot2 <- function() {
    plot(transect(), col = "red", axes = T)
    plot(shpmData(), add = T, axes = T)
  }
  
  
  #==============================================================================================================================================
  #
  makePlot2Reactive <- reactive({
    plot(transect(), col = "red", axes = T)
    plot(shpmData(), add = T, axes = T)
  })
  
  
  #==============================================================================================================================================
  #
  reactiveGridSize <- reactive({
    #Sys.sleep(3)
    return(as.numeric(input$gridsize))
  })
  
  
  #==============================================================================================================================================
  #
  hexgridReactive <- reactive({
    return(input$useHexgrid)
  })
  
  
  #==============================================================================================================================================
  #
  longitude1Reactive <- reactive({
    if (isInvalid(input$longitude1))
      return()
    
    return(as.numeric(input$longitude1))
  })
  
  
  #==============================================================================================================================================
  #
  longitude2Reactive <- reactive({
    if (isInvalid(input$longitude2))
      return()
    
    return(as.numeric(input$longitude2))
  })
  
  
  #==============================================================================================================================================
  #
  latitude1Reactive <- reactive({
    if (isInvalid(input$latitude1))
      return()
    
    return(as.numeric(input$latitude1))
    
  })
  
  #==============================================================================================================================================
  #
  latitude2Reactive <- reactive({
    if (isInvalid(input$latitude2))
      return()
    
    return(as.numeric(input$latitude2))
    
  })
  
  
  #==============================================================================================================================================
  #
  deg2rad <- function(deg) {
    return(deg * pi / 180)
  }
  
  
  #==============================================================================================================================================
  #
  HaversineFormulaUSingRadians <-
    function(lat1, lat2, long1, long2) {
      R <- 6371
      delta.long <- (long2 - long1)
      delta.lat <- (lat2 - lat1)
      a <-
        sin(delta.lat / 2) ^ 2 + cos(lat1) * cos(lat2) * sin(delta.long / 2) ^
        2
      c <- 2 * asin(min(1, sqrt(a)))
      d = R * c
      return(d)
    }
  
  
  #==============================================================================================================================================
  #
  HaversineFormulaUSingDegrees <-
    function(lat1, lat2, long1, long2) {
      return(HaversineFormulaUSingRadians(
        deg2rad(long1),
        deg2rad(lat1),
        deg2rad(long2),
        deg2rad(lat2)
      ))# Distance in km
    }
  
  
  #==============================================================================================================================================
  # source: geoavir
  nbrRowsOfCreateGridInvalid <-
    function(lat1, lat2, long1, long2, size, max = 999) {
      Latitude = c(lat1, lat2)
      Longitude = c(long1, long2)
      Grid.size = c(size, size)
      
      long.dist1 <-
        abs(matrix(c(Longitude[1], Latitude[1]), ncol = 2) -
              destPoint(
                c(Longitude[1], Latitude[1]),
                b = 90,
                d = Grid.size[1],
                r = 6378137
              ))[1]
      long.dist2 <-
        abs(matrix(c(Longitude[2], Latitude[1]), ncol = 2) -
              destPoint(
                c(Longitude[2], Latitude[1]),
                b = 270,
                d = Grid.size[1],
                r = 6378137
              ))[1]
      lat.dist1 <-
        abs(matrix(c(Longitude[1], Latitude[1]), ncol = 2) -
              destPoint(
                c(Longitude[1], Latitude[1]),
                b = 180,
                d = Grid.size[2],
                r = 6378137
              ))[2]
      lat.dist2 <-
        abs(matrix(c(Longitude[2], Latitude[1]), ncol = 2) -
              destPoint(
                c(Longitude[2], Latitude[1]),
                b = 0,
                d = Grid.size[2],
                r = 6378137
              ))[2]
      degree.dist <-
        c(mean(c(long.dist1, long.dist2)), mean(c(lat.dist1,
                                                  lat.dist2)))
      xseq <- seq(Longitude[1],
                  Longitude[2],
                  by = degree.dist[1])
      
      yseq <- seq(Latitude[1],
                  Latitude[2],
                  by = degree.dist[2])
      
      nbrRows <- length(xseq) * length(yseq)
      
      if (is.na(nbrRows) || nbrRows > max)
        return(TRUE)
      return(FALSE)
      
    }
  
  
  #==============================================================================================================================================
  #
  reactiveGrid <- reactive({
    if (is.null(reactiveGridSize())) {
      return(invisible())
    }
    
    if (is.null(input$generateGridButton))
      return()
    
    if (input$generateGridButton == 0)
      return()
    
    isolate({
      new.grid <- grid.new(
        shpm.data = shpmData(),
        size = reactiveGridSize(),
        latitude1 = latitude1Reactive(),
        latitude2 = latitude2Reactive(),
        longitude1 = longitude1Reactive(),
        longitude2 = longitude2Reactive(),
        hexgrid = hexgridReactive(),
        projection.string = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      )
      
      #       size<- reactiveGridSize()
      #
      #       new.grid<-create.grid(Latitude=c(latitude1Reactive(),
      #                                        latitude2Reactive()),
      #                             Longitude=c(longitude1Reactive(),
      #                                         longitude2Reactive()),
      #                             Grid.size=c(size,
      #                                         size),
      #                             Clip=FALSE,
      #                             clip.shape= shpmData(),
      #                             projection=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      #
      #       new.grid$ID<-paste("parc",new.grid$ID,sep="")
      #
      
      return(new.grid)
    })
  })
  
  
  #==============================================================================================================================================
  #
  reactiveGridFinal <- reactive({
    if (is.null(transect()) || is.null(reactiveGrid())) {
      return(invisible())
    }
    
    if (is.null(input$generateFinalGridButton))
      return(invisible())
    
    if (input$generateFinalGridButton == 0)
      return()
    
    isolate({
      if (isInvalid(transect()) || isInvalid(reactiveGrid())) {
        return(invisible())
      }
      
      new.grid <- grid.final(transect(), reactiveGrid())
      
      if (!is.null(input$clip) & input$clip == "TRUE") {
        new.grid <-
          grid.clip(new.grid, shpmData()) #grid.clip(new.grid, transect())
      }
      
      
    })
    
    return(new.grid)
  })
  
  
  
  #==============================================================================================================================================
  #
  reactiveGridFinalColumnNames <- reactive({
    if (isInvalid(reactiveGridFinal()))
      return(invisible())
    
    grid.final.column.names <-  names(reactiveGridFinal()@data)
    
    return(grid.final.column.names)
  })
  
  
  #==============================================================================================================================================
  #
  makeGridPlotReactive <- reactive({
    if (is.null(transect()) ||
        is.null(shpmData()) || is.null(reactiveGrid())) {
      return(invisible())
    }
    
    size <- reactiveGridSize()
    
    if (nbrRowsOfCreateGridInvalid(
      latitude1Reactive(),
      latitude2Reactive(),
      longitude1Reactive(),
      longitude2Reactive(),
      size
    )) {
      session$sendCustomMessage(
        type = 'testmessage',
        message = list(
          a = 1,
          b = "Il ne peut y avoir plus de 1000 strates, pri\350re d'utiliser une taille de cellule plus grande ou r\351duisez la zone d'\351tude",
          controller = input$controller
        )
      )
      return(invisible())
      
    }
    
    plot(transect(), col = "red", axes = T)
    plot(shpmData(), add = T, axes = T)
    plot(reactiveGrid(), add = T, axes = T)
    
  })
  
  
  #==============================================================================================================================================
  #
  makeFinalGridPlot <- function() {
    new.grid <- reactiveGridFinal()
    
    plot(new.grid, axes = T)
    plot(transect(),
         col = "red",
         add = T,
         axes = T)
    plot(shpmData(), add = T, axes = T)
    
  }
  
  
  #==============================================================================================================================================
  #
  makeFinalGridPlotReactive <- reactive({
    new.grid <- reactiveGridFinal()
    plot(new.grid, axes = T)
    plot(transect(),
         col = "red",
         add = T,
         axes = T)
    plot(shpmData(), add = T, axes = T)
  })
  
  
  
  #==============================================================================================================================================
  #
  lsubUnique       <- function(resumePlotID) {
    if (is.null(input$lsubnbrvars))
      return(invisible())
    
    if (isInvalid(input$lsubnbrvars) || isInvalid(lsubnbrvars()))
      return(invisible())
    
    if (allSpecies()) {
      return(NULL)
    }
    
    species <- selectedSepecieAllOrRare()
    specie  <-
      ifelse(resumePlotID == 0, species[1], species[resumePlotID])
    
    #args <- setNames(list('values'), list('var'))
    args <-
      setNames(list(specie[[1]]), lapply(1:1, lsubRead('datasetVariables')))
    
    #cat('specie = ', specie[[1]], '\n')
    
    #print('args = ')
    #print(args)
    
    return(args)
  }
  
  
  #==============================================================================================================================================
  #
  distanceWrapWithOptionReactive <- function(resumePlotID = 0) {
    if (is.null(input$stratification))
      return(invisible())
    
    stratum <- "STR_LABEL"
    
    if (input$stratification == "stratgrille") {
      d <- prepareDatasetWithGrid()
      stratum <- stratumInput()
    } else{
      if (input$stratification == "stratzone" &
          !is.null(input$fileShapeFormes))
      {
        d <- prepareDatasetShapeFileWithZone()
      }
      else{
        return(invisible())
      }
    }
    
    m <-  distance.sampling.model.fit(
      d = d,
      output.dir = output.dir,
      pathMCDS = file.path(rootDirReactive(), "input"),
      STR_LABEL = "square",
      STR_AREA = "square_area",
      lsub = lsubUnique(resumePlotID),
      stratum = stratumInput(),
      detection = detectionInput(),
      empty = NULL,
      split = TRUE,
      period = periodInput(),
      multiplier = multiplierInput(),
      verbose    = FALSE,
      SMP_LABEL  = "SMP_LABEL",
      SMP_EFFORT = "SMP_EFFORT",
      breaks = breaksInput(),
      SIZE = count.field(),
      covariates = covariatesInput(),
      DISTANCE = DISTANCEInput(),
      factor = factorInput(),
      rare = rareInput(),
      monotone = "Strict",
      estimator = estimatorInput(),
      distanceAnalysis = distanceAnalysisReactive()
    )
    
    return(m)
    
  }
  
  
  #==============================================================================================================================================
  #
  distanceAnalysisReactive <- reactive({
    if (!is.null(input$distanceAnalysis) &
        input$distanceAnalysis == "FALSE") {
      return(FALSE)
    } else{
      return(TRUE)
    }
  })
  
  #==============================================================================================================================================
  #
  stratificationShapeFilePlusieursZones <- reactive({
    shapefile.data <- stratificationShapeFile()
    
    temp <-
      spTransform(
        shapefile.data,
        CRS(
          "+proj=utm +zone=17 +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        )
      )  #project to calculate area with package rgeos
    
    temp <- gArea(temp, byid = T) / (1000 * 1000)
    shapefile.data$area <-
      temp[match(shapefile.data$id, names(temp))]
    
    shapefile.data <-
      spTransform(shapefile.data, CRS(proj4string(transect())))
    
    return(shapefile.data)
    
  })
  
  
  #==============================================================================================================================================
  #
  stratificationShapeFile <- reactive({
    shapeFile <- input$fileShapeFormes
    if (is.null(shapeFile))
      return(NULL)
    
    dir.create(file.path(rootDirReactive(), "stratification"))
    stratification.dir <-
      paste(rootDirReactive(), "stratification", sep = "/")
    files.to.be.deleted <- list.files(path = stratification.dir)
    
    if (length(files.to.be.deleted) != 0)
      do.call(file.remove, c(lapply(files.to.be.deleted, function(x)
        paste(stratification.dir, x, sep = "/"))))
    
    unzip(shapeFile$datapath,
          exdir = stratification.dir,
          overwrite = TRUE)
    shapename <- list.files(stratification.dir, pattern = "\\.shp$")
    shapename <-
      tools::file_path_sans_ext(shapename, compression = FALSE)
    shapefile.data <- readOGR(stratification.dir, shapename)
    
    return(shapefile.data)
    
  })
  
  #==============================================================================================================================================
  #
  prepareDatasetShapeFileWithZone <- reactive({
    return(
      dataset.prepare.with.zones(
        dataset(),
        shapefile.data = stratificationShapeFile(),
        transect(),
        sample = SamplingUnitInput()
      )
    )
  })
  
  
  #==============================================================================================================================================
  #
  prepareDatasetWithGrid <- reactive({
    if (is.null(reactiveGridFinal()) || is.null(transect())) {
      return(invisible())
    }
    
    return(
      dataset.prepare.with.grid(
        dataset(),
        reactiveGridFinal(),
        transect(),
        reactiveGridSize(),
        sample = SamplingUnitInput()
      )
    )
    
  })
  
  
  #==============================================================================================================================================
  #
  reactiveDistanceWrap <- function(resumePlotID = 0) {
    m <- distanceWrapWithOptionReactive(resumePlotID)
    return(m)
  }
  
  
  #==============================================================================================================================================
  #
  sendUploadMessageToClient <-
    function(type = 'uploadFile',
             file_id = "file1",
             action = 'start',
             langage = "fr") {
      session$sendCustomMessage(
        type = type,
        message = list(
          file_id = file_id,
          action = action,
          langage = langage
        )
      )
    }
  
  
  #==============================================================================================================================================
  #
  datasetVariableNamesReactive <- reactive({
    print('in datasetVariableNamesReactive')
    
    input$loadDataButton
    
    if (input$loadDataButton == 0) {
      return(invisible())
    }
    
    if (is.null(input$dataset) ||
        is.na(input$dataset) || input$dataset == 0) {
      return(invisible())
    }
    
    # isolate({
    
    # if (is.null(input$loadingShown) || input$loadingShown == 0) {
    #   print('is.null(input$loadingShown) || input$loadingShown == 0')
    #   return(invisible())
    # }
    
    isolate({
      print('datasetVariableNamesReactive: dans isolate')
      
      sendLoadingMessageToClient(type = 'displayLoading',
                                 text =  "Loading data...",
                                 action = 'start')
      
      if (input$dataset == "FALSE") {
        #print("if (input$dataset == FALSE) { (begin)" )
        csvFile.variable.names <- names(csvFileUploaded())
        #print("if (input$dataset == FALSE) { (end)" )
        
      } else{
        if (isInvalid(reactiveEmbeddedDataset())) {
          sendLoadingMessageToClient(type = 'displayLoading',
                                     text = "Loading",
                                     action = 'end')
          return(invisible())
        }
        
        #csvFile.variable.names <- dataset.names()
        csvFile.variable.names <-
          names(dataset.embedded.load(dataset.name = reactiveEmbeddedDataset()))
        # sendLoadingMessageToClient(type = 'displayLoading',
        #                            text = "Loading",
        #                            action = 'end')
      }
      
      sendLoadingMessageToClient(type = 'displayLoading',
                                 text = "Loading",
                                 action = 'end')
      
      return(csvFile.variable.names)
    })
    # })
  })
  
  
  #==============================================================================================================================================
  #
  csvFileUploaded <- reactive({
    datasetFile <- fileInput()
    
    if (is.null(datasetFile) ||
        is.null(input$dataset) ||
        is.na(input$dataset) ||
        input$dataset == 0  || input$dataset == "TRUE") {
      return(invisible())
    }
    
    sendUploadMessageToClient(
      type = 'uploadFile',
      file_id = "file1Message",
      action = 'start',
      langage = input$language
    )
    
    csvFile <-
      read.table(datasetFile$datapath, sep = ";", header = TRUE)
    
    sendUploadMessageToClient(
      type = 'uploadFile',
      file_id = "file1Message",
      action = 'end',
      langage = input$language
    )
    
    return(csvFile)
  })
  
  
  #==============================================================================================================================================
  #
  output$transect.id.value <- renderUI({
    print('in output$transect.id.value')
    csvFile.variable.names <- datasetVariableNamesReactive()
    
    if (is.null(csvFile.variable.names)) {
      return("WatchID")
    }
    
    selectInputWithoutLabel(
      inputId = "transect.id.value.select",
      choices = as.list(csvFile.variable.names),
      selected = ifelse(
        "WatchID" %in% csvFile.variable.names,
        "WatchID",
        csvFile.variable.names[[1]]
      ),
      multiple = FALSE,
      selectize = TRUE
    )
  })
  
  
  #==============================================================================================================================================
  #
  output$distance.field.value <- renderUI({
    print('in output$distance.field.value')
    csvFile.variable.names <- datasetVariableNamesReactive()
    
    if (is.null(csvFile.variable.names)) {
      return("Distance")
    }
    
    selectInputWithoutLabel(
      inputId = "distance.field.value.select",
      choices = as.list(csvFile.variable.names),
      selected = ifelse(
        "Distance" %in% csvFile.variable.names,
        "Distance",
        csvFile.variable.names[[1]]
      ),
      multiple = FALSE,
      selectize = TRUE
    )
    
  })
  
  
  #==============================================================================================================================================
  #
  distance.field <- reactive({
    if (input$dataset == "FALSE") {
      return(input$distance.field.value.select)
    }
    else {
      return("Distance")
    }
  })
  
  
  #==============================================================================================================================================
  #
  output$effort.field.value <- renderUI({
    print('in output$effort.field.value')
    csvFile.variable.names <- datasetVariableNamesReactive()
    
    if (is.null(csvFile.variable.names)) {
      return("WatchLenKm")
    }
    
    selectInputWithoutLabel(
      inputId = "effort.field.value.select",
      choices = as.list(csvFile.variable.names),
      selected = ifelse(
        "WatchLenKm" %in% csvFile.variable.names,
        "WatchLenKm",
        csvFile.variable.names[[1]]
      ),
      multiple = FALSE,
      selectize = TRUE
    )
    
  })
  
  
  #==============================================================================================================================================
  #
  output$lat.field.value <- renderUI({
    print('in output$lat.field.value')
    csvFile.variable.names <- datasetVariableNamesReactive()
    
    if (is.null(csvFile.variable.names)) {
      return("LatStart")
    }
    
    selectInputWithoutLabel(
      inputId = "lat.field.value.select",
      choices = as.list(csvFile.variable.names),
      selected = ifelse(
        "LatStart" %in% csvFile.variable.names,
        "LatStart",
        csvFile.variable.names[[1]]
      ),
      multiple = FALSE,
      selectize = TRUE
    )
    
  })
  
  
  #==============================================================================================================================================
  #
  output$long.field.value <- renderUI({
    print('in output$long.field.value')
    csvFile.variable.names <- datasetVariableNamesReactive()
    
    if (is.null(csvFile.variable.names)) {
      return("LongStart")
    }
    
    selectInputWithoutLabel(
      inputId = "long.field.value.select",
      choices = as.list(csvFile.variable.names),
      selected = ifelse(
        "LongStart" %in% csvFile.variable.names,
        "LongStart",
        ifelse(
          "LonStart" %in% csvFile.variable.names,
          "LonStart",
          csvFile.variable.names[[1]]
        )
      ),
      multiple = FALSE,
      selectize = TRUE
    )
    
  })
  
  
  #==============================================================================================================================================
  #
  output$date.field.value <- renderUI({
    print('in output$date.field.valu')
    csvFile.variable.names <- datasetVariableNamesReactive()
    
    if (is.null(csvFile.variable.names)) {
      return("Date")
    }
    
    selectInputWithoutLabel(
      inputId = "date.field.value.select",
      choices = as.list(csvFile.variable.names),
      selected = ifelse(
        "Date" %in% csvFile.variable.names,
        "Date",
        csvFile.variable.names[[1]]
      ),
      multiple = FALSE,
      selectize = TRUE
    )
    
  })
  
  
  #==============================================================================================================================================
  #
  output$sp.field.value <- renderUI({
    print('in output$sp.field.value')
    csvFile.variable.names <- datasetVariableNamesReactive()
    
    if (is.null(csvFile.variable.names)) {
      return("Alpha")
    }
    
    selectInputWithoutLabel(
      inputId = "sp.field.value.select",
      choices = as.list(csvFile.variable.names),
      selected = ifelse(
        "Alpha" %in% csvFile.variable.names,
        "Alpha",
        csvFile.variable.names[[1]]
      ),
      multiple = FALSE,
      selectize = TRUE
    )
    
    
  })
  
  
  #==============================================================================================================================================
  #
  output$count.field.value <- renderUI({
    print('in output$count.field.value')
    csvFile.variable.names <- datasetVariableNamesReactive()
    
    if (is.null(csvFile.variable.names)) {
      return("Count")
    }
    
    selectInputWithoutLabel(
      inputId = "count.field.value.select",
      choices = as.list(csvFile.variable.names),
      selected = ifelse(
        "Count" %in% csvFile.variable.names,
        "Count",
        csvFile.variable.names[[1]]
      ),
      multiple = FALSE,
      selectize = TRUE
    )
    
  })
  
  
  #==============================================================================================================================================
  ##
  count.field <- reactive({
    if (input$dataset == "FALSE") {
      return(input$count.field.value.select)
    }
    else {
      return("Count")
    }
  })
  
  #   #==============================================================================================================================================
  #   ##
  #   observeEvent(input$loadDataButton, {
  #
  #     #     print('obsLoadDataButton 01')
  #     #     if(is.null(input$loadDataButton))
  #     #       return(invisible())
  #     #
  #     #     print('obsLoadDataButton 02')
  #     #     if(input$loadDataButton==0)
  #     #       return()
  #     #
  #     print('obsLoadDataButton 02')
  #
  #   })
  
  #   obs1B <- observe({
  #     input$loadDataButton
  #     print('obsLoadDataButton 01')
  #   })
  
  #   output$summary <- renderPrint({
  #     input$inputIdTest
  #   })
  
  #   obs1B <- observe({
  #     print(input$inputIdTest)
  #   })
  
  #   observeEvent(input$inputIdTest, {
  #     cat("Showing", input$inputIdTest, "rows\n")
  #   })
  
  
  #   #==============================================================================================================================================
  #   ##
  #   obsB <- observe({
  #
  #     if(is.null(input$fileShapeFormes))
  #       return(invisible())
  #
  #     sendUploadMessageToClient(type = 'uploadFile', file_id = "fileShapeFormesMessage", action = 'start', langage= input$language)
  #
  #     input$fileShapeFormes
  #
  #     sendUploadMessageToClient(type = 'uploadFile', file_id = "fileShapeFormesMessage", action = 'end', langage= input$language)
  #
  #   })
  
  
  #==============================================================================================================================================
  ##
  output$densities <- renderUI({
    if (isInvalid(input$classificationButton))
      return(invisible())
    
    if (input$classificationButton == 0)
      return(NULL)
    
    isolate({
      if (is.null(transect())) {
        return(invisible())
      }
      
      sendLoadingMessageToClient(type = 'displayLoading',
                                 text = "Loading",
                                 action = 'start')
      
      if (input$stratification %in% c("stratgrille", "stratzone")) {
        if (is.null(reactiveSubsetSelectedVariableValue()) ||
            allSpecies()) {
          archive.name <- generateAndsaveToFileShapeFileDensities()
          
          local({
            output$downloadButtonDensityMapPlot <- downloadHandler(
              filename = function() {
                'density.pdf'
              },
              
              content = function(file) {
                path.list <- density2.map(PDF = T)
                file.copy(path.list$path.to.image, file)
                
              },
              contentType = 'application/pdf'
              
            )
            
            output$downloadDensitiesCSV <- downloadHandler(
              filename    = function() {
                paste("densites", Sys.time(), '.txt', sep = '')
              },
              content     = function(file) {
                write.csv(densitiesData(), file)
              },
              contentType = 'text/x-comma-separated-values'
              
            )
            
            output$downloadShapeFile <- downloadHandler(
              filename    = function() {
                archive.name
              },
              
              content     = function(file) {
                path.list <- density2.map(PDF = F)
                file.copy(path.list$archive.name, file)
              }
            )
            
          })
          
          actionButtonTag <- div(
            class = "row",
            div(
              class = "col-md-12 well",
              div(class = "row",
                  div(class = "col-md-12",
                      h4(
                        ifelse(
                          input$language == "fr",
                          "Données de densités : ",
                          "Densities data:"
                        )
                      ))),
              
              br(),
              div(class = "row",
                  div(
                    class = "col-md-12",
                    actionButton2(
                      "visualizeDensityMapPlot_x",
                      class =
                        'btn action-button  btn btn-primary btn-block shiny-bound-input densityMapPlot',
                      label =
                        ifelse(
                          input$language == "fr",
                          #encode("Visualiser la carte des densités"),
                          "Générer et visualiser la carte des densités",
                          "Generate and visualize the density map"
                        )
                    )
                  )),
              br(),
              div(class = "row",
                  div(
                    class = "col-md-12",
                    downloadButton(
                      'downloadButtonDensityMapPlot',
                      label = ifelse(
                        input$language == "fr",
                        #encode('Télécharger la carte des densités'),
                        'Télécharger la carte des densités',
                        'Download the density map'
                      ),
                      class = 'btn btn-inverse btn-block shiny-bound-input'
                    )
                  )),
              br(),
              div(class = "row",
                  div(
                    class = "col-md-12",
                    downloadButton(
                      'downloadDensitiesCSV',
                      label = ifelse(
                        input$language == "fr",
                        #encode('Exporter les densités dans un fichier texte (csv)'),
                        'Exporter les densités dans un fichier texte (csv)',
                        'Download the densitites as a csv file'
                      ),
                      #class='btn btn-inverse btn-block shiny-bound-input')
                      class = 'btn btn-block shiny-bound-input'
                    )
                  )),
              br(),
              div(class = "row",
                  div(
                    class = "col-md-12",
                    downloadButton(
                      'downloadShapeFile',
                      label = ifelse(
                        input$language == "fr",
                        #encode("Fichiers de formes (shapefile) de l'analyse"),
                        "Fichiers de formes (shapefile) de l'analyse",
                        "Download the resulting shapefile"
                      ),
                      class = 'btn btn-success btn-block shiny-bound-input'
                    )
                  ))
            )
          )
          
          sendLoadingMessageToClient(type = 'displayLoading',
                                     text = "Loading",
                                     action = 'end')
          
          return(do.call(tagList, list(actionButtonTag, br(), br())))
          
        }
        else{
          # rare or species selected
          
          local({
            output$downloadALLDensities <- downloadHandler(
              filename    = function() {
                "densities-all-results.zip"
              },
              content     = function(file) {
                file.copy(generateAndDownloadAllDensities(), file)
              }
            )
          })
          
          subset.names <- selectedSepecieAllOrRare()
          
          # case of rare: the base model
          if (!isInvalid(input$sub_group_variable_values_for_rare) &
              input$sub_group_variable_values_for_rare != "NULL") {
            # begin rare
            
            subset.i.name <- "visualizeDensityMapPlot_0"
            download.subset.i.name <-
              "downloadButtonDensityMapPlot_0"
            download.density.csv.i.name <- "downloadDensitiesCSV_0"
            download.density.zip.i.name <- "downloadShapeFile_0"
            
            specie <- subset.names[1]
            
            archive.name <-
              generateAndsaveToFileShapeFileDensities(0)
            
            
            local({
              output[[download.subset.i.name]] <- downloadHandler(
                filename = function() {
                  'density.pdf'
                },
                
                content = function(file) {
                  path.list <- density2.map(resumePlotID = 0, PDF = T)
                  file.copy(path.list$path.to.image, file)
                  
                },
                contentType = 'application/pdf'
              )
              
              output[[download.density.csv.i.name]] <-
                downloadHandler(
                  filename    = function() {
                    paste("densites", Sys.time(), '.txt', sep = '')
                  },
                  
                  content     = function(file) {
                    write.csv(densitiesData(resumePlotID = 0), file)
                  },
                  contentType = 'text/x-comma-separated-values'
                )
              
              output[[download.density.zip.i.name]] <-
                downloadHandler(
                  filename    = function() {
                    archive.name
                  },
                  content     = function(file) {
                    file.copy(
                      paste(
                        rootDirReactive(),
                        "output",
                        "rare-base-model" ,
                        archive.name,
                        sep = "/"
                      ),
                      file
                    )
                  }
                )
              
            })
            
            actionButtonTag <- div(
              class = "row",
              div(
                class = "col-md-12 well",
                div(class = "row",
                    div(class = "col-md-12",
                        h4(
                          ifelse(
                            input$language == "fr",
                            #encode(paste0(paste0("Données de densités de \"", subset.names[i]),"\" : ")),
                            paste0("Données de densités de \"", subset.names[1], "\" : "),
                            paste0("Data densities of \"", subset.names[1], "\":")
                          )
                        ))),
                br(),
                
                div(class = "row",
                    div(
                      class = "col-md-12",
                      actionButton2(
                        subset.i.name,
                        class =
                          "btn btn-primary btn-block shiny-bound-input densityMapPlot",
                        label =
                          ifelse(
                            input$language == "fr",
                            #encode(paste0(paste0("Visualiser la carte des densités de \"", subset.names[i]),"\"")),
                            paste0(
                              "Générer et visualiser la carte des densités de \"",
                              subset.names[1],
                              "\""
                            ),
                            paste0(
                              "Generate and visualize the density map of \"",
                              subset.names[1],
                              "\""
                            )
                          )
                      )
                    )),
                br(),
                div(class = "row",
                    div(
                      class = "col-md-12",
                      downloadButton(
                        download.subset.i.name,
                        label = ifelse(
                          input$language == "fr",
                          #encode(paste0(paste0("Télécharger la carte des densités de  \"", subset.names[i]),"\"")),
                          paste0(
                            "Télécharger la carte des densités de  \"",
                            subset.names[1],
                            "\""
                          ),
                          paste0("Download the density map of \"", subset.names[1], "\"")
                        ),
                        class = 'btn btn-inverse btn-block shiny-bound-input'
                      )
                    )),
                br(),
                div(class = "row",
                    div(
                      class = "col-md-12",
                      downloadButton(
                        download.density.csv.i.name,
                        label = ifelse(
                          input$language == "fr",
                          #encode(paste0(paste0("Exporter les densités de  \"", subset.names[i]),"\" dans un fichier texte (csv)")),
                          paste0(
                            "Exporter les densités de  \"",
                            subset.names[1],
                            "\" dans un fichier texte (csv)"
                          ),
                          paste0(
                            "Download the densitites of \"",
                            subset.names[1],
                            "\" as a csv file"
                          )
                        ),
                        class = 'btn btn-block shiny-bound-input'
                      )
                    )),
                br(),
                div(class = "row",
                    div(
                      class = "col-md-12",
                      downloadButton(
                        download.density.zip.i.name,
                        label = ifelse(
                          input$language == "fr",
                          #encode(paste0(paste0("Fichiers de formes (shapefile) de l'analyse pour  \"", subset.names[i]),"\"")),
                          paste0(
                            "Fichiers de formes (shapefile) de l'analyse pour  \"",
                            subset.names[1],
                            "\""
                          ),
                          paste0(
                            "Download the resulting shapefile of the analysis for \"",
                            subset.names[1],
                            "\""
                          )
                        ),
                        class = 'btn btn-success btn-block shiny-bound-input'
                      )
                    ))
                
              )
            )
            
            resumePlotButtons <-  list(actionButtonTag, br(), br())
            
          }# end rare
          else{
            # begin else rare
            
            resumePlotButtons <-
              lapply(1:length(subset.names), function(i) {
                subset.i.name <- paste("visualizeDensityMapPlot_", i, sep = "")
                download.subset.i.name <-
                  paste("downloadButtonDensityMapPlot_", i, sep = "")
                download.density.csv.i.name <-
                  paste("downloadDensitiesCSV_", i, sep = "")
                download.density.zip.i.name <-
                  paste("downloadShapeFile_", i, sep = "")
                
                specie <- subset.names[i]
                
                archive.name <-
                  generateAndsaveToFileShapeFileDensities(i)
                
                
                local({
                  output[[download.subset.i.name]] <- downloadHandler(
                    filename = function() {
                      'density.pdf'
                    },
                    
                    content = function(file) {
                      tmp.i <-
                        as.numeric(substr(
                          download.subset.i.name,
                          nchar(download.subset.i.name),
                          nchar(download.subset.i.name)
                        ))
                      path.list <-
                        density2.map(resumePlotID = tmp.i, PDF = T)
                      file.copy(path.list$path.to.image, file)
                      
                    },
                    contentType = 'application/pdf'
                  )
                  
                  output[[download.density.csv.i.name]] <-
                    downloadHandler(
                      filename    = function() {
                        paste("densites", Sys.time(), '.txt', sep = '')
                      },
                      
                      content     = function(file) {
                        tmp.i <-
                          as.numeric(substr(
                            download.density.csv.i.name,
                            nchar(download.density.csv.i.name),
                            nchar(download.density.csv.i.name)
                          ))
                        
                        write.csv(densitiesData(resumePlotID = tmp.i), file)
                      },
                      contentType = 'text/x-comma-separated-values'
                    )
                  
                  output[[download.density.zip.i.name]] <-
                    downloadHandler(
                      filename    = function() {
                        archive.name
                      },
                      content     = function(file) {
                        file.copy(
                          paste(
                            rootDirReactive(),
                            "output",
                            specie[[1]] ,
                            archive.name,
                            sep = "/"
                          ),
                          file
                        )
                      }
                    )
                  
                })
                
                actionButtonTag <- div(
                  class = "row",
                  div(
                    class = "col-md-12 well",
                    div(class = "row",
                        div(class = "col-md-12",
                            h4(
                              ifelse(
                                input$language == "fr",
                                #encode(paste0(paste0("Données de densités de \"", subset.names[i]),"\" : ")),
                                paste0(
                                  "Données de densités de \"",
                                  subset.names[i],
                                  "\" : "
                                ),
                                paste0("Data densities of \"", subset.names[i], "\":")
                              )
                            ))),
                    br(),
                    
                    div(class = "row",
                        div(
                          class = "col-md-12",
                          actionButton2(
                            subset.i.name,
                            class =
                              "btn btn-primary btn-block shiny-bound-input densityMapPlot",
                            label =
                              ifelse(
                                input$language == "fr",
                                #encode(paste0(paste0("Visualiser la carte des densités de \"", subset.names[i]),"\"")),
                                paste0(
                                  "Générer et visualiser la carte des densités de \"",
                                  subset.names[i],
                                  "\""
                                ),
                                paste0(
                                  "Generate and visualize the density map of \"",
                                  subset.names[i],
                                  "\""
                                )
                              )
                          )
                        )),
                    br(),
                    div(
                      class = "row",
                      div(
                        class = "col-md-12",
                        downloadButton(
                          download.subset.i.name,
                          label = ifelse(
                            input$language == "fr",
                            #encode(paste0(paste0("Télécharger la carte des densités de  \"", subset.names[i]),"\"")),
                            paste0(
                              "Télécharger la carte des densités de  \"",
                              subset.names[i],
                              "\""
                            ),
                            paste0(
                              "Download the density map of \"",
                              subset.names[i],
                              "\""
                            )
                          ),
                          class = 'btn btn-inverse btn-block shiny-bound-input'
                        )
                      )
                    ),
                    br(),
                    div(
                      class = "row",
                      div(
                        class = "col-md-12",
                        downloadButton(
                          download.density.csv.i.name,
                          label = ifelse(
                            input$language == "fr",
                            #encode(paste0(paste0("Exporter les densités de  \"", subset.names[i]),"\" dans un fichier texte (csv)")),
                            paste0(
                              paste0("Exporter les densités de  \"", subset.names[i]),
                              "\" dans un fichier texte (csv)"
                            ),
                            paste0(
                              paste0("Download the densitites of \"", subset.names[i]),
                              "\" as a csv file"
                            )
                          ),
                          class = 'btn btn-block shiny-bound-input'
                        )
                      )
                    ),
                    br(),
                    div(
                      class = "row",
                      div(
                        class = "col-md-12",
                        downloadButton(
                          download.density.zip.i.name,
                          label = ifelse(
                            input$language == "fr",
                            #encode(paste0(paste0("Fichiers de formes (shapefile) de l'analyse pour  \"", subset.names[i]),"\"")),
                            paste0(
                              "Fichiers de formes (shapefile) de l'analyse pour  \"",
                              subset.names[i],
                              "\""
                            ),
                            paste0(
                              "Download the resulting shapefile of the analysis for \"",
                              subset.names[i],
                              "\""
                            )
                          ),
                          class = 'btn btn-success btn-block shiny-bound-input'
                        )
                      )
                    )
                    
                  )
                )
                
                list(actionButtonTag, br(), br())
              })
            
            actionButtonTagAll <-  div(class = "row",
                                       div(class = "col-md-12 well",
                                           div(
                                             class = "row",
                                             div(
                                               class = "col-md-12",
                                               downloadButton(
                                                 "downloadALLDensities",
                                                 label =
                                                   ifelse(
                                                     input$language == "fr",
                                                     "Générer et télécharger les fichiers des formes de toutes les analyses",
                                                     "Generate and download the shapefiles of all analysis"
                                                   ),
                                                 class =
                                                   'btn btn-success btn-block shiny-bound-input'
                                               )
                                             )
                                           )))
            
            resumePlotButtons <-
              list(resumePlotButtons, br(), actionButtonTagAll)
            
          }# end else rare
          
          sendLoadingMessageToClient(type = 'displayLoading',
                                     text = "Loading",
                                     action = 'end')
          
          #return(do.call(tagList, list(resumePlotButtons, br(), actionButtonTagAll)))
          return(do.call(tagList, resumePlotButtons))
          
        }
      } else
      {
        sendLoadingMessageToClient(type = 'displayLoading',
                                   text = "Loading",
                                   action = 'end')
        return(invisible())
      }
      
    })
    
  })
  
  
  #==============================================================================================================================================
  ##
  generateAndDownloadAllDensities <- reactive({
    species <- selectedSepecieAllOrRare()
    
    archives.list <- list()
    
    for (i in 1:length(species)) {
      l.temp <- density2.map(i, PDF = T)
      archives.list <- c(archives.list, l.temp$archive.name)
    }
    
    if (length(archives.list) == 0)
      return(invisible())
    
    dir.name         <- "./output"
    zip.archive.name <- "densities-all-results.zip"
    
    file.remove("./output/densities-all-results.zip")
    
    zip(paste(dir.name, zip.archive.name, sep = "/"),
        files = archives.list,
        flags = "-j")
    
    return(paste0(dir.name, "/", zip.archive.name))
  })
  
  
  #==============================================================================================================================================
  ##
  output$couleurs <- renderUI({
    if (is.null(input$nbrCouleurs)) {
      return(invisible())
    }
    
    nbr.couleurs <- as.numeric(input$nbrCouleurs)
    
    nbr.couleurs <- ifelse(nbr.couleurs < 12, nbr.couleurs, 12)
    
    colors <-
      c(
        "Green",
        "Yellow",
        "Red",
        "Black",
        "White",
        "Blue",
        "Cyan",
        "Magenta",
        "Gray",
        "Maroon",
        "Purple",
        "Navy"
      )
    
    couleursInputs <- lapply(1:nbr.couleurs, function(i) {
      couleur.i.name <- paste("classification.couleur_", i, sep = "")
      
      couleurInputTag <-
        div(class = "container-fluid", div(class = "row",
                                           div(
                                             class = "col-md-10",
                                             selectInput(
                                               inputId = couleur.i.name,
                                               "",
                                               choices =
                                                 as.list(colors),
                                               selected = colors[i],
                                               multiple =
                                                 FALSE,
                                               selectize = TRUE
                                             )
                                           )))
      
      list(couleurInputTag, br())
    })
    
    return(do.call(tagList, couleursInputs))
    
  })
  
  
  #==============================================================================================================================================
  ##
  output$densitesClasses <- renderUI({
    if (is.null(input$nbrClasses)) {
      return(invisible())
    }
    
    nbr.classes <- as.numeric(input$nbrClasses)
    
    classes.default <- c(0, 0.5, 0.75, 0.95)
    
    classificationInputs <- lapply(1:nbr.classes, function(i) {
      classe.i.name <- paste("classeInput_", i, sep = "")
      
      i.modulo <- (i %% 4)
      
      classInputTag <- div(class = "row",
                           div(
                             class = "col-md-10",
                             numericInput(
                               classe.i.name,
                               label = ifelse(
                                 input$language == "fr",
                                 encode(paste0("Classe #", i)),
                                 paste0("Class #", i)
                               ),
                               classes.default[ifelse(i.modulo ==
                                                        0, 4, i.modulo)]
                             )
                           ))
      
      list(classInputTag, br())
    })
    
    return(do.call(tagList, classificationInputs))
    
  })
  
  
  #==============================================================================================================================================
  ##
  output$visualizeDensityMapPlot <- renderImage({
    input$changedID
    
    if (is.null(input$densityMapID))
      return(NULL)
    
    if (input$stratification %in% c("stratgrille", "stratzone")) {
      if (input$densityMapID == "x") {
        path.list <- density2.map(PDF = F)
        
        list(
          src = path.list$path.to.image,
          contentType = 'image/png',
          width  = 960,
          height = 800,
          alt = "This is alternate text"
        )
        
      } else{
        path.list <- density2.map(as.numeric(input$densityMapID), PDF = F)
        
        list(
          src = path.list$path.to.image,
          contentType = 'image/png',
          width  = 960,
          height = 800,
          alt = "This is alternate text"
        )
      }
      
    }
    
  }, deleteFile = FALSE) #, height=400, width=800)
  
  
  #==============================================================================================================================================
  ## remplacement de output$distancePlotImage ... qui est cindée en deux expressions: output$distanceData et output$distanceHist
  output$distanceData <- renderUI({
    if (is.null(input$visualiserStatistiquesButton))
      return(invisible())
    
    if (input$visualiserStatistiquesButton == 0)
      return(invisible())
    
    isolate({
      pdf.file.name <- makeDistancePlotFile(resumePlotID = 1, PDF = TRUE)
      # remove the first two characters ("./output/...")
      pdf.file.name <-
        substr(pdf.file.name, 3, nchar(pdf.file.name))
      tags$iframe(src = pdf.file.name,
                  style = "width:100%;height:400px;border:0px;",
                  scrolling = "yes")
    })
  })
  
  
  #==============================================================================================================================================
  ##
  output$distanceDataID <- renderUI({
    input$changedID2
    input$sub_group_variable_values_for_rare
    
    if (is.null(input$resumeplotID))
      return()
    
    isolate({
      pdf.file.name <-
        makeDistancePlotFile(resumePlotID = as.numeric(input$resumeplotID),
                             PDF = TRUE)
      
      # remove the first two characters ("./output/...")
      pdf.file.name <-
        substr(pdf.file.name, 3, nchar(pdf.file.name))
      
      tags$iframe(src = pdf.file.name,
                  style = "width:100%;height:400px;border:0px;",
                  scrolling = "yes")
      
    })
  })
  
  
  #==============================================================================================================================================
  ##
  makeDistancePlotFile <- function(resumePlotID = 1,
                                   PDF = TRUE) {
    dir.name.output <- "output"
    file.name <- "distance"
    file.extension <- ".pdf"
    
    if (allSpecies()) {
      subdir.name <- "all"
      species.label <- c("all Species")
      resumePlotID <- 0
    }
    else {
      if (resumePlotID == 0) {
        resumePlotID <- 1
      }
      species       <- selectedSepecieAllOrRare()
      specie        <- species[resumePlotID]
      species.label <- c(specie[[1]])
      
      if (is.null(rareInput())) {
        subdir.name <- specie[[1]]
      } else{
        subdir.name <- "rare-base-model"
      }
    }
    
    output.dir <<- paste("./output", subdir.name, sep = "/")
    dir.absolute.path <-
      file.path(rootDirReactive(), dir.name.output, subdir.name)
    
    dir.create(file.path(rootDirReactive(), dir.name.output))
    dir.create(dir.absolute.path)
    
    l <- list(list.files(dir.absolute.path))
    do.call(file.remove, c(lapply(l, function(x)
      paste(dir.absolute.path, x, sep = "/"))))
    
    m <- reactiveDistanceWrap(resumePlotID)
    
    #setwd(dir.absolute.path)
    
    summary(
      model = m,
      species.subtitles = species.label,
      file = file.name,
      directory.absolute.path = dir.absolute.path,
      distanceAnalysis = distanceAnalysisReactive()
    )
    
    #setwd("..")
    
    return(file.path(output.dir, paste0(file.name, file.extension)))
  }
  
  
  #==============================================================================================================================================
  ##
  selectedSepecieOrRare <- function(resumePlotID = 1) {
    species <- reactiveSubsetSelectedVariableValue()
    specie <- species[resumePlotID]
    
    if (!isInvalid(input$sub_group_variable_values_for_rare) &
        input$sub_group_variable_values_for_rare != "NULL") {
      return(input$sub_group_variable_values_for_rare)
    }
    
    species <- reactiveSubsetSelectedVariableValue()
    return(species[resumePlotID])
    
  }
  
  
  #==============================================================================================================================================
  ##
  selectedSepecieAllOrRare <- function() {
    if (!isInvalid(input$sub_group_variable_values_for_rare) &
        input$sub_group_variable_values_for_rare != "NULL") {
      return(c(input$sub_group_variable_values_for_rare))
    }
    
    return(reactiveSubsetSelectedVariableValue())
    
  }
  
  
  
  #==============================================================================================================================================
  ##
  makeDistancePlotFileZip <- function(resumePlotID = -1) {
    if (allSpecies()) {
      pattern <-  "_.tmp"
    } else
      if (resumePlotID == 0) {
        resumePlotID.tmp  <- 1
        dir.name.base <- "rare-base-model"
        pattern <-  ".tmp"
      }
    else{
      #if(resumePlotID!=-1){
      resumePlotID.tmp  <- resumePlotID
      specie  <- selectedSepecieOrRare(resumePlotID)
      dir.name.base <- specie[[1]]
      pattern       <-
        paste0("(", dir.name.base, ".tmp)", "|(_.tmp)")
      # }
    }
    
    dir.name                       <-
      ifelse(allSpecies(),
             "./output/all",
             paste0("./output/", dir.name.base))
    zip.archive.name               <-
      ifelse(allSpecies(),
             "distance.zip",
             paste0("distance-", dir.name.base, ".zip"))
    #results.files.pattern          <- ifelse(allSpecies() || resumePlotID==0, ".tmp", paste0(dir.name.base, ".tmp"))
    #results.files.pattern          <- ifelse(allSpecies(), ".tmp", paste0(dir.name.base, ".tmp"))
    results.files.pattern          <-
      ifelse(allSpecies(), "_.tmp", pattern)
    
    pdf.file.name <-
      makeDistancePlotFile(resumePlotID = resumePlotID.tmp, PDF = TRUE)
    
    files.to.be.archived <-
      c(list.files(path = dir.name, pattern = results.files.pattern),
        "distance.pdf")
    
    file.remove(paste(dir.name, zip.archive.name, sep = "/"))
    
    zip(
      paste(dir.name, zip.archive.name, sep = "/"),
      files = paste(dir.name, files.to.be.archived , sep = "/"),
      flags = "-j"
    )
    
    return(paste0(dir.name, "/", zip.archive.name))
    
  }
  
  
  #==============================================================================================================================================
  ##
  makeDistancePlotAllFileZip <- function() {
    species <- selectedSepecieAllOrRare()
    
    archives.list <- list()
    
    for (i in 1:length(species)) {
      archives.list <- c(archives.list, makeDistancePlotFileZip(i))
    }
    
    if (length(archives.list) == 0)
      return(invisible())
    
    dir.name         <- "./output"
    zip.archive.name <- "distance-all-results.zip"
    
    file.remove("./output/distance-all-results.zip")
    
    zip(paste(dir.name, zip.archive.name, sep = "/"),
        files = archives.list,
        flags = "-j")
    
    return(paste0(dir.name, "/", zip.archive.name))
    
  }
  
  
  #==============================================================================================================================================
  ##
  plotHeightandWidth <- function() {
    if (reactiveDistanceAnalysis() != "TRUE") {
      return(0)
    }
    return("auto")
  }
  
  
  ##==============================================================================================================================================
  ##
  shapefileAndData <- function(densites) {
    input$lsubnbrvars
    
    if (!(input$stratification %in% c("stratzone", "stratgrille")))
      return(NULL)
    
    if (input$classificationButton == 0)
      return()
    
    isolate({
      classes     <- getClasses()
      
      if (input$stratification == "stratgrille") {
        grille <-  TRUE
        new.grid    <- reactiveGridFinal()
      }
      else{
        grille <-  FALSE
        shapefile.data <- stratificationShapeFile()
        new.grid <-
          new.grid.with.zones(shapefile.data, shapeFileInput())
      }
      
      nbr.arround <- as.numeric(input$nbrArrond)
      
      list.grid.brks.tags <- grid.brks.tags(
        new.grid = new.grid,
        densities = densites,
        classes = classes,
        nbr.arround = nbr.arround,
        stratgrille = grille
      )
      
      return(
        list(
          grid = list.grid.brks.tags$grid,
          brks = list.grid.brks.tags$brks,
          tags = list.grid.brks.tags$tags
        )
      )
      
    })
    
  }
  
  
  #==============================================================================================================================================
  ##
  getClasses <- reactive({
    if (input$classificationButton == 0)
      return(NULL)
    
    isolate({
      if (is.null(input$nbrClasses)) {
        return(NULL)
      }
      
      nbr.classes <- as.numeric(input$nbrClasses)
      
      if (is.null(nbr.classes) || nbr.classes < 1) {
        return(NULL)
      }
      
      classes <- c()
      j <- 1
      
      for (i in 1:nbr.classes) {
        input.i <- input[[paste("classeInput_", i, sep = "")]]
        input.i.nbr <- as.numeric(input.i)
        
        if (!is.null(input.i.nbr)) {
          classes[j] <- input.i.nbr
          j <- j + 1
        }
      }
      
      #cat("classes = ", classes, '\n')
      
      return(classes)
      
    })
  })
  
  
  #==============================================================================================================================================
  ##
  getColors <- reactive({
    if (is.null(input$nbrCouleurs)) {
      return(NULL)
    }
    
    nbr.couleurs <- as.numeric(input$nbrCouleurs)
    
    if (is.null(nbr.couleurs) || nbr.couleurs < 1) {
      return(NULL)
    }
    
    couleurs <- c()
    
    for (i in 1:nbr.couleurs) {
      #  c("green","yellow", "red")
      couleurs[i] <-
        input[[paste("classification.couleur_", i, sep = "")]]
    }
    
    return(couleurs)
    
  })
  
  
  #==============================================================================================================================================
  ##
  saveToFileGridShapeFileDensities <-
    function(dir, specie, new.grid2) {
      thetime <- Sys.time()
      
      dir.tmp <- dir
      
      #     if(allSpecies())
      #       dir.tmp <- "output/all"
      #     else
      #       dir.tmp <- paste0("output/", specie)
      
      #print("allSpecies() = ")
      #print(allSpecies())
      
      suffixe <-
        ifelse(allSpecies(), "densites", paste0("densites-", specie))
      
      #print("suffixe = ")
      #print(suffixe)
      
      new.shpfilename <- gsub("( )|:", "-", suffixe)
      archive.name <- gsub("( )|:", "-", paste(suffixe, ".zip"))
      
      #dir.create(file.path(rootDirReactive(), dir.tmp)) #; setwd(file.path(rootDirReactive(), "data"))
      dir.create(file.path(dir.tmp))
      
      archive.dir <- dir.tmp
      
      files.to.be.deleted <-
        list.files(path = archive.dir, pattern = new.shpfilename)
      if (length(files.to.be.deleted) != 0)
        do.call(file.remove, c(lapply(files.to.be.deleted, function(x)
          paste(archive.dir, x, sep = "/"))))
      
      #print("archive.dir (before) = ")
      #print(archive.dir)
      
      archive.dir.size <- nchar(archive.dir)
      
      if (substr(archive.dir, archive.dir.size, archive.dir.size) == "/")
        archive.dir <- substr(archive.dir, 1, archive.dir.size - 1)
      
      #print("archive.dir (after) = ")
      #print(archive.dir)
      
      writeOGR(
        new.grid2,
        dsn = archive.dir ,
        new.shpfilename,
        driver = "ESRI Shapefile",
        overwrite_layer = TRUE,
        check_exists = TRUE,
        delete_dsn = T
      )
      
      files.to.be.archived <-
        list.files(path = archive.dir, pattern = new.shpfilename)
      
      if (length(files.to.be.archived))
        zip(
          paste(archive.dir, archive.name, sep = "/"),
          files = paste(archive.dir, files.to.be.archived , sep = "/"),
          flags = "-j"
        )
      
      return(archive.name)
      
    }
  
  
  #==============================================================================================================================================
  ##
  densitiesData <- function(resumePlotID = 0) {
    input$lsubnbrvars
    m <- reactiveDistanceWrap(resumePlotID)
    
    return(densities.data(model = m, distanceAnalysis = distanceAnalysisReactive()))
  }
  
  
  #==============================================================================================================================================
  ##
  reactiveDensitiesFileName <- reactive({
    filename <- "densites"
    storecsv <- rootDirReactive()
    name <- paste(storecsv, "//", filename, ".txt", sep = "")
  })
  
  
  #==============================================================================================================================================
  ##
  output$plot1 <- renderPlot({
    p <- makePlot1Reactive()
    
    return(p)
    
    #},width = plotHeightandWidth, height = plotHeightandWidth)
  }, height = 400, width = 800)
  
  
  #==============================================================================================================================================
  ##
  output$plot2 <- renderPlot({
    p <- makePlot2Reactive()
    
    return(p)
    
  }, height = 400, width = 800)
  
  #==============================================================================================================================================
  ##
  output$plot1and2 <- renderPlot({
    return(makePlot2Reactive())
    
  }, height = 400, width = 800)
  
  
  #==============================================================================================================================================
  ##
  output$gridplot <- renderPlot({
    if (input$generateGridButton == 0) {
      return()
    }
    
    isolate({
      makeGridPlotReactive()
    })
    
    #},width = plotHeightandWidth, height = plotHeightandWidth)
  }, height = 400, width = 800)
  
  
  #==============================================================================================================================================
  ##
  output$finalgridplot <- renderPlot({
    if (is.null(input$stratification) ||
        input$stratification != "stratgrille")
      return(invisible())
    
    if (is.null(input$generateFinalGridButton))
      return(invisible())
    
    if (input$generateFinalGridButton == 0)
      return()
    
    isolate({
      print(makeFinalGridPlotReactive())
    })
    
    #},width = plotHeightandWidth, height = plotHeightandWidth)
  }, height = 400, width = 800)
  
  
  #==============================================================================================================================================
  ##
  reactiveDistanceAnalysis <-  reactive({
    if (isInvalid(input$distanceAnalysis)) {
      return("FALSE")
    }
    
    return(input$distanceAnalysis)
    
  })
  
  
  #==============================================================================================================================================
  ##
  output$distanceButtons <- renderUI({
    if (is.null(input$lsubnbrvars))
      return(invisible())
    
    if (!allSpecies() &
        (!isInvalid(reactiveSubsetSelectedVariableValue())) &
        length(reactiveSubsetSelectedVariableValue()) > 0) {
      # case of rare: the base model
      if (!isInvalid(input$sub_group_variable_values_for_rare) &
          input$sub_group_variable_values_for_rare != "NULL") {
        #print('*********************in rare ...')
        local({
          button.id <- "downloadButtonResumePlot_0"
          
          #print('in RARE .... button.id = ')
          #print(button.id)
          
          output[[button.id]] <- downloadHandler(
            filename = function() {
              'distance-basemodel.zip'
            },
            
            content = function(file) {
              #               tmp.i<- as.numeric(substr(button.id, nchar(button.id), nchar(button.id)))
              #               print('tmp.i = '); print(tmp.i)
              file.copy(makeDistancePlotFileZip(resumePlotID = 0), file)
            }
          )
        })
      } else{
        #print('*********************not in rare ...')
        
        subset.names <- selectedSepecieAllOrRare()
        
        for (i in 1:length(subset.names)) {
          local({
            button.id <- paste0("downloadButtonResumePlot_", i)
            
            output[[button.id]] <- downloadHandler(
              filename = function() {
                paste('distance-', i, '-.zip', sep = '')
              },
              
              content = function(file) {
                tmp.i <-
                  as.numeric(substr(button.id, nchar(button.id), nchar(button.id)))
                
                #                 print("tmp.i = ")
                #                 print(tmp.i)
                
                file.copy(makeDistancePlotFileZip(resumePlotID = tmp.i),
                          file)
              }#, contentType = 'application/pdf'
            )
          })
        }
        
        local({
          button.id <- "downloadAllButtonResumePlots"
          output[[button.id]] <- downloadHandler(
            filename = function() {
              'distance-all-categories.zip'
            },
            content = function(file) {
              file.copy(makeDistancePlotAllFileZip(), file)
            }
          )
        })
        
      }# else rare
      
      brTag <- br()
      
      #rareBaseModelTags        <- ""
      #buttonTelechargerToutTag <- ""
      resumePlotButtons        <- ""
      
      # case of rare: the base model
      if (!isInvalid(input$sub_group_variable_values_for_rare) &
          input$sub_group_variable_values_for_rare != "NULL") {
        # begin rare
        
        subset.i.name          <- "visualizeResumePlot_0"
        download.subset.i.name <- "downloadButtonResumePlot_0"
        
        actionButtonTagForBaseModel <- div(class = "row",
                                           div(
                                             class = "col-md-4",
                                             actionButton2(
                                               subset.i.name,
                                               class =
                                                 "btn btn-primary btn-block shiny-bound-input resumeplote",
                                               label =
                                                 ifelse(
                                                   input$language == "fr",
                                                   "Visualisez le résultat des sous-groupes sélectionnés",
                                                   "Visualize the result of the selected subgroups"
                                                 )
                                             )
                                           ))
        
        buttonTelechargerTagForBaseModel <- div(class = "row",
                                                div(
                                                  class = "col-md-4",
                                                  downloadButton(
                                                    download.subset.i.name,
                                                    label =
                                                      ifelse(
                                                        input$language == "fr",
                                                        "Télécharger le résultat des sous-groupes sélectionnés",
                                                        "Download the result of the selected subgroups"
                                                      ),
                                                    class =
                                                      'btn btn-inverse btn-block shiny-bound-input resumeplote2'
                                                  )
                                                ))
        
        #rareBaseModelTags <- list(brTag, actionButtonTagForBaseModel, brTag, buttonTelechargerTagForBaseModel, brTag)
        resumePlotButtons <-
          list(
            brTag,
            actionButtonTagForBaseModel,
            brTag,
            buttonTelechargerTagForBaseModel,
            brTag
          )
        
      }# end rare
      else{
        # begin else rare
        
        resumePlotButtons <-
          lapply(1:length(subset.names), function(i) {
            subset.i.name <- paste("visualizeResumePlot_", i, sep = "")
            
            download.subset.i.name <-
              paste("downloadButtonResumePlot_", i, sep = "")
            
            actionButtonTag <- div(class = "row",
                                   div(
                                     class = "col-md-4",
                                     actionButton2(
                                       subset.i.name,
                                       class =
                                         "btn btn-primary btn-block shiny-bound-input resumeplote",
                                       label =
                                         ifelse(
                                           input$language == "fr",
                                           paste0(
                                             "Visualiser le résultat du subset \"",
                                             subset.names[i],
                                             "\""
                                           ),
                                           paste0(
                                             "Visualize the result of subset \"",
                                             subset.names[i],
                                             "\""
                                           )
                                         )
                                     )
                                   ))
            
            buttonTelechargerTag <- div(class = "row",
                                        div(
                                          class = "col-md-4",
                                          downloadButton(
                                            download.subset.i.name,
                                            #"downloadButtonResumePlot"
                                            label = ifelse(
                                              input$language == "fr",
                                              #encode(paste0(paste0("Télécharger le resultat du sous-groupe \"", subset.names[i]),"\"")),
                                              paste0(
                                                "Télécharger le résultat du sous-groupe \"",
                                                subset.names[i],
                                                "\""
                                              ),
                                              paste0("Download the result of subset \"", subset.names[i], "\"")
                                            ),
                                            class = 'btn btn-inverse btn-block shiny-bound-input resumeplote2'
                                          )
                                        ))
            
            list(actionButtonTag,
                 brTag,
                 buttonTelechargerTag,
                 brTag,
                 brTag)
          })
        
        buttonTelechargerToutTag <- div(class = "row",
                                        div(
                                          class = "col-md-4",
                                          downloadButton(
                                            "downloadAllButtonResumePlots",
                                            label = ifelse(
                                              input$language == "fr",
                                              "Générer et télécharger les résultats de tous les sous-groupes",
                                              "Generate and download the result of all subsets"
                                            ),
                                            class = 'btn btn-success btn-block shiny-bound-input'
                                          )
                                        ))
        
        resumePlotButtons <-
          list(resumePlotButtons,
               brTag,
               brTag,
               buttonTelechargerToutTag)
        
      }# end else rare
      
      
      #do.call(tagList, list(div(class="well col-md-12", resumePlotButtons, rareBaseModelTags, brTag, buttonTelechargerToutTag)))
      #do.call(tagList, list(div(class="well col-md-12", resumePlotButtons, brTag, buttonTelechargerToutTag)))
      do.call(tagList, list(div(class = "well col-md-12", resumePlotButtons)))
      
    }
    else{
      if (allSpecies()) {
        local({
          output$downloadButtonResumeDistance <- downloadHandler(
            filename = function() {
              'distance.zip'
            },
            
            content = function(file) {
              file.copy(makeDistancePlotFileZip(), file)
            }#, contentType = 'application/pdf'
          )
        })
        
        actionButtonTag <- div(class = "row",
                               div(
                                 class = "col-md-4",
                                 actionButton2(
                                   "visualiserStatistiquesButton",
                                   class =
                                     'btn action-button  btn btn-primary btn-block shiny-bound-input',
                                   label =
                                     ifelse(
                                       input$language == "fr",
                                       #encode("Générer et visualiser les statistiques du modèle"),
                                       "Générer et visualiser les statistiques du modèle",
                                       "Generate and visualize the statistics of the model"
                                     )
                                 )
                               ))
        
        buttonTelechargerTag <- div(class = "row",
                                    div(
                                      class = "col-md-4",
                                      downloadButton(
                                        'downloadButtonResumeDistance',
                                        label = ifelse(
                                          input$language == "fr",
                                          #encode('Télécharger les statistiques du modèle'),
                                          'Télécharger les statistiques du modèle',
                                          'Download the statistics of the model'
                                        ),
                                        class = 'btn btn-inverse btn-block shiny-bound-input'
                                      )
                                    ))
        
        do.call(tagList, list(div(
          class = "well col-md-12",
          list(actionButtonTag, br(), buttonTelechargerTag, br())
        )))
        
      }
      else{
        return("")
      }
    }
  })
  
  
  #==============================================================================================================================================
  ##
  output$downloadFinalGrid <- renderUI({
    if (is.null(input$stratification) ||
        input$stratification != "stratgrille")
      return(invisible())
    
    outTag <- downloadButton(
      'downloadFinalGridButton',
      label = ifelse(
        input$language == "fr",
        'PDF de la grille finale',
        'Download the final grid'
      ),
      class = 'btn btn-inverse btn-block shiny-bound-input'
    )
    
    do.call(tagList, list(outTag, br()))
    
  })
  
  
  #==============================================================================================================================================
  ##
  output$downloadFinalGridButton <- downloadHandler(
    filename = function() {
      'finalgrid.pdf'
    },
    content = function(file) {
      pdf(file,
          onefile = T,
          width = 8.5,
          height = 11)
      makeFinalGridPlot()
      dev.off()
    },
    contentType = 'application/pdf'
  )
  
  
  #==============================================================================================================================================
  ##
  output$downloadPlot <- renderUI({
    outTag <- downloadButton(
      'downloadButton',
      label = ifelse(
        input$language == "fr",
        #encode('PDf des résultats'),
        'PDf des résultats',
        'Download the results'
      ),
      class = 'btn btn-inverse btn-block shiny-bound-input'
    )
    
    do.call(tagList, list(outTag, br()))
    
  })
  
  
  #==============================================================================================================================================
  ##
  output$downloadButton <- downloadHandler(
    filename = function() {
      'selected-map.pdf'
    },
    content = function(file) {
      pdf(file,
          onefile = T,
          width = 8.5,
          height = 11)
      makePlot2()
      dev.off()
    },
    contentType = 'application/pdf'
  )
  
  
  #==============================================================================================================================================
  ##
  selectInputWithoutLabel <-
    function(inputId,
             choices,
             selected = NULL,
             multiple = FALSE,
             selectize = TRUE,
             width = NULL,
             size = NULL) {
      # resolve names
      choices <- shiny:::choicesWithNames(choices)
      
      # default value if it's not specified
      if (is.null(selected)) {
        if (!multiple)
          selected <- firstChoice(choices)
      } else
        selected <-
          shiny:::validateSelected(selected, choices, inputId)
      
      if (!is.null(size) & selectize) {
        stop("'size' argument is incompatible with 'selectize=TRUE'.")
      }
      
      # create select tag and add options
      selectTag <- tags$select(
        id = inputId,
        class = if (!selectize)
          "form-control",
        size = size,
        selectOptions(choices, selected)
      )
      if (multiple)
        selectTag$attribs$multiple <- "multiple"
      
      # return the select tag
      res <- div(
        class = "form-group shiny-input-container",
        style = if (!is.null(width))
          paste0("width: ", validateCssUnit(width), ";"),
        div(selectTag)
      )
      
      if (!selectize)
        return(res)
      
      selectizeIt(inputId, res, NULL, nonempty = !multiple &
                    !("" %in% choices))
    }
  
  
  #==============================================================================================================================================
  ##
  selectInputWithLabel <-
    function(inputId,
             label = '',
             choices,
             selected = NULL,
             multiple = FALSE,
             selectize = TRUE,
             width = NULL) {
      # resolve names
      choices <- shiny:::choicesWithNames(choices)
      
      # default value if it's not specified
      if (is.null(selected)) {
        if (!multiple)
          selected <- shiny:::firstChoice(choices)
      } else
        selected <-
          shiny:::validateSelected(selected, choices, inputId)
      
      # create select tag and add options
      selectTag <-
        tags$select(id = inputId, shiny:::selectOptions(choices, selected))
      if (multiple)
        selectTag$attribs$multiple <- "multiple"
      
      # return label and select tag
      res <- div(class = "form-group shiny-input-container",
                 shiny:::controlLabel(inputId, label),
                 div(selectTag))
      
      if (!selectize)
        return(res)
      
      selectizeIt(inputId,
                  res,
                  NULL,
                  width,
                  nonempty = !multiple & !("" %in% choices))
    }
  
  
  
  firstChoice <- function(choices) {
    if (length(choices) == 0L)
      return()
    choice <- choices[[1]]
    if (is.list(choice))
      firstChoice(choice)
    else
      choice
  }
  
  # Create tags for each of the options; use <optgroup> if necessary.
  # This returns a HTML string instead of tags, because of the 'selected'
  # attribute.
  selectOptions <- function(choices, selected = NULL) {
    html <-
      mapply(
        choices,
        names(choices),
        FUN = function(choice, label) {
          if (is.list(choice)) {
            # If sub-list, create an optgroup and recurse into the sublist
            sprintf(
              '<optgroup label="%s">\n%s\n</optgroup>',
              htmlEscape(label, TRUE),
              selectOptions(choice, selected)
            )
            
          } else {
            # If single item, just return option string
            sprintf(
              '<option value="%s"%s>%s</option>',
              htmlEscape(choice, TRUE),
              if (choice %in% selected)
                ' selected'
              else
                '',
              htmlEscape(label)
            )
          }
        }
      )
    
    HTML(paste(html, collapse = '\n'))
  }
  
  # need <optgroup> when choices contains sub-lists
  needOptgroup <- function(choices) {
    any(vapply(choices, is.list, logical(1)))
  }
  
  
  selectizeInput <-
    function(inputId,
             ...,
             options = NULL,
             width = NULL) {
      selectizeIt(inputId,
                  selectInput(inputId, ..., selectize = FALSE, width = width),
                  options)
    }
  
  # given a select input and its id, selectize it
  selectizeIt <-
    function(inputId, select, options, nonempty = FALSE) {
      res <- checkAsIs(options)
      
      selectizeDep <- htmlDependency(
        "selectize",
        "0.11.2",
        c(href = "shared/selectize"),
        stylesheet = "css/selectize.bootstrap3.css",
        head = format(tagList(
          HTML('<!--[if lt IE 9]>'),
          tags$script(src = 'shared/selectize/js/es5-shim.min.js'),
          HTML('<![endif]-->'),
          tags$script(src = 'shared/selectize/js/selectize.min.js')
        ))
      )
      
      if ('drag_drop' %in% options$plugins) {
        selectizeDep <- list(
          selectizeDep,
          htmlDependency(
            'jqueryui',
            '1.11.4',
            c(href = 'shared/jqueryui'),
            script = 'jquery-ui.min.js'
          )
        )
      }
      
      # Insert script on same level as <select> tag
      #     select$children[[2]] <- tagAppendChild(
      #       select$children[[2]],
      select$children[[1]] <- tagAppendChild(
        select$children[[1]],
        tags$script(
          type = 'application/json',
          `data-for` = inputId,
          `data-nonempty` = if (nonempty)
            '',
          `data-eval` = if (length(res$eval))
            HTML(toJSON(res$eval)),
          if (length(res$options))
            HTML(toJSON(res$options))
          else
            '{}'
        )
      )
      
      attachDependencies(select, selectizeDep)
    }
  
  # for options passed to DataTables/Selectize/..., the options of the class AsIs
  # will be evaluated as literal JavaScript code
  checkAsIs <- function(options) {
    evalOptions <- if (length(options)) {
      nms <- names(options)
      if (length(nms) == 0L ||
          any(nms == ''))
        stop("'options' must be a named list")
      i <- unlist(lapply(options, function(x) {
        is.character(x) & inherits(x, 'AsIs')
      }))
      if (any(i)) {
        # must convert to character, otherwise toJSON() turns it to an array []
        options[i] <- lapply(options[i], paste, collapse = '\n')
        nms[i]  # options of these names will be evaluated in JS
      }
    }
    list(options = options, eval = evalOptions)
  }
  
  #   #==============================================================================================================================================
  #   ##
  #   firstChoice <- function(choices) {
  #     if (length(choices) == 0L) return()
  #     choice <- choices[[1]]
  #     if (is.list(choice)) firstChoice(choice) else choice
  #   }
  #
  #
  #   # Create tags for each of the options; use <optgroup> if necessary.
  #   # This returns a HTML string instead of tags, because of the 'selected'
  #   # attribute.
  #   selectOptions <- function(choices, selected = NULL) {
  #     html <- mapply(choices, names(choices), FUN = function(choice, label) {
  #       if (is.list(choice)) {
  #         # If sub-list, create an optgroup and recurse into the sublist
  #         sprintf(
  #           '<optgroup label="%s">\n%s\n</optgroup>',
  #           htmlEscape(label),
  #           selectOptions(choice, selected)
  #         )
  #
  #       } else {
  #         # If single item, just return option string
  #         sprintf(
  #           '<option value="%s"%s>%s</option>',
  #           htmlEscape(choice),
  #           if (choice %in% selected) ' selected' else '',
  #           htmlEscape(label)
  #         )
  #       }
  #     })
  #
  #     HTML(paste(html, collapse = '\n'))
  #   }
  #
  #   # need <optgroup> when choices contains sub-lists
  #   needOptgroup <- function(choices) {
  #     any(vapply(choices, is.list, logical(1)))
  #   }
  #
  #
  #   selectizeInput <- function(inputId, ..., options = NULL, width = NULL) {
  #     selectizeIt(inputId, selectInput(inputId, ..., selectize = FALSE), options, width)
  #   }
  #
  #   # given a select input and its id, selectize it
  #   selectizeIt <- function(inputId, select, options, width = NULL, nonempty = FALSE) {
  #     res <- shiny:::checkAsIs(options)
  #
  #     selectizeDep <- htmlDependency(
  #       "selectize", "0.11.2", c(href = "shared/selectize"),
  #       stylesheet = "css/selectize.bootstrap2.css",
  #       head = format(tagList(
  #         HTML('<!--[if lt IE 9]>'),
  #         tags$script(src = 'shared/selectize/js/es5-shim.min.js'),
  #         HTML('<![endif]-->'),
  #         tags$script(src = 'shared/selectize/js/selectize.min.js')
  #       ))
  #     )
  
  #     # Insert script on same level as <select> tag
  #     #   select$children[[1]] <- tagAppendChild(
  #     #     select$children[[1]],
  #     select$children[[1]] <- tagAppendChild(
  #       select$children[[1]],
  #       tags$script(
  #         type = 'application/json',
  #         `data-for` = inputId, `data-nonempty` = if (nonempty) '',
  #         `data-eval` = if (length(res$eval)) HTML(toJSON(res$eval)),
  #         `data-width` = validateCssUnit(width),
  #         if (length(res$options)) HTML(toJSON(res$options)) else '{}'
  #       )
  #     )
  #
  #     attachDependencies(select, selectizeDep)
  #   }
  
  
  #==============================================================================================================================================
  ##
  select2InputWithLabel <-
    function(inputId,
             label,
             choices = NULL,
             selected = NULL,
             type = c("input", "select"),
             drag.and.drop = FALSE,
             ...) {
      type <- match.arg(type)
      tags.choices <- wrap(paste0(choices, collapse = '","'))
      if (type == "input") {
        tagList(
          shinysky:::includeSelect2(),
          tags$p(label),
          tags$input(
            id = inputId,
            value = paste0(selected, collapse = ","),
            class = "shinysky-select2Input",
            ...
          ),
          tags$script(
            sprintf(
              "$(function() { $('#%s').select2({width:'resolve',tags:[%s]})})",
              inputId,
              tags.choices
            )
          ),
          tags$script(
            sprintf(
              '$(function() {
              $("#%s").select2("container").find("ul.select2-choices").sortable({
              containment: "parent",
              start: function() { $("#%s").select2("onSortStart"); },
              update: function() { $("#%s").select2("onSortEnd"); }
              });})',
inputId,
inputId,
inputId
          )
)
)
} else if (type == "select") {
  tagList(
    shinysky:::includeSelect2(),
    selectInput(inputId, label, choices, selected, ...),
    tags$script(
      sprintf(
        "$(function() { $('#%s').select2({width:'resolve'})})",
        inputId
      )
    )
  )
}
      }
  
  #==============================================================================================================================================
  ##
  select2InputWithoutLabel <-
    function(inputId,
             choices = NULL,
             selected = NULL,
             type = c("input", "select"),
             drag.and.drop = FALSE,
             ...) {
      type <- match.arg(type)
      tags.choices <- wrap(paste0(choices, collapse = '","'))
      if (type == "input") {
        tagList(
          shinysky:::includeSelect2(),
          tags$input(
            id = inputId,
            value = paste0(selected, collapse = ","),
            class = "shinysky-select2Input",
            ...
          ),
          tags$script(
            sprintf(
              "$(function() { $('#%s').select2({width:'resolve',tags:[%s]})})",
              inputId,
              tags.choices
            )
          ),
          tags$script(
            sprintf(
              '$(function() {
              $("#%s").select2("container").find("ul.select2-choices").sortable({
              containment: "parent",
              start: function() { $("#%s").select2("onSortStart"); },
              update: function() { $("#%s").select2("onSortEnd"); }
              });})',
inputId,
inputId,
inputId
          )
            )
          )
} else if (type == "select") {
  tagList(
    shinysky:::includeSelect2(),
    selectInputWithoutLabel(inputId, choices, selected, ...),
    tags$script(
      sprintf(
        "$(function() { $('#%s').select2({width:'resolve'})})",
        inputId
      )
    )
    #tags$script(sprintf("$(function() { $('#%s').select2({width:200px})})",inputId))
  )
}
      }
  
  
  #==============================================================================================================================================
  ##
  wrap <- function(str, with = '"') {
    paste0(with, str, with)
  }
  
  
  #==============================================================================================================================================
  ##
  updateSelect2Input <-
    function(session ,
             inputId,
             label,
             choices = NULL,
             selected = NULL) {
      session$sendCustomMessage(type = "updateShinySkySelect2",
                                list(
                                  id = inputId,
                                  choices = choices,
                                  selected = selected,
                                  label = label
                                ))
    }
  
  
  
  #==============================================================================================================================================
  ##
  actionButton2 <-
    function(inputId,
             label,
             btn.style = "" ,
             class = "",
             icon = NULL) {
      if (!is.null(icon))
        buttonContent <- list(icon, label)
      else
        buttonContent <- label
      
      if (btn.style %in% c("primary",
                           "info",
                           "success",
                           "warning",
                           "danger",
                           "inverse",
                           "link")) {
        btn.css.class <- paste("btn", btn.style, sep = "-")
      } else
        btn.css.class = ""
      
      tags$button(
        id = inputId,
        type = "button",
        class = paste("btn action-button", btn.css.class, class, collapse =
                        " "),
        buttonContent
      )
    }
  
  
  #==============================================================================================================================================
  ##
  ppprint <- function(message) {
    print(message)
  }
  
  # observeEvent(input$distanceAnalysis, {
  #   if(!is.null(input$distanceAnalysis) & input$distanceAnalysis == "FALSE")
  #     shinyjs::disable("nbrBreakClasses");
  # })
  
  #==============================================================================================================================================
  ##
  output$nbrBreakClassesSection <- renderUI({
    if (!is.null(input$distanceAnalysis) &
        input$distanceAnalysis == "FALSE") {
      #shinyjs::disable(numericInput("nbrBreakClasses","",2,min = 2,max = 2))
      numericInput("nbrBreakClasses", "", 2, min = 2, max = 2)
    }
    else
    {
      numericInput("nbrBreakClasses", "", 5, min = 1, max = 24)
    }
    
  })
  
  
  #==============================================================================================================================================
  ##
  output$breaksNote <- renderUI({
    if (!is.null(input$distanceAnalysis) &
        input$distanceAnalysis == "FALSE") {
      message <-
        ifelse(
          input$language == "fr",
          "Déterminer les limites du transect",
          "Determine transect limits"
        )
    } else {
      message <-
        ifelse(
          input$language == "fr",
          "Limites de distance entre les classes utilisées. Les valeurs par défaut
          sont pour les classes de distance 0-50m, 50-100m, 100-200m, et 200-300m.",
          "Enter the distance breaks between distance bins.
          The default values are for bins 0-50m, 50-100m, 100-200m, and 200-300m."
        )
    }
    
    HTML(message)
    })
  
  #==============================================================================================================================================
  ##
  output$breakClasses <- renderUI({
    if (isInvalid(input$nbrBreakClasses)) {
      return(invisible())
    }
    
    nbr.break.classes <- as.numeric(input$nbrBreakClasses)
    
    if (!is.null(input$distanceAnalysis) &
        input$distanceAnalysis == "FALSE") {
      # it is important as some browsers do not lock the input field to the maximum value
      nbr.break.classes <- 2
    }
    
    if (nbr.break.classes == 2)
      breaks.default <- c(0, 300)
    else
      breaks.default <- c(0, 50, 100, 200, 300)
    
    breakInputs <- lapply(1:nbr.break.classes, function(i) {
      break.i.name <- paste("breakInput_", i, sep = "")
      
      i.modulo <- (i %% nbr.break.classes)
      
      
      breakInputTag <- div(class = "row",
                           div(
                             class = "col-sd-5 col-md-5",
                             numericInput(
                               break.i.name,
                               label = ifelse(
                                 input$language == "fr",
                                 encode(paste0("Limite #", i)),
                                 paste0("Break #", i)
                               ),
                               breaks.default[ifelse(i.modulo ==
                                                       0, nbr.break.classes, i.modulo)]
                             )
                           ))
      
      list(breakInputTag, br())
    })
    
    return(do.call(tagList, breakInputs))
    
  })
  
  
  #==============================================================================================================================================
  ##
  breakClasses <- reactive({
    if (is.null(input$nbrBreakClasses)) {
      return(NULL)
    }
    
    nbr.break.classes <- as.numeric(input$nbrBreakClasses)
    
    if (!is.null(input$distanceAnalysis) &
        input$distanceAnalysis == "FALSE") {
      # it is important as some browsers do not lock the input field to the maximum value
      nbr.break.classes <- 2
    }
    
    if (is.null(nbr.break.classes) ||
        nbr.break.classes < 1) {
      return(NULL)
    }
    
    breaks <- c()
    j <- 1
    
    #cat('nbr.break.classes = ', nbr.break.classes,  '\n')
    
    for (i in 1:nbr.break.classes) {
      break.input.i     <- input[[paste("breakInput_", i, sep = "")]]
      break.input.i.nbr <- as.numeric(break.input.i)
      
      if (!is.null(break.input.i.nbr)) {
        breaks[j] <- break.input.i.nbr
        j <- j + 1
      }
      
    }
    
    breaks
    #cat('breaks = ', breaks,  '\n')
    
    return(breaks)
    
  })
  
  
  #==============================================================================================================================================
  ##
  sendLoadingMessageToClient <-
    function(type = 'loadingMessage',
             text = "Loading",
             action = 'start',
             langage = "fr") {
      session$sendCustomMessage(type = type,
                                message = list(text = text,
                                               action = action))
    }
  
  
  #==============================================================================================================================================
  ##
  density2.map  <- function(resumePlotID = NULL,
                            PDF = F) {
    input$lsubnbrvars
    
    if (input$classificationButton == 0)
      return()
    
    if (!(input$stratification %in% c("stratzone", "stratgrille")))
      return(invisible())
    
    densities   <- densitiesData(resumePlotID)
    gridBrkList <- shapefileAndData(densities)
    
    if (is.null(gridBrkList)) {
      return(NULL)
    }
    
    new.grid2  <- gridBrkList$grid
    brks       <- gridBrkList$brks
    tags       <- gridBrkList$tags
    shpm       <- shpmData()
    
    isolate({
      couleurs <- getColors()
      species  <- selectedSepecieAllOrRare()
      specie   <-
        ifelse(resumePlotID == 0, species[1], species[resumePlotID])
      
      if (allSpecies()) {
        image.name  <-
          paste("dentisities", ifelse(PDF, ".pdf", ".png"), sep = "")
        working.dir <- paste0(rootDirReactive(), "/output/all")
      } else{
        image.name  <-
          paste0("densities-", specie, ifelse(PDF, ".pdf", ".png"))
        working.dir <-
          paste0(
            rootDirReactive(),
            "/output/",
            ifelse(resumePlotID == 0, "rare-base-model", specie)
          )
      }
      
      if (!is.null(input$distanceAnalysis) &
          input$distanceAnalysis == "FALSE") {
        titre <-
          ifelse(input$language == "fr",
                 "Densités non corrigées" ,
                 "Uncorrected densities")
      }
      else
      {
        titre <-
          ifelse(input$language == "fr",
                 "Densités corrigées" ,
                 "Corrected densities")
      }
      
      legendtitle   <-
        ifelse(input$language == "fr", "oiseaux/km2", "birds/km2")
      
      if (input$stratification == "stratgrille") {
        stratgrille <- TRUE
      } else{
        if (input$stratification == "stratzone" &
            !is.null(input$fileShapeFormes)) {
          stratgrille <- FALSE
        } else{
          return(NULL)
        }
      }
      
      dentisity.map.2(
        densities = densities,
        new.grid2 = new.grid2,
        brks = brks,
        tags = tags,
        shpm = shpm,
        couleurs = c("green", "yellow", "red"),
        stratgrille = stratgrille,
        specie = specie,
        titre = titre,
        legendtitle = legendtitle,
        PDF = PDF,
        working.dir = working.dir,
        # absolute path
        output.file.name = image.name,
        all.species = allSpecies()
      )
      
      setwd("../..")
      setwd(rootDirReactive())
      
      dir.name <- ifelse(
        allSpecies(),
        paste0(rootDirReactive(), "/output/all"),
        paste0(
          rootDirReactive(),
          "/output/",
          ifelse(resumePlotID == 0, "rare-base-model", specie)
        )
      )
      
      path.to.image       <- paste(dir.name, image.name, sep = "/")
      files.to.be.deleted <-
        list.files(path = dir.name, pattern = "densite")
      
      if (length(files.to.be.deleted) != 0)
        do.call(file.remove, c(lapply(files.to.be.deleted, function(x)
          paste(dir.name, x, sep = "/"))))
      
      archive.name <-
        saveToFileGridShapeFileDensities(dir = dir.name, specie, new.grid2)
      write.table(
        densitiesData(resumePlotID),
        file = paste(dir.name, "densites.csv", sep = "/") ,
        sep = ";",
        row.names = FALSE,
        na = "",
        col.names = TRUE
      )
      zip(
        paste(dir.name, archive.name, sep = "/"),
        files = list(path.to.image, paste(dir.name, "densites.csv", sep = "/")) ,
        flags = "-j"
      )
      archive.name <- paste(dir.name, archive.name, sep = "/")
      
      return(list(archive.name = archive.name, path.to.image = path.to.image))
    })
  }
  
  
  #==============================================================================================================================================
  ##
  generateAndsaveToFileShapeFileDensities <-
    function(resumePlotID = NULL) {
      if (!(input$stratification %in% c("stratzone", "stratgrille")))
        return(NULL)
      
      densities           <- densitiesData(resumePlotID)
      list.with.new.grid2 <- shapefileAndData(densities)
      
      if (is.null(list.with.new.grid2))
        return(NULL)
      
      species <- selectedSepecieAllOrRare()
      specie <-
        ifelse(resumePlotID == 0, species[1], species[resumePlotID])
      
      dir <- ifelse(
        allSpecies(),
        paste0(rootDirReactive(), "/output/all"),
        paste0(
          rootDirReactive(),
          "/output/",
          ifelse(resumePlotID == 0, "rare-base-model", specie)
        )
      )
      
      archive.name <-
        saveToFileGridShapeFileDensities(dir = dir, specie, list.with.new.grid2$grid)
      
      return(archive.name)
    }
  
  #==============================================================================================================================================
  ##
  output$classesSection <- renderUI({
    if (isInvalid(input$stratification) ||
        input$stratification != "stratgrille")
      return("")
    
    tmp.html.tag <-
      "<div class='row'> <div class='col-md-12'> <label for='nbrClasses'><b>"
    tmp.html.tag <- paste0(
      tmp.html.tag,
      ifelse(
        input$language == "fr",
        "Nombre de classes désirées au-del&agrave; de la classe &#x00ab; sans oiseaux &#x00bb;. Les classes seront d&eacute;finies &agrave; l&#x2019;aide de quantiles selon les param&egrave;tres ci-dessous :",
        "Desired number of classes beyond the 'no bird' class. The classes will be defined using quantiles with the settings below:"
      )
    )
    
    tmp.html.tag <- paste0(
      tmp.html.tag,
      "</b></label> <input id='nbrClasses' type='number' min='1' step='1' value='4' class='shiny-bound-input'></div></div><div class='row'><label><b>"
    )
    
    tmp.html.tag <- paste0(
      tmp.html.tag,
      ifelse(
        input$language == "fr",
        "Valeurs des limites des quantiles pour la classification cartographique, doit &ecirc;tre entre 0 et 1, exclusivement. Une classe &#x00ab; 0 &#x00bb; o&ugrave; aucune observation n&#x2019;a &eacute;t&eacute; faite sera cr&eacute;&eacute;e automatiquement :",
        "Values of quantile limits for the density classification, must be between 0 and 1, exclusively. A '0' class where no observations were made will be created automatically:"
      )
    )
    
    tmp.html.tag <- paste0(
      tmp.html.tag,
      "</b></label><div class='col-md-10 well well-darker'><div class='alert alert-info'><button type='button' class='close' data-dismiss='alert'>×</button><strong>Note: </strong>"
    )
    
    tmp.html.tag <- paste0(
      tmp.html.tag,
      ifelse(
        input$language == "fr",
        "D&eacute;terminer les limites inf&eacute;rieures des classes en inscrivant les quantiles d&eacute;sir&eacute;s. La premi&egrave;re classe doit &ecirc;tre &#x00ab; 0 &#x00bb; pour visualiser les zones visit&eacute;es sans oiseaux. La limite sup&eacute;rieure de la derni&egrave;re classe serait la valeur du quantile 0.995 du jeu de donn&eacute;es. Par exemple, pour obtenir les cinq classes de quantiles suivantes : 0, 0-0.5, 0.5-0.75, 0.75-0.95, 0.95 et plus, vous devez entrer les quatre nombres suivants : 0, 0.5, 0.75, et 0.95.",
        "Determine the lower class limits by entering the desired quantiles. The first class should be '0' to visualize the visited areas without birds. The upper limit of the last class will be the 0.995 quantile of the dataset. For example, to obtain the following five classes of quantiles: 0, 0-0.5, 0.5-0.75, 0.75-0.95, and 0.95, you must enter the following four numbers: 0, 0.5, 0.75, and 0.95."
      )
    )
    
    tmp.html.tag <-
      paste0(
        tmp.html.tag,
        "</div><div id='densitesClasses' class='shiny-html-output' name='densitesClasses' style='width: 100%;'></div></div></div>"
      )
    
    HTML(tmp.html.tag)
  })
  
  
  #==============================================================================================================================================
  ##
  xtable.html.to.string <- function(table) {
    return(
      print(
        table,
        type = "html",
        caption.placement = getOption('xtable.caption.placement', 'top'),
        html.table.attributes = 'class="table table-striped"',
        print.results = FALSE
      )
    )
  }
  
  #==============================================================================================================================================
  ##
  allSpecies <- function() {
    if (input$lsubnbrvars == "All/Tout") {
      return(TRUE)
    }
    return(FALSE)
  }
  
  
  #==============================================================================================================================================
  ##
  output$date <- renderText({
    current.date <- Sys.Date()
    today <-  format(current.date, format = "%d - %m -  %Y")
  })
  
  #==============================================================================================================================================
  ##
  output$covariatesSelectedValues <- renderUI({
    outtag <- selectInputWithoutLabel(
      inputId = "covariates",
      choices = as.list(dataset.numerical.names()),
      #selected = "NULL",
      multiple = TRUE,
      selectize = TRUE
    )
    
  })
  
  
  #==============================================================================================================================================
  ##
  output$sectionForFactorSelectedValues <- renderUI({
    if (!is.null(input$distanceAnalysis) &
        input$distanceAnalysis == "FALSE")
      return(invisible())
    
    htmlTag.prefixe <-
      " <label class='control-label col-sd-3 col-md-3' for='factorid'>
    <a href='#' class='tooltips tooltipsgen' title='' data-original-title='"
    
    htmlTag.sufixe <-
      "'>Factor :</a> </label> <div class='col-sd-2 col-md-2'>" # <div class='controls col-md-9'>"
    
    htmlTag.data <-
      ifelse(
        input$language == "fr",
        "Choisir les variables catégoriques à inclure dans le processus de sélection de la fonction de détection.",
        "Select the categorical variables that you want to include in the model selection process of the detection function."
      )
    
    htmlTag.select <- selectInputWithoutLabel(
      inputId = "factor",
      choices = as.list(dataset.character.names()),
      multiple = TRUE,
      selectize = TRUE
    )
    
    htmlTag.close <- "</div> </div>"
    
    HTML(paste(
      paste0(
        htmlTag.prefixe,
        htmlTag.data,
        htmlTag.sufixe,
        htmlTag.select,
        htmlTag.close
      ),
      collapse = "\n"
    ))
  })
  
  
  #==============================================================================================================================================
  #
  covariatesInput <- reactive({
    if (isInvalid(input$covariates))
      return(invisible())
    
    a <- input$covariates  #a <-  NULL
    
    if (a == "NULL")
      return(NULL)
    
    return(c(a))
  })
  
  
  #==============================================================================================================================================
  #
  factorInput <- reactive({
    if (isInvalid(input$factorid))
      return(invisible())
    
    a <- input$factor  #a <-  NULL
    
    if (a == "NULL")
      return(NULL)
    
    return(c(a))
  })
  
  
  #==============================================================================================================================================
  ##
  output$rareSection <- renderUI({
    if (!is.null(input$distanceAnalysis) &
        input$distanceAnalysis == "FALSE")
      return(invisible())
    
    
    htmlTag.prefixe <-
      "<label class='control-label col-sm-3 col-md-3'>
    <a href='#' class='tooltips tooltipsgen' title='' data-original-title='"
    
    htmlTag.sufixe <- "'>Rare :</a>
    </label>
    <div class='shiny-html-output  col-sm-2 col-md-2' id='rareValues'>
    </div>"
    
    htmlTag.data <-
      ifelse(
        input$language == "fr",
        "Il est généralement suggéré d’avoir = 60-80 détections d’une espèce dans votre zones d’étude pour obtenir des résultats valides. Dans les cas où le nombre de détections semble insuffisant, vous pouvez utiliser cette option pour déclarer l’espèce « rare » et sa probabilité de détection sera basée sur les détections de l’ensemble des espèces déclarées au champ précédent : sous-groupes/catégories",
        "It is generally suggested to have >= 60-80 detections to obtain valid results. If you don't have enough detections for a given species, you can use this option to identify a rare species for which the detection probability will be based on those selected in the Categories field above."
      )
    
    HTML(paste(
      paste0(htmlTag.prefixe, htmlTag.data, htmlTag.sufixe),
      collapse = "\n"
    )) #HTML(paste0(htmlTag.prefixe,htmlTag.data, htmlTag.sufixe ))
    
  })
  
  #==============================================================================================================================================
  ##
  output$covariatesSection <- renderUI({
    if (!is.null(input$distanceAnalysis) &
        input$distanceAnalysis == "FALSE")
      return(invisible())
    
    htmlTag.prefixe <-
      "<label class='control-label col-sd-3 col-md-3' for='covariates'>
    <a href='#' class='tooltips tooltipsgen' title='' data-original-title='"
    
    htmlTag.sufixe <- "'>Covariables :</a>
    </label>
    
    <div class='col-sd-2 col-md-2 shiny-html-output'  id='covariatesSelectedValues'>
    <!--input class='col-md-2 shiny-bound-input' name='covariates' type='text' value='NULL'-->
    </div>
    "
    
    htmlTag.data <-
      ifelse(
        input$language == "fr",
        "Choisir les variables numériques à inclure dans le processus de sélection de la fonction de détection.",
        "Select the numerical variables that you want to include in the model selection process of the detection function."
      )
    
    HTML(paste(
      paste0(htmlTag.prefixe, htmlTag.data, htmlTag.sufixe),
      collapse = "\n"
    ))
    
  })
  
  #==============================================================================================================================================
  ##
  output$estimatorSection <- renderUI({
    if (!is.null(input$distanceAnalysis) &
        input$distanceAnalysis == "FALSE")
      return(invisible())
    
    htmlTag.prefixe <- "<div class='control-group'>
    <label class='control-label col-md-3'>
    <a href='#' class='tooltips tooltipsgen' title='' data-original-title='"
    
    htmlTag.sufixe <- "'>Estimator :</a>
    </label>
    <div class='controls'>
    <div class='raw-float'>
    <div id='selectedEstimators' class='col-md-10 shiny-input-checkboxgroup shiny-input-container'>
    "
    
    htmlTag.end <- "</div></div></div></div>"
    
    htmlTag.options <- ""
    
    estimators <-
      c("UN-CO", "UN-PO", "HN-CO", "HN-HE", "HA-CO", "HA-PO")
    
    for (i in 1:6) {
      htmlTag.options <-
        paste0(
          htmlTag.options,
          " <div class='checkbox'> <label>
          <input type='checkbox' name='selectedEstimators' checked='checked' id='selectedEstimators",
          i,
          "' value='",
          estimators[i],
          "' />
          <col-md->",
          estimators[i],
          "</col-md-></label></div>"
        )
    }
    
    
    htmlTag.data <-
      ifelse(
        input$language == "fr",
        "Choisir les fonctions et expansions parmi lesquelles le meilleur modèle sera choisi par AIC.",
        "Select key functions and expansion terms from which the best model will be selected by AIC."
      )
    
    HTML(paste(
      paste0(
        htmlTag.prefixe,
        htmlTag.data,
        htmlTag.sufixe ,
        htmlTag.options,
        htmlTag.end
      ),
      collapse = "\n"
    ))
    
  })
  
  #   observe({
  #     input$selectedEstimators
  #     print("input$selectedEstimators = ")
  #     print(input$selectedEstimators)
  #
  #     print("class(input$selectedEstimators) = ")
  #     print(class(input$selectedEstimators))
  #
  #   })
  
  
  #==============================================================================================================================================
  ##
  output$detectionSection <- renderUI({
    if (!is.null(input$distanceAnalysis) &
        input$distanceAnalysis == "FALSE")
      return(invisible())
    
    htmlTag.prefixe <-
      "<label class='control-label col-sd-3 col-md-3' for='detection'>
    <a href='#' class='tooltips tooltipsgen' title='' data-original-title='"
    
    htmlTag.sufixe <-
      "'>Detection :</a></label> <div class='col-sd-2 col-md-2'>"
    
    htmlTag.data <-
      ifelse(
        input$language == "fr",
        "La fonction de détection est calculée à l’échelle globale de la zone d’étude par défaut. Si vous désirez, elle peut être recalculée pour chaque strate en sélectionnant Stratum dans ce champ",
        "Select key functions and expansion terms from which the best model will be selected by AIC."
      )
    
    selectTag <- selectInputWithoutLabel(
      inputId = "detection",
      choices = list("All", "Stratum"),
      selected = "All",
      multiple = FALSE,
      selectize = TRUE
    )
    
    HTML(paste(
      paste0(
        htmlTag.prefixe,
        htmlTag.data,
        htmlTag.sufixe,
        selectTag,
        "</div>"
      ),
      collapse = "\n"
    ))
    
  })
  
  script <- "
  //$('a').tooltip();
  $('.tooltipsgen').each(function() {
  $(this).tooltip();
  })
  "
  
  session$onFlushed(function() {
    session$sendCustomMessage(type = 'jsCode', message = list(value = script))
  }, once = FALSE)
  
}
