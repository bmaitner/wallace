poccs_NSR_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("goNSR"), "Run module NSR"),
    checkboxInput(ns("batchNSR"), label = strong("Batch"), value = FALSE),
    actionButton(ns("goTossNonnatives"), "Exclude Introduced Records"),
    actionButton(ns("goTossCrap"), "Exclude Unresolved Records"),
    actionButton(ns("goTossNatives"), "Exclude Native Records"),

  )
}

########################################################

poccs_NSR_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  curSp <- common$curSp
  allSp <- common$allSp
  occs <- common$occs



  observeEvent(input$goNSR, {

    req(curSp(), occs(),allSp())

    # loop over all species if batch is on
    if (input$batchNSR == TRUE) spLoop <- allSp() else spLoop <- curSp()
    ############




      for (sp in spLoop) {

        withProgress(message = paste0("Processing native status of ", spName(sp), "..."), {

        #Log batch setting for debugging
        req(sp)

        # FUNCTION CALL ####
        occs_NSR <- poccs_NSR(occs = spp[[sp]]$occs,spN = sp)

        req(occs_NSR)

        # LOAD INTO SPP ####
        # record present occs before thinning (this may be different from occData$occOrig)
        spp[[sp]]$occs <- occs_NSR

        spp[[sp]]$procOccs$NSRresults <- occs_NSR

        # METADATA ####
        # perhaps there should be a metadata field?

        })#progress thing
      }





    common$update_component(tab = "Map")
  }  ) #go nsr


  observeEvent(input$goTossNatives, {

    req(curSp(), occs(),allSp())

    # loop over all species if batch is on
    if (input$batchNSR == TRUE) spLoop <- allSp() else spLoop <- curSp()
    ############

    for (sp in spLoop) {

      #Log batch setting for debugging

      # FUNCTION CALL ####
      occs_NSR <- spp[[sp]]$occs

      #Check whether the NSR was run
      if (is.null(occs_NSR$isIntroduced)){
        logger %>% writeLog(type = 'error', 'NSR has not been run')
        return()
        }

      #Check whether you'll be left with anything
      if (nrow(occs_NSR[which(occs_NSR$isIntroduced != 0),]) < 1){
        logger %>% writeLog(type = 'error', 'All points will be removed.')
        return()}


      #Toss natives
      occs_NSR <- occs_NSR[which(occs_NSR$isIntroduced!=0),]

      # LOAD INTO SPP ####
      # record present occs before thinning (this may be different from occData$occOrig)
      spp[[sp]]$occs <- occs_NSR

      # METADATA ####
      # perhaps there should be a thinDist metadata field?
    }
    common$update_component(tab = "Map")
  }  ) #go toss natives



  observeEvent(input$goTossNonnatives, {

    req(curSp(), occs(),allSp())

    # loop over all species if batch is on
    if (input$batchNSR == TRUE) spLoop <- allSp() else spLoop <- curSp()
    ############

    for (sp in spLoop) {

      #Log batch setting for debugging

      # FUNCTION CALL ####
      occs_NSR <- spp[[sp]]$occs

      #Check whether the NSR was run
      if (is.null(occs_NSR$isIntroduced)){
        logger %>% writeLog(type = 'error', 'NSR has not been run')
        return()}

      #Check whether you'll be left with anything
      if (nrow(occs_NSR[which(occs_NSR$isIntroduced != 1),] ) < 1){
        logger %>% writeLog(type = 'error', 'All points will be removed.')
        return()}

      #Toss natives
      occs_NSR <- occs_NSR[which(occs_NSR$isIntroduced!=1),]

      # LOAD INTO SPP ####
      spp[[sp]]$occs <- occs_NSR

      # METADATA ####
      # perhaps there should be a thinDist metadata field?
    }
    common$update_component(tab = "Map")
  }  ) #go toss nonnatives


  observeEvent(input$goTossCrap, {

    req(curSp(), occs(),allSp())

    # loop over all species if batch is on
    if (input$batchNSR == TRUE) spLoop <- allSp() else spLoop <- curSp()
    ############

    for (sp in spLoop) {

      #Log batch setting for debugging

      # FUNCTION CALL ####
      occs_NSR <- spp[[sp]]$occs

      #Check whether the NSR was run
      if (is.null(occs_NSR$isIntroduced)){
        logger %>% writeLog(type = 'error', 'NSR has not been run')
        return()}

      #Check whether you'll be left with anything
      if (nrow(occs_NSR[which(!is.na(occs_NSR$isIntroduced)),] )< 1){
        logger %>% writeLog(type = 'error', 'All points will be removed.')
        return()}

      #Toss natives
      occs_NSR <- occs_NSR[which(!is.na(occs_NSR$isIntroduced)),]

      # LOAD INTO SPP ####
      spp[[sp]]$occs <- occs_NSR

      # METADATA ####
      # perhaps there should be a thinDist metadata field?
    }
    common$update_component(tab = "Map")
  }  ) #go toss nonnatives




}





##########################################################

poccs_NSR_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

#############################################################

poccs_NSR_module_map <- function(map, common) {
  # Map logic

  curSp <- common$curSp
  spp <- common$spp
  req(spp[[curSp()]]$occs)
  occs <- common$occs

  #color code points native vs introduced
  if (!is.null(spp[[curSp()]]$occs$isIntroduced)) {

    map %>% clearAll() %>%
      addCircleMarkers(data = spp[[curSp()]]$occs[which(spp[[curSp()]]$occs$isIntroduced==0),], lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'blue', fill = TRUE, fillColor = "blue",
                       fillOpacity = 1, weight = 2, popup = ~pop) %>%
      addCircleMarkers(data = spp[[curSp()]]$occs[which(spp[[curSp()]]$occs$isIntroduced==1),], lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 1, weight = 2, popup = ~pop) %>%
      addCircleMarkers(data = spp[[curSp()]]$occs[which(is.na(spp[[curSp()]]$occs$isIntroduced)),], lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'brown', fill = TRUE, fillColor = "brown",
                       fillOpacity = 1, weight = 2, popup = ~pop) %>%

      zoom2Occs(occs()) %>%
      addLegend("bottomright", colors = c('red', 'blue','brown'), title = "Occ Records",
                labels = c('introduced', 'native','unresolved'), opacity = 1)
  } else {
    # if you haven't run NSR, map all points red
    map %>% clearAll() %>%
      addCircleMarkers(data = spp[[curSp()]]$occs, lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
      zoom2Occs(occs()) %>%
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
  }




}




###############################################################

poccs_NSR_module_rmd <- function(species) {
  # Variables used in the module's Rmd code
  list(
    NSR_knit = species$rmm$code$wallace$someFlag,
    var1 = species$rmm$code$wallace$someSetting1,
    var2 = species$rmm$code$wallace$someSetting2
  )
}

