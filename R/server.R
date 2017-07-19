shinyServer(function(input, output) {
  
  lengthData <- reactive({
    if(is.null(input$dataType)) return(NULL)
    inFile <- input$data
    #
    if (is.null(inFile) & input$dataType != "Use sample Data")
      return(NULL)

    if (input$dataType == "Use sample Data") df = df_length else df = read_csv(inFile$datapath)
    if (input$sizeMax != -999) df <- df %>% filter(length_cm<input$sizeMax)
    if (input$insideArea == TRUE) df <- df %>% filter(inside_area == "Inside")
    df
  })
  
  catchData <- reactive({
    if(is.null(input$dataType)) return(NULL)
    inFile <- input$dataLandings
    
    if (is.null(inFile) & input$dataType != "Use sample Data")
      return(NULL)
    
    if (input$dataType == "Use sample Data") df = df_catch else df = read_csv(inFile$datapath)
    if (input$sizeMax != -999 & "length_cm" %in% colnames(df)) df <- df %>% filter(length_cm<input$sizeMax)
    if (input$insideArea == TRUE) df <- df %>% filter(inside_area == "Inside")
    df
    
  })
  
  biomassData <- reactive({
    if(is.null(input$dataType)) return(NULL)
    inFile <- input$dataBiomass
    #
    if (is.null(inFile) & input$dataType != "Use sample Data")
      return(NULL)
    
    if (input$dataType == "Use sample Data") df = df_biomass else df = read_csv(inFile$datapath)

    df
  })
  
  densityData <- reactive({
    if(is.null(input$dataType)) return(NULL)
    inFile <- input$dataDensity
    #
    if (is.null(inFile) & input$dataType != "Use sample Data")
      return(NULL)
    
    if (input$dataType == "Use sample Data") df = df_density else df = read_csv(inFile$datapath)
    
    
    df
  })
  

  lhiData <- reactive({
    if(is.null(input$countrySelection) | is.null(input$speciesSelection)) return(NULL)
    lhiData <- lhi_database %>%
      filter(Country == input$countrySelection &
             Species == input$speciesSelection)
    lhiData
  })
  
  metaDataReactive <- reactive({
    if(is.null(input$countrySelection) | is.null(input$speciesSelection)) return(NULL)
    metaDataReactive <- metadata %>%
      filter(Country == input$countrySelection &
               Species == input$speciesSelection)
    metaDataReactive
  })
  
  output$speciesUIText <- renderUI({
    if (nrow(lhiData()) == 0) valueDefault <- NA else valueDefault <- lhiData()$Species
    
    textInput("Code", label = "Scientific Name", value = valueDefault)
  })
  
  output$commonUIText <- renderUI({
    if (nrow(lhiData()) == 0) valueDefault <- NA else valueDefault <- lhiData()$Common
    
    textInput("Common", label = "Common Name", value = valueDefault)
  })
  
  output$codeUI <- renderUI({
    if (nrow(lhiData()) == 0) valueDefault <- NA else valueDefault <- lhiData()$Code
    
    textInput("Code", label = "6-letter speies code", value = valueDefault)
  })
  
  output$linfUI <- renderUI({
        if (nrow(lhiData()) == 0) linfDefault <- NA else linfDefault <- lhiData()$L_inf
        numericInput("Linf", label = "L_Infinity (von Bertalannfy growth parameter)", value = linfDefault)
  })
  
  output$linfMetaUI <- renderUI({
    if (nrow(lhiData()) == 0) referenceDefault <- NA else referenceDefault <- metaDataReactive()$L_inf
    textInput("Linf_Reference",label="L_Infinity reference",value = referenceDefault)
  })
  
  output$kUI <- renderUI({
    if (nrow(lhiData()) == 0) kDefault <- NA else kDefault <- lhiData()$k
    numericInput("k", label = "k (von Bertalannfy growth parameter)", value = kDefault)
  })

  output$kMetaUI <- renderUI({
    if (nrow(lhiData()) == 0) referenceDefault <- NA else referenceDefault <- metaDataReactive()$k
    textInput("k_Reference",label="k reference",value = referenceDefault)
  })
  
  output$t0UI <- renderUI({
    if (nrow(lhiData()) == 0) t0Default <- NA else t0Default <- lhiData()$t0
    numericInput("t0", label = "t0 (von Bertalannfy growth parameter, theoretical age at length 0)", value = t0Default)
  })
  
  output$t0MetaUI <- renderUI({
    if (nrow(lhiData()) == 0) referenceDefault <- NA else referenceDefault <- metaDataReactive()$t0
    textInput("t0_Reference",label="t0 reference",value = referenceDefault)
  })
  
  output$mUI <- renderUI({
    if (nrow(lhiData()) == 0) mDefault <- NA else mDefault <- lhiData()$M
    numericInput("M", label = "M (Natural Mortality)", value = mDefault)
  })
  
  output$catchReference <- renderUI({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(input$yearLandingsSelection) | is.null(catchData())) return()
    df <- catchData()
    totalFN = function(x,y){
      if (is.na(y)) total = x else total = y
      return(total)
    }
    
    catchEffortData=df[,c("fisher_days","sampled_catch","total_catch")] = lapply(df[,c("fisher_days","sampled_catch","total_catch")],as.numeric,na.rm=TRUE)
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    #df$date <- as.Date(df$date,"%m/%d/%Y")
    #df$month <- as.Date(cut(df$date, breaks = "month"))
    dataSubset = subset(df,species==input$speciesSelection)
    
    
    
    dataSubset = dataSubset %>%
      group_by(year,permanent_trip_id) %>%
      summarize(catch = sum(sampled_catch,na.rm=TRUE),
                fisher_days = mean(fisher_days),
                CPUE = catch/fisher_days)
    
    if (input$tripMax != -999) dataSubset <- dataSubset %>% filter(catch < input$tripMax)
    
    dataSubset = dataSubset %>%  
      group_by(year) %>%
      mutate(sampling_days = length(unique(permanent_trip_id))) %>%
      summarize(catch = ifelse(input$totalCatch == TRUE, sum(catch,na.rm=TRUE),sum(catch,na.rm=TRUE)/sampling_days),
                CPUE = median(CPUE,na.rm=TRUE),
                fisher_days = ifelse(input$totalCatch == TRUE,sum(fisher_days,na.rm=TRUE),sum(fisher_days,na.rm=TRUE)/sampling_days)) %>%
      filter(catch>0 & CPUE > 0  & fisher_days > 0)
    
    dataSubset <- dataSubset %>%
      filter(year < input$yearLandingsSelection)
    
    yearList <- dataSubset$year
    sliderInput("catchReferenceSlider", label = ("Select the reference period over which the catch and/or CPUE reference values should be calculated. This time period should represent a desirable fishery condition. The catch and/or CPUE reference will be calculated as the average value over these years."), min = min(yearList), 
                max = max(yearList), value = c(min(yearList), max(yearList)),
                step = 1)
    
  })
  
  output$mUI <- renderUI({
    if (nrow(lhiData()) == 0) mDefault <- NA else mDefault <- lhiData()$M
    numericInput("M", label = "M (Natural Mortality)", value = mDefault)
  })
  
  output$mMetaUI <- renderUI({
    if (nrow(lhiData()) == 0) referenceDefault <- NA else referenceDefault <- metaDataReactive()$M
    textInput("M_Reference",label="M reference",value = referenceDefault)
  })
  
  output$waUI <- renderUI({
    if (nrow(lhiData()) == 0) waDefault <- NA else waDefault <- lhiData()$Wa
    numericInput("w_a", label = "w_a (length-weight relationship parameter a)", value = waDefault)
  })
  
  output$waMetaUI <- renderUI({
    if (nrow(lhiData()) == 0) referenceDefault <- NA else referenceDefault <- metaDataReactive()$Wa
    textInput("w_a_Reference",label="w_a reference",value = referenceDefault)
  })
  
  output$wbUI <- renderUI({
    if (nrow(lhiData()) == 0) wbDefault <- NA else wbDefault <- lhiData()$Wb
    numericInput("w_b", label = "w_b (length-weight relationship parameter b)", value = wbDefault)
  })
  
  output$wbMetaUI <- renderUI({
    if (nrow(lhiData()) == 0) referenceDefault <- NA else referenceDefault <- metaDataReactive()$Wb
    textInput("w_b_Reference",label="w_b reference",value = referenceDefault)
  })
  
  output$m50UI <- renderUI({
    if (nrow(lhiData()) == 0) m50Default <- NA else m50Default <- lhiData()$m50
    numericInput("m50", label = "m50 (Lengthat which 50% of individuals have reached maturity.)", value = m50Default)
  })
  
  output$m50MetaUI <- renderUI({
    if (nrow(lhiData()) == 0) referenceDefault <- NA else referenceDefault <- metaDataReactive()$m50
    textInput("m50_Reference",label="m50 reference",value = referenceDefault)
  })
  
  output$m95UI <- renderUI({
    if (nrow(lhiData()) == 0) m95Default <- NA else m95Default <- lhiData()$m95
    numericInput("m95", label = "m95 (Lengthat which 95% of individuals have reached maturity.)", value = m95Default)
  })
  
  output$m95MetaUI <- renderUI({
    if (nrow(lhiData()) == 0) referenceDefault <- NA else referenceDefault <- metaDataReactive()$m95
    textInput("m95_Reference",label="m95 reference",value = referenceDefault)
  })

  output$inputDataBiomass <- DT::renderDataTable(
    biomassData()
  )
  
  output$inputDataDensity <- DT::renderDataTable(
    densityData()
  )
  
  
  output$inputData <- DT::renderDataTable(
    lengthData()
  )
  
  output$assessmentTier <- renderText({
    
    paste("Your assessment and management level is Tier",tierTable()$tier,sep=" ")
  })
  
  output$renderTierTable = DT::renderDataTable(tierTable()$table, options = list(paging=FALSE, searching = FALSE))
  
  tierTable <- reactive({
    dataList <- input$checkDataGroup
    dataTypes = c("Local Ecological Knowledge", "Length composition data","Landings and Effort Data","Underwater Visual Survey Data")
    dataAvailable = c("dataLEK" %in% dataList,"dataLength" %in% dataList,"landingsData" %in% dataList,"underwaterData" %in% dataList)
   
    if(dataAvailable[1]) yearsLEK = 1 else yearsLEK = 0
    if(!dataAvailable[2]) yearsLength = 0 else{
      if (is.null(lengthData()) & input$dataType != "Use sample Data")  yearsLength = 0 else {
        #df = lengthData()
        yearsLength = length(unique(lengthData()$year))}}
    
    #df<-catchData()
    #yearsLandings = length(unique(catchData()))
    if(!dataAvailable[3]) yearsLandings = 0 else{
      if (is.null(catchData()) & input$dataType != "Use sample Data")  yearsLandings = 0 else {
        #df = catchData()
        yearsLandings = length(unique(catchData()$year))}}

    if(!dataAvailable[4]) yearsUnderwater = 0 else{
    if ((is.null(densityData()) | is.null(biomassData())) & input$dataType != "Use sample Data")  yearsUnderwater = 0 else {

      yearsUnderwater = length(unique(c(densityData()$Year,biomassData()$Year)))}}
    
    
    
    dataYears = c(yearsLEK,yearsLength,yearsLandings,yearsUnderwater)
    df = data.frame(cbind(dataTypes,dataAvailable,dataYears))
    colnames(df) = c("Data Type","Is data available?","Years of available data")
    #df
    dfAvailble = as.logical(df$"Is data available?")
    dfYears = as.numeric(df$"Years of available data")

    if (dfAvailble[3] & !is.na(dfYears[3]>=2)) tier = 3 else {
      if ((dfAvailble[2] & !is.na(dfYears[2]>=1)) | (dfAvailble[4] & !is.na(dfYears[4]>=1 ))) tier = 2 else {
        tier = 1
      }}
    return(list(table=df,
                tier=tier))
  })
  
  output$inputDataLandings <- DT::renderDataTable(
    catchData()
  )
  
  output$LHITable <- DT::renderDataTable(LHIInput(), options = list(paging=FALSE, searching = FALSE))
  
  LHIInput <- reactive({
    if(is.null(input$siteSelection) | is.null(input$speciesSelection)) return()
    table = data.frame(matrix(NA,nrow=1,ncol=13))
    
    colnames(table) = c("Country",
                        "Site",
                        "Scientific Name",
                        "Common Name",
                        "Code",
                        "Linf",
                        "k",
                        "t0",
                        "M",
                        "w_a",
                        "w_b",
                        "m50",
                        "m95")
    
    table[1,] = c(input$countrySelection,
                  input$siteSelection,
                  input$speciesSelection,
                  input$Common,
                  input$Code,
                  input$Linf,
                  input$k,
                  input$t0,
                  input$M,
                  input$w_a,
                  input$w_b,
                  input$m50,
                  input$m95)
    table[2,] = c(input$countrySelection,
                  input$siteSelection,
                  input$speciesSelection,
                  input$Common,
                  input$Code,
                  input$Linf_Reference,
                  input$k_Reference,
                  input$t0_Reference,
                  input$M_Reference,
                  input$w_a_Reference,
                  input$w_b_Reference,
                  input$m50_Reference,
                  input$m95_Reference)
    rownames(table) = c("Parameter value","Reference")
    table
  })
  
  
  output$indicatorLengthChecks <- renderUI({
    listLength <- c("Does the length frequency data represent the size structure of the entire population?",
                    "Is the fishery currently in equilibrium (i.e., relatively stable environmental conditions, fishing pressure, stock status, etc.)?",
                    "Does the fishery experience relatively stable recruitment over time?",
                    "Is the species relatively slow growing and long-lived?",
                    "Please look at the length-frequency histogram in the Assessment tab (Step 5). Does the histogram look complete, and is there a single uni-modal peak?")
    checkboxGroupInput("lengthChecks",label="Please answer the following questions about your length data.",choices = listLength)
  })
  
  output$indicatorLandingsChecks <- renderUI({
    listLandings <-c("Has management been relatively stable over the last several years?",
                     "Has the use of specific gears been relatively stable over the last several years?")
    checkboxGroupInput("landingsChecks",label="Please answer the following questions about your landings data.",choices = listLandings)
  
  })
  
  output$indicatorUnderwaterChecks <- renderUI({
    listUnderwater <- c("Is the no-take zone well enforced?",
                        "Has the no-take zone been in place long enough for the population living inside the zone to be a proxy for an un-fished population?",
                        "Are the no-take zone placed in a similar habitat to the fished area?",
                        "Is the species sedentary or only moderately mobile?")
    checkboxGroupInput("underwaterChecks",label="Please answer the following questions about your underwater visual survey data.",choices = listUnderwater)
  })
  
  output$fmcUI <- renderUI({
    
    
    fmcList1 = c("Gear Restrictions – Gear Type (such as banning destructive fishing gear)",
                 "Sex-Specific Controls",
                 "Seasonal Closures to Protect Vulnerable Life History Stages",
                 "Protection of Ecologically Important Species")
    
    fmcList2 = c(fmcList1,"Bag or Trip Limit",
                 "Size Limit",
                 "Temporal Limit",
                 "Gear Restrictions – Gear Number (also known as Deployment Limits)")
    
    fmcList3 = c(fmcList2,"Catch Limit")
    
    if(tierTable()$tier == 1) fmcList = fmcList1
    if(tierTable()$tier == 2) fmcList = fmcList2
    if(tierTable()$tier == 3) fmcList = fmcList3
    
    checkboxGroupInput(inputId = "fmcSelection",label = "Select at least one fisheries management control:",choices=fmcList,selected="Gear Restrictions – Gear Type (such as banning destructive fishing gear)")
    
  })
  
  output$destructiveUI <- renderUI({
    if (!("dataLEK" %in% input$checkDataGroup) | !("Presence of Destructive Fishing Gear" %in% input$indicatorLEKSelection)) return()
    TRP = input$destructive_TRP
    LRP = input$destructive_LRP
    selectInput(inputId ="destructive_PI",label="Instructions: Based on your local ecological knolwedge, enter the value for this performance indicator. The options available are automatically generated based on the TRP and LRP defined in Step 3.",choices=c(TRP,LRP),selected=NULL)
  })
  
  output$seasonsUI <- renderUI({
    if (!("dataLEK" %in% input$checkDataGroup) | !("Changes in Fishing Seasons" %in% input$indicatorLEKSelection)) return()
    TRP = input$season_TRP
    LRP = input$season_LRP
    selectInput(inputId ="seasons_PI",label="Instructions: Based on your local ecological knolwedge, enter the value for this performance indicator. The options available are automatically generated based on the TRP and LRP defined in Step 3.",choices=c(TRP,LRP),selected=NULL)
  })
  
  output$compositionUI <- renderUI({
    if (!("dataLEK" %in% input$checkDataGroup) | !("Changes in Target Species Composition" %in% input$indicatorLEKSelection)) return()
    TRP = input$composition_TRP
    LRP = input$composition_LRP
    selectInput(inputId ="composition_PI",label="Instructions: Based on your local ecological knolwedge, enter the value for this performance indicator. The options available are automatically generated based on the TRP and LRP defined in Step 3.",choices=c(TRP,LRP),selected=NULL)
  })
  

  
  output$vulnerabilityUI <- renderUI({
    if (!("dataLEK" %in% input$checkDataGroup) | !("Target Species Vulnerability" %in% input$indicatorLEKSelection)) return()
    TRP = input$vulnerability_TRP
    LRP = input$vulnerability_LRP
    selectInput(inputId ="vulnerability_PI",label="Instructions: Based on your local ecological knolwedge, enter the value for this performance indicator. The options available are automatically generated based on the TRP and LRP defined in Step 3.",choices=c(TRP,LRP),selected=NULL)
  })
  
  output$indicatorLEKUI <- renderUI({
    indicatorList = c("Presence of Destructive Fishing Gear","Changes in Fishing Seasons","Changes in Target Species Composition","Target Species Vulnerability")
    checkboxGroupInput(inputId = "indicatorLEKSelection",label = "Select at least one local ecological knowledge performance indicator:",choices=indicatorList,selected="Presence of Destructive Fishing Gear")
  })
  
  output$indicatorLengthUI <- renderUI({
    if(is.null(tierTable()$tier)) return()
    if(tierTable()$tier < 2) return()
    df <- lengthData()
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    if (nrow(df) < 500 | length(input$lengthChecks)<5) indicatorList = c("Froese Sustainability Indicators") else{
      indicatorList = c("Froese Sustainability Indicators","Fishing Mortality / Natural Mortality (LBAR)","Fishing Mortality / Natural Mortality (Catch Curve)","Spawning Potential Ratio (SPR)")
    }
    #indicatorList = c("Average Length (Fishery Dependent)","Fishing Mortality / Natural Mortality (Catch Curve)","Fishing Mortality / Natural Mortality (LBAR)","Spawning Potential Ratio (SPR)","Froese Sustainability Indicators")
    #indicatorList = c("Average Length (Fishery Dependent)","Fishing Mortality / Natural Mortality (LBAR)","Froese Sustainability Indicators")
    
    #checkboxGroupInput(inputId = "indicatorLengthSelection",label = "Select at least one length-based performance indicator:",choices=indicatorList,selected="Average Length (Fishery Dependent)")
    checkboxGroupInput(inputId = "indicatorLengthSelection",label = "Select at least one length-based performance indicator. Note that if fewer than 500 length samples are available, only Froese indicators are possible:",choices=indicatorList,selected="Froese Sustainability Indicators")
  })
  
  output$indicatorLandingsUI <- renderUI({
    if(is.null(tierTable()$tier)) return()
    if(tierTable()$tier < 3 | length(input$landingsChecks)<2) return()
    indicatorList = c("Total Landings","CPUE")
    checkboxGroupInput(inputId = "indicatorLandingsSelection",label = "Select at least one landings-based performance indicator:",choices=indicatorList,selected="Total Landings")
  })
  
  output$indicatorUnderwaterUI <- renderUI({
    if(is.null(tierTable()$tier)) return()
    if(tierTable()$tier < 2 |length(input$underwaterChecks)<4) return()
    indicatorList = c("Biomass Ratio (aggregated across species)","Density Ratio (Target Species)")
    checkboxGroupInput(inputId = "indicatorUnderwaterSelection",label = "Select at least one underwater-survey-based performance indicator:",choices=indicatorList,selected="Biomass Ratio (aggregated across species)")
  })
  
  output$gearUI <- renderUI({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection)) return()
    df <- lengthData()
    
    df = subset(df,species==input$speciesSelection)
    df = subset(df,site==input$siteSelection)
    gears = c("Look at All Gear Types","Aggregate Across Gear Types",as.vector(unique(df$gear)))
    selectInput(inputId="gearSelection","Select Gear Type for Visualization",choices=gears,selected="Aggregate Across Gear Types")
  })
  
  output$gearGlobalUI <- renderUI({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(lengthData())) return()
    if (is.null(lengthData()) & input$dataType != "Use sample Data") {
      textInput(inputId="gearGlobalSelection","Enter Gear Type for Analysis")} else{
        
        df <- lengthData()
        
        df = subset(df,species==input$speciesSelection)
        df = subset(df,site==input$siteSelection)
        gears = c("Aggregate Across Gear Types",as.vector(unique(df$gear)))
        selectInput(inputId="gearGlobalSelection","Select Gear Type for Length-Based Analysis",choices=gears,selected="Aggregate Across Gear Types")}
  })
  
  output$speciesUI <- renderUI({
    
    if (is.null(input$dataType)) {
       textInput(inputId="speciesSelection","Enter Species for Analysis") } else{
        
        if(is.null(lengthData())) speciesLength <- NA else speciesLength <- as.vector(unique(lengthData()$species))
        if(is.null(catchData())) speciesCatch <- NA else speciesCatch <- as.vector(unique(catchData()$species))
        if(is.null(densityData())) speciesDensity <- NA else speciesDensity <- as.vector(unique(densityData()$Species))

        
        species = unique(na.trim(c(speciesLength,speciesCatch,speciesDensity)))
        selectInput(inputId="speciesSelection","Select Species for Analysis",choices=species)}
  })
  
  output$siteUI <- renderUI({
    if (is.null(lengthData()) | is.null(input$dataType)) {
      textInput(inputId="siteSelection","Enter Site for Analysis")} else {
        
        df <- lengthData()
        
        sites = as.vector(unique(df$site))
        selectInput(inputId="siteSelection","Select Site for Analysis",choices=sites)}
  })
  
  output$yearUI <- renderUI({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection)) return()
    
    if (is.null(lengthData()) & input$dataType != "Use sample Data") return()
    
    df <- lengthData()
    
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    years = c("Look at All Years","Aggregate Across Years",as.vector(unique(df$year)))
    selectInput(inputId="yearSelection","Select Year for Visualization",choices=years,selected="Aggregate Across Years")
  })
  
  output$yearGlobalUI <- renderUI({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(lengthData())) return()
    
    if (is.null(lengthData()) & input$dataType != "Use sample Data") {
      textInput(inputId="yearGlobalSelection","Enter Year for Length-Based Analysis") } else {
        
        df <- lengthData()
        
        df = subset(df,site==input$siteSelection)
        df = subset(df,species==input$speciesSelection)
        yearsNum <- as.vector(unique(df$year))
        years = c("Aggregate Across Years",yearsNum)
        selectInput(inputId="yearGlobalSelection","Select Year for Length-Based Analysis",choices=years,selected=max(yearsNum))}
  })
  
  
  
  output$yearLandingsUI <- renderUI({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(catchData())) return()
    
    if (is.null(catchData()) & input$dataType != "Use sample Data") {
      textInput(inputId="yearLandingsSelection","Enter Year for Landings Data Analysis") } else {
        
        df <- catchData()

        df = subset(df,site==input$siteSelection)
        df = subset(df,species==input$speciesSelection)
        years = c(as.vector(unique(df$year)))
        selectInput(inputId="yearLandingsSelection","Select Year for Landings Data Analysis",choices=years,selected=max(years))}
  })
  
  output$yearUnderwaterUI <- renderUI({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(biomassData()) | is.null(densityData())) return()
    
    if ((is.null(biomassData()) | is.null(densityData())) & input$dataType != "Use sample Data") {
      textInput(inputId="yearUnderwaterSelection","Enter Year for Underwater Visual Survey Data Analysis") } else {
        
        df1 <-  densityData() %>%
          filter(Species == input$speciesSelection)
        df2 <- biomassData()
        
        years <- unique(c(df1$Year,df2$Year))
        selectInput(inputId="yearUnderwaterSelection","Enter Year for Underwater Visual Survey Data Analysis",choices=years,selected=max(years))}
  })
  
  output$histogram <- renderPlot({
    print(plotInput())
  })
  
  plotInput <- function(){
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(input$gearGlobalSelection) | is.null(input$yearGlobalSelection) | is.null(lengthData())) return()
    # inFile <- input$data
    # 
    # if (is.null(inFile) & input$dataType != "Use sample Data")
    #   return(NULL)
    # 
    # if (input$dataType == "Use sample Data") df = df_length else df = read.csv(inFile$datapath,header=TRUE)
    df <- lengthData()
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    #froeseTemp = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,df$length_cm)
    
    if (input$gearGlobalSelection != "Aggregate Across Gear Types" & input$yearGlobalSelection != "Aggregate Across Years"){
      
      if(input$gearGlobalSelection != "Look at All Gear Types") df = subset(df,gear==input$gearGlobalSelection)
      if(input$yearGlobalSelection != "Look at All Years") df = subset(df,year==input$yearGlobalSelection)
      froeseTemp = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,input$m95,df$length_cm)
      print(ggplot(df, aes(x=length_cm)) +
              geom_histogram(position="identity", binwidth=input$binSize,fill="#00ADB7",colour="black") +
              ggtitle(paste(paste("Site: ",input$siteSelection,sep=""),
                            paste("\nSpecies: ",input$speciesSelection,sep=""),
                            paste("\nSize at maturity (red line): ",round(froeseTemp$Lmat,2),"; Percent mature in this figure: ",round(froeseTemp$percentMature,2),"%",sep=""),
                            paste("\nOptimal size (green line): ",round(froeseTemp$Lopt,2),"; Percent optimal in this figure: ",round(froeseTemp$percentOpt,2),"%",sep=""),
                            paste("\nMegaspawner size (blue line): ",round(froeseTemp$Lmega,2),"; Percent megaspawner in this figure: ",round(froeseTemp$percentMega,2),"%",sep=""))) +
              facet_grid(gear~year) +
              xlab("Length (cm)") +
              ylab("Count") +
              geom_vline(xintercept = froeseTemp$Lmat,color="red",type=2) +
              geom_vline(xintercept = froeseTemp$Lopt,color="green",type=2) +
              geom_vline(xintercept = froeseTemp$Lmega,color="blue",type=2) +
              #xlim(0,input$Linf) +
              theme_bw() +
              theme(text = element_text(size=12),
                    plot.title = element_text(size=12),
                    axis.title = element_text(size=12),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 12),
                    strip.text.y = element_text(size = 12)))
    }
    
    if (input$gearGlobalSelection == "Aggregate Across Gear Types" & input$yearGlobalSelection != "Aggregate Across Years"){
      
      if(input$yearGlobalSelection != "Look at All Years") df = subset(df,year==input$yearGlobalSelection)
      froeseTemp = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,input$m95,df$length_cm)
      print(ggplot(df, aes(x=length_cm)) +
              geom_histogram(position="identity", binwidth=input$binSize,fill="#00ADB7",colour="black") +
              ggtitle(paste(paste("Site: ",input$siteSelection,sep=""),
                            paste("\nSpecies: ",input$speciesSelection,sep=""),
                            paste("\nGear Type: ",input$gearGlobalSelection,sep=""),      
                            paste("\nYear: ",input$yearGlobalSelection,sep=""),
                            paste("\nSize at maturity (red line): ",round(froeseTemp$Lmat,2),"; Percent mature in this figure: ",round(froeseTemp$percentMature,2),"%",sep=""),
                            paste("\nOptimal size (green line): ",round(froeseTemp$Lopt,2),"; Percent optimal in this figure: ",round(froeseTemp$percentOpt,2),"%",sep=""),
                            paste("\nMegaspawner size (blue line): ",round(froeseTemp$Lmega,2),"; Percent megaspawner in this figure: ",round(froeseTemp$percentMega,2),"%",sep=""))) +
              facet_grid(.~year) +
              xlab("Length (cm)") +
              ylab("Count") +
              geom_vline(xintercept = froeseTemp$Lmat,color="red",type=2) +
              geom_vline(xintercept = froeseTemp$Lopt,color="green",type=2) +
              geom_vline(xintercept = froeseTemp$Lmega,color="blue",type=2) +
              #xlim(0,input$Linf) +
              theme_bw() +
              theme(text = element_text(size=12),
                    plot.title = element_text(size=12),
                    axis.title = element_text(size=12),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 12),
                    strip.text.y = element_text(size = 12)))
    }
    
    if (input$gearGlobalSelection != "Aggregate Across Gear Types" & input$yearGlobalSelection == "Aggregate Across Years"){
      
      if(input$gearGlobalSelection != "Look at All Gear Types") df = subset(df,gear==input$gearGlobalSelection)
      froeseTemp = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,input$m95,df$length_cm)
      print(ggplot(df, aes(x=length_cm)) +
              geom_histogram(position="identity", binwidth=input$binSize,fill="#00ADB7",colour="black") +
              ggtitle(paste(paste("Site: ",input$siteSelection,sep=""),
                            paste("\nSpecies: ",input$speciesSelection,sep=""),
                            paste("\nGear Type: ",input$gearGlobalSelection,sep=""),      
                            paste("\nYear: ",input$yearGlobalSelection,sep=""),
                            paste("\nSize at maturity (red line): ",round(froeseTemp$Lmat,2),"; Percent mature in this figure: ",round(froeseTemp$percentMature,2),"%",sep=""),
                            paste("\nOptimal size (green line): ",round(froeseTemp$Lopt,2),"; Percent optimal in this figure: ",round(froeseTemp$percentOpt,2),"%",sep=""),
                            paste("\nMegaspawner size (blue line): ",round(froeseTemp$Lmega,2),"; Percent megaspawner in this figure: ",round(froeseTemp$percentMega,2),"%",sep=""))) +
              facet_grid(gear~.) +
              xlab("Length (cm)") +
              geom_vline(xintercept = froeseTemp$Lmat,color="red",type=2) +
              geom_vline(xintercept = froeseTemp$Lopt,color="green",type=2) +
              geom_vline(xintercept = froeseTemp$Lmega,color="blue",type=2) +
              ylab("Count") +
              #xlim(0,input$Linf) +
              theme_bw() +
              theme(text = element_text(size=12),
                    plot.title = element_text(size=12),
                    axis.title = element_text(size=12),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 12),
                    strip.text.y = element_text(size = 12)))
    }
    
    if (input$gearGlobalSelection == "Aggregate Across Gear Types" & input$yearGlobalSelection == "Aggregate Across Years"){
      froeseTemp = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,input$m95,df$length_cm)
      print(ggplot(df, aes(x=length_cm)) +
              geom_histogram(position="identity", binwidth=input$binSize,fill="#00ADB7",colour="black") +
              ggtitle(paste(paste("Site: ",input$siteSelection,sep=""),
                            paste("\nSpecies: ",input$speciesSelection,sep=""),
                            paste("\nGear Type: ",input$gearGlobalSelection,sep=""),      
                            paste("\nYear: ",input$yearGlobalSelection,sep=""),
                            paste("\nSize at maturity (red line): ",round(froeseTemp$Lmat,2),"; Percent mature in this figure: ",round(froeseTemp$percentMature,2),"%",sep=""),
                            paste("\nOptimal size (green line): ",round(froeseTemp$Lopt,2),"; Percent optimal in this figure: ",round(froeseTemp$percentOpt,2),"%",sep=""),
                            paste("\nMegaspawner size (blue line): ",round(froeseTemp$Lmega,2),"; Percent megaspawner in this figure: ",round(froeseTemp$percentMega,2),"%",sep=""))) +
              xlab("Length (cm)") +
              ylab("Count") +
              geom_vline(xintercept = froeseTemp$Lmat,color="red",type=2) +
              geom_vline(xintercept = froeseTemp$Lopt,color="green",type=2) +
              geom_vline(xintercept = froeseTemp$Lmega,color="blue",type=2) +
              #xlim(0,input$Linf) +
              theme_bw() +
              theme(text = element_text(size=12),
                    plot.title = element_text(size=12),
                    axis.title = element_text(size=12),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 12),
                    strip.text.y = element_text(size = 12)))
    }
    
  }
  
  output$LBARBox <- renderPlot({
    print(plotLBAR())
  })
  
  output$CPUEPlots <- renderPlot({
    print(plotCPUE()$plots)
  })
  
  output$UVCPlots <- renderPlot({
    print(plotUVC())
  })
  
  output$CCPlot <- renderPlot({
    print(plot(CCInput()$ccOutputs,plot_selec = TRUE))
  })
  
  output$SPRPlot <- renderPlot({
    grid.arrange(plotSize(SPRInput()$myFit),
                 plotMat(SPRInput()$myFit),
                 ncol=1)
  })
  
  
  plotUVC <- function(){
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(input$yearUnderwaterSelection) | is.null(biomassData()) | is.null(densityData())) return()
    densityDF <- densityData() %>%
      filter(Year <= input$yearUnderwaterSelection)
    density_processed <- densityDF %>%
      group_by(Year,Species,Reserve) %>%
      summarize(Density = mean(Density)) %>%
      spread(key=Reserve,value=Density) %>%
      rename(Density_Fished = `0`,
             Density_Unfished = `1`) %>%
      mutate(Density_Ratio = Density_Fished/Density_Unfished)
    
    biomassDF <- biomassData()%>%
      filter(Year <= input$yearUnderwaterSelection)
    
    biomass_processed <- biomassDF %>%
      group_by(Year,Reserve) %>%
      summarize(Biomass = mean(Biomass))  %>%
      spread(key=Reserve,value=Biomass) %>%
      rename(Biomass_Fished = `0`,
             Biomass_Unfished =`1`) %>%
      mutate(Biomass_Ratio = Biomass_Fished/Biomass_Unfished)
    
    
    biomass_plot <- biomass_processed %>%
      ggplot(aes(x=Year,y=Biomass_Ratio)) +
      theme_bw() +
      geom_line(linetype=2,colour="#00ADB7") +
      geom_point(size=4,colour="#EA883A") +
      geom_hline(yintercept = input$BR_LRP,color="red") +
      geom_hline(yintercept = input$BR_TRP,color="green") +
      ggtitle("Green line: Target reference point\nRed line: Limit reference point") +
      ylab("Fished:Unfished Multispecies Biomass Ratio\n(Aggregated ecosystem-level)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            text = element_text(size=12),
            plot.title = element_text(size=12),
            axis.title = element_text(size=12))
    
    density_plot <- density_processed %>%
      filter(Species == input$speciesSelection) %>%
      ggplot(aes(x=Year,y=Density_Ratio)) +
      theme_bw() +
      geom_line(linetype=2,colour="#00ADB7") +
      geom_point(size=4,colour="#EA883A") +
      geom_hline(yintercept = input$DR_LRP,color="red") +
      geom_hline(yintercept = input$DR_TRP,color="green") +
      ggtitle("Green line: Target reference point\nRed line: Limit reference point") +
      ylab("Fished:Unfished Density Ratio\n(Single Species)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            text = element_text(size=12),
            plot.title = element_text(size=12),
            axis.title = element_text(size=12))
     grid.arrange(biomass_plot,density_plot,nrow=2,top=paste(paste("Site: ",input$siteSelection,sep=""),
                                                                   paste("\nSpecies: ",input$speciesSelection,sep="")))
  }
  
  plotCPUE <- function(){
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(input$yearLandingsSelection) | is.null(catchData()) | is.null(input$catchReferenceSlider[1])) return()
    df <- catchData()
    totalFN = function(x,y){
      if (is.na(y)) total = x else total = y
      return(total)
    }
    
    catchEffortData=df[,c("fisher_days","sampled_catch","total_catch")] = lapply(df[,c("fisher_days","sampled_catch","total_catch")],as.numeric,na.rm=TRUE)
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    #df$date <- as.Date(df$date,"%m/%d/%Y")
    #df$month <- as.Date(cut(df$date, breaks = "month"))
    dataSubset = subset(df,species==input$speciesSelection)
    
    
    
    dataSubset = dataSubset %>%
      group_by(year,permanent_trip_id) %>%
      summarize(catch = sum(sampled_catch,na.rm=TRUE),
                fisher_days = mean(fisher_days),
                CPUE = catch/fisher_days)
    
    if (input$tripMax != -999) dataSubset <- dataSubset %>% filter(catch < input$tripMax)
    
    
    dataSubset = dataSubset %>%  
      group_by(year) %>%
      mutate(sampling_days = length(unique(permanent_trip_id))) %>%
      summarize(catch = ifelse(input$totalCatch == TRUE, sum(catch,na.rm=TRUE),sum(catch,na.rm=TRUE)/sampling_days),
                CPUE = median(CPUE,na.rm=TRUE),
                fisher_days = ifelse(input$totalCatch == TRUE,sum(fisher_days,na.rm=TRUE),sum(fisher_days,na.rm=TRUE)/sampling_days)) %>%
      filter(catch>0 & CPUE > 0  & fisher_days > 0)
    

      catchRP <- dataSubset %>%
        filter(year %in% input$catchReferenceSlider) %>%
        summarize(catchRP = mean(catch)) %>%
        as.numeric()
      
      catchLast <- dataSubset %>%
        filter(year == max(year)) %>%
        dplyr::select(catch) %>%
        as.numeric()
      
      catchRelativeFinal <- (catchLast - catchRP) / catchRP * 100
      
      cpueRP <- dataSubset %>%
        filter(year %in% input$catchReferenceSlider) %>%
        summarize(cpueRP = mean(CPUE)) %>%
        as.numeric()
      
      cpueLast <- dataSubset %>%
        filter(year == max(year)) %>%
        dplyr::select(CPUE) %>%
        as.numeric()
      
      cpueRelativeFinal <- (cpueLast - cpueRP) / cpueRP * 100
      

      catchLabel <- ifelse(input$totalCatch == TRUE, "Total annual catch [kg/year]","Normalized annual catch [kg/year/sampling-day]")
      effortLabel <- ifelse(input$totalCatch == TRUE, "Total annual effort [fisher-days]","Normalized annual effort [fisher-days/sampling-day]")
      
      effortPlot = ggplot(dataSubset,aes(year,fisher_days)) +
        theme_bw() +
        geom_line(linetype=2,colour="#00ADB7") +
        geom_point(size=4,colour="#EA883A") +
        ylab(effortLabel) +
        xlab("Year") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              text = element_text(size=12),
              plot.title = element_text(size=12),
              axis.title = element_text(size=12))
      
      landingsPlot = ggplot(dataSubset,aes(year,catch)) +
        geom_line(linetype=2,colour="#00ADB7") +
        geom_point(size=4,colour="#EA883A") +
        geom_segment(aes(x = input$catchReferenceSlider[1], y = catchRP, xend = tail(input$catchReferenceSlider,n=1), yend = catchRP, colour = "red"),show.legend=FALSE) +
        theme_bw() +
        ylab(catchLabel) +
        xlab("Year") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              text = element_text(size=12),
              plot.title = element_text(size=12),
              axis.title = element_text(size=12)) +
        ggtitle(paste("Reference period shown as red line",
                      "\nAverage catch from ", input$catchReferenceSlider[1], " to ", tail(input$catchReferenceSlider,n=1),": ",round(catchRP,2),
                      "\nCatch in ",max(dataSubset$year),": ",round(catchLast,2),
                      "\nPercentage change in catch from reference period to ",max(dataSubset$year),": ",round(catchRelativeFinal,2),"%",sep=""))
      
      CPUEPlot = ggplot(dataSubset,aes(year,CPUE)) +
        geom_line(linetype=2,colour="#00ADB7") +
        geom_point(size=4,colour="#EA883A") +
        geom_segment(aes(x = input$catchReferenceSlider[1], y = cpueRP, xend = tail(input$catchReferenceSlider,n=1), yend = cpueRP, colour = "red"),show.legend=FALSE) +
        theme_bw() +
        ylab("Median CPUE [kg/fisher-day]") +
        xlab("Year") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              text = element_text(size=12),
              plot.title = element_text(size=12),
              axis.title = element_text(size=12)) +
        ggtitle(paste("Reference period shown as red line",
                      "\nAverage CPUE from ", input$catchReferenceSlider[1], " to ", tail(input$catchReferenceSlider,n=1),": ",round(cpueRP,2),
                      "\nCPUE in ",max(dataSubset$year),": ",round(cpueLast,2),
                      "\nPercentage change in CPUE from reference period to ",max(dataSubset$year),": ",round(cpueRelativeFinal,2),"%",sep=""))
      
      return(list(plots = grid.arrange(landingsPlot,effortPlot,CPUEPlot,nrow=3,top=paste(paste("Site: ",input$siteSelection,sep=""),
                                                                     paste("\nSpecies: ",input$speciesSelection,sep=""))),
           catchRelativeFinal = catchRelativeFinal,
           cpueRelativeFinal = cpueRelativeFinal))
           
  }
  
  
  plotLBAR <- function(){
    
    df = LBARInput()
    
    df$FvM = as.numeric(df$FvM)
    
    maxY = max(df$FvM+1,input$FvMLBAR_LRP +1)
    minY = min(df$FvM - 1, input$FvMLBAR_TRP - 1)
    
    if (input$gearGlobalSelection != "Aggregate Across Gear Types" & input$yearGlobalSelection != "Aggregate Across Years"){
      
      if(input$gearGlobalSelection != "Look at All Gear Types") df = subset(df,gear==input$gearGlobalSelection)
      if(input$yearGlobalSelection != "Look at All Years") df = subset(df,year==input$yearGlobalSelection)
      
      print(ggplot(df, aes(x="",y=FvM)) +
              geom_boxplot(position="identity", binwidth=input$binSize,fill="#00ADB7",colour="black") +
              ggtitle(paste(paste("Site: ",input$siteSelection,sep=""),
                            paste("\nSpecies: ",input$speciesSelection,sep=""),
                            paste("\nGear Type: ",input$gearGlobalSelection,sep=""),      
                            paste("\nYear: ",input$yearGlobalSelection,sep=""))) +
              facet_grid(gear~year) +
              xlab("") +
              ylab("F/M") +
              #ylim(minY,maxY) +
              geom_hline(yintercept=input$FvMLBAR_TRP,color="green",linetype=2) +
              geom_hline(yintercept=input$FvMLBAR_LRP,color="red",linetype=2) +
              theme_bw() +
              theme(text = element_text(size=20),
                    plot.title = element_text(size=20),
                    axis.title = element_text(size=25),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 25),
                    strip.text.y = element_text(size = 25)))
    }
    
    if (input$gearGlobalSelection == "Aggregate Across Gear Types" & input$yearGlobalSelection != "Aggregate Across Years"){
      
      if(input$yearGlobalSelection != "Look at All Years") df = subset(df,year==input$yearGlobalSelection)
      
      print(ggplot(df, aes(x="",y=FvM)) +
              geom_boxplot(position="identity", binwidth=input$binSize,fill="#00ADB7",colour="black") +
              ggtitle(paste(paste("Site: ",input$siteSelection,sep=""),
                            paste("\nSpecies: ",input$speciesSelection,sep=""),
                            paste("\nGear Type: ",input$gearGlobalSelection,sep=""),      
                            paste("\nYear: ",input$yearGlobalSelection,sep=""))) +
              facet_grid(.~year,scale="free_y") +
              xlab("") +
              ylab("F/M") +
              ylim(minY,maxY) +
              geom_hline(yintercept=input$FvMLBAR_TRP,color="green",linetype=2) +
              geom_hline(yintercept=input$FvMLBAR_LRP,color="red",linetype=2) +
              theme_bw() +
              theme(text = element_text(size=20),
                    plot.title = element_text(size=20),
                    axis.title = element_text(size=25),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 25),
                    strip.text.y = element_text(size = 25)))
    }
    
    if (input$gearGlobalSelection != "Aggregate Across Gear Types" & input$yearGlobalSelection == "Aggregate Across Years"){
      
      if(input$gearGlobalSelection != "Look at All Gear Types") df = subset(df,gear==input$gearGlobalSelection)
      
      print(ggplot(df, aes(x="",y=FvM)) +
              geom_boxplot(position="identity", binwidth=input$binSize,fill="#00ADB7",colour="black") +
              ggtitle(paste(paste("Site: ",input$siteSelection,sep=""),
                            paste("\nSpecies: ",input$speciesSelection,sep=""),
                            paste("\nGear Type: ",input$gearGlobalSelection,sep=""),      
                            paste("\nYear: ",input$yearGlobalSelection,sep=""))) +
              facet_grid(gear~.,scale="free_y") +
              xlab("") +
              ylab("F/M") +
              ylim(minY,maxY) +
              geom_hline(yintercept=input$FvMLBAR_TRP,color="green",linetype=2) +
              geom_hline(yintercept=input$FvMLBAR_LRP,color="red",linetype=2) +
              theme_bw() +
              theme(text = element_text(size=20),
                    plot.title = element_text(size=20),
                    axis.title = element_text(size=25),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 25),
                    strip.text.y = element_text(size = 25)))
    }
    
    if (input$gearGlobalSelection == "Aggregate Across Gear Types" & input$yearGlobalSelection == "Aggregate Across Years"){
      
      print(ggplot(df, aes(x="",y=FvM)) +
              geom_boxplot(position="identity", binwidth=input$binSize,fill="#00ADB7",colour="black") +
              ggtitle(paste(paste("Site: ",input$siteSelection,sep=""),
                            paste("\nSpecies: ",input$speciesSelection,sep=""),
                            paste("\nGear Type: ",input$gearGlobalSelection,sep=""),      
                            paste("\nYear: ",input$yearGlobalSelection,sep=""))) +
              xlab("") +
              ylab("F/M") +
              #ylim(c(minY,maxY)) +
              geom_hline(yintercept=input$FvMLBAR_TRP,color="green",linetype=2) +
              geom_hline(yintercept=input$FvMLBAR_LRP,color="red",linetype=2) +
              theme_bw() +
              theme(text = element_text(size=20),
                    plot.title = element_text(size=20),
                    axis.title = element_text(size=25),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 25),
                    strip.text.y = element_text(size = 25)))
    }
    
  }
  
  output$LBAR <- DT::renderDataTable(LBARInput())
  
  output$CC <- DT::renderDataTable(CCInput()$table)
  
  output$Landings <- DT::renderDataTable(landingsInput())
  
  output$CPUE <- DT::renderDataTable(cpueInput())
  
  output$SPR <- DT::renderDataTable(SPRInput()$table)
  
  output$lengthFD <- DT::renderDataTable(lengthFDInput())
  
  output$Froese <- DT::renderDataTable(FroeseInput())
  
  output$BR <- DT::renderDataTable(brInput())
  
  output$DR <- DT::renderDataTable(drInput())
  
  output$summary <-DT::renderDataTable(summaryInput())
  
  summaryInput<-reactive({
    summaryTable = data.frame(matrix(NA,nrow=1,ncol=7))
    
    summaryTable = summaryTable[-1,]
    
    Names <- c("Assessment",
               "Year",
               "Site",
               "Species",
               "PI",
               "TRP",
               "LRP",
               "Result")
    
    if ("dataLEK" %in% input$checkDataGroup & "Presence of Destructive Fishing Gear" %in% input$indicatorLEKSelection){
      if (input$destructive_PI == input$destructive_TRP) result = "Green" else result = "Red"
      newRow = c("Presence of Destructive Fishing Gear",
                 input$yearLEKSelection,
                 input$siteSelection,
                 input$speciesSelection,
                 input$destructive_PI,
                 input$destructive_TRP,
                 input$destructive_LRP,
                 result)
      
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      colnames(newRow) = Names
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("dataLEK" %in% input$checkDataGroup & "Changes in Fishing Seasons" %in% input$indicatorLEKSelection){
      if (input$seasons_PI == input$season_TRP) result = "Green" else result = "Red"
      newRow = c("Changes in Fishing Seasons",
                 input$yearLEKSelection,
                 input$siteSelection,
                 input$speciesSelection,
                 input$seasons_PI,
                 input$season_TRP,
                 input$season_LRP,
                 result)
      
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      colnames(newRow) = Names
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("dataLEK" %in% input$checkDataGroup & "Changes in Target Species Composition" %in% input$indicatorLEKSelection){
      if (input$composition_PI == input$composition_TRP) result = "Green" else result = "Red"
      newRow = c("Changes in Target Species Composition",
                 input$yearLEKSelection,
                 input$siteSelection,
                 input$speciesSelection,
                 input$composition_PI,
                 input$composition_TRP,
                 input$composition_LRP,
                 result)
      
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      colnames(newRow) = Names
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("dataLEK" %in% input$checkDataGroup & "Target Species Vulnerability" %in% input$indicatorLEKSelection){
      if (input$vulnerability_PI == input$vulnerability_TRP) result = "Green" else result = "Red"
      newRow = c("Target Species Vulnerability",
                 input$yearLEKSelection,
                 input$siteSelection,
                 input$speciesSelection,
                 input$vulnerability_PI,
                 input$vulnerability_TRP,
                 input$vulnerability_LRP,
                 result)
      
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      colnames(newRow) = Names
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("landingsData" %in% input$checkDataGroup & "Total Landings" %in% input$indicatorLandingsSelection){
      newRow = c("Total Landings",
                 input$yearLandingsSelection,
                 input$siteSelection,
                 input$speciesSelection,
                 levels(droplevels(landingsInput()$Change_In_Landings)),
                 input$landings_TRP,
                 input$landings_LRP,
                 levels(droplevels(landingsInput()$Result_Landings)))

      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      colnames(newRow) = Names
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("landingsData" %in% input$checkDataGroup & "CPUE" %in% input$indicatorLandingsSelection){
      
      newRow = c("CPUE",
                 input$yearLandingsSelection,
                 input$siteSelection,
                 input$speciesSelection,
                 levels(droplevels(cpueInput()$Change_In_CPUE)),
                 input$CPUE_TRP,
                 input$CPUE_LRP,
                 levels(droplevels(cpueInput()$Result_CPUE)))

      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      colnames(newRow) = Names
      summaryTable = rbind(summaryTable,newRow)}
    
    
    
    if ("dataLength" %in% input$checkDataGroup & "Fishing Mortality / Natural Mortality (LBAR)" %in% input$indicatorLengthSelection) {

      newRow = c("LBAR",
                 input$yearGlobalSelection,
                 LBARInput()[,c("Site",
                                "Species",
                                "FvM",
                                "TRP_FvM",
                                "LRP_FvM",
                                "Result_FvM")])
      
      newRow = (data.frame(newRow,stringsAsFactors = FALSE))
      colnames(newRow) = Names
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("dataLength" %in% input$checkDataGroup & "Fishing Mortality / Natural Mortality (Catch Curve)" %in% input$indicatorLengthSelection) {
      
      newRow = c("Catch Curve",
                 input$yearGlobalSelection,
                 CCInput()$table[,c("Site",
                                "Species",
                                "FvM",
                                "FvMCC_TRP",
                                "FvMCC_LRP",
                                "Result_CC")])
      
      newRow = (data.frame(newRow,stringsAsFactors = FALSE))
      colnames(newRow) = Names
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("dataLength" %in% input$checkDataGroup & "Froese Sustainability Indicators" %in% input$indicatorLengthSelection) {
      newRow = c("Froese Indicators",
                 input$yearGlobalSelection,
                 input$siteSelection,
                 input$speciesSelection,
                 input$froese_Result,
                 "Green",
                 "Red",
                 input$froese_Result)
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      colnames(newRow) = Names
      summaryTable = rbind(summaryTable,newRow)}

    if ("dataLength" %in% input$checkDataGroup & "Spawning Potential Ratio (SPR)" %in% input$indicatorLengthSelection) {
      
      newRow = c("SPR",
                 input$yearGlobalSelection,
                 SPRInput()$table[,c("Site",
                                "Species",
                                "SPR",
                                "TRP_SPR",
                                "LRP_SPR",
                                "Result_SPR")])
      
      newRow = (data.frame(newRow,stringsAsFactors = FALSE))
      colnames(newRow) = Names
      summaryTable = rbind(summaryTable,newRow)}  
    
      if ("underwaterData" %in% input$checkDataGroup & "Density Ratio (Target Species)" %in% input$indicatorUnderwaterSelection){

        newRow = c("Density Ratio",
                   input$yearUnderwaterSelection,
                   input$siteSelection,
                   input$speciesSelection,
                   drInput()$Density_Ratio,
                   input$DR_TRP,
                   input$DR_LRP,
                   drInput()$Result_Density_Ratio)

        newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
        colnames(newRow) = Names

        summaryTable = rbind(summaryTable,newRow)}
        
        if ("underwaterData" %in% input$checkDataGroup & "Biomass Ratio (aggregated across species)" %in% input$indicatorUnderwaterSelection){
          
         
           newRow = c("Biomass Ratio",
                      input$yearUnderwaterSelection,
                     input$siteSelection,
                     input$speciesSelection,
                     brInput()$Biomass_Ratio,
                     input$BR_TRP,
                     input$BR_LRP,
                     brInput()$Result_Biomass_Ratio)
        
        newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
        colnames(newRow) = Names
        
        summaryTable = rbind(summaryTable,newRow)}
      

   
    colnames(summaryTable) = Names
    rownames(summaryTable) = c()
    summaryTable
  })
  
  
  SPRInput <- reactive({
    #if (is.na(input$FM) | is.na(input$SL50) | is.na(input$SL95)) return(NULL)
    df <- lengthData()
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    if (input$gearGlobalSelection != "Aggregate Across Gear Types") {
      dfSubset = subset(df,gear==input$gearGlobalSelection)
      gearLabel = input$gearGlobalSelection
    } else {
      dfSubset = df
      gearLabel = "Aggregate Across Gear Types"
    }
    
    if (input$yearGlobalSelection != "Aggregate Across Years") {
      dfSubset = subset(dfSubset,year==input$yearGlobalSelection)
      yearLabel = input$yearGlobalSelection
    } else {
      dfSubset = dfSubset
      yearLabel = "Aggregate Across Years"
    }
    lengthData = dfSubset$length_cm[!is.na(dfSubset$length_cm)]
    lengthData <- as.matrix(lengthData)
    #write_csv(lengthData,"tempLengthData.csv")

    
    table = data.frame(matrix(NA,nrow=1,ncol=9))
    colnames(table) = c("Site",
                        "Species",
                        "SL50",
                        "SL95",
                        "FvM",
                        "SPR",
                        "TRP_SPR",
                        "LRP_SPR",
                        "Result_SPR")
    
    MyPars <- new("LB_pars")
    
    MyPars@Linf <- input$Linf 
    MyPars@L50 <- input$m50 
    MyPars@L95 <- input$m95
    MyPars@MK <- input$M / input$k 
    MyPars@M <- input$M
    #MyPars@SL50 <- input$SL50
    #MyPars@SL95 <- input$SL95
    #MyPars@FM <- input$FM
    F <- as.numeric(CCInput()$table$F)
    M <- input$M
    S50 <- as.numeric(CCInput()$table$S50)
    S95 <- as.numeric(CCInput()$table$S95)
    FvM <- as.numeric(CCInput()$table$FvM)
    MyPars@SL50 <- S50
    MyPars@SL95 <- S95
    MyPars@FM <- FvM
    MyPars@BinWidth <- 1
    MyPars@BinMax <- round(max(c(lengthData,input$Linf))+1)
    MyPars@BinMin <- 0
    MyPars@L_units <- "cm"
    MyPars@Walpha_units <- "cm"
    MyPars@Walpha <- input$w_a
    MyPars@Wbeta <- input$w_b
    SPR_Lengths <- new("LB_lengths", file = lengthData, LB_pars = MyPars,dataType = "raw")
    SPR_Lengths@Years <- as.numeric(input$yearGlobalSelection)
    myFit <- LBSPRfit(MyPars, SPR_Lengths)
    sprFit <- as.numeric(myFit@Ests[1,4])
    sl50Fit <- as.numeric(myFit@Ests[1,1])
    sl95Fit <- as.numeric(myFit@Ests[1,2])
    FvMFit <- as.numeric(myFit@Ests[1,3])
    #MySim <- LBSPRsim(MyPars)
    #browser()
    #SPR <- round(MySim@SPR, 2)
    
    if (is.nan(sprFit) | is.infinite(sprFit)) result = "Cannot Interpret" else{
      if (sprFit >= input$SPR_TRP) result = "Green"
      if (sprFit < input$SPR_TRP & sprFit >= input$SPR_LRP) result = "Yellow"
      if (sprFit < input$SPR_LRP) result = "Red"
    }
    
    table[1,] = c(input$siteSelection,
                  input$speciesSelection,
                  sl50Fit,
                  sl95Fit,
                  FvMFit,
                  sprFit,
                  input$SPR_TRP,
                  input$SPR_LRP,
                  result)
    
    return(list(table = table,
                myFit = myFit))
  })
  
  brInput <- reactive({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(input$yearUnderwaterSelection) | is.null(biomassData()) | is.null(densityData())) return()
    
    biomassDF <- biomassData() %>%
      filter(Year <= input$yearUnderwaterSelection)

    BR <- biomassDF %>%
      group_by(Year,Reserve) %>%
      summarize(Biomass = mean(Biomass))  %>%
      spread(key=Reserve,value=Biomass) %>%
      rename(Biomass_Fished = `0`,
             Biomass_Unfished =`1`) %>%
      mutate(Biomass_Ratio = Biomass_Fished/Biomass_Unfished) %>%
      filter(Year == max(.$Year))
    
    BR <- BR$Biomass_Ratio
    
    if (is.nan(BR) | is.infinite(BR)) result = "Cannot Interpret" else{
      if (BR > input$BR_TRP) result = "Green"
      if (BR > input$BR_LRP & BR<input$BR_TRP) result = "Yellow"
      if (BR < input$BR_LRP) result = "Red"
    }
    table = data.frame(matrix(NA,nrow=1,ncol=7))
    
    colnames(table) = c("Site",
                        "Species",
                        "year",
                        "Biomass_Ratio",
                        "TRP_Biomass_Ratio",
                        "LRP_Biomass_Ratio",
                        "Result_Biomass_Ratio")

    table[1,] <- c(input$siteSelection,
                   input$speciesSelection,
                   max(biomassDF$Year),
                   BR,
                   input$BR_TRP,
                   input$BR_LRP,
                   result)
    table
    
  })
  
  drInput <- reactive({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(input$yearUnderwaterSelection) | is.null(biomassData()) | is.null(densityData())) return()
    
    densityDF <- densityData()%>%
      filter(Species == input$speciesSelection &
               Year <= input$yearUnderwaterSelection)
    DR <- densityDF %>%
      group_by(Year,Species,Reserve) %>%
      summarize(Density = mean(Density)) %>%
      spread(key=Reserve,value=Density) %>%
      rename(Density_Fished = `0`,
             Density_Unfished = `1`) %>%
      mutate(Density_Ratio = Density_Fished/Density_Unfished) %>% 
      filter(Year == max(.$Year))
    
    DR <- DR$Density_Ratio
    
    if (is.nan(DR) | is.infinite(DR)) result = "Cannot Interpret" else{
      if (DR > input$DR_TRP) result = "Green"
      if (DR > input$DR_LRP & DR<input$DR_TRP) result = "Yellow"
      if (DR < input$DR_LRP) result = "Red"
    }
    table = data.frame(matrix(NA,nrow=1,ncol=7))
    
    colnames(table) = c("Site",
                        "Species",
                        "year",
                        "Density_Ratio",
                        "TRP_Density_Ratio",
                        "LRP_Density_Ratio",
                        "Result_Density_Ratio")

    table[1,] <- c(input$siteSelection,
                   input$speciesSelection,
                   max(densityDF$Year),
                   DR,
                   input$DR_TRP,
                   input$DR_LRP,
                   result)
    table
    
  })
  
  LBARInput <- reactive({
    
    df <- lengthData()
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    
    
    #if (input$gearGlobalSelection == "Aggregate Across Gear Types") g = 1 else g = length(as.vector(unique(df$gear)))
    #if (input$yearGlobalSelection == "Aggregate Across Years") y = 1 else y = length(as.vector(unique(df$year)))
    g = 1
    y = 1
    table = data.frame(matrix(NA,nrow=g*y,ncol=13))
    
    colnames(table) = c("Site",
                        "Species",
                        "gear",
                        "year",
                        "Sample Size",
                        "L_c",
                        "LBAR",
                        "Z",
                        "F",
                        "FvM",
                        "TRP_FvM",
                        "LRP_FvM",
                        "Result_FvM")
    
    for (i in 1:g){
      
      for (j in 1:y){
        
        if (input$gearGlobalSelection != "Aggregate Across Gear Types") {
          dfSubset = subset(df,gear==input$gearGlobalSelection)
          gearLabel = input$gearGlobalSelection
        } else {
          dfSubset = df
          gearLabel = "Aggregate Across Gear Types"
        }
        
        if (input$yearGlobalSelection != "Aggregate Across Years") {
          dfSubset = subset(dfSubset,year==input$yearGlobalSelection)
          yearLabel = input$yearGlobalSelection
        } else {
          dfSubset = dfSubset
          yearLabel = "Aggregate Across Years"
        }
        
        lengthData = dfSubset$length_cm[!is.na(dfSubset$length_cm)]
        sampleSize = length(lengthData)
        lcSubset = Mode(lengthData)
        
        lbar = LBAR(lcSubset,input$Linf,input$k,input$M,lengthData)$lbar
        Z = LBAR(lcSubset,input$Linf,input$k,input$M,lengthData)$Z
        F = LBAR(lcSubset,input$Linf,input$k,input$M,lengthData)$F
        FvM = LBAR(lcSubset,input$Linf,input$k,input$M,lengthData)$FvM
        
        if (is.nan(FvM) | is.infinite(FvM)) result = "Cannot Interpret" else{
          if (FvM < input$FvMLBAR_TRP) result = "Green"
          if (FvM > input$FvMLBAR_TRP & FvM<input$FvMLBAR_LRP) result = "Yellow"
          if (FvM > input$FvMLBAR_LRP) result = "Red"
        }
        
        if (i==1 & j ==1) k = 1 else k = k+1
        
        table[k,] = c(input$siteSelection,
                      input$speciesSelection,
                      gearLabel,
                      yearLabel,
                      sampleSize,
                      lcSubset,
                      lbar,
                      Z,
                      F,
                      FvM,
                      input$FvMLBAR_TRP,
                      input$FvMLBAR_LRP,
                      result)
      }
    }
    
    if(input$gearGlobalSelection != "Look at All Gear Types") tableSubset = table[table$"gear" == input$gearGlobalSelection,] else tableSubset = table
    if(input$yearGlobalSelection != "Look at All Years") tableSubset = tableSubset[table$"year" == input$yearGlobalSelection,] else tableSubset = tableSubset
    
    tableSubset = tableSubset[rowSums(is.na(tableSubset)) != ncol(tableSubset),]
    tableSubset
    
  })
  
  CCInput <- reactive({
    
    df <- lengthData()
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    
    
    #if (input$gearGlobalSelection == "Aggregate Across Gear Types") g = 1 else g = length(as.vector(unique(df$gear)))
    #if (input$yearGlobalSelection == "Aggregate Across Years") y = 1 else y = length(as.vector(unique(df$year)))
    g = 1
    y = 1
    table = data.frame(matrix(NA,nrow=g*y,ncol=13))
    
    colnames(table) = c("Site",
                        "Species",
                        "gear",
                        "year",
                        "Sample Size",
                        "SL50",
                        "SL95",
                        "Z",
                        "F",
                        "FvM",
                        "FvMCC_TRP",
                        "FvMCC_LRP",
                        "Result_CC")
    
    for (i in 1:g){
      
      for (j in 1:y){
        
        if (input$gearGlobalSelection != "Aggregate Across Gear Types") {
          dfSubset = subset(df,gear==input$gearGlobalSelection)
          gearLabel = input$gearGlobalSelection
        } else {
          dfSubset = df
          gearLabel = "Aggregate Across Gear Types"
        }
        
        if (input$yearGlobalSelection != "Aggregate Across Years") {
          dfSubset = subset(dfSubset,year==input$yearGlobalSelection)
          yearLabel = input$yearGlobalSelection
        } else {
          dfSubset = dfSubset %>%
            mutate(year = as.numeric(format(Sys.Date(), "%Y")))
          
          yearLabel = "Aggregate Across Years"
        }
        
        lengthData = dfSubset$length_cm[!is.na(dfSubset$length_cm)]
        sampleSize = length(lengthData)
        dfSubset <- dfSubset %>%
          mutate(Date = ymd(paste(year,"01","01",sep="-")))

        lfq_dat <- lfqCreate(dfSubset,Lname = "length_cm", Dname = "Date", aggregate_dates = FALSE,
                             length_unit = "cm", bin_size = 1, plot=FALSE) %>%
          lfqModify()
        
        lfq_dat$Linf <- input$Linf
        lfq_dat$K <- input$k
        lfq_dat$t0 <- input$t0
        
        # t0 <- lfq_dat$t0
        # K <-lfq_dat$K
        # Linf <- lfq_dat$Linf
        # #browser()
        # midLengths <- lfq_dat$midLengths
        # interval <- midLengths[2] - midLengths[1]
        # lowerLengths <- midLengths - (interval / 2)
        # t_L1 <- (t0 - (1/K)) * log(1 - (lowerLengths / Linf))
        # 
        # dt <- rep(NA,length(midLengths))
        # for(x1 in 1:(length(dt)-1)){
        #   dt[x1] <- t_L1[x1+1] - t_L1[x1]
        # }
        # ln_Linf_L <- log(Linf - lowerLengths)
        # t_midL <- (t0 - (1/K)) * log(1 - (midLengths / Linf))
        # catch <- lfq_dat$catch
        # lnC <- log(catch)
        # lnC_dt <- log(catch / dt)
        # lnC_dt[which(lnC_dt == -Inf | is.nan(lnC_dt))] <- NA
        catch <- lfq_dat$catch
        catch<-rowSums(catch)
        cutMin <- which(catch == max(catch,na.rm=TRUE))
        catchCut <- catch[seq(cutMin,length(catch))]
        cutMax <- ifelse(0 %in% catchCut,
                         cutMin + which(catchCut == 0)[1] - 2,
                         length(catch))
        #browser()
        #lnC_dt <- rowSums(lnC_dt,na.rm=TRUE)

        ccOutputs <- catchCurve(lfq_dat,
                                catch_columns = 1,
                                calc_ogive = TRUE,
                                cumulative = FALSE,
                                reg_int = c(cutMin,cutMax))
        
        #browser()
        
        Z = ccOutputs$Z
        F = ccOutputs$Z - input$M
        FvM = F / input$M
        S50 = ccOutputs$L50
        S95 = ccOutputs$L95
        
        if (is.nan(FvM) | is.infinite(FvM)) result = "Cannot Interpret" else{
          if (FvM < input$FvMCC_TRP) result = "Green"
          if (FvM > input$FvMCC_TRP & FvM<input$FvMCC_LRP) result = "Yellow"
          if (FvM > input$FvMCC_LRP) result = "Red"
        }
        
        if (i==1 & j ==1) k = 1 else k = k+1

        table[k,] = c(input$siteSelection,
                      input$speciesSelection,
                      gearLabel,
                      yearLabel,
                      sampleSize,
                      S50,
                      S95,
                      Z,
                      F,
                      FvM,
                      input$FvMCC_TRP,
                      input$FvMCC_LRP,
                      result)
        return(list(table = table,
                    ccOutputs = ccOutputs))
      }
    }
    
    if(input$gearGlobalSelection != "Look at All Gear Types") tableSubset = table[table$"gear" == input$gearGlobalSelection,] else tableSubset = table
    if(input$yearGlobalSelection != "Look at All Years") tableSubset = tableSubset[table$"year" == input$yearGlobalSelection,] else tableSubset = tableSubset
    
    tableSubset = tableSubset[rowSums(is.na(tableSubset)) != ncol(tableSubset),]
    tableSubset
    
  })
  
  lengthFDInput <- reactive({
    
    df <- lengthData()
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    
    
    #if (input$gearGlobalSelection == "Aggregate Across Gear Types") g = 1 else g = length(as.vector(unique(df$gear)))
    #if (input$yearGlobalSelection == "Aggregate Across Years") y = 1 else y = length(as.vector(unique(df$year)))
    g = 1
    y = 1
    table = data.frame(matrix(NA,nrow=g*y,ncol=9))
    
    colnames(table) = c("Site",
                        "Species",
                        "gear",
                        "year",
                        "Sample Size",
                        "Average_Length_FD",
                        "Average_Length_FD_TRP",
                        "Average_Length_FD_LRP",
                        "Average_Length_FD_Result")
    
    for (i in 1:g){
      
      for (j in 1:y){
        
        if (input$gearGlobalSelection != "Aggregate Across Gear Types") {
          dfSubset = subset(df,gear==input$gearGlobalSelection)
          gearLabel = input$gearGlobalSelection
        } else {
          dfSubset = df
          gearLabel = "Aggregate Across Gear Types"
        }
        
        if (input$yearGlobalSelection != "Aggregate Across Years") {
          dfSubset = subset(dfSubset,year==input$yearGlobalSelection)
          yearLabel = input$yearGlobalSelection
        } else {
          dfSubset = dfSubset
          yearLabel = "Aggregate Across Years"
        }
        
        lengthData = dfSubset$length_cm[!is.na(dfSubset$length_cm)]
        sampleSize = length(lengthData)
        averageLengthFD = mean(lengthData)
        
        if (is.nan(averageLengthFD) | is.infinite(averageLengthFD)) result = "Cannot Interpret" else{
          if (averageLengthFD > input$lengthFD_TRP) result = "Green"
          if (averageLengthFD < input$lengthFD_TRP & averageLengthFD>input$lengthFD_LRP) result = "Yellow"
          if (averageLengthFD < input$lengthFD_LRP) result = "Red"
        }
        
        if (i==1 & j ==1) k = 1 else k = k+1
        
        table[k,] = c(input$siteSelection,
                      input$speciesSelection,
                      gearLabel,
                      yearLabel,
                      sampleSize,
                      averageLengthFD,
                      input$lengthFD_TRP,
                      input$lengthFD_LRP,
                      result)
      }
    }
    
    if(input$gearGlobalSelection != "Look at All Gear Types") tableSubset = table[table$"gear" == input$gearGlobalSelection,] else tableSubset = table
    if(input$yearGlobalSelection != "Look at All Years") tableSubset = tableSubset[table$"year" == input$yearGlobalSelection,] else tableSubset = tableSubset
    
    tableSubset = tableSubset[rowSums(is.na(tableSubset)) != ncol(tableSubset),]
    tableSubset
    
  })
  
  FroeseInput <- reactive({
    
    df <- lengthData()
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    
    #if (input$gearGlobalSelection == "Aggregate Across Gear Types") g = 1 else g = length(as.vector(unique(df$gear)))
    #if (input$yearGlobalSelection == "Aggregate Across Years") y = 1 else y = length(as.vector(unique(df$year)))
    g = 1
    y = 1
    table = data.frame(matrix(NA,nrow=g*y,ncol=20))
    
    colnames(table) = c("Site",
                        "Species",
                        "gear",
                        "year",
                        "Sample Size",
                        "L_Mature",
                        "L_Opt",
                        "L_Mega",
                        "Percent_Mature",
                        "Percent_Opt",
                        "PercentMega",
                        "TRP_Percent_Mature",
                        "LRP_Percent_Mature",
                        "TRP_Percent_Opt",
                        "LRP_Percent_Opt",
                        "TRP_Percent_Mega",
                        "LRP_Percent_Mega",
                        "Result_Mature",
                        "Result_Opt",
                        "Result_Mega")
    
    for (i in 1:g){
      
      for (j in 1:y){
        
        if (input$gearGlobalSelection != "Aggregate Across Gear Types") {
          dfSubset = subset(df,gear==input$gearGlobalSelection[i])
          gearLabel = input$gearGlobalSelection
        } else {
          dfSubset = df
          gearLabel = "Aggregate Across Gear Types"
        }
        
        if (input$yearGlobalSelection != "Aggregate Across Years") {
          dfSubset = subset(dfSubset,year==input$yearGlobalSelection)
          yearLabel = input$yearGlobalSelection
        } else {
          dfSubset = dfSubset
          yearLabel = "Aggregate Across Years"
        }
        
        lengthData = dfSubset$length_cm[!is.na(dfSubset$length_cm)]
        sampleSize = length(lengthData)
        
        Lmat = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,input$m95,lengthData)$Lmat
        Lopt = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,input$m95,lengthData)$Lopt
        Lmega = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,input$m95,lengthData)$Lmega
        percentMature = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,input$m95,lengthData)$percentMature
        percentOpt = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,input$m95,lengthData)$percentOpt
        percentMega = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,input$m95,lengthData)$percentMega
        
        
        if (is.nan(percentMature) | is.infinite(percentMature)) resultMature = "Cannot Interpret" else{
          if (percentMature < input$percentMature_TRP & percentMature>input$FvMLBAR_LRP) resultMature = "Yellow"
          if (percentMature <= input$percentMature_LRP) resultMature = "Red"
          if (percentMature >= input$percentMature_TRP) resultMature = "Green"
        }
        
        if (is.nan(percentOpt) | is.infinite(percentOpt)) resultOpt = "Cannot Interpret" else{
          if (percentOpt < input$percentOptimal_TRP & percentOpt>input$percentOptimal_LRP) resultOpt = "Yellow"
          if (percentOpt <= input$percentOptimal_LRP) resultOpt = "Red"
          if (percentOpt >= input$percentOptimal_TRP) resultOpt = "Green"
        }
        
        if (is.nan(percentMega) | is.infinite(percentMega)) resultMega = "Cannot Interpret" else{
          if (percentMega < input$percentMega_TRP & percentMega>input$percentMega_LRP) resultMega = "Yellow"
          if (percentMega <= input$percentMega_LRP) resultMega = "Red"
          if (percentMega >= input$percentMega_TRP) resultMega = "Green"
        }
        
        if (i==1 & j ==1) k = 1 else k = k+1
        
        table[k,] = c(input$siteSelection,
                      input$speciesSelection,
                      gearLabel,
                      yearLabel,
                      sampleSize,
                      Lmat,
                      Lopt,
                      Lmega,
                      percentMature,
                      percentOpt,
                      percentMega,
                      input$percentMature_TRP,
                      input$percentMature_LRP,
                      input$percentOptimal_TRP,
                      input$percentOptimal_LRP,
                      input$percentMega_TRP,
                      input$percentMega_LRP,
                      resultMature,
                      resultOpt,
                      resultMega)
      }
    }
    
    if(input$gearGlobalSelection != "Look at All Gear Types") tableSubset = table[table$"gear" == input$gearGlobalSelection,] else tableSubset = table
    if(input$yearGlobalSelection != "Look at All Years") tableSubset = tableSubset[table$"year" == input$yearGlobalSelection,] else tableSubset = tableSubset
    
    tableSubset = tableSubset[rowSums(is.na(tableSubset)) != ncol(tableSubset),]
    tableSubset
    
  })
  
  landingsInput <- reactive({
    
    table = data.frame(matrix(NA,nrow=1,ncol=6))
    table <- table[-1,]
    
    
    catchRelative <- plotCPUE()$catchRelativeFinal

        if (is.nan(catchRelative) | is.infinite(catchRelative)) result = "Cannot Interpret" else{
          if (catchRelative > input$landings_TRP) result = "Green"
          if (catchRelative < input$landings_TRP & catchRelative>input$landings_LRP) result = "Yellow"
          if (catchRelative < input$landings_LRP) result = "Red"
        }
        
       
        
        newRow = c(input$siteSelection,
                      input$speciesSelection,
                      catchRelative,
                      input$landings_TRP,
                      input$landings_LRP,
                      result)
        
        table <- rbind(table,newRow)
        colnames(table) <- c("Site",
                             "Species",
                             "Change_In_Landings",
                             "landings_TRP",
                             "landings_LRP",
                             "Result_Landings")
        table
      })
  
  cpueInput <- reactive({
    
    table = data.frame(matrix(NA,nrow=1,ncol=6))
    table <- table[-1,]
    
    
    cpueRelative <- plotCPUE()$cpueRelativeFinal
    
    if (is.nan(cpueRelative) | is.infinite(cpueRelative)) result = "Cannot Interpret" else{
      if (cpueRelative > input$CPUE_TRP) result = "Green"
      if (cpueRelative < input$CPUE_TRP & cpueRelative>input$CPUE_LRP) result = "Yellow"
      if (cpueRelative < input$CPUE_LRP) result = "Red"
    }
    
    
    
    newRow = c(input$siteSelection,
               input$speciesSelection,
               cpueRelative,
               input$CPUE_TRP,
               input$CPUE_LRP,
               result)
    
    table <- rbind(table,newRow)
    colnames(table) <- c("Site",
                         "Species",
                         "Change_In_CPUE",
                         "CPUE_TRP",
                         "CPUE_LRP",
                         "Result_CPUE")
    table
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("LF_Plot_",input$siteSelection,"_",input$speciesSelection,"_",input$gearGlobalSelection,"_",input$yearGlobalSelection,".pdf", sep=""))
    },
    content = function(file) {
      pdf(file)
      print(plotInput())
      dev.off()
    }) 
  
  output$downloadLBAR <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("LBAR_Results_",input$siteSelection,"_",input$speciesSelection,"_",input$gearGlobalSelection,"_",input$yearGlobalSelection,".csv", sep=""))
    },
    content = function(file) {
      write.csv(LBARInput(),file)
    }) 
  
  output$downloadCC <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("CC_Results_",input$siteSelection,"_",input$speciesSelection,"_",input$gearGlobalSelection,"_",input$yearGlobalSelection,".csv", sep=""))
    },
    content = function(file) {
      write.csv(CCInput()$table,file)
    }) 
  
  output$downloadSPR <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("SPR_Results_",input$siteSelection,"_",input$speciesSelection,"_",input$gearGlobalSelection,"_",input$yearGlobalSelection,".csv", sep=""))
    },
    content = function(file) {
      write.csv(SPRInput()$table,file)
    }) 
  
  output$downloadLengthFD <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("averageLengthFD_Results_",input$siteSelection,"_",input$speciesSelection,"_",input$gearGlobalSelection,"_",input$yearGlobalSelection,".csv", sep=""))
    },
    content = function(file) {
      write.csv(lengthFDInput(),file)
    }) 
  
  output$downloadFroese <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("Froese_Results_",input$siteSelection,"_",input$speciesSelection,"_",input$gearGlobalSelection,"_",input$yearGlobalSelection,".csv", sep=""))
    },
    content = function(file) {
      write.csv(FroeseInput(),file)
    }) 
  
  output$downloadLBARPlot <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("LBAR_Plot_",input$siteSelection,"_",input$speciesSelection,"_",input$gearGlobalSelection,"_",input$yearGlobalSelection,".pdf", sep=""))
    },
    content = function(file) {
      pdf(file)
      print(plotLBAR())
      dev.off()
    }) 
  
  output$summaryDocDownload <- downloadHandler(
    filename = function() {
      paste("AFAM_Summary",input$siteSelection,input$speciesSelection,input$gearGlobalSelection,input$yearGlobalSelection, sep = '.', switch(
        input$format,HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      #src <- normalizePath('AFAMSummary.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      #owd <- setwd(tempdir())
      #on.exit(setwd(owd))
      #file.copy(src, 'AFAMSummary.Rmd')
      #file.copy(src, 'indicatorTable.csv')
      
      library(rmarkdown)
      out <- render('www/AFAMSummary.rmd', switch(
        input$format,
        HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    })
  
  outputOptions(output, "summaryDocDownload", suspendWhenHidden=FALSE)
  
  output$downloadLandingsPlot <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("Landings_Plot_",input$siteSelection,"_",input$speciesSelection,"_",input$gearGlobalSelection,"_",input$yearGlobalSelection,".pdf", sep=""))
    },
    content = function(file) {
      pdf(file)
      print(plotCPUE()$plots)
      dev.off()
    }) 
  
  
  
  output$downloadLHI <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("LHI_",input$siteSelection,"_",input$speciesSelection,"_",input$gearGlobalSelection,"_",input$yearGlobalSelection,".csv", sep=""))
    },
    content = function(file) {
      write.csv(LHIInput(),file)
    }) 
  
  output$downloadSummary <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("Summary_Results_",input$siteSelection,"_",input$speciesSelection,"_",input$gearGlobalSelection,"_",input$yearGlobalSelection,".csv", sep=""))
    },
    content = function(file) {
      write.csv(summaryInput(),file)
    }) 
  
  output$HCRUI <- renderUI({
    List = c(input$indicatorLEKSelection,input$indicatorLengthSelection,input$indicatorLandingsSelection,input$indicatorUnderwaterSelection)
    if(length(List) == 0) return()
    conMat = expand.grid(rep(list(c("Green","Yellow")),length(List)))
    for (i in 1:nrow(conMat)){
      if (length(List) == 1){
        if (i ==1) newHCR = paste(List," is ",t(apply(conMat, 1, paste0))[1,],".",sep="",collapse="")
        if (i ==2) newHCR = paste(List," is ",rev(t(apply(conMat, 1, paste0))[1,]),".",sep="",collapse="")
      } else newHCR = paste(List," is ",t(apply(conMat, 1, paste0))[i,],".",sep="",collapse="")
      if (i==1) fullList = newHCR else fullList = c(fullList,newHCR)
    }
    limits = paste(List," is Red",sep="")
    fullList = c(fullList,limits)
    if (is.null(List)) return(NULL)
    
    LL <- vector("list",length(fullList))        
    for(i in 1:length(fullList)){
      LL[[i]] <- list(
        textInput(inputId = paste0("HCR",fullList[i]), label = fullList[i]),
        hr())
    }
    return(LL)
  })
  
  output$IntepretationUI <- renderUI({
    List = c(input$indicatorLEKSelection,input$indicatorLengthSelection,input$indicatorLandingsSelection,input$indicatorUnderwaterSelection)
    if(length(List) == 0) return()
    conMat = expand.grid(rep(list(c("Green","Yellow")),length(List)))
    for (i in 1:nrow(conMat)){
      if (length(List) == 1){
        if (i ==1) newHCR = paste(List," is ",t(apply(conMat, 1, paste0))[1,],".",sep="",collapse="")
        if (i ==2) newHCR = paste(List," is ",rev(t(apply(conMat, 1, paste0))[1,]),".",sep="",collapse="")
      } else newHCR = paste(List," is ",t(apply(conMat, 1, paste0))[i,],".",sep="",collapse="")
      if (i==1) fullList = newHCR else fullList = c(fullList,newHCR)}
    limits = paste(List," is Red",sep="")
    fullList = c(fullList,limits)
    
    if (is.null(List)) return(NULL)
    
    LL <- vector("list",length(fullList))        
    for(i in 1:length(fullList)){
      LL[[i]] <- list(
        textInput(inputId = paste0("Interpretation",fullList[i]), label = fullList[i]),
        hr())
    }
    return(LL)
  })
  

  output$HCRTriggerUI <- renderUI({
    List = c(input$indicatorLEKSelection,input$indicatorLengthSelection,input$indicatorLandingsSelection,input$indicatorUnderwaterSelection)
    conMat = expand.grid(rep(list(c("Green","Yellow")),length(List)))
    for (i in 1:nrow(conMat)){
      if (length(List) == 1){
        if (i ==1) newHCR = paste(List," is ",t(apply(conMat, 1, paste0))[1,],".",sep="",collapse="")
        if (i ==2) newHCR = paste(List," is ",rev(t(apply(conMat, 1, paste0))[1,]),".",sep="",collapse="")
      } else newHCR = paste(List," is ",t(apply(conMat, 1, paste0))[i,],".",sep="",collapse="")
      if (i==1) fullList = newHCR else fullList = c(fullList,newHCR)
    }
    limits = paste(List," is Red",sep="")
    fullList = c(fullList,limits)
    
    if (is.null(List)) return(NULL)
    
    LL <- list(selectInput(inputId = "selectedResult", 
                           label = h4("Select assessment result:"), 
                           choices = fullList, 
                           selected = NULL))
    
    return(LL)
  })
  
  output$InterpretationTriggerText <- renderText({
    paste("Likely interpretation (defined in Step 4): ",eval(parse(text = paste0("input$\"Interpretation",input$selectedResult,"\""))))
  })
  
  output$HCRTriggerText <-renderText({
    paste("Harvest control rule to trigger (defined in Step 4): ",eval(parse(text = paste0("input$\"HCR",input$selectedResult,"\""))))
  })
  
  output$renderSummary <- renderUI({
    tagList(
      rmarkdown::render("AFAMSummary.rmd"),
      inclRmd("AFAMSummary.html")
    )
  })
  
  observeEvent(input$report,
               tagList(
                 rmarkdown::render("www/AFAMSummary.rmd"),
                 inclRmd("www/AFAMSummary.html")
               )
  )
  
  #output$helpFile2<-renderUI({getPage("help/2_overview.md")})
  output$helpFile3<-renderUI({getPage("help/3_Step1.md")})
  output$helpFile4<-renderUI({getPage("help/4_Step2.html")})
  output$helpFile5<-renderUI({getPage("help/5_Step3.html")})
  output$helpFile6<-renderUI({getPage("help/6_Step4.html")})
  output$helpFile7<-renderUI({getPage("help/7_Step5.html")})
  output$helpFile8<-renderUI({getPage("help/8_Step6.html")})
  output$helpFile9<-renderUI({getPage("help/9_Step7.html")})
  output$helpFile10<-renderUI({getPage("help/10_Step8.html")})
  
})

LBAR = function(Lc,Linf,k,M,lfDist){
  lbar = mean(lfDist[lfDist>=Lc & lfDist<Linf])
  Z = k*(Linf-lbar)/(lbar-Lc)
  F = Z - M
  return(list(Z=Z,
              F=F,
              FvM=F/M,
              lbar=lbar))
}

Mode <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

froese = function(l_inf,M,k,t0,wa,wb,m50,m95,lengthData){
  
  n0 = 1000
  lengthVec = seq(1,l_inf)
  ageVec = t0 - log(1-lengthVec/l_inf)/k
  weightVec = wa * lengthVec ^ wb
  
  nVec = vector()
  
  for (i in 1:length(ageVec)){
    nVec[i] = n0 * exp(-M*ageVec[i])
  }
  
  biomassVec = nVec * weightVec
  #Lopt = which(biomassVec == max(biomassVec))
  Lopt = l_inf * 3 / (3 + M / k)
  Lopt_lower = Lopt * 0.9
  Lopt_upper = Lopt * 1.1
  Lmega = Lopt * 1.1
  Lmat = m95
  percentMature = length(which(lengthData>Lmat)) / length(lengthData) * 100
  percentOpt = length(which(lengthData>Lopt_lower & lengthData<Lopt_upper)) / length(lengthData) * 100
  percentMega = length(which(lengthData>Lmega)) / length(lengthData) * 100
  
  return(list(percentMature = percentMature,
              percentOpt = percentOpt,
              percentMega = percentMega,
              Lopt = Lopt,
              Lopt_lower = Lopt_lower,
              Lopt_upper = Lopt_upper,
              Lmega = Lmega,
              Lmat = m95))}

inclRmd <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = '\n') %>%
    knitr::knit2html(text = ., fragment.only = TRUE, options = "",
                     stylesheet=file.path(r_path,"../www/empty.css")) %>%
    gsub("&lt;!--/html_preserve--&gt;","",.) %>%
    gsub("&lt;!--html_preserve--&gt;","",.) %>%
    HTML %>%
    withMathJax
}

getPage<-function(name) {
  return(includeMarkdown(rmarkdown::render(name)))
}