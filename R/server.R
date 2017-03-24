shinyServer(function(input, output) {
  
  lengthData <- reactive({
    if(is.null(input$dataType)) return(NULL)
    inFile <- input$data
    #
    if (is.null(inFile) & input$dataType != "Use Dummy Data")
      return(NULL)

    if (input$dataType == "Use Dummy Data") df = df_length else df = read_csv(inFile$datapath)
    df
  })

  catchData <- reactive({
    if(is.null(input$dataType)) return(NULL)
    inFile <- input$dataLandings

    if (is.null(inFile) & input$dataType != "Use Dummy Data")
      return(NULL)

    if (input$dataType == "Use Dummy Data") df = df_catch else df = read_csv(inFile$datapath)

    df

  })

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
      if (is.null(lengthData()) & input$dataType != "Use Dummy Data")  yearsLength = 0 else {
        df = lengthData()
        yearsLength = length(unique(df$year))}}
    
    df<-catchData()
    yearsLandings = length(unique(df$year))
    if(!dataAvailable[3]) yearsLandings = 0 else{
      if (is.null(catchData()) & input$dataType != "Use Dummy Data")  yearsLandings = 0 else {
        df = catchData()
        yearsLandings = length(unique(df$year))}}
    
    yearsUnderwater = 0
    
    
    
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
    table = data.frame(matrix(NA,nrow=1,ncol=9))
    
    colnames(table) = c("Site",
                        "Species",
                        "Linf",
                        "k",
                        "M",
                        "t0",
                        "w_a",
                        "w_b",
                        "m50")
    
    table[1,] = c(input$siteSelection,
                  input$speciesSelection,
                  input$Linf,
                  input$k,
                  input$M,
                  input$t0,
                  input$w_a,
                  input$w_b,
                  input$m50)
    
    table
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
  
  output$landingsUI <- renderUI({
    if (!("landingsData" %in% input$checkDataGroup) | !("Total Landings" %in% input$indicatorLandingsSelection)) return()
    TRP = input$landings_TRP
    LRP = input$landings_LRP
    selectInput(inputId ="landings_PI",label="Instructions: Based on the landings data visualization, enter the value for this performance indicator. The options available are automatically generated based on the TRP and LRP defined in Step 3.",choices=c(TRP,LRP),selected=NULL)
  })
  
  output$CPUEUI <- renderUI({
    if (!("landingsData" %in% input$checkDataGroup) | !("CPUE" %in% input$indicatorLandingsSelection)) return()
    TRP = input$CPUE_TRP
    LRP = input$CPUE_LRP
    selectInput(inputId ="CPUE_PI",label="Instructions: Based on the landings data visualization, enter the value for this performance indicator. The options available are automatically generated based on the TRP and LRP defined in Step 3.",choices=c(TRP,LRP),selected=NULL)
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
    #indicatorList = c("Average Length (Fishery Dependent)","Fishing Mortality / Natural Mortality (Catch Curve)","Fishing Mortality / Natural Mortality (LBAR)","Spawning Potential Ratio (SPR)","Froese Sustainability Indicators")
    indicatorList = c("Average Length (Fishery Dependent)","Fishing Mortality / Natural Mortality (LBAR)","Froese Sustainability Indicators")
    checkboxGroupInput(inputId = "indicatorLengthSelection",label = "Select at least one length-based performance indicator:",choices=indicatorList,selected="Average Length (Fishery Dependent)")
  })
  
  output$indicatorLandingsUI <- renderUI({
    if(is.null(tierTable()$tier)) return()
    if(tierTable()$tier < 3) return()
    indicatorList = c("Total Landings","CPUE")
    checkboxGroupInput(inputId = "indicatorLandingsSelection",label = "Select at least one landings-based performance indicator:",choices=indicatorList,selected="Total Landings")
  })
  
  output$indicatorUnderwaterUI <- renderUI({
    if(is.null(tierTable()$tier)) return()
    if(tierTable()$tier < 2) return()
    indicatorList = c("Fished:Unfished Density Ratio (Target Species)","Fished:Unfished Biomass Ratio (coral reef threshold aggregated across species)","Average Length (Fishery Independent)")
    checkboxGroupInput(inputId = "indicatorUnderwaterSelection",label = "Select at least one underwater-survey-based performance indicator:",choices=indicatorList,selected="Fished:Unfished Density Ratio (Target Species)")
  })
  
  output$gearUI <- renderUI({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection)) return()
    df <- lengthData()
    
    df = subset(df,species==input$speciesSelection)
    df = subset(df,site==input$siteSelection)
    gears = c("Look at All Gear Types","Aggregate Across Gear Types",as.vector(unique(df$gear)))
    selectInput(inputId="gearSelection","Select Gear Type for Visualization",choices=gears,selected="Look at All Gear Types")
  })
  
  output$gearGlobalUI <- renderUI({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(lengthData())) return()
    if (is.null(lengthData()) & input$dataType != "Use Dummy Data") {
      textInput(inputId="gearGlobalSelection","Enter Gear Type for Analysis")} else{
        
        df <- lengthData()
        
        df = subset(df,species==input$speciesSelection)
        df = subset(df,site==input$siteSelection)
        gears = c("Aggregate Across Gear Types",as.vector(unique(df$gear)))
        selectInput(inputId="gearGlobalSelection","Select Gear Type for Visualization and Analysis",choices=gears,selected="Look at All Gear Types")}
  })
  
  output$speciesUI <- renderUI({
    
    if (is.null(lengthData()) | is.null(input$dataType)) {
       textInput(inputId="speciesSelection","Enter Species for Analysis") } else{
        
        df <- lengthData()
        
        
        species = as.vector(unique(df$species))
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
    
    if (is.null(lengthData()) & input$dataType != "Use Dummy Data") return()
    
    df <- lengthData()
    
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    years = c("Look at All Years","Aggregate Across Years",as.vector(unique(df$year)))
    selectInput(inputId="yearSelection","Select Year for Visualization",choices=years,selected="Look at All Years")
  })
  
  output$yearGlobalUI <- renderUI({
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(lengthData())) return()
    
    if (is.null(lengthData()) & input$dataType != "Use Dummy Data") {
      textInput(inputId="yearGlobalSelection","Enter Year for Analysis") } else {
        
        df <- lengthData()
        
        df = subset(df,site==input$siteSelection)
        df = subset(df,species==input$speciesSelection)
        years = c("Aggregate Across Years",as.vector(unique(df$year)))
        selectInput(inputId="yearGlobalSelection","Select Year for Visualization and Analysis",choices=years,selected="Look at All Years")}
  })
  
  output$histogram <- renderPlot({
    print(plotInput())
  })
  
  plotInput <- function(){
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(input$gearSelection) | is.null(input$yearSelection) | is.null(lengthData())) return()
    # inFile <- input$data
    # 
    # if (is.null(inFile) & input$dataType != "Use Dummy Data")
    #   return(NULL)
    # 
    # if (input$dataType == "Use Dummy Data") df = df_length else df = read.csv(inFile$datapath,header=TRUE)
    df <- lengthData()
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    #froeseTemp = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,df$length_cm)
    
    if (input$gearSelection != "Aggregate Across Gear Types" & input$yearSelection != "Aggregate Across Years"){
      
      if(input$gearSelection != "Look at All Gear Types") df = subset(df,gear==input$gearSelection)
      if(input$yearSelection != "Look at All Years") df = subset(df,year==input$yearSelection)
      froeseTemp = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,df$length_cm)
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
              xlim(0,input$Linf) +
              theme_bw() +
              theme(text = element_text(size=20),
                    plot.title = element_text(size=20),
                    axis.title = element_text(size=25),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 25),
                    strip.text.y = element_text(size = 25)))
    }
    
    if (input$gearSelection == "Aggregate Across Gear Types" & input$yearSelection != "Aggregate Across Years"){
      
      if(input$yearSelection != "Look at All Years") df = subset(df,year==input$yearSelection)
      froeseTemp = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,df$length_cm)
      print(ggplot(df, aes(x=length_cm)) +
              geom_histogram(position="identity", binwidth=input$binSize,fill="#00ADB7",colour="black") +
              ggtitle(paste(paste("Site: ",input$siteSelection,sep=""),
                            paste("\nSpecies: ",input$speciesSelection,sep=""),
                            paste("\nGear Type: ",input$gearSelection,sep=""),      
                            paste("\nYear: ",input$yearSelection,sep=""),
                            paste("\nSize at maturity (red line): ",round(froeseTemp$Lmat,2),"; Percent mature in this figure: ",round(froeseTemp$percentMature,2),"%",sep=""),
                            paste("\nOptimal size (green line): ",round(froeseTemp$Lopt,2),"; Percent optimal in this figure: ",round(froeseTemp$percentOpt,2),"%",sep=""),
                            paste("\nMegaspawner size (blue line): ",round(froeseTemp$Lmega,2),"; Percent megaspawner in this figure: ",round(froeseTemp$percentMega,2),"%",sep=""))) +
              facet_grid(.~year) +
              xlab("Length (cm)") +
              ylab("Count") +
              geom_vline(xintercept = froeseTemp$Lmat,color="red",type=2) +
              geom_vline(xintercept = froeseTemp$Lopt,color="green",type=2) +
              geom_vline(xintercept = froeseTemp$Lmega,color="blue",type=2) +
              xlim(0,input$Linf) +
              theme_bw() +
              theme(text = element_text(size=20),
                    plot.title = element_text(size=20),
                    axis.title = element_text(size=25),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 25),
                    strip.text.y = element_text(size = 25)))
    }
    
    if (input$gearSelection != "Aggregate Across Gear Types" & input$yearSelection == "Aggregate Across Years"){
      
      if(input$gearSelection != "Look at All Gear Types") df = subset(df,gear==input$gearSelection)
      froeseTemp = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,df$length_cm)
      print(ggplot(df, aes(x=length_cm)) +
              geom_histogram(position="identity", binwidth=input$binSize,fill="#00ADB7",colour="black") +
              ggtitle(paste(paste("Site: ",input$siteSelection,sep=""),
                            paste("\nSpecies: ",input$speciesSelection,sep=""),
                            paste("\nGear Type: ",input$gearSelection,sep=""),      
                            paste("\nYear: ",input$yearSelection,sep=""),
                            paste("\nSize at maturity (red line): ",round(froeseTemp$Lmat,2),"; Percent mature in this figure: ",round(froeseTemp$percentMature,2),"%",sep=""),
                            paste("\nOptimal size (green line): ",round(froeseTemp$Lopt,2),"; Percent optimal in this figure: ",round(froeseTemp$percentOpt,2),"%",sep=""),
                            paste("\nMegaspawner size (blue line): ",round(froeseTemp$Lmega,2),"; Percent megaspawner in this figure: ",round(froeseTemp$percentMega,2),"%",sep=""))) +
              facet_grid(gear~.) +
              xlab("Length (cm)") +
              geom_vline(xintercept = froeseTemp$Lmat,color="red",type=2) +
              geom_vline(xintercept = froeseTemp$Lopt,color="green",type=2) +
              geom_vline(xintercept = froeseTemp$Lmega,color="blue",type=2) +
              ylab("Count") +
              xlim(0,input$Linf) +
              theme_bw() +
              theme(text = element_text(size=20),
                    plot.title = element_text(size=20),
                    axis.title = element_text(size=25),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 25),
                    strip.text.y = element_text(size = 25)))
    }
    
    if (input$gearSelection == "Aggregate Across Gear Types" & input$yearSelection == "Aggregate Across Years"){
      froeseTemp = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,df$length_cm)
      print(ggplot(df, aes(x=length_cm)) +
              geom_histogram(position="identity", binwidth=input$binSize,fill="#00ADB7",colour="black") +
              ggtitle(paste(paste("Site: ",input$siteSelection,sep=""),
                            paste("\nSpecies: ",input$speciesSelection,sep=""),
                            paste("\nGear Type: ",input$gearSelection,sep=""),      
                            paste("\nYear: ",input$yearSelection,sep=""),
                            paste("\nSize at maturity (red line): ",round(froeseTemp$Lmat,2),"; Percent mature in this figure: ",round(froeseTemp$percentMature,2),"%",sep=""),
                            paste("\nOptimal size (green line): ",round(froeseTemp$Lopt,2),"; Percent optimal in this figure: ",round(froeseTemp$percentOpt,2),"%",sep=""),
                            paste("\nMegaspawner size (blue line): ",round(froeseTemp$Lmega,2),"; Percent megaspawner in this figure: ",round(froeseTemp$percentMega,2),"%",sep=""))) +
              xlab("Length (cm)") +
              ylab("Count") +
              geom_vline(xintercept = froeseTemp$Lmat,color="red",type=2) +
              geom_vline(xintercept = froeseTemp$Lopt,color="green",type=2) +
              geom_vline(xintercept = froeseTemp$Lmega,color="blue",type=2) +
              xlim(0,input$Linf) +
              theme_bw() +
              theme(text = element_text(size=20),
                    plot.title = element_text(size=20),
                    axis.title = element_text(size=25),
                    strip.background = element_rect(fill="#EA883A"),
                    strip.text.x = element_text(size = 25),
                    strip.text.y = element_text(size = 25)))
    }
    
  }
  
  output$LBARBox <- renderPlot({
    print(plotLBAR())
  })
  
  output$CPUEPlots <- renderPlot({
    print(plotCPUE())
  })
  
  plotCPUE <- function(){
    if(is.null(input$speciesSelection) | is.null(input$siteSelection) | is.null(input$gearSelection) | is.null(input$yearSelection) | is.null(catchData())) return()
    df <- catchData()
    totalFN = function(x,y){
      if (is.na(y)) total = x else total = y
      return(total)
    }
    
    catchEffortData=df[,c("hours","sampled_catch","total_catch")] = lapply(df[,c("hours","sampled_catch","total_catch")],as.numeric,na.rm=TRUE)
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    #df$date <- as.Date(df$date,"%m/%d/%Y")
    #df$month <- as.Date(cut(df$date, breaks = "month"))
    dataSubset = subset(df,species==input$speciesSelection)
    
    
    dataSubset = dataSubset %>%
      rowwise() %>% 
      mutate(tripCatch = totalFN(sampled_catch,total_catch)) %>% 
      mutate(CPUE = tripCatch/hours)
    
    #if (input$landingsTiming == "Yearly"){
      
      effortPlot = ggplot(dataSubset,aes(year,hours)) +
        stat_summary(fun.y = sum, geom = "point",size=4,colour="#EA883A") +
        stat_summary(fun.y = sum, geom = "line",linetype=2,colour="#00ADB7") +
        theme_bw() +
        ylab("Total Annual Effort [hours]") +
        xlab("Year") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              text = element_text(size=20),
              plot.title = element_text(size=20),
              axis.title = element_text(size=25))
      
      landingsPlot = ggplot(dataSubset,aes(year,tripCatch)) +
        stat_summary(fun.y = sum, geom = "point",size=4,colour="#EA883A") +
        stat_summary(fun.y = sum, geom = "line",linetype=2,colour="#00ADB7") +
        theme_bw() +
        ylab("Total Annual Catch [kg]") +
        xlab("Year") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              text = element_text(size=20),
              plot.title = element_text(size=20),
              axis.title = element_text(size=25))
      
      CPUEPlot = ggplot(dataSubset,aes(year,CPUE)) +
        stat_summary(fun.y = median, geom = "point",size=4,colour="#EA883A") +
        stat_summary(fun.y = median, geom = "line",linetype=2,colour="#00ADB7") +
        theme_bw() +
        ylab("Median Annual CPUE [kg/hour]") +
        xlab("Year") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              text = element_text(size=20),
              plot.title = element_text(size=20),
              axis.title = element_text(size=25))
      
      grid.arrange(landingsPlot,effortPlot,CPUEPlot,nrow=3,top=paste(paste("Site: ",input$siteSelection,sep=""),
                                                                     paste("\nSpecies: ",input$speciesSelection,sep="")))
    # }
    # 
    # if (input$landingsTiming == "Monthly"){
    #   
    #   effortPlot = ggplot(dataSubset,aes(month,hours)) +
    #     stat_summary(fun.y = sum, geom = "point",size=3,colour="#EA883A") +
    #     stat_summary(fun.y = sum, geom = "line",linetype=2,colour="#00ADB7") +
    #     scale_x_date(labels = date_format("%Y-%m"),breaks = date_breaks("month")) +
    #     theme_bw() +
    #     ylab("Total Monthly Effort [hours]") +
    #     xlab("Month") +
    #     theme(axis.text.x = element_text(angle = 90, hjust = 1),
    #           text = element_text(size=20),
    #           plot.title = element_text(size=20),
    #           axis.title = element_text(size=25))
    #   
    #   landingsPlot = ggplot(dataSubset,aes(month,tripCatch)) +
    #     stat_summary(fun.y = sum, geom = "point",size=3,colour="#EA883A") +
    #     stat_summary(fun.y = sum, geom = "line",linetype=2,colour="#00ADB7") +
    #     scale_x_date(labels = date_format("%Y-%m"),breaks = date_breaks("month")) +
    #     theme_bw() +
    #     ylab("Total Monthly Catch [kg]") +
    #     xlab("Month") +
    #     theme(axis.text.x = element_text(angle = 90, hjust = 1),
    #           text = element_text(size=20),
    #           plot.title = element_text(size=20),
    #           axis.title = element_text(size=25))
    #   
    #   CPUEPlot = ggplot(dataSubset,aes(month,CPUE)) +
    #     stat_summary(fun.y = median, geom = "point",size=3,colour="#EA883A") +
    #     stat_summary(fun.y = median, geom = "line",linetype=2,colour="#00ADB7") +
    #     scale_x_date(labels = date_format("%Y-%m"),breaks = date_breaks("month")) +
    #     theme_bw() +
    #     ylab("Median Monthly CPUE [kg/hour]") +
    #     xlab("Month") +
    #     theme(axis.text.x = element_text(angle = 90, hjust = 1),
    #           text = element_text(size=20),
    #           plot.title = element_text(size=20),
    #           axis.title = element_text(size=25))
    #   
    #   grid.arrange(landingsPlot,effortPlot,CPUEPlot,nrow=3,top=paste(paste("Site: ",input$siteSelection,sep=""),
    #                                                                  paste("\nSpecies: ",input$speciesSelection,sep="")))
    # }
  }
  
  plotLBAR <- function(){
    
    df = LBARInput()
    
    df$FvM = as.numeric(df$FvM)
    
    maxY = max(df$FvM+1,input$FvMLBAR_LRP +1)
    minY = min(df$FvM - 1, input$FvMLBAR_TRP - 1)
    
    if (input$gearGlobalSelection != "Aggregate Across Gear Types" & input$yearGlobalSelection != "Aggregate Across Years"){
      
      if(input$gearGlobalSelection != "Look at All Gear Types") df = subset(df,gear==input$gearGlobalSelection)
      if(input$yearGlobalSelection != "Look at All Years") df = subset(df,year==input$yearSelection)
      
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
  
  output$lengthFD <- DT::renderDataTable(lengthFDInput())
  
  output$Froese <- DT::renderDataTable(FroeseInput())
  
  output$summary <-DT::renderDataTable(summaryInput())
  
  summaryInput<-reactive({
    summaryTable = data.frame(matrix(NA,nrow=1,ncol=7))
    
    summaryTable = summaryTable[-1,]
    
    
    
    if ("dataLEK" %in% input$checkDataGroup & "Presence of Destructive Fishing Gear" %in% input$indicatorLEKSelection){
      if (input$destructive_PI == input$destructive_TRP) result = "Green" else result = "Red"
      newRow = c("Presence of Destructive Fishing Gear",
                 input$siteSelection,
                 input$speciesSelection,
                 input$destructive_PI,
                 input$destructive_TRP,
                 input$destructive_LRP,
                 result)
      
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      names(newRow) = names(summaryTable)
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("dataLEK" %in% input$checkDataGroup & "Changes in Fishing Seasons" %in% input$indicatorLEKSelection){
      if (input$seasons_PI == input$season_TRP) result = "Green" else result = "Red"
      newRow = c("Changes in Fishing Seasons",
                 input$siteSelection,
                 input$speciesSelection,
                 input$seasons_PI,
                 input$season_TRP,
                 input$season_LRP,
                 result)
      
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      names(newRow) = names(summaryTable)
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("dataLEK" %in% input$checkDataGroup & "Changes in Target Species Composition" %in% input$indicatorLEKSelection){
      if (input$composition_PI == input$composition_TRP) result = "Green" else result = "Red"
      newRow = c("Changes in Target Species Composition",
                 input$siteSelection,
                 input$speciesSelection,
                 input$composition_PI,
                 input$composition_TRP,
                 input$composition_LRP,
                 result)
      
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      names(newRow) = names(summaryTable)
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("dataLEK" %in% input$checkDataGroup & "Target Species Vulnerability" %in% input$indicatorLEKSelection){
      if (input$vulnerability_PI == input$vulnerability_TRP) result = "Green" else result = "Red"
      newRow = c("Target Species Vulnerability",
                 input$siteSelection,
                 input$speciesSelection,
                 input$vulnerability_PI,
                 input$vulnerability_TRP,
                 input$vulnerability_LRP,
                 result)
      
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      names(newRow) = names(summaryTable)
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("landingsData" %in% input$checkDataGroup & "Total Landings" %in% input$indicatorLandingsSelection){
      if (input$landings_PI == input$landings_TRP) result = "Green" else result = "Red"
      newRow = c("Total Landings",
                 input$siteSelection,
                 input$speciesSelection,
                 input$landings_PI,
                 input$landings_TRP,
                 input$landings_LRP,
                 result)
      
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      names(newRow) = names(summaryTable)
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("landingsData" %in% input$checkDataGroup & "CPUE" %in% input$indicatorLandingsSelection){
      if (input$CPUE_PI == input$CPUE_TRP) result = "Green" else result = "Red"
      newRow = c("CPUE",
                 input$siteSelection,
                 input$speciesSelection,
                 input$CPUE_PI,
                 input$CPUE_TRP,
                 input$CPUE_LRP,
                 result)
      
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      names(newRow) = names(summaryTable)
      summaryTable = rbind(summaryTable,newRow)}
    
    
    
    if ("dataLength" %in% input$checkDataGroup & "Fishing Mortality / Natural Mortality (LBAR)" %in% input$indicatorLengthSelection) {
      newRow = c("LBAR",
                 LBARInput()[,c("Site",
                                "Species",
                                "FvM",
                                "TRP_FvM",
                                "LRP_FvM",
                                "Result_FvM")])
      
      newRow = data.frame(newRow,stringsAsFactors = FALSE)
      names(newRow) = names(summaryTable)
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("dataLength" %in% input$checkDataGroup & "Average Length (Fishery Dependent)" %in% input$indicatorLengthSelection) {
      newRow = c("Average Length (Fishery Dependent)",
                 lengthFDInput()[,c("Site",
                                    "Species",
                                    "Average_Length_FD",
                                    "Average_Length_FD_TRP",
                                    "Average_Length_FD_LRP",
                                    "Average_Length_FD_Result")])
      
      newRow = data.frame(newRow,stringsAsFactors = FALSE)
      names(newRow) = names(summaryTable)
      summaryTable = rbind(summaryTable,newRow)}
    
    if ("dataLength" %in% input$checkDataGroup & "Froese Sustainability Indicators" %in% input$indicatorLengthSelection) {
      newRow = c("Aggregated Froese Sustainability Indicators",
                 input$siteSelection,
                 input$speciesSelection,
                 input$froese_Result,
                 "Green",
                 "Red",
                 input$froese_Result)
      #       newRow = c("Froese Sustainability Indicators - Percent Mature",
      #                         FroeseInput()[,c("Site",
      #                                          "Species",
      #                                          "Percent_Mature",
      #                                          "TRP_Percent_Mature",
      #                                          "LRP_Percent_Mature",
      #                                          "Result_Mature")])
      #       newRow = data.frame(newRow,stringsAsFactors = FALSE)
      #       names(newRow) = names(summaryTable)
      #       summaryTable = rbind(summaryTable,newRow)
      #       
      #       newRow = c("Froese Sustainability Indicators - Percent Optimal",
      #                                      FroeseInput()[,c("Site",
      #                                                       "Species",
      #                                                       "Percent_Opt",
      #                                                       "TRP_Percent_Opt",
      #                                                       "LRP_Percent_Opt",
      #                                                       "Result_Opt")])
      #       newRow = data.frame(newRow,stringsAsFactors = FALSE)
      #       names(newRow) = names(summaryTable)
      #       summaryTable = rbind(summaryTable,newRow)
      #       
      #       newRow = c("Froese Sustainability Indicators - Percent Megaspawner",
      #                                      FroeseInput()[,c("Site",
      #                                                       "Species",
      #                                                       "PercentMega",
      #                                                       "TRP_Percent_Mega",
      #                                                       "LRP_Percent_Mega",
      #                                                       "Result_Mega")])
      
      newRow = t(data.frame(newRow,stringsAsFactors = FALSE))
      names(newRow) = names(summaryTable)
      summaryTable = rbind(summaryTable,newRow)}
    
    colnames(summaryTable) = c("Assessment",
                               "Site",
                               "Species",
                               "PI",
                               "TRP",
                               "LRP",
                               "Result")
    
    summaryTable
  })
  
  LBARInput <- reactive({
    
    df <- df_length
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    
    
    if (input$gearGlobalSelection == "Aggregate Across Gear Types") g = 1 else g = length(as.vector(unique(df$gear)))
    if (input$yearGlobalSelection == "Aggregate Across Years") y = 1 else y = length(as.vector(unique(df$year)))
    
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
          dfSubset = subset(df,gear==as.vector(unique(df$gear))[i])
          gearLabel = as.vector(unique(df$gear))[i]
        } else {
          dfSubset = df
          gearLabel = "Aggregate Across Gear Types"
        }
        
        if (input$yearGlobalSelection != "Aggregate Across Years") {
          dfSubset = subset(dfSubset,year==as.vector(unique(df$year))[j])
          yearLabel = as.vector(unique(df$year))[j]
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
  
  lengthFDInput <- reactive({
    
    df <- df_length
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    
    
    if (input$gearGlobalSelection == "Aggregate Across Gear Types") g = 1 else g = length(as.vector(unique(df$gear)))
    if (input$yearGlobalSelection == "Aggregate Across Years") y = 1 else y = length(as.vector(unique(df$year)))
    
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
          dfSubset = subset(df,gear==as.vector(unique(df$gear))[i])
          gearLabel = as.vector(unique(df$gear))[i]
        } else {
          dfSubset = df
          gearLabel = "Aggregate Across Gear Types"
        }
        
        if (input$yearGlobalSelection != "Aggregate Across Years") {
          dfSubset = subset(dfSubset,year==as.vector(unique(df$year))[j])
          yearLabel = as.vector(unique(df$year))[j]
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
    
    df <- df_length
    df = subset(df,site==input$siteSelection)
    df = subset(df,species==input$speciesSelection)
    
    if (input$gearGlobalSelection == "Aggregate Across Gear Types") g = 1 else g = length(as.vector(unique(df$gear)))
    if (input$yearGlobalSelection == "Aggregate Across Years") y = 1 else y = length(as.vector(unique(df$year)))
    
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
          dfSubset = subset(df,gear==as.vector(unique(df$gear))[i])
          gearLabel = as.vector(unique(df$gear))[i]
        } else {
          dfSubset = df
          gearLabel = "Aggregate Across Gear Types"
        }
        
        if (input$yearGlobalSelection != "Aggregate Across Years") {
          dfSubset = subset(dfSubset,year==as.vector(unique(df$year))[j])
          yearLabel = as.vector(unique(df$year))[j]
        } else {
          dfSubset = dfSubset
          yearLabel = "Aggregate Across Years"
        }
        
        lengthData = dfSubset$length_cm[!is.na(dfSubset$length_cm)]
        sampleSize = length(lengthData)
        
        Lmat = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,lengthData)$Lmat
        Lopt = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,lengthData)$Lopt
        Lmega = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,lengthData)$Lmega
        percentMature = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,lengthData)$percentMature
        percentOpt = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,lengthData)$percentOpt
        percentMega = froese(input$Linf,input$M,input$k,-input$t0,input$w_a,input$w_b,input$m50,lengthData)$percentMega
        
        
        if (is.nan(percentMature) | is.infinite(percentMature)) resultMature = "Cannot Interpret" else{
          if (percentMature < input$percentMature_TRP & percentMature>input$FvMLBAR_LRP) resultMature = "Yellow"
          if (percentMature < input$percentMature_LRP) resultMature = "Red"
          if (percentMature > input$percentMature_TRP) resultMature = "Green"
        }
        
        if (is.nan(percentOpt) | is.infinite(percentOpt)) resultOpt = "Cannot Interpret" else{
          if (percentOpt < input$percentOptimal_TRP & percentOpt>input$percentOptimal_LRP) resultOpt = "Yellow"
          if (percentOpt < input$percentOptimal_LRP) resultOpt = "Red"
          if (percentOpt > input$percentOptimal_TRP) resultOpt = "Green"
        }
        
        if (is.nan(percentMega) | is.infinite(percentMega)) resultMega = "Cannot Interpret" else{
          if (percentMega < input$percentMega_TRP & percentMega>input$percentMega_LRP) resultMega = "Yellow"
          if (percentMega < input$percentMega_LRP) resultMega = "Red"
          if (percentMega > input$percentMega_TRP) resultMega = "Green"
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
  
  output$downloadPlot <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("LF_Plot_",input$siteSelection,"_",input$speciesSelection,"_",input$gearSelection,"_",input$yearSelection,".pdf", sep=""))
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
      out <- render('AFAMSummary.rmd', switch(
        input$format,
        HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    })
  
  outputOptions(output, "summaryDocDownload", suspendWhenHidden=FALSE)
  
  output$downloadLandingsPlot <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("Landings_Plot_",input$siteSelection,"_",input$speciesSelection,"_",input$gearSelection,"_",input$yearSelection,".pdf", sep=""))
    },
    content = function(file) {
      pdf(file)
      print(plotCPUE())
      dev.off()
    }) 
  
  
  
  output$downloadLHI <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("LHI_",input$siteSelection,"_",input$speciesSelection,"_",input$gearSelection,"_",input$yearSelection,".csv", sep=""))
    },
    content = function(file) {
      write.csv(LHIInput(),file)
    }) 
  
  output$downloadSummary <- downloadHandler(
    filename = function(){
      gsub(" ", "", paste("Summary_Results_",input$siteSelection,"_",input$speciesSelection,"_",input$gearSelection,"_",input$yearSelection,".csv", sep=""))
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
  
  output$HCRListUI <- renderUI({
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
    
    LL <- vector("list",length(fullList))        
    for(i in 1:length(fullList)){
      LL[[i]] <- list(h6(paste(i,". ",fullList[i],sep="")),
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

froese = function(l_inf,M,k,t0,wa,wb,m50,lengthData){
  
  m95 = 1.14 * m50
  n0 = 1000
  lengthVec = seq(1,l_inf)
  ageVec = t0 - log(1-lengthVec/l_inf)/k
  weightVec = wa * lengthVec ^ wb
  
  nVec = vector()
  
  for (i in 1:length(ageVec)){
    nVec[i] = n0 * exp(-M*ageVec[i])
  }
  
  biomassVec = nVec * weightVec
  Lopt = which(biomassVec == max(biomassVec))
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