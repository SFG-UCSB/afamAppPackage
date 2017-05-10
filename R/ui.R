# ui.R
shinyUI(fluidPage(
  navbarPage(("AFAM Toolkit Dashboard"),
             tabPanel("Instructions",
                      fluidRow(
                        img(src="FF_Logo.png"),
                        includeMarkdown("data/instructions.rmd"))

             ),
             tabPanel("Step 1 – Upload data, select species, and determine assessment and management tier",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Instructions: Select available data types, upload data (you may choose either real or dummy data), select site, and select species for analysis. A data summary will be shown on the right. Your assessment and management tier will also be automatically calculated."),
                          checkboxGroupInput("checkDataGroup", label = "Select the available data types", 
                                             choices = list("Local Ecological Knowledge" = "dataLEK", "Length composition data" = "dataLength", "Landings and Effort Data" = "landingsData","Underwater Visual Survey Data" = "underwaterData"),
                                             selected = c("dataLEK","dataLength","landingsData")),
                          hr(),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLength') != -1 | input.checkDataGroup.indexOf('landingsData') != -1 | input.checkDataGroup.indexOf('underwaterData') != -1",
                            selectInput("dataType",label="Do you wish to use a real data set or a dummy data set?",choices=c("Use Dummy Data","Use Real Data"),selected="Use Dummy Data"),
                            conditionalPanel(
                              condition = "input.checkDataGroup.indexOf('dataLength') != -1 & input.dataType == 'Use Real Data'",
                              fileInput("data",label = "Upload Length Data. Make sure your input *.csv has the following column headers: site, year, species, gear, length_cm"),
                              hr()
                            ),
                            conditionalPanel(
                              condition = "input.checkDataGroup.indexOf('landingsData') != -1 & input.dataType == 'Use Real Data'",
                              fileInput("dataLandings",label = "Upload Landings Data. Make sure your landings input *.csv has the following column headers: site, year, date, annual_trip_id, hours, permanent_trip_id, gear, inside_TURF, species, sampled_catch, total_catch"),
                              hr()
                            ),
                            conditionalPanel(
                              condition = "input.checkDataGroup.indexOf('underwaterData') != -1 & input.dataType == 'Use Real Data'",
                              fileInput("dataUnderwater",label = "Upload Underwater Visual Survey Data. Make sure your landings input *.csv has the following column headers: site, year, date,species,length,count"),
                              hr()
                            )
                          ),
                          uiOutput("siteUI"),
                          uiOutput("speciesUI"),
                          uiOutput("gearGlobalUI"),
                          uiOutput("yearGlobalUI")
                        ),
                        mainPanel(
                          h3("Below is a summary of your available data, your assessment and management tier (automatically calculated based on the available data), and tables of raw data (either dummy data or real data)"),
                          h1("Data Summary"),
                          DT::dataTableOutput("renderTierTable"),
                          hr(),
                          h1("Assessment and Management  Tier"),
                          h3(textOutput("assessmentTier")),
                          hr(),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLength') != -1",
                            h1("Raw Length Data"),
                            DT::dataTableOutput("inputData"),
                            hr()
                          ),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('landingsData') != -1",
                            h1("Raw Landings and Effort Data"),
                            dataTableOutput("inputDataLandings"),
                            hr()
                          ),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('underwaterData') != -1",
                            h1("Raw Underwater Visual Survey Data"),
                            hr())
                        ))),
             tabPanel("Step 2 – Select fisheries management control(s)",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Instructions: Select your fisheries management control(s). This list is automatically updated based on your assessment and management tier."),
                          uiOutput("fmcUI")
                        ),
                        mainPanel(

                        ))),
             tabPanel("Step 3 – Select performance indicators and reference points",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Instructions: Select at least one performance indicator from each data stream. The list will be automatically populated based on your available data and assessment and management tier. On the right, enter target reference points (TRPs) and limit reference points (LRPs) for each indicator. Default reference points are provided, but you may choose to update these based on species, local ecological knowledge, and community goals."),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLEK') != -1",
                            uiOutput("indicatorLEKUI"),
                            hr()
                          ),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLength') != -1",
                            uiOutput("indicatorLengthUI"),
                            hr()
                          ),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('landingsData') != -1",
                            uiOutput("indicatorLandingsUI"),
                            hr()
                          ),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('underwaterData') != -1",
                            uiOutput("indicatorUnderwaterUI"),
                            hr())
                        ),
                        mainPanel(
                          fixedRow(
                            column(4,
                                   h3("Performance Indicator")
                            ),
                            column(4,
                                   h3("Target Reference Point (TRP)")
                            ),
                            column(4,
                                   h3("Limit Reference Point (LRP)")
                            )),
                          hr(),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLEKSelection && input.checkDataGroup.indexOf('dataLEK') != -1 && input.indicatorLEKSelection.indexOf('Presence of Destructive Fishing Gear') != -1",
                            fixedRow(
                              column(4,
                                     h4("Presence of Destructive Fishing Gear")
                              ),
                              column(4,
                                     textInput("destructive_TRP", label = NULL, value = "No destructive fishing practices being used")
                              ),
                              column(4,
                                     textInput("destructive_LRP", label = NULL, value = "Destructive fishing practices being used"))
                                     ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLEKSelection && input.checkDataGroup.indexOf('dataLEK') != -1 && input.indicatorLEKSelection.indexOf('Changes in Fishing Seasons') != -1",
                            fixedRow(
                              column(4,
                                     h4("Changes in Fishing Seasons")
                              ),
                              column(4,
                                     textInput("season_TRP", label = NULL, value = "No changes in the fishing season")
                              ),
                              column(4,
                                     textInput("season_LRP", label = NULL, value = "Increased variability in fishing season, or decreased fishing season"))
                                     ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLEKSelection && input.checkDataGroup.indexOf('dataLEK') != -1 && input.indicatorLEKSelection.indexOf('Changes in Target Species Composition') != -1",
                            fixedRow(
                              column(4,
                                     h4("Changes in Target Species Composition")
                              ),
                              column(4,
                                     textInput("composition_TRP", label = NULL, value = "No change in composition of caught species")
                              ),
                              column(4,
                                     textInput("composition_LRP", label = NULL, value = "Change in composition of caught species (fewer species, more pelagics) or loss of major fishing targets, predators, or grazers"))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLEKSelection && input.checkDataGroup.indexOf('dataLEK') != -1 && input.indicatorLEKSelection.indexOf('Target Species Vulnerability') != -1",
                            fixedRow(
                              column(4,
                                     h4("Target Species Vulnerability")
                              ),
                              column(4,
                                     textInput("vulnerability_TRP", label = NULL, value = "Low vulnerability estimate")
                              ),
                              column(4,
                                     textInput("vulnerability_LRP", label = NULL, value = "High vulnerability estimate"))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Average Length (Fishery Dependent)') != -1",
                            fixedRow(
                              column(4,
                                     h4("Average Length (Fishery Dependent)")
                              ),
                              column(4,
                                     numericInput("lengthFD_TRP", label = NULL, value = 45)
                              ),
                              column(4,
                                     numericInput("lengthFD_LRP", label = NULL, value = 30))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Fishing Mortality / Natural Mortality (Catch Curve)') != -1",
                            fixedRow(
                              column(4,
                                     h4("Fishing Mortality / Natural Mortality (Catch Curve)")
                              ),
                              column(4,
                                     numericInput("FvMCC_TRP", label = NULL, value = 1)
                              ),
                              column(4,
                                     numericInput("FvMCC_LRP", label = NULL, value = 2))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Fishing Mortality / Natural Mortality (LBAR)') != -1",
                            fixedRow(
                              column(4,
                                     h4("Fishing Mortality / Natural Mortality (LBAR)")
                              ),
                              column(4,
                                     numericInput("FvMLBAR_TRP", label = NULL, value = 1)
                              ),
                              column(4,
                                     numericInput("FvMLBAR_LRP", label = NULL, value = 2))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Spawning Potential Ratio (SPR)') != -1",
                            fixedRow(
                              column(4,
                                     h4("Spawning Potential Ratio (SPR)")
                              ),
                              column(4,
                                     numericInput("SPR_TRP", label = NULL, value = 0.4)
                              ),
                              column(4,
                                     numericInput("SPR_LRP", label = NULL, value = 0.2))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Froese Sustainability Indicators') != -1",
                            fixedRow(
                              column(4,
                                     h4("Froese Percent Optimal")
                              ),
                              column(4,
                                     numericInput("percentOptimal_TRP", label = NULL, value = 100)
                              ),
                              column(4,
                                     numericInput("percentOptimal_LRP", label = NULL, value = 80))
                            ),hr(),
                            fixedRow(
                              column(4,
                                     h4("Froese Percent Mature")
                              ),
                              column(4,
                                     numericInput("percentMature_TRP", label = NULL, value = 90)
                              ),
                              column(4,
                                     numericInput("percentMature_LRP", label = NULL, value = 50))
                            ),hr(),
                            fixedRow(
                              column(4,
                                     h4("Froese Percent Megaspawner")
                              ),
                              column(4,
                                     numericInput("percentMega_TRP", label = NULL, value = 20)
                              ),
                              column(4,
                                     numericInput("percentMega_LRP", label = NULL, value = 30))
                            ),hr(),
                            fixedRow(
                              column(4,
                                     h4("Froese Indicators Aggregated")
                              ),
                              column(4,
                                     textInput("froese_TRP", label = NULL, value = "Green")
                              ),
                              column(4,
                                     textInput("froese_LRP", label = NULL, value = "Red"))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLandingsSelection && input.checkDataGroup.indexOf('landingsData') != -1 && input.indicatorLandingsSelection.indexOf('Total Landings') != -1",
                            fixedRow(
                              column(4,
                                     h4("Total Landings")
                              ),
                              column(4,
                                     textInput("landings_TRP", label = NULL, value = "Increasing or stabilized total landings")
                              ),
                              column(4,
                                     textInput("landings_LRP", label = NULL, value = "Rapidly decreasing total landings"))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLandingsSelection && input.checkDataGroup.indexOf('landingsData') != -1 && input.indicatorLandingsSelection.indexOf('CPUE') != -1",
                            fixedRow(
                              column(4,
                                     h4("CPUE")
                              ),
                              column(4,
                                     textInput("CPUE_TRP", label = NULL, value = "Increasing or stabilized CPUE")
                              ),
                              column(4,
                                     textInput("CPUE_LRP", label = NULL, value = "Rapidly decreasing CPUE"))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorUnderwaterSelection && input.checkDataGroup.indexOf('underwaterData') != -1 && input.indicatorUnderwaterSelection.indexOf('Fished:Unfished Density Ratio (Target Species)') != -1",
                            fixedRow(
                              column(4,
                                     h4("Fished:Unfished Density Ratio (Target Species)")
                              ),
                              column(4,
                                     numericInput("DR_TRP", label = NULL, value = 0.6)
                              ),
                              column(4,
                                     numericInput("DR_LRP", label = NULL, value = 0.4))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorUnderwaterSelection && input.checkDataGroup.indexOf('underwaterData') != -1 && input.indicatorUnderwaterSelection.indexOf('Fished:Unfished Biomass Ratio (coral reef threshold aggregated across species)') != -1",
                            fixedRow(
                              column(4,
                                     h4("Fished:Unfished Biomass Ratio (coral reef threshold aggregated across species)")
                              ),
                              column(4,
                                     numericInput("BR_TRP", label = NULL, value = 0.5)
                              ),
                              column(4,
                                     numericInput("BR_LRP", label = NULL, value = 0.25))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorUnderwaterSelection && input.checkDataGroup.indexOf('underwaterData') != -1 && input.indicatorUnderwaterSelection.indexOf('Average Length (Fishery Independent)') != -1",
                            fixedRow(
                              column(4,
                                     h4("Average Length (Fishery Independent)")
                              ),
                              column(4,
                                     textInput("lengthFI_TRP", label = NULL, value = "Increaseing or stabilized in average length")
                              ),
                              column(4,
                                     textInput("lengthFI_LRP", label = NULL, value = "Decreasing in average length"))
                            ),hr())
                            ))),
             tabPanel("Step 4: Define Harvest Control Rules",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Instructions: For each possible assessment result, write out the likely interpretation for what would cause that result, and also specify the harvest control rule that will be triggered if that interpretation is validated. The harvest control rule should make some adjustment to the fishery management control(s) selected in Step 2.")
                        ),
                        mainPanel(
                          fixedRow(
                            column(6,h3("Enter a likely interpretation for each possible assessment result")),
                            column(6,h3("Enter a management response for each possible interpretation"))),
                          hr(),
                          fixedRow(
                            column(6,uiOutput("IntepretationUI")),
                            column(6,uiOutput("HCRUI"))
                          )
                        ))),
             tabPanel("Step 5: Perform Assessment Techniques",
                      tabsetPanel(
                        tabPanel("Step 5 Instructions",
                                 h4("Instructions: There are tabs for each type of performance indicator (sorted by data type). Under each tab, there is a sub-tab for each performance indicator and associated assessment technique. Assessment tabs are left blank if the corresponding performance indicator was not selected during Step 3. If assessment techniques are available, complete them moving left to right.")
                        ),
                        tabPanel("Local Ecological Knowledge Indicators",
                                 tabsetPanel(
                                   tabPanel("Presence of Destructive Fishing Gear Indicator",
                                            sidebarLayout(
                                              sidebarPanel(
                                                uiOutput("destructiveUI")
                                              ),
                                              mainPanel(
                                              ))),
                                   tabPanel("Changes in Fishing Seasons Indicator",
                                            sidebarLayout(
                                              sidebarPanel(
                                                uiOutput("seasonsUI")
                                              ),
                                              mainPanel(
                                              ))),
                                   tabPanel("Changes in Target Species Composition Indicator",
                                            sidebarLayout(
                                              sidebarPanel(
                                                uiOutput("compositionUI")
                                              ),
                                              mainPanel(
                                              ))),
                                   tabPanel("Target Species Vulnerability Indicator",
                                            sidebarLayout(
                                              sidebarPanel(
                                                uiOutput("vulnerabilityUI")
                                              ),
                                              mainPanel(
                                              )))
                                 )),
                        tabPanel("Length-Based Performance Indicators",
                                 tabsetPanel(
                                   tabPanel("Life History Information Input",
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.checkDataGroup.indexOf('dataLength') != -1",
                                                  h4("Instructions: Enter life history information for site and species selected in Step 1. Ensure parameters use consistent length and weight units (i.e., centimeters and grams)."),
                                                  numericInput("Linf", label = "L_Infinity (von Bertalannfy growth parameter)", value = 76.4),
                                                  numericInput("k", label = "k (von Bertalannfy growth parameter)", value = 0.09),
                                                  numericInput("M", label = "M (Natural Mortality)", value = 0.2083),
                                                  numericInput("t0", label = "t0 (von Bertalannfy growth parameter, theoretical age at length 0)", value = -5.7),
                                                  numericInput("w_a", label = "w_a (length-weight relationship parameter a)", value = 0.0000029138),
                                                  numericInput("w_b", label = "w_b (length-weight relationship parameter b)", value = 3.2697),
                                                  numericInput("m50", label = "m50 (Lengthat which 50% of individuals have reached maturity.)", value = 36.6),
                                                  downloadButton("downloadLHI",label="Download Life History Information"))
                                              ),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.checkDataGroup.indexOf('dataLength') != -1",
                                                  h2("Life History Information"),
                                                  DT::dataTableOutput("LHITable"))
                                              )
                                            )),
                                   tabPanel("Data Visualization",
                                            sidebarLayout(

                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.checkDataGroup.indexOf('dataLength') != -1",
                                                  sliderInput("binSize",label="Select Bin Size (cm)",min=1,max=10,value=1,width=200),
                                                  uiOutput("gearUI"),
                                                  uiOutput("yearUI"),
                                                  downloadButton("downloadPlot",label="Download Plot"))
                                              ),
                                              mainPanel(
                                                plotOutput("histogram"),
                                                tags$head(tags$style("#histogram{height:80vh !important;}"))
                                              )
                                            )
                                   ),
                                   tabPanel("Average Length (Fishery Dependent)",
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Average Length (Fishery Dependent)') != -1",
                                                  h4("Instructions: Average length will be automatically calculated using the life history information provided and year and gear selected."),
                                                  downloadButton("downloadLengthFD",label="Download Average Length (Fishery Dependent) Results (CSV)")
                                                )),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Average Length (Fishery Dependent)') != -1",
                                                  h3("Average Length (Fishery Dependent) is calculated automatically."),
                                                  DT::dataTableOutput("lengthFD")
                                                )
                                              )
                                            )),
                                   tabPanel("Fishing Mortality / Natural Mortality Indicator (LBAR)",
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Fishing Mortality / Natural Mortality (LBAR)') != -1",
                                                  h4("Instructions: The results for the LBAR method will be automatically calculated using the life history information provided and data visualizations performed."),
                                                  downloadButton("downloadLBAR",label="Download LBAR Results (CSV)"),
                                                  downloadButton("downloadLBARPlot",label="Download LBAR Results (Plot)"))
                                              ),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Fishing Mortality / Natural Mortality (LBAR)') != -1",
                                                  h2("LBAR Model Outputs"),
                                                  h4("LBAR, Z, F, and F/M have been calculated using the life history information provided in Tab 4 (L_inf, k, and M) along with the length at full selectivity (L_c), which is automatically calculated from the histograms in Tab 5"),
                                                  DT::dataTableOutput("LBAR"),
                                                  plotOutput("LBARBox",height=1600))
                                              )
                                            )
                                   ),
                                   tabPanel("Froese Sustainability Indicators",
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Froese Sustainability Indicators') != -1",
                                                  h4("Instructions: The results for the Froese Sustainability Indicators method will be automatically calculated using the life history information provided and data visualizations performed. Once this is done, select the aggregated assessment result below. Unless you have reason to believe otherwise, select the most critical of the 3 results (percent mature, percent optimal, and percent megaspawner)."),
                                                  selectInput(inputId = "froese_Result",label = "Select the aggregated assessment result from the Froese Sustainability Indicators technique:",choices=c("Green","Yellow","Red"),selected=NULL),
                                                  downloadButton("downloadFroese",label="Download Froese Results (CSV)"))
                                              ),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Froese Sustainability Indicators') != -1",
                                                  h2("Froese Model Outputs"),
                                                  h4("L_Mature, L_Optimal, L_Mega, Percent Mature, Percent Optimal, and Percent Megaspawner have been calculated automatically using the life history information and length data provided."),
                                                  DT::dataTableOutput("Froese"))
                                              )
                                            )
                                   )
                                 )),
                        tabPanel("Landings-based Performance Indicators",
                                 tabsetPanel(
                                   tabPanel("Data Visualization",
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.checkDataGroup.indexOf('landingsData') != -1",
                                                  #selectInput("landingsTiming", "Select the time interval over which to plot catch, effort, and CPUE data", choices=c("Monthly","Yearly"), selected = "Yearly"),
                                                  downloadButton("downloadLandingsPlot",label="Download landings, effort, and CPUE Results (Plot)"))
                                              ),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.checkDataGroup.indexOf('landingsData') != -1",
                                                  h2("Trends in catch, effort, and CPUE"),
                                                  plotOutput("CPUEPlots",height=1600))
                                              )
                                            )
                                   ),
                                   tabPanel("Total Landings Indicator",
                                            sidebarLayout(
                                              sidebarPanel(
                                                uiOutput("landingsUI")
                                              ),
                                              mainPanel(
                                              ))),
                                   tabPanel("CPUE Indicator",
                                            sidebarLayout(
                                              sidebarPanel(
                                                uiOutput("CPUEUI")
                                              ),
                                              mainPanel(
                                              )))

                                 )),
                        tabPanel("Underwater Visual Survey Performance Indicators",
                                 tabsetPanel(
                                   tabPanel("Data Visualization",
                                            sidebarLayout(
                                              sidebarPanel(
                                              ),
                                              mainPanel(
                                              )
                                            )
                                   ),
                                   tabPanel("Fished:Unfished Density Ratio (Target Species)",
                                            sidebarLayout(
                                              sidebarPanel(
                                              ),
                                              mainPanel(

                                              ))),
                                   tabPanel("Fished:Unfished Biomass Ratio (coral reef threshold aggregated across species)",
                                            sidebarLayout(
                                              sidebarPanel(

                                              ),
                                              mainPanel(

                                              ))),
                                   tabPanel("Average Length (Fishery Independent)",
                                            sidebarLayout(
                                              sidebarPanel(

                                              ),
                                              mainPanel(

                                              )))

                                 )))),
             tabPanel("Step 6: Interpret Assessment Results",
                      sidebarLayout(
                        sidebarPanel(
                          downloadButton("downloadSummary",label="Download Summary of Performance Indicators")
                        ),
                        mainPanel(
                          h2("Summary of All Performance Indicators"),
                          DT::dataTableOutput("summary"),
                          textInput("stakeholderInterpretation",label=h3("Based on this summary, what is your stakeholder group's interpretation of fishery performance?"))
                        )
                      )
             ),
             tabPanel("Step 7: Adjust fisheries management controls using defined harvest control rules",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Instructions: After interpreting your assessment results in Step 6, select the result below to determine which of your pre-defined harvest control rules should be triggered."),
                          uiOutput("HCRTriggerUI"),
                          verbatimTextOutput("InterpretationTriggerText"),
                          verbatimTextOutput("HCRTriggerText")
                        ),
                        mainPanel(

                        )
                      )
             ),
             tabPanel("Step 8: Complete your Fishery Management Plan",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons('format', 'Select the format for your AFAM summary document:', c('HTML', 'Word'),inline = TRUE),
                          downloadButton("summaryDocDownload", label = "Download AFAM summary document")
                        ),
                        mainPanel(
                          uiOutput("renderSummary")
                        )
                      )
             )
             
             )))