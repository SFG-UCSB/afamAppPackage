# ui.R
shinyUI(fluidPage(
  navbarPage(("AFAM Toolkit Dashboard"),
             tabPanel("Instructions",
                      fluidRow(
                        includeMarkdown("www/introduction.md"),
                        helpText(a(h1("Click for help!"), href=paste(savedURL,"toolkit-overview.html",sep=""),target="_blank"))
                        )

             ),
             tabPanel("Step 1 – Upload data, select species, and determine assessment and management tier",
                      sidebarLayout(
                        sidebarPanel(
                          #helpText(a(h1("Click for help!"), href="_book/Step1.html",target="_blank")),
                          helpText(a(h1("Click for help!"), href=paste(savedURL,"Step1.html",sep=""),target="_blank")),
                          h4("Instructions: Select available data types, upload data (you may choose either real or sample data), select site, and select species for analysis. A data summary will be shown on the right. Your assessment and management tier will also be automatically calculated."),
                          checkboxGroupInput("checkDataGroup", label = "Select the available data types", 
                                             choices = list("Local Ecological Knowledge" = "dataLEK", "Length composition data" = "dataLength", "Landings and Effort Data" = "landingsData","Underwater Visual Survey Data" = "underwaterData"),
                                             selected = "dataLEK"),
                          hr(),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLength')!=-1 | input.checkDataGroup.indexOf('landingsData')!=-1",
                            checkboxInput("insideArea",label="Only include data inside designated area, such as a TURF?",value=TRUE)
                          ),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLength')!=-1",
                            numericInput("sizeMax",label="Enter the largest feasible size that should be observed in the catch. Sizes above this will be removed as outliers. Leave this as -999 if you do not believe there are any outliers to remove.",value=-999)
                          ),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('landingsData')!=-1",
                            numericInput("tripMax",label="Enter the largest feasible catch for a single trip. Catches in a single trip above this will be removed as outliers. Leave this as -999 if you do not believe there are any outliers to remove.",value=-999),
                            checkboxInput("totalCatch",label="Does the catch and effort data represent the total landings in the fishery? If so, check this box. If the data is only a sub-sample, leave this box is un-checked, and the dashboard will normalize catch and effort by the number of sampling days.",value=FALSE)
                          ),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLength') != -1 | input.checkDataGroup.indexOf('landingsData') != -1 | input.checkDataGroup.indexOf('underwaterData') != -1",
                            selectInput("dataType",label="Do you wish to use a real data set or a sample data set?",choices=c("Use sample Data","Use Real Data"),selected="Use sample Data"),
                            conditionalPanel(
                              condition = "input.checkDataGroup.indexOf('dataLength') != -1 & input.dataType == 'Use Real Data'",
                              fileInput("data",label = "Upload Length Data. Make sure your input *.csv has the following column headers: site, year, species, gear, length_cm,inside_area,"),
                              hr()
                            ),
                            conditionalPanel(
                              condition = "input.checkDataGroup.indexOf('landingsData') != -1 & input.dataType == 'Use Real Data'",
                              fileInput("dataLandings",label = "Upload Landings Data. Make sure your landings input *.csv has the following column headers: site, year, date, fisher_days, inside_area, permanent_trip_id, gear, species, sampled_catch, total_catch"),
                              hr()
                            ),
                            conditionalPanel(
                              condition = "input.checkDataGroup.indexOf('underwaterData') != -1 & input.dataType == 'Use Real Data'",
                              fileInput("dataUnderwater",label = "Upload Underwater Visual Survey Data. Make sure your landings input *.csv has the following column headers: site, year, date,species,length,count"),
                              hr()
                            )
                          ),
                          conditionalPanel(
                            condition = "(input.checkDataGroup.indexOf('dataLength') != -1 | input.checkDataGroup.indexOf('landingsData') != -1 | input.checkDataGroup.indexOf('underwaterData') != -1) & input.dataType == 'Use sample Data'",
                            ("We would like to thank Wildlife Conservation Society and Karimunjawa National Park, Indonesia for providing the sample data included in the dashboard. We encourage you to use the data to better learn the functionality of the dashboard, but please contact Gavin McDonald (gmcdonald@bren.ucsb.edu) before using the data for any other purposes."),
                            hr()
                          ),
                          selectInput("countrySelection",label="Select country",choices=c("Indonesia","Philippines","Brazil")),
                          uiOutput("siteUI"),
                          uiOutput("speciesUI"),
                          uiOutput("gearGlobalUI"),
                          uiOutput("yearGlobalUI")
                        ),
                        mainPanel(
                          h3("Below is a summary of your available data, your assessment and management tier (automatically calculated based on the available data), and tables of raw data (either sample data or real data)"),
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
                          helpText(a(h1("Click for help!"), href=paste(savedURL,"Step2.html",sep=""),target="_blank")),
                          h4("Instructions: Select your fisheries management control(s). This list is automatically updated based on your assessment and management tier."),
                          uiOutput("fmcUI")
                        ),
                        mainPanel(

                        ))),
             tabPanel("Step 3 – Select performance indicators and reference points",
                      sidebarLayout(
                        sidebarPanel(
                          helpText(a(h1("Click for help!"), href=paste(savedURL,"Step3.html",sep=""),target="_blank")),
                          h4("Instructions: First, if you have any length, landings, or underwwater visual survey data, please answer the following questions. The answers will help determine what assessment options are available. Once you've answered these questions, you will select your assessments below."),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLength') != -1",
                            uiOutput("indicatorLengthChecks")),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('landingsData') != -1",
                            uiOutput("indicatorLandingsChecks")),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('underwaterData') != -1",
                            uiOutput("indicatorUnderwaterChecks")),
                          h4("Next, select at least one performance indicator from each data stream. The list will be automatically populated based on your available data, assessment and management tier, and the answers to the above questions. On the right, enter target reference points (TRPs) and limit reference points (LRPs) for each indicator. Default reference points are provided, but you may choose to update these based on species, local ecological knowledge, and community goals."),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLEK') != -1 &
                            (input.checkDataGroup.indexOf('dataLength') == -1 &
                            input.checkDataGroup.indexOf('landingsData') == -1 &
                            input.checkDataGroup.indexOf('underwaterData') == -1)",
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
                                     h4("Percentage change in Total Landings from reference period (0% represents stable)")
                              ),
                              column(4,
                                     numericInput("landings_TRP", label = NULL, value = 0)
                              ),
                              column(4,
                                     numericInput("landings_LRP", label = NULL, value = -50))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorLandingsSelection && input.checkDataGroup.indexOf('landingsData') != -1 && input.indicatorLandingsSelection.indexOf('CPUE') != -1",
                            fixedRow(
                              column(4,
                                     h4("Percentage change in CPUE from reference period (0% represents stable)")
                              ),
                              column(4,
                                     numericInput("CPUE_TRP", label = NULL, value = 0)
                              ),
                              column(4,
                                     numericInput("CPUE_LRP", label = NULL, value = -50))
                            ),hr()),
                          conditionalPanel(
                            condition = "input.checkDataGroup && input.indicatorUnderwaterSelection && input.checkDataGroup.indexOf('underwaterData') != -1 && input.indicatorUnderwaterSelection.indexOf('Density Ratio (Target Species)') != -1",
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
                            condition = "input.checkDataGroup && input.indicatorUnderwaterSelection && input.checkDataGroup.indexOf('underwaterData') != -1 && input.indicatorUnderwaterSelection.indexOf('Biomass Ratio (aggregated across species)') != -1",
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
                          helpText(a(h1("Click for help!"), href=paste(savedURL,"Step4.html",sep=""),target="_blank")),
                          h4("Instructions: For each possible assessment result, write out the likely interpretations for what would cause that result, and also specify the harvest control rule that will be triggered if any of the interpretations happen. The harvest control rule should make some adjustment to the fishery management control(s) selected in Step 2.")
                          ),
                        mainPanel(
                          fixedRow(
                            column(6,h3("Enter one or more likely interpretations for each possible assessment result")),
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
                                 helpText(a(h1("Click for help!"), href=paste(savedURL,"Step5.html",sep=""),target="_blank")),
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
                                                  h4("Instructions: If your species and country is already included in life history database, the life history parameters will be automatically populated below. A number of key target species found in the Philippines, Indonesia, and Brazil are included for your convenience. Ensure that these parameters look reasonable, and double-check that the references come from a similar geographic location and ecosystem. If the species and country is not included in the database, or if certain parameters and references aren't appropriate, you will need to enter these parameters manually, along with their references. Ensure parameters use consistent length and weight units that match your data (i.e., centimeters and grams). Once the table on the right has been populated, you may save this for your records using the button below."),
                                                  uiOutput("speciesUIText"),
                                                  uiOutput("commonUIText"),
                                                  uiOutput("codeUI"),
                                                  uiOutput("linfUI"),
                                                  uiOutput("linfMetaUI"),
                                                  uiOutput("kUI"),
                                                  uiOutput("kMetaUI"),
                                                  uiOutput("t0UI"),
                                                  uiOutput("t0MetaUI"),
                                                  uiOutput("mUI"),
                                                  uiOutput("mMetaUI"),
                                                  uiOutput("waUI"),
                                                  uiOutput("waMetaUI"),
                                                  uiOutput("wbUI"),
                                                  uiOutput("wbMetaUI"),
                                                  uiOutput("m50UI"),
                                                  uiOutput("m50MetaUI"),
                                                  uiOutput("m95UI"),
                                                  uiOutput("m95MetaUI"),
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
                                                tags$head(tags$style("#histogram{height:60vh !important;}"))
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
                                                  h4("L_Mature, L_Optimal, L_Mega, Percent Mature, Percent Optimal, and Percent Megaspawner have been calculated"),
                                                  DT::dataTableOutput("Froese"))
                                              )
                                            )
                                   ),
                                   # tabPanel("Average Length (Fishery Dependent)",
                                   #          sidebarLayout(
                                   #            sidebarPanel(
                                   #              conditionalPanel(
                                   #                condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Average Length (Fishery Dependent)') != -1",
                                   #                h4("Instructions: Average length will be automatically calculated using the life history information provided and year and gear selected."),
                                   #                downloadButton("downloadLengthFD",label="Download Average Length (Fishery Dependent) Results (CSV)")
                                   #              )),
                                   #            mainPanel(
                                   #              conditionalPanel(
                                   #                condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Average Length (Fishery Dependent)') != -1",
                                   #                h3("Average Length (Fishery Dependent) is calculated automatically."),
                                   #                DT::dataTableOutput("lengthFD")
                                   #              )
                                   #            )
                                   #          )),
                                   tabPanel("Fishing Mortality / Natural Mortality Indicator (LBAR)",
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Fishing Mortality / Natural Mortality (LBAR)') != -1",
                                                  h4("Instructions: The results for the LBAR method will be automatically calculated using the life history information provided and data visualizations performed."),
                                                  downloadButton("downloadLBAR",label="Download LBAR Results (CSV)")
                                                  #downloadButton("downloadLBARPlot",label="Download LBAR Results (Plot)"))
                                              )),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Fishing Mortality / Natural Mortality (LBAR)') != -1",
                                                  h2("LBAR Model Outputs"),
                                                  h4("LBAR, Z, F, and F/M have been calculated"),
                                                  DT::dataTableOutput("LBAR")
                                                  #plotOutput("LBARBox",height=400,width=400))
                                              )
                                            )
                                   )),
                                   tabPanel("Fishing Mortality / Natural Mortality (Catch Curve)",
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Fishing Mortality / Natural Mortality (Catch Curve)') != -1",
                                                  h4("Instructions: Hit the Go! button to begin the analysis. A window should pop up, where you should select the beginning and end points over which the catch curve should be calcualted. The beginning point should be the length at full selectivity. The end point should be the last point in the data set before there are any zero measurements. You may find consulting the length histogram helpful. The results for the catch curve method will then be calculated. You may re-do the analysis by pressing the Go! button again."),
                                                  actionButton("goButton", "Go!"),
                                                  downloadButton("downloadCC",label="Download Catch Curve Results (CSV)"),
                                                  h5(a("This calculation uses the TropFishR package.", href="https://github.com/tokami/TropFishR",target="_blank"))
                                                  #downloadButton("downloadCCPlot",label="Download LBAR Results (Plot)"))
                                              )),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Fishing Mortality / Natural Mortality (Catch Curve)') != -1",
                                                  h2("Catch Curve Model Outputs"),
                                                  h4("Z, F, and F/M have been calculated"),
                                                  DT::dataTableOutput("CC")
                                                  #plotOutput("LBARBox",height=400,width=400))
                                                )
                                              )
                                            )),
                                   tabPanel("Spawning Potential Ratio (SPR)",
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Spawning Potential Ratio (SPR)') != -1",
                                                  h4("Instructions: Enter the 3 parameters below, and SPR will be automatically calcualted. If you did the Catch curve method, you can get SL95 and SL50 from the output table for that. If not, you can use the length histogram. SL95 should be the mode of the histogram. SL50 should be the length that has roughly half the number of counts as the mode."),
                                                  numericInput(inputId = "FM",label="Enter the F/M ratio calculated from either Catch Curve or LBAR.",min=0,max=10,value=NULL),
                                                  numericInput(inputId = "SL95",label="Enter the Length at 95% selectivity.",min=0,value=NULL),
                                                  numericInput(inputId = "SL50",label="Enter the Length at 50% selectivity.",min=0,value=NULL),
                                                  downloadButton("downloadSPR",label="Download SPR Results (CSV)"),
                                                  h5(a("This calculation uses the LBSPR package.", href="https://github.com/AdrianHordyk/LBSPR",target="_blank"))
                                                  #downloadButton("downloadSPRPlot",label="Download SPR Results (Plot)"))
                                              )),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Spawning Potential Ratio (SPR)') != -1",
                                                  h2("SPR Model Outputs"),
                                                  h4("SPR been calculated using the provided inputs"),
                                                  DT::dataTableOutput("SPR")
                                                  #plotOutput("SPRBox",height=400,width=400))
                                              )
                                            )
                                   )
                                 ))),
                        tabPanel("Landings-based Performance Indicators",
                                 tabsetPanel(
                                   tabPanel("Data Visualization",
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.checkDataGroup.indexOf('landingsData') != -1",
                                                 uiOutput("catchReference"),
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

                                              ),
                                              mainPanel(
                                                h3("Below are the results of the landings assessment."),
                                                DT::dataTableOutput("Landings")
                                              ))),
                                   tabPanel("CPUE Indicator",
                                            sidebarLayout(
                                              sidebarPanel(

                                              ),
                                              mainPanel(
                                                h3("Below are the results of the CPUE assessment."),
                                                DT::dataTableOutput("CPUE")
                                              )))

                                 )),
                        tabPanel("Underwater Visual Survey Performance Indicators",
                                 tabsetPanel(
                                   tabPanel("Biomass Ratio (aggregated across species)",
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorUnderwaterSelection && input.checkDataGroup.indexOf('underwaterData') != -1 && input.indicatorUnderwaterSelection.indexOf('Biomass Ratio (aggregated across species)') != -1",
                                                  numericInput(inputId="BR_PI",min=0,max=10,value=NULL,label="Instructions: Enter the aggregated biomass-ratio from the underwater visual survey data. This is the ratio of the total biomass (aggregated across all species) inside the no-take zone divided by the total biomass outside the no-take zone.")
                                                )),
                                              mainPanel(
                                                
                                              ))),
                                   tabPanel("Density Ratio (Target Species)",
                                            sidebarLayout(
                                              sidebarPanel(
                                              conditionalPanel(
                                                condition = "input.checkDataGroup && input.indicatorUnderwaterSelection && input.checkDataGroup.indexOf('underwaterData') != -1 && input.indicatorUnderwaterSelection.indexOf('Density Ratio (Target Species)') != -1",
                                                numericInput(inputId="DR_PI",min=0,max=10,value=NULL,label="Instructions: Enter the target species density-ratio from the underwater visual survey data. This is the ratio of the density (only for the target species) inside the no-take zone divided by the total density outside the no-take zone.")
                                              )),
                                              mainPanel(

                                              )))

                                 )))),
             tabPanel("Step 6: Interpret Assessment Results",
                      sidebarLayout(
                        sidebarPanel(
                          helpText(a(h1("Click for help!"), href=paste(savedURL,"Step6.html",sep=""),target="_blank")),
                          h4("Instructions: A summary of all assessment results is provided to the right. Look these over with your stakeholder working group, and record the group's interpretation of how the fishery is doing."),
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
                          helpText(a(h1("Click for help!"), href=paste(savedURL,"Step7.html",sep=""),target="_blank")),
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
                          helpText(a(h1("Click for help!"), href=paste(savedURL,"Step8.html",sep=""),target="_blank")),
                          h5("To get a copy of your AFAM report, first click the Generate Report button. After that, you can click the link below to open the report in a new browser tab."),
                          actionButton("report", "Generate Report"),
                          helpText(a(h2("Click for the report (after generating)"), href="AFAMSummary.html",target="_blank")),
                          hr(),
                          radioButtons('format', h5('You may also save a copy of the report. Select the format for your AFAM summary document:'), c('HTML', 'Word'),inline = TRUE),
                          downloadButton("summaryDocDownload", label = "Download AFAM summary document")
                        ),
                        mainPanel(
                          
                          #uiOutput("renderSummary")
                        )
                      )
             )
             
             )))