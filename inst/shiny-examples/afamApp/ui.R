# ui.R
shinyUI(fluidPage(
  navbarPage(("AFAM Toolkit Dashboard"),
             tabPanel("Instructions",
                      fluidRow(
                        includeMarkdown("www/introduction.md"),
                        helpText(a(h1("Click for help!"), href=paste(savedURL,"index.html",sep=""),target="_blank")),
                        helpText(a(h3("You may also download a PDF copy of the guidebook here."), href=paste(savedURL,"AFAM_Toolkit_Guidance_Document.pdf",sep=""),target="_blank")),
                        img(src = "_book/myMediaFolder/media/2_image1.png")
                        )

             ),
             tabPanel("Step 1 – Upload data, select species, and determine assessment and management tier",
                      sidebarLayout(
                        sidebarPanel(
                          #helpText(a(h1("Click for help!"), href="_book/Step1.html",target="_blank")),
                          helpText(a(h1("Click for help!"), href=paste(savedURL,"Step1.html",sep=""),target="_blank")),
                          h4("Instructions: Select available data types, upload data (you may choose either real or sample data), and enter additional information for the analysis such as the target species. A data summary will be shown on the right. Your assessment and management tier will also be automatically calculated."),
                          checkboxGroupInput("checkDataGroup", label = "Select the available data types", 
                                             choices = list("Local Ecological Knowledge" = "dataLEK", "Length composition data" = "dataLength", "Landings and Effort Data" = "landingsData","Underwater Visual Survey Data" = "underwaterData"),
                                             selected = "dataLEK"),
                          hr(),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLength') != -1 | input.checkDataGroup.indexOf('landingsData') != -1 | input.checkDataGroup.indexOf('underwaterData') != -1",
                            selectInput("dataType",label="Do you wish to use a real data set or a sample data set?",choices=c("Use sample Data","Use Real Data"),selected="Use Real Data"),
                            conditionalPanel(
                              condition = "input.checkDataGroup.indexOf('dataLength') != -1 & input.dataType == 'Use Real Data'",
                              downloadButton(outputId = "download_length_data_sample", label = "Download sample length data"),
                              fileInput("data",label = "Upload Length Data. Make sure your input *.csv has the following column headers: country, site, year, species, gear, length_cm, count (the number of measurements corresponding to a given length; if unsure, this value should be 1), inside_area (TRUE or FALSE, whether or not the measurement is from inside the fishing area of interest). Each row should represent an individual length measurement. You may also download a sample data set as an example."),
                              hr()
                            ),
                            conditionalPanel(
                              condition = "input.checkDataGroup.indexOf('landingsData') != -1 & input.dataType == 'Use Real Data'",
                              downloadButton(outputId = "download_landings_data_sample", label = "Download sample landings data"),
                              fileInput("dataLandings",label = "Upload Landings Data. Make sure your landings input *.csv has the following column headers: country, site, year, species, gear, inside_area (TRUE or FALSE, whether or not the measurement is from inside the fishing area of interest), fisher_days (the measure of effort), sampled_catch, total_catch. Each row should represent an individual catch sample. You may also download a sample data set as an example."),
                              hr()
                            ),
                            conditionalPanel(
                              condition = "input.checkDataGroup.indexOf('underwaterData') != -1 & input.dataType == 'Use Real Data'",
                              downloadButton(outputId = "download_uvc_eco_data_sample", label = "Download sample underwater visual survey ecosystem-level biomass data"),
                              fileInput("dataBiomass",label = "Upload Underwater Visual Survey ecosystem-level biomass data. Make sure your input *.csv has the following column headers: Year, country, site, species, Biomass_Fished (aggregated biomass in fished area, often in kg/Ha), and Biomass_Unfished (aggregated biomass in unfished area). You may also download a sample data set as an example."),
                              hr(),
                              downloadButton(outputId = "download_uvc_species_data_sample", label = "Download sample underwater visual survey species-level density data"),
                              fileInput("dataDensity",label = "Upload Underwater Visual Survey species-level density data. Make sure your input *.csv has the following column headers: Year, country, site, species, Density_Fished (abundance density in fished area, often in individuals/Ha), and Density_Unfished (abundance density in unfished area). You may also download a sample data set as an example."),
                              hr()
                            )
                          ),
                          conditionalPanel(
                            condition = "(input.checkDataGroup.indexOf('dataLength') != -1 | input.checkDataGroup.indexOf('landingsData') != -1 | input.checkDataGroup.indexOf('underwaterData') != -1) & input.dataType == 'Use sample Data'",
                            ("We would like to thank Wildlife Conservation Society, Karimunjawa National Park, Indonesia, and Rare for providing the sample data included in the dashboard. We encourage you to use the data to better learn the functionality of the dashboard, but please contact Gavin McDonald (gmcdonald@bren.ucsb.edu) before using the data for any other purposes."),
                            hr()
                          ),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLEK')!=-1",
                            numericInput("numYearsLEK",label="Enter the number of years for which you have local ecological knowledge",value=1)
                          ),
                          uiOutput("countryUI"),
                          uiOutput("siteUI"),
                          uiOutput("speciesUI"),
                          conditionalPanel(
                            condition = "input.checkDataGroup.indexOf('dataLength')!=-1 | input.checkDataGroup.indexOf('landingsData')!=-1",
                            checkboxInput("insideArea",label="Only include data inside designated area, such as a TURF?",value=TRUE)
                          )),
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
                            h1("Raw Length Data (already expanded using count column)"),
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
                            h2("Ecosystem-level aggregated biomass data"),
                            dataTableOutput("inputDataBiomass"),
                            h2("Species-level density data"),
                            dataTableOutput("inputDataDensity"),
                            hr())
                        ))),
             tabPanel("Step 2 – Select fisheries management control(s)",
                      sidebarLayout(
                        sidebarPanel(
                          helpText(a(h1("Click for help!"), href=paste(savedURL,"Step2.html",sep=""),target="_blank")),
                          h4("Instructions: Select your fisheries management control(s). This list is automatically updated based on your assessment and management tier."),
                          h4("Note: You may wish to come back to this step after looking at the data. For example, looking at the length histogram could help you determine if a size limit is appropriate."),
                          h4("Note: This is a record keeping step if you are going through the entire AFAM process. This step will also require significant input from stakeholders (see guidance document). You may skip this step if you simply wish to visualize and analyze your data."),
                          uiOutput("fmcUI")
                        ),
                        mainPanel(
                          img(src = "_book/myMediaFolder/media/FMCs.png")
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
                            # fixedRow(
                            #   column(4,
                            #          h4("Froese Percent Optimal")
                            #   ),
                            #   column(4,
                            #          numericInput("percentOptimal_TRP", label = NULL, value = 100)
                            #   ),
                            #   column(4,
                            #          numericInput("percentOptimal_LRP", label = NULL, value = 80))
                            # ),hr(),
                            # fixedRow(
                            #   column(4,
                            #          h4("Froese Percent Mature")
                            #   ),
                            #   column(4,
                            #          numericInput("percentMature_TRP", label = NULL, value = 90)
                            #   ),
                            #   column(4,
                            #          numericInput("percentMature_LRP", label = NULL, value = 50))
                            # ),hr(),
                            # fixedRow(
                            #   column(4,
                            #          h4("Froese Percent Megaspawner")
                            #   ),
                            #   column(4,
                            #          numericInput("percentMega_TRP", label = NULL, value = 20)
                            #   ),
                            #   column(4,
                            #          numericInput("percentMega_LRP", label = NULL, value = 30))
                            # ),hr(),
                            fixedRow(
                              column(4,
                                     h4("Froese Indicators Status")
                              ),
                              column(4,
                                     h4("Spawning biomass above reference point")
                              ),
                              column(4,
                                     h4("Spawning biomass below reference point"))
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
                          h4("Instructions: For each possible assessment result, write out the likely interpretations for what would cause that result, and also specify the harvest control rule that will be triggered if any of the interpretations happen. The harvest control rule should make some adjustment to the fishery management control(s) selected in Step 2."),
                          h4("Note: This is a record keeping step if you are going through the entire AFAM process. This step will also require significant input from stakeholders (see guidance document). You may skip this step if you simply wish to visualize and analyze your data.")
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
                                   tabPanel("Setup",
                                            sidebarLayout(
                                              sidebarPanel(
                                                numericInput("yearLEKSelection",label="Enter year for LEK analysis",value=as.numeric(format(Sys.Date(), "%Y")))
                                              ),
                                              mainPanel(
                                              ))),
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
                                                  h4("Instructions: If your species and country is already included in life history database, the life history parameters will be automatically populated below. A number of key target species found in the Philippines, Indonesia, Mozambique, and Brazil are included for your convenience. Ensure that these parameters look reasonable, and double-check that the references come from a similar geographic location and ecosystem. If the species and country is not included in the database, or if certain parameters and references aren't appropriate, you will need to enter these parameters manually, along with their references. Ensure parameters use consistent length and weight units that match your data (i.e., centimeters and grams). Once the table on the right has been populated, you may save this for your records using the button below. You may also download a copy of the complete database and species list to see what's currently included."),
                                                  h4("Note: Please ensure that your species scientific name is spelled correctly, or the dashboard may not be able to locate it in the life history database."),
                                                  downloadButton("downloadLHIFull",label="Download full database and species list"),
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
                                                  downloadButton("downloadLHI",label="Download Life History Information for this species"))
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
                                                  uiOutput("gearGlobalUI"),
                                                  uiOutput("yearGlobalUI"),
                                                  numericInput("sizeMax",label="Enter the largest feasible fish length that should be observed in the catch. Lengths above this will be removed as outliers. A good starting point is 1.3 * L_Infinity. Leave this as -999 if you do not believe there are any outliers to remove.",value=-999),
                                                  numericInput("sizeMin",label="Enter the smallest feasible fish length that should be observed in the catch. Lengths below this will be removed as outliers. Leave this as -999 if you do not believe there are any outliers to remove.",value=-999),
                                                  #uiOutput("gearUI"),
                                                  #uiOutput("yearUI"),
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
                                                  h4("Instructions: The results for the Froese Sustainability Indicators method will be automatically calculated using the life history information provided according to the Froese 2004 and Cope and Punt 2009 methedology."),
                                                  h5(a("Froese, R. (2004). Keep it simple: three indicators to deal with overfishing. Fish and fisheries, 5(1), 86-91.",href="http://onlinelibrary.wiley.com/doi/10.1111/j.1467-2979.2004.00144.x/full",target="_blank")),
                                                  h5(a("Cope and Punt 2009. Length‐Based Reference Points for Data‐Limited Situations: Applications and Restrictions. Marine and Coastal Fisheries 1.1 (2009): 169-186.",href="http://onlinelibrary.wiley.com/doi/10.1577/C08-025.1/full",target="_blank")),
                                                  #selectInput(inputId = "froese_Result",label = "Select the aggregated assessment result from the Froese Sustainability Indicators technique:",choices=c("Green","Yellow","Red"),selected=NULL),
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
                                                  h4("Instructions: The results for the LBAR method will be automatically calculated using the life history information provided using the Ault et al. 2005 methedology."),
                                                  h5(a("Ault, J. S., Smith, S. G., & Bohnsack, J. A. (2005). Evaluation of average length as an estimator of exploitation status for the Florida coral-reef fish community. ICES Journal of Marine Science: Journal du Conseil, 62(3), 417-423.",href="https://doi.org/10.1016/j.icesjms.2004.12.001",target="_blank")),
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
                                                  h4("Instructions: The catch curve method will be used to automatically calculate total mortality (Z), the ratio F/M, and selectivity parameters according to Sparre and Venama 1998 methedology. The linear regression will be automatically fit from the mode of the histogram to the largest non-zero length class in the measured catch."),
                                                  h5(a("Sparr, P., and Venema, S.C., 1998. Introduction to tropical fish stock assessment - Part 1: Manual. FAO Fish. Tech. Pap. 306.", href="http://www.fao.org/docrep/W5449E/W5449E00.htm",target="_blank")),
                                                  h5(a("This calculation uses the TropFishR R Package.", href="https://github.com/tokami/TropFishR",target="_blank")),
                                                  #actionButton("goButton", "Go!"),
                                                  downloadButton("downloadCC",label="Download Catch Curve Results (CSV)")
                                                  #downloadButton("downloadCCPlot",label="Download LBAR Results (Plot)"))
                                              )),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Fishing Mortality / Natural Mortality (Catch Curve)') != -1",
                                                  h2("Catch Curve Model Outputs"),
                                                  h4("Z, F, and F/M have been calculated"),
                                                  DT::dataTableOutput("CC"),
                                                  h4("Below are the results of the model fit to the length data (top) and the calculated selectivity curve (bottom)."),
                                                  plotOutput("CCPlot",height=800,width=800)
                                                )
                                              )
                                            )),
                                   tabPanel("Spawning Potential Ratio (SPR)",
                                            sidebarLayout(
                                              sidebarPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Spawning Potential Ratio (SPR)') != -1",
                                                  h4("Instructions: SPR will automatically be calculated for you using the methedology in Prince et al. 2011"),
                                                  h5(a("Prince, J., Victor, S., Kloulchad, V., & Hordyk, A. (2015). Length based SPR assessment of eleven Indo-Pacific coral reef fish populations in Palau. Fisheries Research, 171, 42-58.",href="https://doi.org/10.1016/j.fishres.2015.06.008",target="_blank")),
                                                  h5(a("This calculation uses the LBSPR R Package for calculating SPR.", href="https://github.com/AdrianHordyk/LBSPR",target="_blank")),
                                                  downloadButton("downloadSPR",label="Download SPR Results (CSV)")
                                              )),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLengthSelection && input.checkDataGroup.indexOf('dataLength') != -1 && input.indicatorLengthSelection.indexOf('Spawning Potential Ratio (SPR)') != -1",
                                                  h2("SPR Model Outputs"),
                                                  h4("SPR and the selectivity parameters have been calculated using the provided inputs"),
                                                  DT::dataTableOutput("SPR"),
                                                  h4("Below are the results of the model fit to the length data (top) and the calculated maturity and selectivity curves (bottom)."),
                                                  plotOutput("SPRPlot",height=800,width=800)
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
                                                  condition = "input.checkDataGroup && input.indicatorLandingsSelection && input.checkDataGroup.indexOf('landingsData') != -1",
                                                  uiOutput("yearLandingsUI"),
                                                  uiOutput("catchReference"),
                                                  numericInput("tripMax",label="Enter the largest feasible catch for a single trip. Catches in a single trip above this will be removed as outliers. Leave this as -999 if you do not believe there are any outliers to remove.",value=-999),
                                                  checkboxInput("totalCatch",label="Does the catch and effort data represent the total landings in the fishery? If so, check this box. If the data is only a sub-sample, leave this box is un-checked, and the dashboard will normalize catch and effort by the number of sampling days.",value=FALSE),
                                                  downloadButton("downloadLandingsPlot",label="Download landings, effort, and CPUE Results (Plot)"))
                                              ),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLandingsSelection && input.checkDataGroup.indexOf('landingsData') != -1",
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
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLandingsSelection && input.checkDataGroup.indexOf('landingsData') != -1 && input.indicatorLandingsSelection.indexOf('Total Landings') != -1",
                                                  h3("Below are the results of the landings assessment."),
                                                  DT::dataTableOutput("Landings"))
                                              ))),
                                   tabPanel("CPUE Indicator",
                                            sidebarLayout(
                                              sidebarPanel(

                                              ),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorLandingsSelection && input.checkDataGroup.indexOf('landingsData') != -1 && input.indicatorLandingsSelection.indexOf('CPUE') != -1",
                                                h3("Below are the results of the CPUE assessment."),
                                                DT::dataTableOutput("CPUE"))
                                              )))

                                 )),
                        tabPanel("Underwater Visual Survey Performance Indicators",
                                 tabsetPanel(
                                   tabPanel("Data Visualization",
                                            sidebarLayout(
                                              sidebarPanel(
                                                uiOutput("yearUnderwaterUI")
                                              ),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.checkDataGroup.indexOf('underwaterData') != -1",
                                                plotOutput("UVCPlots",height=1000))
                                              )
                                            )),
                                   tabPanel("Biomass Ratio (aggregated across species)",
                                            sidebarLayout(
                                              sidebarPanel(
                                                h5("Instructions: Biomass ratio will be automatically calculated according to the McClanahan et al. 2011 and Karr et al. 2015 methedology."),
                                                h5(a("McClanahan, T. R., Graham, N. A., MacNeil, M. A., Muthiga, N. A., Cinner, J. E., Bruggemann, J. H., & Wilson, S. K. (2011). Critical thresholds and tangible targets for ecosystem-based management of coral reef fisheries. Proceedings of the National Academy of Sciences, 108(41), 17230-17233.", href="http://www.pnas.org/content/108/41/17230.short",target="_blank")),
                                                h5(a("Karr, K. A., Fujita, R., Halpern, B. S., Kappel, C. V., Crowder, L., Selkoe, K. A., ... & Rader, D. (2015). Thresholds in Caribbean coral reefs: implications for ecosystem‐based fishery management. Journal of Applied Ecology, 52(2), 402-412..", href="http://onlinelibrary.wiley.com/doi/10.1111/1365-2664.12388/abstract",target="_blank"))
                                                ),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorUnderwaterSelection && input.checkDataGroup.indexOf('underwaterData') != -1 && input.indicatorUnderwaterSelection.indexOf('Biomass Ratio (aggregated across species)') != -1",
                                                h3("Below are the results of the Biomass Ratio assessment."),
                                                DT::dataTableOutput("BR"))
                                              ))),
                                   tabPanel("Density Ratio (Target Species)",
                                            sidebarLayout(
                                              sidebarPanel(
                                                h5("Instructions: Density ratio will be automatically calculated according to the Babcock and MacCall 2011 methedology."),
                                                h5(a("Babcock, E. A., & MacCall, A. D. (2011). How useful is the ratio of fish density outside versus inside no-take marine reserves as a metric for fishery management control rules?. Canadian Journal of Fisheries and Aquatic Sciences, 68(2), 343-359.", href="http://www.nrcresearchpress.com/doi/abs/10.1139/F10-146#.WTBEwRPyui4",target="_blank"))
                                              ),
                                              mainPanel(
                                                conditionalPanel(
                                                  condition = "input.checkDataGroup && input.indicatorUnderwaterSelection && input.checkDataGroup.indexOf('underwaterData') != -1 && input.indicatorUnderwaterSelection.indexOf('Density Ratio (Target Species)') != -1",
                                                h3("Below are the results of the Density Ratio assessment."),
                                                DT::dataTableOutput("DR"))
                                              )))

                                 )))),
             tabPanel("Step 6: Interpret Assessment Results",
                      sidebarLayout(
                        sidebarPanel(
                          helpText(a(h1("Click for help!"), href=paste(savedURL,"Step6.html",sep=""),target="_blank")),
                          h4("Instructions: Once you have gone through Steps 1-6, a summary of all assessment results will be provided to the right. You will also be able to download these results. Look these over with your stakeholder working group, and record the group's interpretation of how the fishery is doing."),
                          h4("Note: This is a record keeping step if you are going through the entire AFAM process. This step will also require significant input from stakeholders (see guidance document). You may skip this step if you simply wish to visualize and analyze your data."),
                          uiOutput("button")
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
                          h4("Note: This is a record keeping step if you are going through the entire AFAM process. This step will also require significant input from stakeholders (see guidance document). You may skip this step if you simply wish to visualize and analyze your data."),
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
                          h5("Once you have completed all steps, you are ready to create your Fishery Management Plan. To get a copy of your AFAM report, first click the Generate Report button. The report will then appear on the right. You may also download a copy of the report below."),
                          h4("Note: You must first complete all 8 steps of the dashboard, or these buttons and the report will not appear. Specifically, you must interpret the results in Step 6 and select a management reponse in Step 7."),
                          #h4("Note: This is a record keeping step if you are going through the entire AFAM process. This step will also require significant input from stakeholders (see guidance document). You may skip this step if you simply wish to visualize and analyze your data."),
                          conditionalPanel(
                            condition = "input.selectedResult && input.stakeholderInterpretation",
                            actionButton("report", "Generate Report"),
                            #helpText(a(h2("Click for the report (after generating)"), href="AFAMSummary.html",target="_blank")),
                            hr(),
                            radioButtons('format', h5('If you wish to save a copy of your report, select the format for your download:'), c('HTML', 'Word','PDF'),inline = TRUE),
                            downloadButton("summaryDocDownload", label = "Download AFAM summary document"))
                        ),
                        mainPanel(
                          
                          uiOutput("renderSummary")
                        )
                      )
             )
             
             )))