#
# This is the user-interface for the EY Outcomes Dashboard
# It has a
#  
#   -Summary and Context Page with sub tabs for
#         -Summary
#         -Context
#   -GLD Page with sub tabls for 
#        - Percentages achieving a GLD
#        - Gaps
#   -Areas of Learning with separate pages for 
#       - Combined Communication and Language and Literacy Areas of Learning
#       - Individual Areas of Learning, Communication and Language, Literacy and Mathematics
#         Each of these has two tabs
#           - Percentages achieving at least the expected level of development
#           - Gaps
#   -Take Up  with percentages benefitting from funded education
# 
#   -User Guidance Page - helper functions are available throughout the dashboard with excerpts to this page

library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    #   https://stackoverflow.com/questions/31013769/locking-r-shiny-dashboard-sidebar-shinydashboard 
    # style = "position: fixed; overflow: visible;",  # this fixed the side bar and broke other things, so not including
     
    selectInput(inputId="Local_Authority", label="LA",
                choices = EYFSP_Data$LA_Name, multiple=FALSE, #selectize=TRUE,
                width = '98%',selected = "Bury"),
    menuItem("Cover Page", tabName = "cover", icon = icon("book-open")),
    menuItem("Summary and Context", tabName = "context", icon = icon("dashboard")),
    menuItem("Good Level of Development", tabName = "GLD", icon = icon("dashboard")),
    menuItem("Areas of Learning", tabName = "AoL", icon = icon("dashboard"),
             menuSubItem("Comm and Language and Lit", tabName = "combined"),
             menuSubItem("Individual Areas", tabName = "individual_AoLs")),
    menuItem("Take up", tabName = "Take_up", icon = icon("dashboard"), 
             badgeLabel = "updated - 2019 data", badgeColor = "yellow"),  
    menuItem("User Guidance", icon = icon("file"), tabName = "notes")
  )#end of sidebar
)

body <- dashboardBody(
  tags$head(includeScript("www/google-analytics.js")),
  
  
  
  tabItems(

# Cover Page --------------------------------------------------------------

    
    tabItem(tabName = "cover",
          h2("Early Years Outcomes Dashboard"),
            h3("About the dashboard and Department for Education's social mobility programme"),
            br(),
            p("In July 2018, the Secretary of State for Education ",a("announced",href="https://www.gov.uk/government/speeches/education-secretary-sets-vision-for-boosting-social-mobility")
            ," his ambition to ",
             # https://stackoverflow.com/questions/54134541/how-to-create-a-single-line-of-text-with-hyperlink-or-other-elements-on-same-lin
               "halve the proportion of children who do not achieve at least expected levels across all goals in the 'communication and language' and 'literacy' areas of learning ",
              "in the Early Years Foundation Stage Profile at the end of reception year by 2028. "
            ),
           
           p("This ambition builds on ",a("'Unlocking Talent, Fulfilling Potential: a plan for improving social mobility through education', ",
             #a("https://www.gov.uk/government/publications/improving-social-mobility-through-education",
               href= "https://www.gov.uk/government/publications/improving-social-mobility-through-education"),"which set out the Government's plans to close the word gap in the early years (‘the first life ambition’). "

             ),
           p("We know that development gaps between disadvantaged children and their peers are particularly pronounced in early language, and these gaps have a profound impact later in life. And we also know that high quality early education can have an impact on long-term social mobility."
             ),
           p("We have created the Early Years Outcomes Dashboard to support local areas by making the most important early years social mobility metrics easily available. The dashboard sets out the latest data on progress in each local authority towards the Secretary of State's ten year ambition, and provides further information to help compare the attainment gap between disadvantaged children (measured by children known to be eligible for free school meals) and their peers. Local authorities will also be able to see the performance of children identified as having a special educational need."
             ),
           
            p("The Early Years Outcomes Dashboard includes: "),
            #https://stackoverflow.com/questions/46766411/whitespace-in-r-shiny
            p(HTML('&emsp;&emsp;'),"- the percentage of children who achieve a good level of development."), 
            p(HTML('&emsp;&emsp;'),"- the percentage of children achieving at least the expected level of development for communication and language and literacy (combined)."),  
            p(HTML('&emsp;&emsp;'),"- the percentage of children achieving at least the expected level of development for communication and language, literacy, and  numeracy."), 
            p(HTML('&emsp;&emsp;'),"- the percentage of children benefitting from funded early education places for the two-year-old offer and funded early education (first 15 hours) for three and four year olds."),
            p("Local authorities will be able to compare the gap between disadvantaged children and their peers both within their areas and against the national average. They will also be able to benchmark themselves against  ten nearest local authority statistical neighbours, and against all local authorities. This will allow local authorities to clearly identify 'peer' authorities who face similar contexts, but who are seeing different results. The overall aim of the Early Years Outcomes Dashboard is to increase focus on disadvantaged children development in the early years.
              "),
            
            br(),
            p(" Please refer to the 'User Guidance' tab for additional information and caveats. In addition, please click the 'question icon' (see an example to the right) for a subset of the information available on the user guidance tab. The icon is situated to the right where available.
              ") %>%  helper(icon = "question-circle", 
                     colour = "turquoise",
                     size = "s",
                     type = "inline",
                     title = "Question icon",
                     content = "By clicking this icon, a subset of information from the user guidance tab will be available."),
            br(),
            br(),
            img(src = "Department_for_Education.png", height = 100, width = 150),
            br(),
            br(),
            p("This is a new service - if you would like to provide feedback on this tool please contact ",a(href="mailto:EarlyYears.SOCIALMOBILITY@education.gov.uk","EarlyYears.SOCIALMOBILITY@education.gov.uk"))
    ),
 

# Context Tab-----------------------------------------------------------------
      tabItem(tabName = "context",
              h2("Summary and Context")%>%  
                # helper(icon = "question", 
                #        colour = "orange",
                #        size = "s",
                #        type = "inline",
                #        title = "Current Details",
                #        content = c(paste("<b>x-variable:</b>" ),
                #                    paste("<b>y-variable:</b>" ),
                #                    paste("<b>Clusters:</b>" ),
                #                    "</br>Note this help icon is rendered on the server!"))
                
                
              helper(
                icon = "question-circle",
                colour = "turquoise",
                type = "markdown",
                title = "",
                content = "context_help"),


#Summary and Context Value Boxes --------------------------------------------
          fluidRow(
            valueBoxOutput("context_GLD_LA_FSM",width = 4),
            valueBoxOutput("context_GLD_nat_all_other",width = 4),
            valueBoxOutput("context_GLD_LA_all_other",width =  4),
            valueBoxOutput("context_GLD_nat",width =  4),
            valueBoxOutput("context_GLD_LA_FSM_within_LA_gap",width =  4),
            valueBoxOutput("context_GLD_LA_FSM_gap_all_other_nat",width =  4),
            valueBoxOutput("context_GLD_LA_FSM_gap_FSM_nat",width =  4)
          ),

# Summary National Graphs -------------------------------------------------


            fluidRow(
                tabBox(width=12,
                selected = "Summary",
                tabPanel("Summary",color="light-blue",
                         fluidRow(
                           column(12,
                                 # https://stackoverflow.com/questions/52985383/shiny-align-input-controls-right-or-left
                                 # div line avoids an issue where arrow overlays yes/no in select input
                                  div(style="display: inline-block;vertical-align:top; width: 120px;text-align:left !important;",
                                           selectInput("show_all_LAs_context_tab",
                                                       "Show all LAs",
                                                       choices = c("Yes","No"),
                                                       selected = "No")
                                     )
                                  , align="right"
                                ),
                             column(12,box(width=12, title="National GLD All Other Compared with National GLD FSM", solidHeader = TRUE , status = "primary",
                                           plotOutput("GLD_national_plot_all_other"),#,height="300px"),
                                           plotOutput("GLD_national_plot_graph_FSM")#,height="300px")
                                           )
                                    )
                                )
                         ),

# Context -----------------------------------------------------------------

                
                tabPanel("Context",
                        fluidRow(
                                  box(width=6, title="Context FSM", solidHeader = TRUE,status = "primary", align= "center",
                                      htmlOutput("FSM_context_graph_title"),
                                      plotOutput("FSM_Num_Perc_plot" ),
                                      tableOutput("FSM_context_table")
                                      ),
                                  box(width=6,title="Context SEN", solidHeader = TRUE,status = "primary", align= "center",
                                      htmlOutput("SEN_context_graph_title"),
                                      plotOutput("SEN_Num_Perc_plot" ),
                                      tableOutput("SEN_context_table")
                                      )
                            
                              )

                      )# tab panel
                 ) # tab box
              )# end of fluid row 
           
            ), #end of Context

# GLD Tab -----------------------------------------------------------------
    tabItem(tabName = "GLD",
            h2("Good Level of Development")%>% 
              helper( 
                colour = "turquoise",
                icon = "question-circle",
                type = "markdown",
                title = "",
                content = "GLD_help"),
            fluidRow(column(12,
                            selectInput("GLD_group_select",
                                        "Select group",
                                        choices = groups,
                                        selected = "FSM")
            )),

# GLD Value Boxes ---------------------------------------------------------

            
            fluidRow(
              valueBoxOutput("GLD_Nat",width = 3),
              valueBoxOutput("GLD_LA",width = 3),
              valueBoxOutput("GLD_LA_Rank",width = 3),
              valueBoxOutput("GLD_LA_SN_Rank",width = 3) 
            ) ,
            fluidRow(
              

# GLD Percentages ---------------------------------------------------------

              
              tabBox(
                title = "",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1",width = 12,#, height = "500px"
                tabPanel("Percentages achieving a good level of development", 
                         fluidRow(
                                      box(width=8, title="Statistical Neighbours", solidHeader = TRUE, status="primary",
                                            htmlOutput("GLD_neighbour_plot_graph_title"),
                                            plotOutput("GLD_neighbour_plot")
                                          ),
                            
                                      box(width=4, title="Trend", solidHeader = TRUE,status="primary",
                                          htmlOutput("GLD_trend_plot_graph_title"),  
                                          plotOutput("GLD_trend_plot")%>%  helper(icon = "question-circle", 
                                                                                  colour = "paleturquoise",
                                                                                  size = "s",
                                                                                  type = "markdown",
                                                                                 # title = "Trend Graph Help",
                                                                                  content = "GLD_trend_help")#"Note, this graph compares all, all other and also FSM with the national average for **all other**,whereas the SEN group is compared with the national average for SEN.")
                                          )
                                 ) #closing bracket for fluidRow        
                      ),# closing bracket for tabPanel

# GLD Gaps ----------------------------------------------------------------
                tabPanel("Gaps", "", 
                         fluidRow(
                           
                           column(12,
                                  
                                  selectInput("GLD_gap_select",
                                              "Select group (FSM only)",
                                              choices = gaps_graph_choice
                                              ) 
                                  )
                                ),
                           fluidRow(
                                    box(width=8, title="Statistical Neighbours Gaps", solidHeader = TRUE,status="primary",
                                        htmlOutput("GLD_neighbour_gap_plot_graph_title"),
                                          plotOutput("GLD_neighbour_gaps_plot")
                                       ),
                              
                                    box(width=4, title="Gaps Trend", solidHeader = TRUE,status="primary",
                                        htmlOutput("GLD_trend_gap_plot_graph_title"),
                                        plotOutput("GLD_trend_gap_plot") 
                                      )
                               
                                  )          
                    )# close tab panel 
              )#end of tab box
            ), #close of fluid row for tab box,

# GLD Text and Table ------------------------------------------------------


            fluidRow( 
              box(Title="About", width=8,
                  htmlOutput("GLD_text")
              ) ,
              box(width=4,
                  collapsible = T, 
                  column( 12,align="center",
                          DTOutput("GLD_trend_table")
                  )  
              )
             ),
              fluidRow( 
                     # https://stackoverflow.com/questions/52985383/shiny-align-input-controls-right-or-left
                     # div line avoids an issue where arrow overlays yes/no in select input
                     div(style="display: inline-block;vertical-align:top; width: 120px;text-align:left !important;",
                         selectInput("show_all_LAs_GLD_tab",
                                     "Show all LAs",
                                     choices = c("Yes","No"),
                                     selected = "No")
                        )
                     , align="right"
                     ),

# GLD National Graph ------------------------------------------------------


            fluidRow(
                      box(
                        title = "National GLD", width = 12, height = "600px",solidHeader = TRUE, status = "primary",
                        plotOutput("GLD_national_plot",height="500px")
                        )
                    # ,
                    # # for debug only
                    # column(12,
                    #   tableOutput("GLD_neighbour_table")
                    #         )
                    )
            ), # End of GLD Tab

#,
    
 # AoL Combined Literacy and Communication  and Language and Literacy -------------------

    tabItem(tabName = "combined",
            h2("Communication and Language and Literacy Areas of Learning (Combined)") %>% 
              helper(
              icon = "question-circle",
              colour = "turquoise",
              type = "markdown",
              title = "",
              content = "AoL_help"),

# AoL Combined Communication Value Boxes ----------------------------------

            
            fluidRow(
              valueBoxOutput("AoL_comb_Nat",width = 3),
              valueBoxOutput("AoL_comb_LA",width = 3),
              valueBoxOutput("AoL_comb_LA_Rank",width = 3),
              valueBoxOutput("AoL_comb_LA_SN_Rank",width = 3)
            ),
          fluidRow(

# AoL CL and Lit Combined Percentages -------------------------------------

            
              tabBox(
                title = "",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", width = 12,
                tabPanel("Percentages achieving at least expected level of development", "",
                         fluidRow(         
                          
                                    box(width=8, title="Statistical Neighbours", solidHeader = TRUE,status="primary",
                                       htmlOutput("AoL_comb_neighbour_plot_graph_title"),
                                       plotOutput("AoL_comb_neighbour_plot")      
                                       ),
                          
                                   box(width=4, title="Trend", solidHeader = TRUE,status="primary",
                                          htmlOutput("AoL_comb_trend_plot_graph_title"),
                                          plotOutput("AoL_comb_trend_plot")
                                      )
                          
                           ) #closing bracket of fluidrow
                      ),# closing bracket of tab panel

# AoL CL and Lit Gaps -----------------------------------------------------

                
                tabPanel("Gaps", "", 
                    fluidRow(
                           box(width=8, title="Statistical Neighbours Gaps", solidHeader = TRUE,status="primary",
                               htmlOutput("AoL_comb_neighbours_gaps_plot_graph_title"),
                              plotOutput("AoL_comb_neighbours_gaps_plot")) ,
                           box(width=4, title="Trend", solidHeader = TRUE, status="primary",
                               htmlOutput("AoL_comb_trend_gap_plot_graph_title"),
                               plotOutput("AoL_comb_trend_gap_plot")
                              )
                            )
                        )# end of tab Panel
              )# end of tab box
              
            ), # end of fluid row

# AoL CL and Lit Text and Table-----------------------------------------------------


                fluidRow(
                           box(Title="About", width=8,
                               htmlOutput("AoL_comb_text")
                       ),
                           box(width=4, 
                               collapsible = T, 
                               column( 12,align="center",
                                       DTOutput("AoL_comb_trend_table")
                               )
                           )
                      ), # end of fluidRow
                

# AoL CL and Lit National -------------------------------------------------

                fluidRow( 
                       # https://stackoverflow.com/questions/52985383/shiny-align-input-controls-right-or-left
                       # div line avoids an issue where arrow overlays yes/no in select input
                       div(style="display: inline-block;vertical-align:top; width: 120px;text-align:left !important;",
                           selectInput("show_all_LAs_AoL_comb_tab",
                                       "Show all LAs",
                                       choices = c("Yes","No"),
                                       selected = "No")
                       )
                       , align="right"
                ),
            
              fluidRow(
                     box(width=12, title="Trend", solidHeader = TRUE,status="primary",
                          plotOutput("AoL_comb_national_plot")
                        )
                    ) # end of Fluid Row    
          ),# end of AoL Combined Tab
    
    #  AoL Individual Areas ---------------------------------------------------------
    
    tabItem(tabName = "individual_AoLs",
            h2("Areas of Learning")  %>% 
              helper(
                icon = "question-circle",
                colour = "turquoise",
                type = "markdown",
                title = "",
                content = "AoL_help"),
            fluidRow(
                    column(2, selectInput("AoL_group_select",
                                          "Select group",
                                          choices = AoL_groups,
                                          selected = "FSM")),
                    column(3,selectInput("AoL_select",
                                         "Select area of learning",
                                         choices = areas_of_learning,
                                         selected = "Communication and Language"))
                  ),

# AoL Individual Areas Value Boxes ---------------------------------------------------------

            
            fluidRow(
              valueBoxOutput("AoL_Nat",width = 3),
              valueBoxOutput("AoL_LA",width = 3),
              valueBoxOutput("AoL_LA_Rank",width = 3),
              valueBoxOutput("AoL_LA_SN_Rank",width = 3)
            ),

# AoL Individual Areas Percentages ---------------------------------------------------------


            fluidRow(
              tabBox(
                title = "",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1",width = 12,#, height = "500px"
                tabPanel("Percentages achieving at least expected level of development", "",
                         fluidRow(  
                                   box(width=8, title="Statistical Neighbours", solidHeader = TRUE, status="primary",
                                       htmlOutput("AoL_neighbour_plot_graph_title"),
                                       plotOutput("AoL_neighbour_plot")
                                  ),
                                   box(width=4, title="Trend", solidHeader = TRUE,status="primary",
                                       htmlOutput("AoL_trend_plot_graph_title"),
                                       plotOutput("AoL_trend_plot")%>% helper(
                                         icon = "question-circle",
                                         colour = "paleturquoise",
                                         type = "markdown",
                                         title = "",
                                         content = "AoL_trend_help")
                                       )
                                )
                       ),

# AoL Individual Areas Gaps -----------------------------------------------
       
         
                tabPanel("Gaps", "",
                         fluidRow(
                           column(12,
                                  selectInput("AoL_gap_select",
                                              "Select group (FSM only)",
                                              choices = gaps_graph_choice)
                                  )
                                ),
                         fluidRow(  
                                    box(width=8, title="Statistical Neighbours Gaps", solidHeader = TRUE, status="primary",
                                         htmlOutput("AoL_neighbours_gaps_plot_graph_title"),
                                         plotOutput("AoL_neighbours_gaps_plot")
                                        ),
                                    box(width=4, title="Trend", solidHeader = TRUE, status="primary",
                                       htmlOutput("AoL_trend_gap_plot_graph_title"), 
                                       plotOutput("AoL_trend_gap_plot")
                                       )
                                )
                    )# end tab panel
                )# close tab box
            )#close fluid row for tab box

# AoL Individual Areas Text Boxes and table -----------------------------------------


                ,fluidRow(
                         box(Title="About", width=8,
                             htmlOutput("AoL_text")
                            ),
                         box(width=4,
                               collapsible = T, 
                               column( 12,align="center",
                                       DTOutput("AoL_trend_table")
                               )
                            ), 

# AoL Indivdual Arees National Plot ---------------------------------------

                        
                        column(12,
                               # https://stackoverflow.com/questions/52985383/shiny-align-input-controls-right-or-left
                               # div line avoids an issue where arrow overlays yes/no in select input
                               div(style="display: inline-block;vertical-align:top; width: 120px;text-align:left !important;",
                                   selectInput("show_all_LAs_AoL_tab",
                                               "Show all LAs",
                                               choices = c("Yes","No"),
                                               selected = "No")
                                  )
                               , align="right"
                              ),        
                      box(width=12,title="National",solidHeader = TRUE,status="primary",
                          plotOutput("AoL_national_plot")   
                          )
                    
                  )# end fluid row after tab box
# Available for debug below
# ,column( 12,align="center",
#         tableOutput("AoL_neighbour_table")) 

                ),# end of AoL individual areas of learning

# Take up Tab -------------------------------------------------------------
    
    
    tabItem(tabName = "Take_up",
            h2("Benefitting from Funded Early Education") %>% 
              helper(
                icon = "question-circle",
                colour = "turquoise",
                type = "markdown",
                title = "",
                content = "Take_up_help"),
            fluidRow(
                  column(12,
                         selectInput("Ages", "Please select",
                                     choices = c("Two Year Olds", "Three and Four Year Olds"), multiple=FALSE, selectize=TRUE,
                                     width = '20%'))
                    ),
            fluidRow(
                        valueBoxOutput("Take_up_Nat",width = 3),
                        valueBoxOutput("Take_up_LA",width = 3),
                        valueBoxOutput("Take_up_LA_Rank",width = 3),
                        valueBoxOutput("Take_up_LA_SN_Rank",width = 3)
                    ),
            fluidRow(
                        column(12,
                                  box(width=8, title="Statistical Neighbours", solidHeader = TRUE,
                                      htmlOutput("Take_up_neighbour_plot_graph_title"),
                                      plotOutput("Take_up_neighbour_plot"),status="primary"
                                      ),    
              
                                  box(width=4, title="Trend", solidHeader =TRUE,status="primary",
                                      htmlOutput("Take_up_trend_plot_graph_title"),
                                      plotOutput("Take_up_trend_plot")
                                      )
                              ),

# Take Up Text and Table --------------------------------------------------

              
              column(12,
                     box(Title="About", width=8,
                         htmlOutput("Take_up_text")
                        ),
                     box(width=4,
                         collapsible = T, 
                         column( 12,align="center",
                                 DTOutput("Take_up_trend_table")
                         )
                     )
                  ),

# Take up National Graph --------------------------------------------------


              column(12,
                     # https://stackoverflow.com/questions/52985383/shiny-align-input-controls-right-or-left
                     # div line avoids an issue where arrow overlays yes/no in select input
                     div(style="display: inline-block;vertical-align:top; width: 120px;text-align:left !important;",
                         selectInput("show_all_LAs_take_up_tab",
                                     "Show all LAs",
                                     choices = c("Yes","No"),
                                     selected = "No")
                        ),align="right"
                     
                    ),
              
                  box(width=12, solidHeader = TRUE, title="National",status = "primary",   
                      plotOutput("Take_up_national_plot")
                      )#,
              # column(12, 
              #        tableOutput("Take_up_table") 
              #       )
            )# end of fluidrow
    ),#closing bracket for take-up


# User Guidance Tab ---------------------------------------------------------------


 
    tabItem(tabName = "notes",
            # h2("Notes"),
            # br(),
            h3("Hints and tips on using the Dashboard"),
            p("- Note, there are often sub tab items within the tabs. For example, in the 'Summary and Content' tab there are both 'Summary' and 'Context' sub tab items beneath the boxes"),
            p("- Scroll down to see all tab content. National graphs are at the bottom of main tabs"),
            p("- For national graphs, see 'Show all LAs' drop down to far right of graph. This toggles showing all LA names or just the selected LA and the statistical neighbours"),
            h3("Abbreviations and Definitions"),
            tableOutput("Notes_table"),
           
            
            h3("Notes and Caveats"),
            tags$ol(
               tags$li("Improving social mobility through education' policy paper, 'Unlocking Talent, Fulfilling Potential' is published here:"),
               tags$a(href="https://www.gov.uk/government/publications/improving-social-mobility-through-education","https://www.gov.uk/government/publications/improving-social-mobility-through-education"),
               tags$li("Data source for EYFSP is the National Pupil Database."),
               tags$li("Data sources for take up rates are the Early Years Census (EYC), School Census (SC), and School Level Annual School Census (SLASC)."),
               tags$li("This dashboard considers the ten nearest statistical neighbours in line with the LAIT. There are many different sets of statistical neighbours as each is derived using a different method which generates different results. All change from time to time as new data are generated and as assumptions change. Some local authorities do not have any close statistical neighbours. 
For further information please look at the LAIT: "),
               tags$a(href="https://www.gov.uk/government/publications/local-authority-interactive-tool-lait.","https://www.gov.uk/government/publications/local-authority-interactive-tool-lait."),
               
    
               tags$li("The information on GLD and Areas of Learning relates to the EYFSP publication. For more information on EYFSP results please see here:"),
               tags$a(href="https://www.gov.uk/government/collections/statistics-early-years-foundation-stage-profile","https://www.gov.uk/government/collections/statistics-early-years-foundation-stage-profile"), 
              
               tags$li("The information on take up relates to the 'Education provision: children under 5 years of age publication'. For more information, please see section on 'Education provision: children under 5 years of age', here:"), 
               tags$a(href="https://www.gov.uk/government/collections/statistics-childcare-and-early-years#provision-for-children-under-5-years-of-age-in-england","https://www.gov.uk/government/collections/statistics-childcare-and-early-years "),
               tags$li("EYFSP: The gap calculations were made using unrounded data held by DfE.  
                       In a small number of instances there may be a small difference in calculating the gaps using published data due to using rounded data."),
               tags$li("EYFSP: For the early years foundation stage profile statistical release, there are slight differences between the numbers reported in this publication when compared to the results published on 18 October 2018. Headline attainment percentages are not affected. For the EY Outcomes Dashboard calculations, the number of pupils eligible was based on in the pupil characteristics statistical release. This means that for GLD for all and the Communication and Langugage and Literature (combined) metrics, results may differ by about 0.1% from the results published on 18 October."),
               tags$li("EYFSP: LA data can only be provided in the dashboard if it is available in the underlying data.
                       For more information, please see the disclosure control section of the technical document for information on data suppression here: "),
               tags$a(href="https://www.gov.uk/government/collections/statistics-early-years-foundation-stage-profile","https://www.gov.uk/government/collections/statistics-early-years-foundation-stage-profile"),
              
               tags$li("EYFSP: Children achieving at least the expected level in the early learning goals within the three prime areas of learning and within literacy and mathematics is classed as achieving 
                      a 'good level of development'."),
              tags$li("EYFSP: Figures based on final data."),
              tags$li("EYFSP: Only includes children with a valid result for every early learning goal."),
              tags$li("EYFSP: All providers of state-funded early years education (including academies and free schools),
                      private, voluntary and independent (PVI) sectors in England are within the scope of the EYFSP data collection."),
              tags$li("EYFSP: Achieved at least the expected level in communication & language and literacy are those children who achieved `expected` or `exceeded` in the following early learning goals: 
                      listening and attention, understanding, speaking, reading and writing."),
              tags$li("EYFSP:  The Communication & Language and Literacy figures in this dashboard are based on data for all children with a valid result for every early learning goal and may not match those in the additional table published in July 2018 as part of the 2017 statistical release. 
                      The figures in this dashboard are based on data matched to the national pupil database (NPD) and exclude shielded children and duplicate records."),
              tags$li("EYFSP: Non SEN Includes pupils for whom SEN provision could not be determined. "),
              tags$li("EYFSP: All other :Includes pupils not eligible for free school meals and for whom free school meal eligibility was unclassified or could not be determined."),
              tags$li("EYFSP: For the Isle of Scilly and for City of London not all data can be provided due to data suppression rules, but a decision has been made to include these LAs in the dashboard GLD and Areas of Learning tabs, with partial data. This also affects Kensington and Chelsea in limited instances."),
              tags$li("EYFSP: City of London is a statistical neighbour for both Kensington and Chelsea and Westminster. When either of these locals authorities is selected, City of London will appear without any data shown, if data suppressed or equal to zero. This is to make it clear that this is an intentional omission due to point above."),
              
              tags$li("EYFSP: Where data is suppressed or equal to zero a local authority will appear with a rank of 152, or 151 if tied with another local authority, for national rank and 11 for statistical neighbour rank."),
              #tags$li("EYFSP: ."),
              
              tags$li("EYFSP/Take up: Statistical Neighbour ranks are out of 11, they include the local authority selected and its ten nearest statistical neighbours."),
              tags$li("EYFSP/Take up: National rank is out of all authorities (152), but note some local authorities may have data suppressed or equal to zero."),
              tags$li("EYFSP: Where data is not available it will sometimes appear as zero and sometimes as NA."),
              tags$li("Take up: Population estimates at lower geographic levels, such as local authority, are subject to a greater degree of error.  
                       In some cases, local authority take-up rates can exceed 100%. Therefore, take-up rates at local authority level should be treated with more caution than national take-up rates."),
              tags$li("Take up of three and four year olds only considers funded early education (first 15 hours)."),
              tags$li("Take up: Count of children aged two at 31 December in the previous calendar year. Numbers of two-year-olds taking up places is expressed as a percentage of the two-year-old population eligible for a funded early education. The estimated number of eligible children is derived from data supplied to the Department for Education by the Department for Work and Pensions in November each year on the number of children believed to meet the benefit and tax credit eligibility criteria. 
                       It represents the best data available on the number of eligible children in each local authority area."),
              tags$li("Take up: For 3- and 4-year-olds, the population used to calculate take-up rates are the adjusted ONS population estimates. 
                        For 2-year-olds, take-up rates are expressed as a percentage of the estimated eligible population. This is the fifth time they have been based upon data from the Department for Work and Pensions. However, for 2019 the estimated eligible population data includes Universal Credit Full Service claimants for the first time. For further information see the accompanying technical document:"),
                      tags$a(href="https://www.gov.uk/government/statistics/education-provision-children-under-5-years-of-age-january-2019","https://www.gov.uk/government/statistics/education-provision-children-under-5-years-of-age-january-2019"),
              tags$li("Take up: For 3- and 4-year-olds, the ONS population estimates for 2017 and 2018 used in this publication have been revised, therefore take up rates for these years may differ to those previously published. For further information, see the accompanying technical document:"),
              tags$a(href="https://www.gov.uk/government/statistics/education-provision-children-under-5-years-of-age-january-2019","https://www.gov.uk/government/statistics/education-provision-children-under-5-years-of-age-january-2019"),
              tags$li("Take up: Count of children aged three and four at 31 December in the previous calendar year. Numbers of three- and four-year-olds taking up places expressed as a 
                      percentage of the three- and four-year-old population."), 
#                      #Population estimates from 2013 to 2017 have been revised, therefore take up rates may differ from previously published figures."),# applied to publication, but not a revision from the most recent 2018 publication
              tags$li("Take up: Any child attending more than one private, voluntary or independent provider will have only been counted once."),
              tags$li("Take up: Includes some local authority day nurseries registered to receive funding, includes maintained nursery, primary and secondary schools, primary and secondary converter academies, 
                      primary and secondary sponsor-led academies, primary and secondary free schools, city technology colleges, special schools and general hospital schools."),
              tags$li("Take up: Note: Some caution should be exercised when comparing take-up rates at local authority level. Further information can be found in the accompanying technical document for 'Education provision: children under 5 years of age', here: "),
                      tags$a(href="https://www.gov.uk/government/collections/statistics-childcare-and-early-years#provision-for-children-under-5-years-of-age-in-england","https://www.gov.uk/government/collections/statistics-childcare-and-early-years"),
              tags$li("Take up: Percentages are shown to the nearest whole number and based on unrounded numbers.")
               
           )
    )
  )#tab items closing bracket
) # end of dashboard body

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Early Years Outcomes Dashboard",titleWidth = 450),
  sidebar,
  body
  
)
# Some notes

# I had this issue https://stackoverflow.com/questions/38925557/switch-between-the-tabitems-shiny-dashboard and need to 
# wrap in sideBarMenu() function

#Calling sub menu items https://www.youtube.com/watch?v=B9gGD6nzIc4
# How to display input boxes side by side https://stackoverflow.com/questions/36709441/how-to-display-widgets-inline-in-shiny/36712447
