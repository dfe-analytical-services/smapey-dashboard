#-========================================================
#- Author: Teresa Loftus 
#- Date:   29/10/2018
#- Description: The purpose of this file is to get the data into the correct format
#- for the Shiny App


# Important Note:
# Functions used throughout the code are intended for use within the code only. They assume correct inputs. 
# No error checking is done for the functions being used elsewhere with incorrect inputs. The code is written so that the inputs
# should come from user selections from the dashboard
# The notes for each function lists what inputs are expected

#-============================================================


library(tidyverse)
library(data.table)
library(rlang)
library(DT)
library(formattable)
library(shinyhelper)

#relative_fp_long_data<- "Data/combined_long_data_2018_R_v5_postbeta_sup.csv"  # this has data for current years including statistical neighbours
relative_fp_long_data<- "Data/combined_long_data_2019_R_v1_postbeta_sup.csv"  # this has data for current years including statistical neighbours
relative_fp_three_years<- "Data/combined_three_years_data_2018_R_v5_postbeta_sup.csv"  # this has trend data

#relative_fp_additional_nat_stats<- "Data/20181220_Additional_National_Stats.csv"
relative_fp_additional_nat_stats<- "Data/Additional_National_Stats_2019_v1.csv" 

#retrieve data
long_data_step1<- read_csv(relative_fp_long_data) 
three_years_data_step1 <- read_csv(relative_fp_three_years)
additional_nat_stats<- read_csv(relative_fp_additional_nat_stats)

long_data_step1<- cbind(long_data_step1,additional_nat_stats)

long_data_step1<-long_data_step1 %>% mutate_if(is.numeric,formattable,digit=1,format = "f")
three_years_data_step1<- three_years_data_step1 %>% mutate_if(is.numeric,formattable,digit=1,format = "f")

#Data for Menus
EYFSP_Data <- three_years_data_step1 %>%  select(LA_Name)
EYFSP_Data <- EYFSP_Data%>%arrange(LA_Name)

#Groups for GLD Tab
groups <- c("All","FSM","All Other","SEN")

#Groups for AoL Tab
AoL_groups <- c("All","FSM","All Other")

 
gaps_graph_choice <- c("Within the LA: gap with all other children"
                       ,"National average: gap with all other children"
                       ,"National average: gap with FSM children")

areas_of_learning <- c("Communication and Language","Literacy","Mathematics")

 

# Get trend data for GLD and AoL ------------------------------------------
get_trend_data_fn <- function(dataset,group,   LA, GLD_or_AoL,AoL_type="CL_and_Lit",latest_year)#=2018) 
{
  # This function get trend data for all the trend graphs not relating to gaps 
  # The function returns a list
  # Item 1 or dataset_name$trend_data         - the trend data
  # Item 2 or dataset_name$trend_graph_title  - the trend graph title
  # Item 3 or dataset_name$LA_legend          - the trend legend
  # Item 4 or dataset_name$National_legend    - the national legend
  # Item 5 or dataset_name$LA_var             - the name of the variable needed for the LA, based on user selection
  # Item 6 or dataset_name$national_var       - the name of the national variable based on user selection
  
  
  # Variables needed
  #     dataset
  #     group         - Subset by All,FSM,All_Other
  #     LA            - LA selected by user
  #     GLD_or_AoL    - GLD/AoL depending on which tab
  #     AoL_type      - Only needed on AoL tab, based on user input for CL/Lit or Maths, specifically CL_and_Lit,CL, Lit, Mat
  #     latest_year   - a year eg 2018
  
  # Example
  # debug_dataset<-Get_trend_data_fn(three_years_data_step1,group="All",LA="Bury",GLD_or_AoL="AoL",AoL_type = "CL_and_Lit",latest_year=2018)
  # debug_dataset$LA_var
  # debug_dataset[[1]]
  
  
   national_group_type<- if_else(group=="FSM"|group=="All_Other","All_Other",group)
  #national_group_type<- if_else(group=="FSM"|group=="All_Other"|(group=="All"&AoL_type!="CL_and_Lit"),"All_Other",group)# For CL and Lit only the all national stats are available for comparison
  national_legend<-
    #if_else(group=="FSM"|group=="All_Other"|(group=="All"&AoL_type!="CL_and_Lit"),"all other",
    if_else(group=="FSM"|group=="All_Other","all other",
                           if_else(group=="SEN","SEN",tolower(group)))
  
  # For trend work out last three years
  previous_year<-latest_year-1
  two_years_previous<-latest_year-2
  
  year_string <- as.character(latest_year)
  previous_year_string <- as.character(previous_year)
  two_years_previous_string <- as.character(two_years_previous)	
  
  
  #create variable names to select from three year trend table
  variable_percent_stem<-if_else(GLD_or_AoL=="GLD",paste0(group,"_","Percent","_",GLD_or_AoL,"_"),
                                 paste0(group,"_","Percent","_",GLD_or_AoL,"_",AoL_type,"_") ) 
  
  variable_percent_stem_nat<-if_else(GLD_or_AoL=="GLD",paste0(group,"_","Percent","_",GLD_or_AoL,"_"),
                                 paste0(national_group_type,"_","Percent","_",GLD_or_AoL,"_",AoL_type,"_") ) 
  
  #Create the variable names that need to be retrieved from the table
  variable_percent_latest_LA<-paste0(variable_percent_stem,latest_year,"_","LA") 
  variable_percent_previous_LA<-paste0(variable_percent_stem,previous_year,"_","LA") 
  variable_percent_two_years_previous_LA<-paste0(variable_percent_stem,two_years_previous,"_","LA")
  variable_percent_latest_National<-paste0(variable_percent_stem_nat,latest_year,"_","National") 
  variable_percent_previous_National<-paste0(variable_percent_stem_nat,previous_year,"_","National") 
  variable_percent_two_years_previous_National<-paste0(variable_percent_stem_nat,two_years_previous,"_","National")
 
  
  #Graph Title needs to specify the group of children correctly and not by abbreviation
  trend_graph_title_group<- if_else(group=="FSM","children known to be eligible for FSM",
                                    if_else(group=="All","all children",
                                            if_else(group=="All_Other","all other children",
                                                    "children identified as SEN")))
  #For legend
  LA_key<-if_else(GLD_or_AoL=="GLD", paste0(LA," %"), #"Percentage achieving a GLD",
                  #"Percentage achieving at least the expected level of development"
                  paste0(LA," %"))
  #National_key= paste0("National average for ",national_legend," children")
  National_key="National %"
  #Graph Title
  Plot_title<-paste0("Trend in percentage of ",trend_graph_title_group," in ",LA," compared to ",national_legend," children nationally.")

  #First retrieve the trend data for the LA 
  LA_data_step1<- dataset %>% filter(.,LA_Name==LA) %>% 
    select(LA_Name,!!quo_name(year_string):=variable_percent_latest_LA,!!quo_name(previous_year_string):=variable_percent_previous_LA,!!quo_name(two_years_previous_string):=variable_percent_two_years_previous_LA)  
  
  LA_data_step2 <- LA_data_step1 %>% gather(key = "Year", value= "Percentage",-LA_Name)  %>% 
    mutate(Region="LA")
  
  # Then retrieve the national data
  National_step1<- dataset  %>% filter(.,LA_Name==LA) %>% 
    select(LA_Name,!!quo_name(year_string):=variable_percent_latest_National,!!quo_name(previous_year_string):=variable_percent_previous_National,!!quo_name(two_years_previous_string):=variable_percent_two_years_previous_National) 
  
  National_step2<- National_step1 %>% gather(key ="Year", value= "Percentage",-LA_Name)  %>% 
    mutate(Region="National")
  
  #Put the LA and National data into the trend table
  GLD_trend_data<- rbind(LA_data_step2,National_step2)
  
  #Function to return the trend data and also the graph and legend titles
  trend_graph_list<- list("trend_data"=GLD_trend_data, "trend_graph_title"=Plot_title,"LA_legend"=LA_key
                          ,"National_legend"=National_key,"LA_var"=variable_percent_latest_LA, "national_var"=variable_percent_latest_National)

} #end of trend graph function


# Alternative Trend Function --------------------------------------------------
get_trend_data_fn_v2 <- function(dataset,group,LA, GLD_or_AoL_or_Take_up,AoL_type="", gap="Percent",gap_type="",latest_year)#=2018) 
{
  # This function returns the trend in gaps and also for take up. Not currently used for GLD/AoL non gap trend graph
  # Future development could be done to require only one trend function
  # The function returns a list
  # Item 1 or dataset_name$trend_data_percents         - the trend data
  # Item 2 or dataset_name$trend_data_gaps             - the trend data for gaps
  # Item 3 or dataset_name$trend_graph_title           - the trend graph title
  # Item 4 or dataset_name$trend_gaps_graph_title      - the trend gaps graph title
  # Item 5 or dataset_name$LA_legend                   - the trend legend
  # Item 6 or dataset_name$National_legend    - the national legend
  # Item 7 or dataset_name$LA_var             - the name of the variable needed for the LA, based on user selection
  # Item 8 or dataset_name$national_var       - the name of the national variable based on user selection
  
  #variables
  
  # dataset  - this will be the trend dataset
  # LA        - user selected local authority
  # GLD_or_AoL_or_Take_up - whether the function is being used for the AoL, GLD or take up tabs
  #   This takes three values
  #         GLD
  #         AoL
  #         Take_up
  # group  - this is the subset, FSM, All, All Other, also SEN for GLD
  #   This takes three or four values
  #           All
  #           All_Other
  #           FSM
  #           SEN
  # Percent_or_Gap - only works with AoL or GLD
  #   This takes two values
  #         Percent
  #         Gap
  # AoL_type -  only used with AoL, CL, Lit or Numeracy
  # gap_type -  only needed for FSM, whether within LA , with FSM national average or the all other national average
  # This takes three values -
  #        gap_within_LA
  #        gap_FSM_Nat_Av
  #        gap_All_Other_Nat_Av
  # current_year takes the numeric value of the year
  # level  -
  #   takes two values
  #       National
  #       LA
  
  
  # Examples
  # GLD_gaps_example<- Get_trend_data_fn_v2(three_years_data_step1,group="All",LA="Bury",GLD_or_AoL="GLD",gap="gap",latest_year=2018)
  # GLD_gaps_example[[1]]
  # GLD_gaps_example$trend_data_percents
  
  #national_group_type<- if_else(group=="FSM"|group=="All_Other"|group=="All","All_Other",group)
  # Compare FSM and All Other with All Other National
  national_group_type<- if_else(group=="FSM"|group=="All_Other","All_Other",group)
  national_legend<-#if_else(group=="FSM"|group=="All_Other"|group=="All","all other",
    if_else(group=="FSM"|group=="All_Other","all other",
                           if_else(group=="SEN","SEN",tolower(group)))
  
  # For trend work out last three years
  previous_year<-latest_year-1
  two_years_previous<-latest_year-2
  
  year_string <- as.character(latest_year)
  previous_year_string <- as.character(previous_year)
  two_years_previous_string <- as.character(two_years_previous)
  
  Percent_or_Gap= if_else(gap=="","Percent","gap")
  gap_type_input<- gap_type #used to work out titles
  gap_type=if_else(group=="FSM",paste0("_",gap_type),"") #used to get the gap type variables
  
  
  #create variable names to select from three year trend table
  variable_percent_stem<-if_else(GLD_or_AoL_or_Take_up=="GLD",paste0(group,"_","Percent","_",GLD_or_AoL_or_Take_up,"_"),
                                # paste0(group,"_",Percent_or_Gap,"_",GLD_or_AoL_or_Take_up,"_",AoL_type,"_") ) #function could be changed to select either percentages or gaps
                                paste0(group,"_","Percent","_",GLD_or_AoL_or_Take_up,"_",AoL_type,"_") ) 
  variable_gap_stem<-if_else(GLD_or_AoL_or_Take_up=="GLD",paste0(group,"_","gap","_",GLD_or_AoL_or_Take_up,"_"),
                             paste0(group,"_",Percent_or_Gap,"_",GLD_or_AoL_or_Take_up,"_",AoL_type,"_") ) 
  
  
  variable_percent_stem_national<-variable_percent_stem
  
  # Compare FSM and all with All Other
  #if(group %in% c("FSM","All")){
  # Compare FSM  with All Other
  if(group =="FSM"){
    variable_percent_stem_national<-if_else(GLD_or_AoL_or_Take_up=="GLD",paste0("All_Other","_","Percent","_",GLD_or_AoL_or_Take_up,"_"),
                if_else(AoL_type == "CL_and_Lit",paste0("All","_","Percent","_",GLD_or_AoL_or_Take_up,"_",AoL_type,"_"), # only all is available for CL and Lit AoL type
                                            paste0("All_Other","_","Percent","_",GLD_or_AoL_or_Take_up,"_",AoL_type,"_") ))
    }
  
  
  #Create the variable names that need to be retrieved from the table
  variable_percent_latest_LA<-paste0(variable_percent_stem,latest_year,"_","LA") 
  variable_percent_previous_LA<-paste0(variable_percent_stem,previous_year,"_","LA") 
  variable_percent_two_years_previous_LA<-paste0(variable_percent_stem,two_years_previous,"_","LA")
  variable_percent_latest_National<-paste0(variable_percent_stem_national,latest_year,"_","National") 
  variable_percent_previous_National<-paste0(variable_percent_stem_national,previous_year,"_","National") 
  variable_percent_two_years_previous_National<-paste0(variable_percent_stem_national,two_years_previous,"_","National")
  
  
  # create gap variable names
  variable_gap_latest_LA<-paste0(variable_gap_stem,latest_year,"_","LA",gap_type) 
  variable_gap_previous_LA<-paste0(variable_gap_stem,previous_year,"_","LA",gap_type) 
  variable_gap_two_years_previous_LA<-paste0(variable_gap_stem,two_years_previous,"_","LA",gap_type)
  
  # Could make function work with user inputs
  # selected_gap_type<- if_else(gap_type=="Within the LA: gap with all other children",paste0("all other children in ",LA),
  #                             if_else(gap_type=="National average: gap with all other children","all other children",
  #                                     
  #                                    if_else(gap_type=="National average: gap with FSM children","children known to be eligible for FSM","")))
  
  selected_gap_type<- if_else(gap_type_input=="gap_within_LA",paste0("all other children in ",LA),
                              if_else(gap_type_input=="gap_All_Other_Nat_Av","all other children",

                                     if_else(gap_type_input=="gap_FSM_Nat_Av","children known to be eligible for FSM","")))
  
  #Which group do we need to compare the chosen group with?
  gap_comparison_group=if_else(group== "FSM" ,selected_gap_type,
                               if_else(group== "All_Other","all other children",
                                       if_else(group=="All", "all children", "children identified as SEN",
                                               if_else(group=="SEN","children identified as SEN", "")
                                       )))
  
  #For CL and Lit combined use all
  gap_comparison_group<-if_else(AoL_type=="CL_and_Lit","all", gap_comparison_group)
  
  
  #Graph Title needs to specify the group of children correctly and not by abbreviation
  trend_graph_title_group<- if_else(group=="FSM","children known to be eligible for FSM",
                                    if_else(group=="All","all children",
                                            if_else(group=="All_Other","all other children",
                                                    "children identified as SEN")))
  
  nationally=if_else(gap_type_input=="gap_within_LA","."," nationally.") # as the comparison is always national unless the FSM gap type is with in the LA
  
  #For legend
  LA_key<-paste0(LA," %")#if_else(GLD_or_AoL_or_Take_up=="GLD","Percentage achieving a GLD","Percentage achieving at least the expected level of development")
  National_key= "National %"#paste0("National average for ",national_legend," children")
  
  #Graph Title
  Plot_title<-paste0("Trend in percentage of ",trend_graph_title_group," in ",LA," compared to ",national_legend," children nationally.")
 
  #Graph Title Gaps
  Plot_title_gaps<-paste0("Trend in percentage point gap of ",trend_graph_title_group," in ",LA," compared to ",gap_comparison_group,nationally)
  
  #First retrieve the trend data for the LA 
  LA_data_step1<- dataset%>% filter(.,LA_Name==LA) %>% 
    select(LA_Name,year1=variable_percent_latest_LA,year2=variable_percent_previous_LA,year3=variable_percent_two_years_previous_LA) %>% 
    
    #dynamically change the column names to the latest three years worth of data
    select(LA_Name, !!quo_name(year_string):=year1,!!quo_name(previous_year_string):=year2,!!quo_name(two_years_previous_string):=year3)    
  
  LA_data_gaps_step1<- dataset%>% filter(.,LA_Name==LA) %>% 
    select(LA_Name,year1=variable_gap_latest_LA,year2=variable_gap_previous_LA,year3=variable_gap_two_years_previous_LA) %>% 
    
    #dynamically change the column names to the latest three years worth of data
    select(LA_Name, !!quo_name(year_string):=year1,!!quo_name(previous_year_string):=year2,!!quo_name(two_years_previous_string):=year3)    
 
  LA_data_step2 <- LA_data_step1 %>% gather(key = "Year", value= "Percentage",-LA_Name)  %>% 
    mutate(Region="LA")
  
  LA_data_gaps_step2 <- LA_data_gaps_step1 %>% gather(key = "Year", value= "Percentage Gap",-LA_Name)  %>% 
    mutate(Region="LA")
  
  # Then retrieve the national data
  National_step1<- dataset  %>% filter(.,LA_Name==LA) %>%
    select(LA_Name,year1=variable_percent_latest_National,year2=variable_percent_previous_National,year3=variable_percent_two_years_previous_National)%>% 
    #dynamically change the column names to the latest three years worth of data
    
    select(LA_Name, !!quo_name(year_string):=year1,!!quo_name(previous_year_string):=year2,!!quo_name(two_years_previous_string):=year3)  
  
  National_step2<- National_step1 %>% gather(key ="Year", value= "Percentage",-LA_Name)  %>% 
    mutate(Region="National")
  
  # #Put the LA and National data into the trend table
  GLD_trend_data<- rbind(LA_data_step2,National_step2)
  
  #Function to return the trend data and also the graph and legend titles
  trend_graph_list<- list("trend_data_percents"=GLD_trend_data, "trend_data_gaps"=LA_data_gaps_step2#GLD_trend_gaps_data
                          , "trend_graph_title"=Plot_title,"trend_gaps_graph_title"=Plot_title_gaps,"LA_legend"=LA_key
                          ,"National_legend"=National_key,"LA_var"=variable_percent_latest_LA, "national_var"=variable_percent_latest_National)
} 


get_graph_title <- function(LA, C="",group="", neighbour_gap_context,GLD_or_AoL_or_take_up="",Percent_or_Gap="",AoL_type="",gap_type="",take_up_ages=""){
  
  # The purpose of this function is to give the graph titles for the neighbours and neighbours gaps graph as well as the context graph
  # Variables
  #   LA                      - user selected LA
  #   neighbour_gap_context   - takes values "neighbour", "neighbour_gap", "context"
  #   Percent_or_Gap          - "Percent"
  #   AoL_type                - Takes the user selected AoL Type
  #   gap_type                - For FSM only, takes user selected gap type
  #   take_up_ages            - For take up, takes user selected age range
  
  
  # Examples
  
  # title_gld<- get_graph_title(LA="Bury", group="FSM", neighbour_gap_context="neighbour_gap",GLD_or_AoL_or_take_up="GLD",gap_type="National average: gap with FSM children")
  # title_gld_2<- get_graph_title(LA="Bury", group="All", neighbour_gap_context="neighbour_gap",GLD_or_AoL_or_take_up="GLD")
  # title_aol<- get_graph_title(LA="Bury", group="All", neighbour_gap_context="neighbour_gap",GLD_or_AoL_or_take_up="AoL")
  # title_aol_2<- get_graph_title(LA="Bury", group="FSM", neighbour_gap_context="neighbour_gap",GLD_or_AoL_or_take_up="AoL",AoL_type = "CL",gap_type="National average: gap with FSM children")
  
  
  group_type<-if_else(group=="All Other","All_Other"
                      ,if_else(group=="All","All",group))
  
  title_group=if_else(group_type=="FSM","children known to be eligible for FSM",
                      if_else(group_type=="All","all children",
                              if_else(group_type=="All_Other","all other children",
                                      if_else(group_type=="SEN","children identified as SEN", tolower(take_up_ages))
                              ))) 
  
  selected_gap_type<- if_else(gap_type=="Within the LA: gap with all other children",paste0("all other children in ",LA),
                              if_else(gap_type=="National average: gap with all other children","all other children",
                                      
                                      if_else(gap_type=="National average: gap with FSM children","children known to be eligible for FSM","")))
  
  # added to differentiate between percenatge gap (within LA) and percentage point gap (every other gap)
    
  gap_type_text<- if_else(selected_gap_type==paste0("all other children in ",LA),"","the national average for ")
  
  
  #Which group do we need to compare the chosen group with?
  gap_comparison_group=if_else(group_type== "FSM" ,selected_gap_type,
                               if_else(group_type== "All_Other","all other children",
                                       if_else(group_type=="All", "all children", "children identified as SEN",
                                               if_else(group_type=="SEN","children identified as SEN", tolower(take_up_ages))
                                       )))
  
  #For CL and Lit combined use all
  gap_comparison_group<-if_else(AoL_type=="CL_and_Lit","All", gap_comparison_group)
  
  
  measure<-   case_when(
    GLD_or_AoL_or_take_up=="GLD" ~ "achieving a good level of development at EYFSP",
    GLD_or_AoL_or_take_up=="AoL" ~ paste0("achieving at least the expected level of development at EYFSP for ",tolower(AoL_type)),
    GLD_or_AoL_or_take_up=="Take_up" ~ " benefitting from funded early education places"
  )

    if(neighbour_gap_context=="context")  {  
    title<- if_else(group=="FSM",paste0("Comparison of the percentage of children known to be eligible for FSM in ",LA," with national percentage.	"),
                    paste0("Comparison of children identified as SEN in ",LA," with national percentage.	"))
  }
  
  if(neighbour_gap_context=="neighbour")    {
    title<- paste0("The percentage of ",title_group," in ", LA," ",measure," compared with the 10 nearest statistical neighbours.")
  }
  
   
  if(neighbour_gap_context=="neighbour_gap")    {
   
    title<- paste0("The percentage point gap between ",title_group," in ", LA," and ",gap_type_text,gap_comparison_group," ",measure," compared with the 10 nearest statistical neighbours.")
  }  
  
  
  #   }  
  
  #return the title
  title  
} 


# Get data for Neighbours Graphs ------------------------------------------
get_data_subset_neighbour<- function(dataset,LA,GLD_or_AoL_or_take_up,group="",Percent_or_Gap="",AoL_type="",gap_type="",take_up_ages="",latest_year){

  # This function gets a subset of data based on the selected LA, the variable name and the dataset.
  # This gives a three year trend of AoL, GLD or Take up. 
  # Example
  # current_year<- 2018
  # debug_aol<-get_data_subset_neighbour(long_data_step1,"Bury",GLD_or_AoL_or_take_up="AoL",AoL_type="CL",group="All" ,Percent_or_Gap="Percent",latest_year = current_year)
  
  #variables
  # dataset  - this will be the trend dataset
  # LA - users selected local authority
  # GLD_or_AoL_or_take_up - whether AoL, GLD or take up
  #   This takes three values
  #         GLD
  #         AoL
  #         Take_up
  # group  - this is the subset, FSM, All, All Other, also SEN for GLD
  #   This takes three or four values
  #           All
  #           All_Other
  #           FSM
  #           SEN
  # Percent_or_Gap - only works with AoL or GLD
  #   This takes two values
  #         Percent
  #         Gap
  # AoL_type -  only used with AoL, CL, Lit or Numeracy
  # gap_type -  only needed for FSM, whether within LA , with FSM national average or the all other national average
  # This takes three values -
  #        gap_within_LA
  #        gap_FSM_Nat_Av
  #        gap_All_Other_Nat_Av
  # take_up_ages 
  # This takes two values - only works with GLD_or_AoL_or_take_up equalling Take_up
  #   two_year_olds
  #   three_and_four_year_olds

  # current_year takes the numeric value of the year
  # level  -
  #   takes two values
  #       National
  #       LA
  # see https://stackoverflow.com/questions/26724124/standard-evaluation-in-dplyr-summarise-on-variable-given-as-a-character-string
  # need some extra coding so r understands that it is being passed a variable name   
  
  comparison_group<-if_else(group=="FSM","All_Other"
                            ,if_else(group=="All","All_Other",group))
  
  #overwrite comparison_group use if CL and Lit combined
  
  comparison_group<-if_else(AoL_type=="CL_and_Lit","All",comparison_group)
 
  EYFSP_variable<-if_else(GLD_or_AoL_or_take_up=="GLD",
                          paste0(group,"_",Percent_or_Gap,"_",GLD_or_AoL_or_take_up,"_",latest_year,"_","LA"),
                          if_else(GLD_or_AoL_or_take_up=="AoL",
                                  paste0(group,"_",Percent_or_Gap,"_",GLD_or_AoL_or_take_up,"_",AoL_type,"_",latest_year,"_","LA"),
                                  paste0(take_up_ages,"_","Percent_",latest_year))) #needs to be upper case
  
  EYFSP_variable<- if_else(Percent_or_Gap=="gap"&group=="FSM",paste0(EYFSP_variable,"_",gap_type),EYFSP_variable)
  
  
  var_nat<- if_else(GLD_or_AoL_or_take_up=="GLD",
                    paste0(group,"_","Percent","_",GLD_or_AoL_or_take_up,"_",latest_year,"_","National"),
                    if_else(GLD_or_AoL_or_take_up=="AoL",
                            paste0(group,"_","Percent","_",GLD_or_AoL_or_take_up,"_",AoL_type,"_",latest_year,"_","National"),
                            paste0(take_up_ages,"_","Percent_",latest_year)))
  
  var_nat_comparison<- if_else(GLD_or_AoL_or_take_up=="GLD",
                               paste0(comparison_group,"_","Percent","_",GLD_or_AoL_or_take_up,"_",latest_year,"_","National"),
                               if_else(GLD_or_AoL_or_take_up=="AoL",
                                       paste0(comparison_group,"_","Percent","_",GLD_or_AoL_or_take_up,"_",AoL_type,"_",latest_year,"_","National"),
                                       paste0(take_up_ages,"_","Percent_",latest_year,"_National")))
  
  
  
  var_rank<- if_else(GLD_or_AoL_or_take_up=="GLD",
                     paste0(group,"_","Rank","_",GLD_or_AoL_or_take_up,"_",latest_year,"_","LA"),
                     if_else(GLD_or_AoL_or_take_up=="AoL",
                             paste0(group,"_","Rank","_",GLD_or_AoL_or_take_up,"_",AoL_type,"_",latest_year,"_","LA"),
                             paste0(take_up_ages,"_","Rank_",latest_year)))
  
  
  #this does not work for FSM gap rank, as the within LA gap will not have same ranking as percentage 
  var_sn_rank<- if_else(GLD_or_AoL_or_take_up=="GLD",
                        paste0(group,"_","SN_Rank","_",GLD_or_AoL_or_take_up,"_",latest_year,"_","LA"),
                        if_else(GLD_or_AoL_or_take_up=="AoL",
                                paste0(group,"_","SN_Rank","_",GLD_or_AoL_or_take_up,"_",AoL_type,"_",latest_year,"_","LA"),
                                paste0(take_up_ages,"_","SN_Rank_",latest_year)))
  
  
  var_sn_rank<- if_else(Percent_or_Gap=="gap"&group=="FSM"&gap_type=="gap_within_LA", 
                        if_else(GLD_or_AoL_or_take_up=="GLD", paste0(group,"_","SN_gap_Rank","_",GLD_or_AoL_or_take_up,"_",latest_year,"_","LA_",gap_type),                                   # 
                                if_else(GLD_or_AoL_or_take_up=="AoL",paste0(group,"_","SN_gap_Rank_",GLD_or_AoL_or_take_up,"_",AoL_type,"_",latest_year,"_LA_",gap_type),var_sn_rank)
                        ),var_sn_rank)
 
  
  last_year<-latest_year-1
  two_years_ago<-latest_year-2
  take_up_last_year_LA<- paste0(take_up_ages,"_","Percent_",last_year)
  take_up_two_years_ago_LA <- paste0(take_up_ages,"_","Percent_",two_years_ago)
  take_up_latest_year_National <- paste0(take_up_ages,"_","Percent_",latest_year,"_National")
  take_up_last_year_National<- paste0(take_up_ages,"_","Percent_",last_year,"_National")
  take_up_two_years_ago_National  <- paste0(take_up_ages,"_","Percent_",two_years_ago,"_National")
  
  
  #select_GLD_or_AoL=c("LEA_NAME","LA_Name","EYFSP_variable","var_nat","var_nat_comparison","var_rank","var_sn_rank")
  select_take_up=c("select_GLD_or_AoL","take_up_last_year_LA","take_up_two_years_ago_LA","take_up_last_year_National","take_up_two_years_ago_LA")

  if(GLD_or_AoL_or_take_up=="GLD"|GLD_or_AoL_or_take_up=="AoL") {
    dataset_subset<- dataset %>% select(LEA_NAME,LA_Name,EYFSP_variable,var_nat,var_nat_comparison,var_rank,var_sn_rank) %>% filter(LEA_NAME==LA)#
  }
  
  if(GLD_or_AoL_or_take_up=="Take_up"){
    dataset_subset<- dataset %>% select(LEA_NAME
                                   ,LA_Name
                                   ,EYFSP_variable
                                   # var_nat
                                   #,var_nat_comparison
                                   ,var_rank
                                   ,var_sn_rank
                                   
                                   ,take_up_two_years_ago_LA
                                   ,take_up_last_year_LA
                                   
                                   ,take_up_two_years_ago_National
                                   ,take_up_last_year_National
                                   ,take_up_latest_year_National
                                   
    )%>% filter(LEA_NAME==LA)
    
    
  }
  
  dataset_subset<- dataset_subset %>% mutate_at(3,as.numeric)
  
  #arrange
  EYFSP_variable_sym <- sym(EYFSP_variable) #change variable name to a symbol
  dataset_subset<- dataset_subset %>% mutate(LA_selected_by_user=if_else(LA_Name==LA,"yes","no"))
  dataset_subset %>% arrange(desc(!!EYFSP_variable_sym)) ## the !! lets r know what to do with symbol
  
  dataset_subset
}


get_trend_take_up_data_fn <- function(dataset, LA,age_group, latest_year) 
{
  # This function gets the three year trend data for take up
  
  
  #Variables
  # dataset: trend dataset for selected LA
  # age_group: age group selected by user
  # year:       latest year
  # LA:         LA selected by the user
  
    
  # The function returns a list
  # Item 1 or dataset_name$trend_data_percents - the trend data
  # Item 2 or dataset_name$trend_graph_title   - the trend graph title
  # Item 3 or dataset_name$LA_legend           - the trend legend
  # Item 4 or dataset_name$National_legend     - the national legend
  # Item 5 or dataset_name$LA_var              - the name of the variable needed for the LA, based on user selection
  # Item 6 or dataset_name$national_var        - the name of the national variable based on user selection
  
  
  # Example
  #ztake_up_data<-get_data_subset_neighbour(long_data_step1,LA="Bury",GLD_or_AoL_or_take_up="Take_up",take_up_ages = "two_year_olds",latest_year = 2018)
  #take_up_trend_example<-Get_trend_take_up_data_fn(dataset=ztake_up_data,LA="Bury",age_group = "two_year_olds",latest_year = 2018)
  
  previous_year<-latest_year-1
  two_years_previous<-latest_year-2
  
  year_string <- as.character(latest_year)
  previous_year_string <- as.character(previous_year)
  two_years_previous_string <- as.character(two_years_previous)
   
  
  
  #create variable names to select from three year trend table
  variable_percent_stem<-paste0(age_group,"_","Percent","_")

  #Create the variable names that need to be retrieved from the table
  variable_percent_latest_LA<-paste0(variable_percent_stem,latest_year) 
  variable_percent_previous_LA<-paste0(variable_percent_stem,previous_year) 
  variable_percent_two_years_previous_LA<-paste0(variable_percent_stem,two_years_previous)
  variable_percent_latest_National<-paste0(variable_percent_stem,latest_year,"_","National") 
  variable_percent_previous_National<-paste0(variable_percent_stem,previous_year,"_","National") 
  variable_percent_two_years_previous_National<-paste0(variable_percent_stem,two_years_previous,"_","National")
 
  #Graph Title needs to specify the group of children correctly and not by abbreviation
  trend_graph_title_group<- if_else(age_group=="two_year_olds","two year olds",
                                    "three and four year olds")
  #For legend
  LA_key<- paste0(LA," take-up percent")
  National_key= "National take-up percent"
  # #Graph Title
  Plot_title<-paste0("Trend in percentage of ",trend_graph_title_group," in ",LA," benefitting from funded early education places compared to ",trend_graph_title_group," children nationally.")
  
  
  #First retrieve the trend data for the LA
  LA_data_step1<- dataset %>% filter(.,LA_Name==LA) %>%
    select(LA_Name,year1=variable_percent_latest_LA,year2=variable_percent_previous_LA,year3=variable_percent_two_years_previous_LA) %>%
  
    #dynamically change the column names to the latest three years worth of data
    select(LA_Name, !!quo_name(year_string):=year1,!!quo_name(previous_year_string):=year2,!!quo_name(two_years_previous_string):=year3)    
  
    LA_data_step2 <- LA_data_step1 %>% gather(key = "Year", value= "Percentage",-LA_Name)  %>%
      mutate(Region="LA")
  
    #Then retrieve the national data
    National_step1<- dataset  %>% filter(.,LA_Name==LA) %>%
      select(LA_Name,year1=variable_percent_latest_National,year2=variable_percent_previous_National,year3=variable_percent_two_years_previous_National)%>%
  
    #dynamically change the column names to the latest three years worth of data
    select(LA_Name, !!quo_name(year_string):=year1,!!quo_name(previous_year_string):=year2,!!quo_name(two_years_previous_string):=year3)
  
    National_step2<- National_step1 %>% gather(key ="Year", value= "Percentage",-LA_Name)  %>%
    mutate(Region="National")
  
  #Put the LA and National data into the trend table
  GLD_trend_data<- rbind(LA_data_step2,National_step2)
  
  #Function to return the trend data and also the graph and legend titles
  trend_graph_list<- list("trend_data_percents"=GLD_trend_data 
                          ,"trend_graph_title"=Plot_title,"LA_legend"=LA_key
                          ,"National_legend"=National_key,"LA_var"=variable_percent_latest_LA, "national_var"=variable_percent_latest_National)
} 



# National Graph with all LAs ---------------------------------------------
# This function is used within the national plot function
get_all_LAs_data_subset<- function(dataset,variable_name){
  # This function gets the data required for the national graph
  # It is called within another function
  
  #variables
  # dataset
  # variable_name
  
  data_subset<- dataset %>%  select(LA_Name,variable_name)  
  variable_name_sym <- sym(variable_name) #change variable name to a symbol
  dataset_subset<- data_subset %>% arrange(!!variable_name_sym) ## the !! lets r know what to do with symbol
  
}


# National Graph Function -------------------------------------------------


get_national_plot <- function(dataset="long_data_step1",neighbour_dataset,metric="GLD",group="FSM",AoL_type="",selected_LA, take_up_age="",year,show_all_LAs=TRUE,national=0,context="no"){ 
  
  # show_all_LAs=TRUE option to have all LAs on the x axis or just the selected LA and the statistical neighbours
  #   
  # Variables
  # dataset="long_data_step1"
  # neighbour_dataset  -- use this to highlight neighbours
  # metric="GLD"      -- can be "GLD", "AoL" or "Take_up"
  # group="FSM"       -- can be "FSM", "All", "All Other" and "SEN". SEN is only available for GLD. for CL and Lit combined only all is available. Not needed for take up
  # AoL_type=""       -- can be CL, Lit, Mat and "CL_and_Lit"
  # selected_LA       -- LA selected by user
  # take_up_age=""    -- can be either "two","three_and_four"
  # year              -- needs to be most recent year for dataset
  # show_all_LAs=TRUE  -- allows to toggle between showing all LA names and just the LA selected and its neighbours
  # Examples
  
  # ztake_up_data<-get_data_subset_neighbour(long_data_step1,LA="Bury",GLD_or_AoL_or_take_up="Take_up",take_up_ages = "two_year_olds",latest_year = 2018)
  # nat_take_up_graph <-get_national_plot(selected_LA="Bury",neighbour_dataset=ztake_up_data,group = "",metric="Take_up",take_up_age="two",year=2018,show_all_LAs="Yes") 
  
  
  
  input_group<-if_else(group=="All Other","All_Other"
                           ,if_else(group=="All","All",group))
  
  variable_percent_stem<-if_else(metric=="GLD",paste0(input_group,"_","Percent","_",metric,"_"),
                                 if_else(metric=="AoL",paste0(input_group,"_","Percent","_",metric,"_",AoL_type,"_")
                                         ,paste0(take_up_age,"_year_olds_Percent_") ))
  
  variable_percent_latest_LA<-paste0(variable_percent_stem,year,"_","LA") 
  
  if(metric=="Take_up"){
    variable_percent_latest_LA<-paste0(variable_percent_stem,year) 
    }

  EYFSP_variable<- as.character(variable_percent_latest_LA)
  EYFSP_variable_sym<- sym(variable_percent_latest_LA)
  
  national_graph_1<- long_data_step1  %>%  get_all_LAs_data_subset(.,EYFSP_variable)
  national_graph_1<- national_graph_1 %>% filter(!!EYFSP_variable_sym !="NULL") %>% distinct()
  national_graph_2<- national_graph_1 %>%  mutate_at(2, as.numeric)
 
   
  #need a list of neighbours
  neighbour_data<- neighbour_dataset %>%
     mutate(to_highlight=if_else(LA_Name==selected_LA,"Selected LA","Statistical Neighbour")) %>%
     select(LA_Name,to_highlight)
  
   national_graph_2 <- national_graph_2 %>% left_join(.,neighbour_data, by="LA_Name") %>%
     distinct() %>%
     mutate(to_highlight=if_else(is.na(to_highlight),"Other LAs",to_highlight)) %>%
     arrange(desc(!!EYFSP_variable_sym)) %>%
     distinct() # keep only unique rows
   
    national_graph_2<- national_graph_2 %>% mutate(LA=if_else(to_highlight=="Other LAs"," ",LA_Name))
    
    #make a factor to preserve order for the graph
    national_graph_2$LA_Name<-factor(national_graph_2$LA_Name, levels = national_graph_2$LA_Name)
    national_graph_2$to_highlight<-factor(national_graph_2$to_highlight, levels = c("Selected LA","Statistical Neighbour","Other LAs"))
    
    # To get the zero after the decimal point in the graph
    EYFSP_variable_2<-paste0(EYFSP_variable,"_2")
    EYFSP_variable_sym<-sym(EYFSP_variable)
    national_graph_2<- national_graph_2 %>%  mutate(EYFSP_variable_2=sprintf('%.1f', !!EYFSP_variable_sym))
    # end of section dded to get the zero after the decimal
    title_part=""
    if(metric=="GLD"){
      title_part=" achieving a good level of development"
    }
    
    if(metric=="AoL"){
      title_part=" achieving at least the expected level of development"
    }
    
    if(metric !="Take_up"){
    title_group=if_else(input_group=="FSM","children known to be eligible for FSM",
                       if_else(input_group=="All","all children",
                               if_else(input_group=="All_Other","all other children",
   "children identified as SEN")))
   }
   
   if(metric =="Take_up"){
     title_group=paste0(take_up_age," year olds benefitting from funded early education places")
   }
  
    p1<-national_graph_2 %>% ggplot(.,aes_string(x="LA_Name",y=EYFSP_variable, fill="to_highlight") )+ #NB aes_string as we are passing a variable rather than an actual name
          geom_col() +
          scale_fill_manual(name= ""
                            ,labels = c(selected_LA, "Neighbour","Other LAs"),
                            values = c("Selected LA"="cornflowerblue", "Statistical Neighbour"="darkseagreen3","Other LAs"="grey90"))+#,guide=FALSE)+

          scale_y_continuous(labels = scales::comma,breaks=seq(0,100,10),limits=c(0, 100))+
          theme_minimal()+
          theme(axis.ticks.x = element_line(colour = "grey30"),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank())+
         labs(x="",y="Percentage",title=paste0("The percentage of ",title_group," in ",selected_LA," compared to ",title_group," nationally.	"))+
         theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4),
               legend.position="bottom")#,
    #coord_flip()
   if(show_all_LAs==FALSE){
    p1<- p1 + scale_x_discrete(labels=national_graph_2$LA)+
              theme(axis.text.x=element_text(angle=90, hjust = 1, vjust = 0.4))
   }
    
    if(metric=="Take_up"){
      
      
      axis_upper_limit<-national_graph_2 %>% select(EYFSP_variable) %>% max()
      p1<- p1+scale_y_continuous(labels = scales::comma,breaks=seq(0,axis_upper_limit,10),limits=c(0, axis_upper_limit))
      
       # p1<- p1 +geom_hline(aes(yintercept=national,linetype="National Percentage"),colour='grey50')+#,linetype="longdash")+
       #   scale_linetype_manual(name = "", values = "longdash", #longdash
       #                         guide = guide_legend(override.aes =list(colour= "grey30")))
    }
    if(context=="no"){ # horizontal line not wanted on context national graphs
    p1<- p1 +geom_hline(aes(yintercept=national,linetype="National percentage"),colour='grey50')+#,linetype="longdash")+
      scale_linetype_manual(name = "", values = "longdash", #longdash
                            guide = guide_legend(override.aes =list(colour= "grey30")))
    }
#https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
  return(p1)

}




get_variables_selected_LA <- function(metric="GLD",group="FSM",AoL_type="",selected_LA,year){ 
   
# Variables 
  #
  # metric="GLD"      -- can be "GLD", "AoL" or "Take_up"
  # group="FSM"       -- can be "FSM", "All", "All Other" and "SEN". SEN is only available for GLD. for CL and Lit combined only all is available. Not needed for take up
  # AoL_type=""       -- can be CL, Lit, Mat and "CL_and_Lit"
  # selected_LA       -- LA selected by user
  # year              -- needs to be most recent dataset 
  
  # returns a list, names are self explanatory
  #  list(variable_percent_latest_LA,variable_percent_latest_Nat,variable_gap_latest_LA_within,variable_gap_latest_LA_with_all_other_nat,variable_gap_latest_LA_with_FSM_nat,variable_percent_latest_Nat_all_other)
  
  # Example    
  # EYFSP_nat_variable<-get_variables_selected_LA(metric="GLD",group="FSM",AoL_type="",selected_LA="Bury",year=2018)
  
  input_group<-if_else(group=="All Other","All_Other"
                       ,if_else(group=="All","All",group))
  
  
  variable_percent_stem<-if_else(metric=="GLD",paste0(input_group,"_","Percent","_",metric,"_"),
                                 paste0(input_group,"_","Percent","_",metric,"_",AoL_type,"_") )
  
  variable_percent_stem_all_other<-paste0("All_Other","_","Percent","_",metric,"_",AoL_type) 
  
  variable_percent_latest_LA<-paste0(variable_percent_stem,year,"_","LA")
  variable_percent_latest_Nat<-paste0(variable_percent_stem,year,"_","National")
  
  variable_percent_latest_Nat_all_other<-paste0(variable_percent_stem_all_other,year,"_","National")
  
  # include gaps
  variable_gap_stem<-if_else(metric=="GLD",paste0(group,"_","gap","_",metric,"_"),
                             paste0(group,"_","gap","_",metric,"_",AoL_type,"_")) 
                             
  variable_gap_latest_LA_within<-paste0(variable_gap_stem,year,"_","LA","_gap_within_LA")
  variable_gap_latest_LA_with_all_other_nat<-paste0(variable_gap_stem,year,"_","LA","_gap_All_Other_Nat_Av")
  variable_gap_latest_LA_with_FSM_nat<-paste0(variable_gap_stem,year,"_","LA","_gap_FSM_Nat_Av")
   
  
  list(variable_percent_latest_LA,variable_percent_latest_Nat,variable_gap_latest_LA_within,variable_gap_latest_LA_with_all_other_nat,variable_gap_latest_LA_with_FSM_nat,variable_percent_latest_Nat_all_other)
  
}





# Small table for notes tab -----------------------------------------------
# Easier to format by putting in table.

notes_tab_terms<- c("All other","AoL","CL","ELG","EYFSP","EY","FSM","Gap within LA","GLD","LA","Lit","NA ","Non SEN","SEN","SMAP","Statistical Neighbours","Unclassified")

notes_tab_explanations<- c("Those children who are not eligible for FSM or their FSM eligibility was unknown",
                           "Area of Learning",
                           "Communication and Language",
                           "Early Learning Goals",
                           "Early Years Foundation Stage Profile",
                           "Early Years",
                           "Those children who are known to be eligible for free school meals",
                           "Gap between FSM and all other children within the LA for a selected measure",
                           "Good Level of Development",
                           "Local Authority",
                           "Literacy",
                           "Not applicable",
                           
                           "Those children not identified as having special educational needs",
                           "Those children identified as having special educational needs",
                           "Social Mobility Action Plan",
                           "For each LA, statistical neighbours  are the LAs with the most similar socio-economic characteristics",
                           "Those children for whom SEN provision could not be determined"
                           
)


notes_df<- data.frame(notes_tab_terms,notes_tab_explanations)

colnames(notes_df)<- c("Term","Explanation")


# Additional code
# Text Wrap Function for Graphs -------------------------------------------
# This was not used, as needed the wrapping to be dynamic. This seemed easiest to do by putting the titles in renderUI
wrapper <- function(x, ...) 
{
  # Function to wrap text on graph headings
  # With thanks to https://stackoverflow.com/questions/2631780/r-ggplot2-can-i-set-the-plot-title-to-wrap-around-and-shrink-the-text-to-fit-t
  paste(strwrap(x, ...), collapse = "\n")
}



# Rounding Function -------------------------------------------------------
# R's base functionality sometimes rounds 0.5 down  and this is not the required behaviour
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}


# not currently used
format_num <- function(col) {
  #https://groups.google.com/forum/#!topic/shiny-discuss/2jlYOYFp2-A
  #Convert data to string with one decimal place to ensure correct output in renderTable
  #as.data.frame(lapply(df, format_num))
  if (is.numeric(col))
    sprintf('%1.1f', col)
  else
    col
}  




# For debug:
#http://shiny.rstudio.com/gallery/current-time.html



options(digits.secs = 3) # Include milliseconds in time display

function(input, output, session) {
  
  output$currentTime <- renderText({
    # invalidateLater causes this output to automatically
    # become invalidated when input$interval milliseconds
    # have elapsed
    invalidateLater(as.integer(input$interval), session)
    
    format(Sys.time())
  })
  
  
  
}

