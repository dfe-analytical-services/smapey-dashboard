#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Throughout the code it is assumed that the functions will only be provided with valid inputs
# Also, sometimes knowledge of the data is used, eg that the column needed is column 3 for example
# The section headings aim to make the code self explanatory
# There is also a format to get
# -- Value Boxes
# -- Data needed for graph
# -- The graph itself
# -- The graph title

# -- Some code has been retained where it is useful for debug and future development
# -- Where some solutions have been found via google I have tried to keep a link to the relevant URL


library(shiny)



shinyServer(function(input, output,session) {
  
  current_year<- 2018
  latest_year_take_up<- 2018
  
  observe_helpers()
  # this does not seem to be working, but leaving here for now:
  # https://shiny.rstudio.com/reference/shiny/0.14/updateSelectInput.html
  
  # This piece of code puts in the different options for the gaps graph only
  
  # Control gap_type menu ---------------------------------------------------
  
  # The gap type is only applicable if group selected is FSM. This code has the effect that the gap_type options are only available when the group is FSM
  
  # For GLD page
  observe_gld_group<- observe({
        x <- input$GLD_group_select
        
        # Can use character(0) to remove all choices
        y<-gaps_graph_choice
        if (x!="FSM")
          y<-character(0)
       
        # Can also set the label and select items
              updateSelectInput(session,"GLD_gap_select",
                                label = "Select gap type (FSM only)",
                                choices = y,
                                selected = tail(y, 1)
                                )
  })
  
  # For AoL single areas page 
  observe_aol_group<- observe({
        x <- input$AoL_group_select
        
        # Can use character(0) to remove all choices
        y<-gaps_graph_choice
        if (x!="FSM")
          y<-character(0)
   
      # Can also set the label and select items
      updateSelectInput(session,"AoL_gap_select",
                      label = "Select gap type (FSM only)",
                      choices = y,
                      selected = tail(y, 1)
                      )
    
  })
  
  
  
  # GLD Get Data -----------------------------------------------
  
  
  
  GLD_neighbours_plot_data <- reactive({
    
    #retrieve inputs
    GLD_input_group<-input$GLD_group_select
    level<-"LA"
    metric<-"GLD"
    selected_LA<-input$Local_Authority
    #input- alter so the group will work with functions that expect input in a specific format
    GLD_input_group<-if_else(GLD_input_group=="All Other","All_Other"
                             ,if_else(GLD_input_group=="All","All",GLD_input_group))
    
    selected_LA<-input$Local_Authority
    level<-"LA"
    metric<-"GLD"
    
    # Use function to get the dataset needed based on the inputs
    GLD_subset_1<-get_data_subset_neighbour(long_data_step1,selected_LA,GLD_or_AoL_or_take_up="GLD", group=GLD_input_group ,Percent_or_Gap="Percent",latest_year = current_year)
    GLD_subset_1<- GLD_subset_1 %>% mutate_at(3,as.numeric)
    sort<-colnames(GLD_subset_1[3]) # coding in this way makes it easier to sort by the chosen column based on user selection
    sort_sym<-  sym(sort)
    GLD_subset_2<- GLD_subset_1 %>%  arrange(desc(!!sort_sym))
    GLD_subset_2$LA_selected_by_user<-factor(GLD_subset_2$LA_selected_by_user,levels=c("yes","no"))
    GLD_subset_2$LA_Name<-factor( GLD_subset_2$LA_Name, levels =  GLD_subset_2$LA_Name) #convert to factor to preserve sort order
    GLD_subset_2
    
  })
  
  
  # GLD Output Boxes --------------------------------------------------------
  
  #Assume knowledge of the data so the column index needed is identified in each value box
  #Column will depend on group. This is because for SEN and All Other do not need to compare with all other, whereas for FSM and All the "All Other" 
  #column is included
  
  
  output$GLD_Nat <- renderValueBox({
    
    EYFSP_nat_variable<-colnames(GLD_neighbours_plot_data()[4])
    
    national<- GLD_neighbours_plot_data()[4] %>% select(!!EYFSP_nat_variable) %>% head(1) %>% as.numeric()
    
    valueBox(
      value = paste0(formatC(national, digits = 1, format = "f"),"%"),
      subtitle =  paste0(input$GLD_group_select,": National Average"),
      icon = icon("area-chart")
      )
  })
  
  
  output$GLD_LA <- renderValueBox({

    EYFSP_LA_variable<-colnames(GLD_neighbours_plot_data()[3])
    
    LA_GLD<-GLD_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    valueBox(
      value = paste0(formatC(LA_GLD, digits = 1, format = "f"),"%"),
      subtitle = paste0(input$GLD_group_select,": LA GLD"),
      icon = icon("area-chart")#,
      )
  })
  
  
  output$GLD_LA_Rank <- renderValueBox({
 
    col<- if_else(input$GLD_group_select %in% c("FSM","All"),6,5) 
    EYFSP_LA_variable<-colnames(GLD_neighbours_plot_data()[col])
    
    LA_GLD<-GLD_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    valueBox(
      value = formatC(LA_GLD, digits = 0, format = "f"),
      subtitle = paste0(input$GLD_group_select,": LA GLD National Rank"),
      icon = icon("area-chart") 
      )
  })
  
  
  output$GLD_LA_SN_Rank <- renderValueBox({
    
    col<- if_else(input$GLD_group_select %in% c("FSM","All"),7,6)
    EYFSP_LA_variable<-colnames(GLD_neighbours_plot_data()[col])
    
    LA_GLD<-GLD_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    valueBox(
      value = formatC(LA_GLD, digits = 0, format = "f"),
      subtitle = paste0(input$GLD_group_select,": LA GLD Statistical Neighbour Rank"),
      icon = icon("area-chart")
      )
  })
  
  
  
  # GLD Stats Neighbour Graph -----------------------------------------------
  
  # available for debug
  output$GLD_neighbour_table <- renderTable({ 
    #GLD_neighbours_gaps_plot_data() 
    
    dataset<-GLD_neighbours_gaps_plot_data()
    EYFSP_variable<-colnames(GLD_neighbours_gaps_plot_data()[3])
    selected_LA<- input$Local_Authority
    GLD_input_group<-input$GLD_group_select
    
    #For graph title/legend/labels
    group_type<-if_else(GLD_input_group=="All Other","All_Other"
                        ,if_else(GLD_input_group=="All","All",GLD_input_group))
    title_group=if_else(group_type=="FSM","children known to be eligible for FSM",
                        if_else(group_type=="All","all children",
                                if_else(group_type=="All_Other","all other children",
                                        "children identified as SEN")))
    
    
    dataset_2<- dataset %>% select("Percentage_Gap"=EYFSP_variable,everything())
    dataset_3<- dataset_2 %>% mutate(LA_selected_by_user=if_else((LA_Name==selected_LA& Percentage_Gap<0),"Chosen LA below national average",
                                                                 if_else(LA_Name==selected_LA& Percentage_Gap>=0,"Chosen LA above or equal to national average",
                                                                         if_else(Percentage_Gap<0,"Neighbour below national average","Neighbour above or equal to national average"))))
    
    dataset_3 %>% select(Percentage_Gap,LA_Name,LA_selected_by_user)
  })
  
  output$GLD_neighbour_plot <- renderPlot({
    
    # Data prep
    dataset<-GLD_neighbours_plot_data()
    EYFSP_variable<-colnames(GLD_neighbours_plot_data()[3])
    # To get the zero after the decimal point in the graph
    EYFSP_variable_2<-paste0(EYFSP_variable,"_2")
    EYFSP_variable_sym<-sym(EYFSP_variable)
    dataset<- dataset %>%  mutate(EYFSP_variable_2=sprintf('%.1f', !!EYFSP_variable_sym))
    # end of section dded to get the zero after the decimal
    
   
    
    selected_LA<- input$Local_Authority
    GLD_input_group<-input$GLD_group_select
    GLD_input_group<-if_else(GLD_input_group=="All Other","All_Other"
                             ,if_else(GLD_input_group=="All","All",GLD_input_group))
    
    National_variable_name<- colnames(dataset[4])
    National_figure<- dataset %>% select(National_variable_name) %>% distinct %>% as.numeric()
    
    #For graph title/legend/labels 
    group_type<-if_else(GLD_input_group=="All Other","All_Other"
                        ,if_else(GLD_input_group=="All","All",GLD_input_group))
    
    
    graph_legend_group<-if_else(group_type=="All_Other","all other",
                                if_else(group_type=="All","all",
                                        group_type))
    graph_legend<- paste0("National percentage for ",graph_legend_group)
    
    # Create plot
    plot_output<- ggplot(dataset,aes_string(x="LA_Name",y=EYFSP_variable, fill="LA_selected_by_user") )+ #NB aes_string as we are passing a variable rather than an actual name
      geom_col() +  
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.justification = "top")+
      scale_y_continuous(labels = scales::comma,breaks=seq(0,100,10),limits=c(0, 100))+
      labs(y="Percentage",x="Neighbour")+ 
      geom_hline(aes(yintercept=National_figure,linetype=graph_legend),colour='grey50')+
      scale_linetype_manual(name = "", values = "longdash",
                            guide = guide_legend(override.aes =list(colour= "grey30")))+  
       
      #geom_text(aes_string(label=EYFSP_variable,vjust = -0.25), position=position_dodge(width=0.9))+ 
      geom_text(aes(label=EYFSP_variable_2,vjust = -0.25), position=position_dodge(width=0.9))+
      #geom_text(aes_string(label=sprintf('%s', EYFSP_variable),vjust = -0.25), position=position_dodge(width=0.9),parse=TRUE)+ 
      #geom_text(aes_string(label=34,vjust = -0.25), position=position_dodge(width=0.9))+ 
     scale_fill_manual(name= ""
                        ,labels = c(selected_LA, "Neighbours"),
                        values = c( "no"="darkseagreen3","yes"="cornflowerblue"))+ 
      guides(
      fill = guide_legend(order = 1))+ # change the order of the legend, as prefer the horizontal line to be after the bars label
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
    
    # There will be two national lines for FSM and All, as the All Other National will be included
    if(group_type %in% c("FSM","All")){
      National_variable_name_2<- colnames(dataset[5])
      National_figure_2<- dataset %>% select(National_variable_name_2) %>% distinct %>% as.numeric()
      plot_output<- plot_output+ geom_hline(aes(yintercept=National_figure_2,linetype="National percentage for all other"),colour='black',show.legend=FALSE)+#,show_guide=FALSE)+
        scale_linetype_manual(name = "", values = c(2,2),#"longdash",
                              guide = guide_legend(override.aes =list(colour= c("black","grey50"))))
    }
    
    plot_output 

    # manually add a legend https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r/24497113        
    # https://stackoverflow.com/questions/39119917/how-to-add-a-legend-to-hline       
    
  })
  
  
  
  
  # GLD Stats Neighbour Plot Title -------------------------------------
  output$GLD_neighbour_plot_graph_title <- renderUI({
    
    selected_LA_Name<-input$Local_Authority
    str1 <- get_graph_title(neighbour_gap_context="neighbour",LA=input$Local_Authority,group=input$GLD_group_select, GLD_or_AoL_or_take_up="GLD")
    HTML(str1)
  })
  
  
  
  # GLD Text Under Stat Neighbours Plot -------------------------------------
  
  
  output$GLD_text <- renderUI({
    
    selected_LA_Name<-input$Local_Authority
    col<- if_else(input$GLD_group_select %in% c("FSM","All"),5,4)
    EYFSP_nat_variable<-colnames(GLD_neighbours_plot_data()[col])
    national_perc<- GLD_neighbours_plot_data() %>% select(!!EYFSP_nat_variable) %>% head(1) %>% as.numeric()
    
    EYFSP_LA_variable<-colnames(GLD_neighbours_plot_data()[3])
    LA_perc<-GLD_neighbours_plot_data() %>% filter(LA_Name==selected_LA_Name) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    # Gaps
    gap_dataset<-GLD_neighbours_gaps_plot_data()
    EYFSP_gap_variable<-colnames(GLD_neighbours_gaps_plot_data()[3])
    selected_LA<- input$Local_Authority
    
    national_gap_perc<- gap_dataset%>% filter(LA_Name==selected_LA)%>%
      select(!!EYFSP_gap_variable) %>%
      as.numeric()
    
    above_below_equal<- if_else(national_gap_perc<0,"below",
                                if_else(national_gap_perc==0,"ie equal to","above"))
    
    
    GLD_input_group<-input$GLD_group_select 
    group_type<-if_else(GLD_input_group=="All Other","All_Other"
                        ,if_else(GLD_input_group=="All","All",GLD_input_group))
    title_group=if_else(group_type=="FSM","children known to be eligible for FSM",
                        if_else(group_type=="All","all children",
                                if_else(group_type=="All_Other","all other children",
                                        "children identified as SEN")))
    
    
    comparison_group=if_else(group_type%in% c("FSM","All_Other","All"),"all other children",
                             
                             "children identified as SEN") 
    
    
    selected_gap_type<- if_else(GLD_input_group=="FSM",input$GLD_gap_select,"")
    selected_gap_type<- if_else(selected_gap_type=="Within the LA: gap with all other children","all other children in ",
                                if_else(selected_gap_type=="National average: gap with all other children","all other children",
                                        if_else(selected_gap_type=="National average: gap with FSM children","children known to be eligible for FSM","")))
    
    gap_comparison_group=if_else(group_type== "FSM" ,selected_gap_type,
                                 if_else(group_type== "All_Other","all other children",
                                         if_else(group_type=="All", "all children", "children identified as SEN")))
  
    
    # need to ensure that numbers such as 72.0% are not reported as 72%
    LA_perc_text<-sprintf('%.1f', LA_perc)
    national_perc_text<- sprintf('%.1f',national_perc)
    abs_national_gap_perc_text<- sprintf('%.1f',abs(national_gap_perc))
    
   # str1 <- paste("For ", input$Local_Authority," ",LA_perc_text,"% of children " , title_group," achieve a good level of development compared to  ",
    str1 <- paste0("In ", input$Local_Authority,", ",LA_perc_text,"% of " , title_group," achieve a good level of development compared to  ",
                  national_perc_text,"% of ",comparison_group," nationally.")
    str2 <- paste0("In ", input$Local_Authority,", the gap for ",title_group," is ", abs_national_gap_perc_text," percentage points ",
                  above_below_equal, " the national average for ",gap_comparison_group,".")
    
    if(group_type=="FSM"&gap_comparison_group=="all other children in "){
      str2 <- paste0("In ", input$Local_Authority,", the gap for ",title_group," is ", abs_national_gap_perc_text," percentage points ",
                    above_below_equal, " ",gap_comparison_group, " ",input$Local_Authority,".")
       

    }
    
    HTML(paste(str1, str2, sep = '<br/>'))
  })
  
  
  
  
  
  # GLD Gaps Stats Neighbour Graphs Data ----------------------------------------
  GLD_neighbours_gaps_plot_data <- reactive({
    GLD_input_group<-input$GLD_group_select
    GLD_input_group<-if_else(GLD_input_group=="All Other","All_Other"
                             ,if_else(GLD_input_group=="All","All",GLD_input_group))
    selected_LA<-input$Local_Authority
    level<-"LA"
    metric<-"GLD"
    
    selected_gap_type<- if_else(GLD_input_group=="FSM",input$GLD_gap_select,"")
    selected_gap_type<- if_else(selected_gap_type=="Within the LA: gap with all other children","gap_within_LA",
                                if_else(selected_gap_type=="National average: gap with all other children","gap_All_Other_Nat_Av",
                                        if_else(selected_gap_type=="National average: gap with FSM children","gap_FSM_Nat_Av","")))
    selected_gap_type<- if_else(GLD_input_group=="FSM",selected_gap_type,"")
    GLD_gaps_subset_1<-get_data_subset_neighbour(long_data_step1,selected_LA,GLD_or_AoL_or_take_up="GLD",group=GLD_input_group,Percent_or_Gap="gap",gap_type=selected_gap_type,latest_year = current_year)#selected_gap_type,latest_year = 2017)
    
    GLD_gaps_subset_1<- GLD_gaps_subset_1 %>% mutate_at(3,as.numeric)
    sort<-colnames(GLD_gaps_subset_1[3])
    sort_sym<-  sym(sort) 
    GLD_gaps_subset_2<- GLD_gaps_subset_1 %>%  arrange(!!sort_sym)#arrange(sort_sym)
    GLD_gaps_subset_2$LA_Name<-factor( GLD_gaps_subset_2$LA_Name, levels =  GLD_gaps_subset_2$LA_Name) #convert to factor to preserve sort order
    GLD_gaps_subset_2
    
  })
  
  
  
  
  
  
  # GLD Stats Neighbour Gaps Graph -----------------------------------------------
  
  output$GLD_neighbour_gaps_plot <- renderPlot({
    
    dataset<-GLD_neighbours_gaps_plot_data()
    EYFSP_variable<-colnames(GLD_neighbours_gaps_plot_data()[3])
    selected_LA<- input$Local_Authority
    GLD_input_group<-input$GLD_group_select
    
    #For graph title/legend/labels
    group_type<-if_else(GLD_input_group=="All Other","All_Other"
                        ,if_else(GLD_input_group=="All","All",GLD_input_group))
    title_group=if_else(group_type=="FSM","children known to be eligible for FSM",
                        if_else(group_type=="All","all children",
                                if_else(group_type=="All_Other","all other children",
                                        "children identified as SEN")))
   
    
    selected_gap_type<- if_else(GLD_input_group=="FSM",input$GLD_gap_select,"")
    selected_gap_type<- if_else(selected_gap_type=="Within the LA: gap with all other children","gap_within_LA",
                                if_else(selected_gap_type=="National average: gap with all other children","gap_All_Other_Nat_Av",
                                        if_else(selected_gap_type=="National average: gap with FSM children","gap_FSM_Nat_Av","")))
    selected_gap_type<- if_else(GLD_input_group=="FSM",selected_gap_type,"")
     
    
    dataset_2<- dataset %>% select("Percentage_Gap"=EYFSP_variable,everything())
    dataset_3<- dataset_2 %>% mutate(LA_selected_by_user=if_else((LA_Name==selected_LA& Percentage_Gap<0),"Chosen LA below national average",
                                                                 if_else(LA_Name==selected_LA& Percentage_Gap>=0,"Chosen LA above or equal to national average",
                                                                         if_else(Percentage_Gap<0,"Neighbour below national average","Neighbour above or equal to national average"))))
    
    
    # To get the zero after the decimal point in the graph
    gap_2<-paste0("Percentage_Gap","_2")
    gap_sym<-sym(gap_2)
    dataset_3<- dataset_3 %>%  mutate(gap_2=sprintf('%.1f', Percentage_Gap))
    # end of section dded to get the zero after the decimal
    
    # Ensures that only one of Chosen LA below/above is used
    labels_for_graph<- dataset_3 %>% select(LA_selected_by_user) %>% distinct()
    
    #add two line to ensure teh data is in descending order- consistent with the neighbours graph
    dataset_3 <- dataset_3 %>% arrange(desc(Percentage_Gap))
    dataset_3$LA_Name<- factor(dataset_3$LA_Name,levels=dataset_3$LA_Name)
    
    
    #selected_LA<- LA#input$Local_Authority
    plot<- ggplot(dataset_3,aes(x=LA_Name,y=Percentage_Gap, fill=LA_selected_by_user) )+ #NB aes_string as we are passing a variable rather than an actual name
      geom_col() + #fill="darkseagreen3"
      scale_fill_manual(name= "",
                       #  labels = c("Chosen LA above or equal to national average", "Neighbour above or equal to national average","Chosen LA below national average","Neighbour below national average"),
                      labels=labels_for_graph,  
                       values = c("Chosen LA above or equal to national average"="cornflowerblue", "Neighbour above or equal to national average"="darkseagreen3",
                                   "Chosen LA below national average"="orange","Neighbour below national average"="navajowhite2"))+#,guide=FALSE
      
      
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank())+
      theme(legend.position = "bottom")+
      #geom_text(aes(label=Percentage_Gap,vjust = ifelse(Percentage_Gap >= 0, -0.25, 1)), position=position_dodge(width=0.9))+#, vjust=-0.25)+
      geom_text(aes(label=gap_2,vjust = ifelse(Percentage_Gap >= 0, -0.25, 1)), position=position_dodge(width=0.9))+#, vjust=-0.25)+
      
     labs(x="",y="Percentage point gap")+
    theme(legend.position = "bottom",
                                        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
    plot
    
  })
  
   
  # output$GLD_neighbour_plot_graph_title <- renderUI({
  #   
  #   selected_LA_Name<-input$Local_Authority
  #   str1 <- get_graph_title(neighbour_gap_context="neighbour",LA=input$Local_Authority,group=input$GLD_group_select, GLD_or_AoL_or_take_up="GLD")
  #   HTML(str1)
  # })
  # 
  
  # GLD Neighbour Gap Plot Title -------------------------------------
  output$GLD_neighbour_gap_plot_graph_title <- renderUI({
    
    selected_LA_Name<-input$Local_Authority
    selected_gap_type<- if_else(input$GLD_group_select=="FSM",input$GLD_gap_select,"")
    str1 <- get_graph_title(neighbour_gap_context="neighbour_gap",LA=input$Local_Authority,group=input$GLD_group_select, GLD_or_AoL_or_take_up="GLD",gap_type=selected_gap_type)
    HTML(str1)
  })
  
  
  # GLD Trend Graph ---------------------------------------------------------
   GLD_trend_plot_data <- reactive({
     
     #retrieve and format inputs
    GLD_input_group<-input$GLD_group_select
    GLD_input_group<-if_else(GLD_input_group=="All Other","All_Other"
                             ,if_else(GLD_input_group=="All","All",GLD_input_group))
    
    
    selected_LA<-input$Local_Authority
    selected_gap_type<- if_else(GLD_input_group=="FSM",input$GLD_gap_select,"")
    selected_gap_type<- if_else(selected_gap_type=="Within the LA: gap with all other children","gap_within_LA",
                                if_else(selected_gap_type=="National average: gap with all other children","gap_All_Other_Nat_Av",
                                        if_else(selected_gap_type=="National average: gap with FSM children","gap_FSM_Nat_Av","")))
    selected_gap_type<- if_else(GLD_input_group=="FSM",selected_gap_type,"")
    
    get_trend_data_fn_v2(three_years_data_step1,group=GLD_input_group,LA=selected_LA,GLD_or_AoL_or_Take_up="GLD",gap_type=selected_gap_type,latest_year=current_year) #for debug
  })
  
  
  
  output$GLD_trend_table <- renderDT({
    
    selected_LA<- input$Local_Authority
    GLD_trend_data<-GLD_trend_plot_data()$trend_data_percents
    
    GLD_trend_data_spread <- GLD_trend_data %>% spread(Year,Percentage) %>%
      mutate(Region=if_else(Region=="LA",selected_LA,Region)) %>%
      select(-LA_Name) %>% select(" "=Region, everything())  %>% 
      # This line is to ensure that point zero ie 77.0 not 77 is displayed
      mutate_if(is.numeric,formatC,digits=1,format="f")
    
    datatable(GLD_trend_data_spread, 
    options=list(
     
            paging=FALSE,
            searching=FALSE,
            info=FALSE
           
              ),
    rownames = FALSE,
    class = 'cell-border stripe'
    )
  })
  
  
  ##Used for debug 
  
  
  # for debug
  output$GLD_table_data <- renderTable({#renderText({ # #depending on dbug need may renderText or renderTable
    GLD_neighbours_gaps_plot_data() 
  })
  
  
  
  output$GLD_trend_plot <- renderPlot({
    
    GLD_trend_data_list<-GLD_trend_plot_data()
    GLD_trend_data<-GLD_trend_plot_data()$trend_data_percents
    Plot_title_1<- GLD_trend_plot_data()$trend_graph_title
    
    LA_key<-GLD_trend_plot_data()$LA_legend
    National_key<-GLD_trend_plot_data()$National_legend
    
    GLD_trend_data_table_1<- GLD_trend_data %>%  spread(Year, Percentage) %>% select(-LA_Name)
    GLD_trend_data_table_2<- GLD_trend_data_table_1 %>%  mutate(Metric=if_else(Region=="LA",LA_key,
                                                                               National_key)) %>% select(-Region) %>% select(Metric, everything())
    GLD_trend_plot<-
      ggplot(data=GLD_trend_data, aes(x= Year, y=Percentage, group=Region,color=Region)) +
      geom_line(linetype="dashed",size=0.8)+#,size=1.3
      geom_point(size=4)+
      
      scale_y_continuous(labels = scales::comma,breaks=seq(0,100,10),limits=c(0, 100))+
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.justification = "top")+
 
      labs(y="Percentage",x="Year")+
      theme(legend.position = "bottom")
    
    #overwriting legends http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
    GLD_trend_plot+ scale_colour_manual(values = c(LA = "cornflowerblue", National = "grey"),
                                        name="",
                                        breaks=c("LA", "National" ),
                                        labels=c(LA_key, National_key))
  })
  
  # GLD Trend Plot Title -------------------------------------
  
  
  output$GLD_trend_plot_graph_title <- renderUI({
    str1 <- GLD_trend_plot_data()$trend_graph_title
    HTML(str1)
  })  
  


# GLD National Plot -------------------------------------------------------

    
  output$GLD_national_plot <- renderPlot({
    GLD_input_group<-input$GLD_group_select
     
    National_variable_name<- colnames(GLD_neighbours_plot_data()[4])
    National_figure<- GLD_neighbours_plot_data() %>% select(National_variable_name) %>% distinct %>% as.numeric()
    GLD_input_group<-if_else(GLD_input_group=="All Other","All_Other"
                             ,if_else(GLD_input_group=="All","All",GLD_input_group))
    show_all_LAs_selection<- if_else(input$show_all_LAs_GLD_tab=="Yes",TRUE,FALSE) #toggle whether to show all LAs or just the selected LA and statistical neighbours
    get_national_plot(selected_LA=input$Local_Authority,neighbour_dataset=GLD_neighbours_plot_data(),group = GLD_input_group,show_all_LAs=show_all_LAs_selection,year=current_year,national=National_figure) 
    
  })
  
  # GLD Trend Gaps Graph ---------------------------------------------------------
  
  GLD_trend_gap_plot_data <- reactive({
    GLD_input_group<-input$GLD_group_select
    GLD_input_group<-if_else(GLD_input_group=="All Other","All_Other"
                             ,if_else(GLD_input_group=="All","All",GLD_input_group))
    
    selected_LA<-input$Local_Authority
    get_trend_data_fn_v2(three_years_data_step1,group=GLD_input_group,LA=selected_LA,GLD_or_AoL_or_Take_up="GLD",gap="gap",latest_year = current_year)  
  })
  
  
  
  # used for debug only
  output$GLD_trend_gap_table <- renderTable({#renderText({ # #depending on dbug need may renderText or renderTable
    
    #GLD_neighbours_plot_data()
    #GLD_national_plot_data()
    #GLD_SEN_Numbers_plot_data()
    #SEN_table_data()
    #GLD_neighbours_gaps_plot_data()
    data<-GLD_trend_plot_data()
    data$trend_data_gaps
  }) 
  
  
  
  output$GLD_trend_gap_plot <- renderPlot({
    
    GLD_trend_data_list<-GLD_trend_plot_data()
    GLD_trend_data<-GLD_trend_plot_data()$trend_data_gaps
    
    gaps_data<-GLD_trend_data %>% mutate(above_or_below=if_else(`Percentage Gap`>=0,"above_or_equal","below"))
    
    
    
    plot<- ggplot(gaps_data,aes(x=Year,y=`Percentage Gap`, fill=above_or_below) )+ #NB aes_string as we are passing a variable rather than an actual name
      geom_col() +  
      scale_fill_manual(name= ""
                        ,labels = c("below", "above_or_equal"),
                        values = c("above_or_equal"="cornflowerblue",
                                   "below"="orange"),guide=FALSE)+#,guide=FALSE
      
      geom_text(aes(label=`Percentage Gap`,vjust = ifelse(`Percentage Gap` >= 0, -0.25, 1)), position=position_dodge(width=0.9))+
      
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank()) +
      labs(y="Percentage point gap")
    
    plot  
  })
  

# GLD Trend Plot Title ----------------------------------------------------

  
  output$GLD_trend_gap_plot_graph_title <- renderUI({
    str1 <- GLD_trend_plot_data()$trend_gaps_graph_title
    HTML(str1)
  })  
  
   
# Context Tab -------------------------------------------------------------


# Context Value Boxes -----------------------------------------------------
  
  #Assume knowledge of the data so the column index needed is identified in each value box
  
  
  output$context_GLD_LA_FSM <- renderValueBox({
    
    EYFSP_nat_variable<-get_variables_selected_LA(metric="GLD",group="FSM",AoL_type="",selected_LA=input$Local_Authority,year=current_year)
    EYFSP_nat_variable_2<-unlist(EYFSP_nat_variable[[1]])
    
    LA<- long_data_step1 %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_nat_variable_2) %>% 
                            distinct() %>% as.numeric()
  
    valueBox(
      value = paste0(formatC(LA, digits = 1, format = "f"),"%"),
      subtitle =  paste0(input$Local_Authority,": FSM GLD"),
      icon = icon("area-chart"),
      color="blue"
    )
  })
  
  
  output$context_GLD_nat <- renderValueBox({
    
    EYFSP_nat_variable<-get_variables_selected_LA(metric="GLD",group="FSM",AoL_type="",selected_LA=input$Local_Authority,year=current_year)
    EYFSP_nat_variable_2<-unlist(EYFSP_nat_variable[[2]])
    
    national<- long_data_step1 %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_nat_variable_2) %>% 
      distinct() %>% as.numeric()

    valueBox(
      value = paste0(formatC(national, digits = 1, format = "f"),"%"),
      subtitle =  "National: FSM GLD",
      icon = icon("area-chart"),
      color="teal"
    )
  })
  
  output$context_GLD_LA_FSM_within_LA_gap <- renderValueBox({
    
    EYFSP_nat_variable<-get_variables_selected_LA(metric="GLD",group="FSM",AoL_type="",selected_LA=input$Local_Authority,year=current_year)
    EYFSP_nat_variable_2<-unlist(EYFSP_nat_variable[[3]])
    
    gap<- long_data_step1 %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_nat_variable_2) %>% 
      distinct() %>% as.numeric()
    
    gap_text=if_else(gap>0,paste0("+",formatC(gap, digits = 1, format = "f")),as.character(formatC(gap, digits = 1, format = "f")))
    gap_text<-paste0(gap_text,"%")
    
    valueBox(
      value = formatC(gap_text, digits = 1, format = "f"),
      subtitle =  "Gap within LA",
      icon = icon("area-chart"),
      color=if_else(is_na(gap),"black",
                    if_else(gap<0,"red","aqua"))
    )
  })
  
  
  
  output$context_GLD_LA_FSM_gap_all_other_nat <- renderValueBox({
    
    EYFSP_nat_variable<-get_variables_selected_LA(metric="GLD",group="FSM",AoL_type="",selected_LA=input$Local_Authority,year=current_year)
    EYFSP_nat_variable_2<-unlist(EYFSP_nat_variable[[4]])
    
    gap<- long_data_step1 %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_nat_variable_2) %>% 
      distinct() %>% as.numeric()
    
    #gap_text=if_else(gap>0,paste0("+",gap),as.character(gap))
    gap_text=if_else(gap>0,paste0("+",formatC(gap, digits = 1, format = "f")),as.character(formatC(gap, digits = 1, format = "f")))
    gap_text<-paste0(gap_text,"%")
    
    
    
    valueBox(
      value = formatC(gap_text, digits = 1, format = "f"),
      subtitle =  "Gap of LA FSM GLD  with All Other National Average",
      icon = icon("area-chart"),
      color=if_else(is_na(gap),"black",
                    if_else(gap<0,"red","aqua"))
    )
  })
  
          
  output$context_GLD_LA_FSM_gap_FSM_nat <- renderValueBox({
    
    EYFSP_nat_variable<-get_variables_selected_LA(metric="GLD",group="FSM",AoL_type="",selected_LA=input$Local_Authority,year=current_year)
    EYFSP_nat_variable_2<-unlist(EYFSP_nat_variable[[5]])
    
    gap<- long_data_step1 %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_nat_variable_2) %>% 
      distinct() %>% as.numeric()
    
    #gap_text=if_else(gap>0,paste0("+",gap),as.character(gap))
    gap_text=if_else(gap>0,paste0("+",formatC(gap, digits = 1, format = "f")),as.character(formatC(gap, digits = 1, format = "f")))
    gap_text<-paste0(gap_text,"%")
    
    valueBox(
      value = formatC(gap_text, digits = 1, format = "f"),
      subtitle =  "Gap FSM GLD with FSM GLD National Average",
      icon = icon("area-chart"),
      color=if_else(is_na(gap),"black",
                    if_else(gap<0,"red","aqua"))
    )
  })
  
  output$context_GLD_nat_all_other <- renderValueBox({
    
    EYFSP_nat_variable<-get_variables_selected_LA(metric="GLD",group="FSM",AoL_type="",selected_LA=input$Local_Authority,year=current_year)
    EYFSP_nat_variable_2<-unlist(EYFSP_nat_variable[[6]])
    
    national<- long_data_step1 %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_nat_variable_2) %>% 
      distinct() %>% as.numeric()
    
    valueBox(
      value = paste0(formatC(national, digits = 1, format = "f"),"%"),
      subtitle =  "National: All Other GLD",
      icon = icon("area-chart"),
      color="teal"
    )
  })
  

# Context GLD National Plot FSM and All Other ---------------------------------------------------

  GLD_national_plot_data_FSM <- reactive({
    EYFSP_nat_variable<-get_variables_selected_LA(metric="GLD",group="FSM",AoL_type="",selected_LA=input$Local_Authority,year=current_year)
    EYFSP_nat_variable_2<-unlist(EYFSP_nat_variable[[1]])  
    national_FSM<- long_data_step1 %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_nat_variable_2) %>% 
      distinct() %>% as.numeric()
    
    show_all_LAs_selection<- if_else(input$show_all_LAs_context_tab=="Yes",TRUE,FALSE)
    get_national_plot(selected_LA=input$Local_Authority,neighbour_dataset=GLD_neighbours_plot_data(),group = "FSM",show_all_LAs=show_all_LAs_selection,year=current_year,national=national_FSM,context="yes")
    
  })
  
  output$GLD_national_plot_graph_FSM <- renderPlot({ 
    show_all_LAs_selection<- if_else(input$show_all_LAs_context_tab=="Yes",TRUE,FALSE)
    GLD_national_plot_data_FSM()
  })
  
  
  GLD_national_data_all_other <- reactive({
    # national all other figure
    EYFSP_nat_variable<-get_variables_selected_LA(metric="GLD",group="FSM",AoL_type="",selected_LA=input$Local_Authority,year=current_year)
    EYFSP_nat_variable_2<-unlist(EYFSP_nat_variable[[6]])  
    national_all_other<- long_data_step1 %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_nat_variable_2) %>% 
      distinct() %>% as.numeric()
    
    show_all_LAs_selection<- if_else(input$show_all_LAs_context_tab=="Yes",TRUE,FALSE)
    get_national_plot(selected_LA=input$Local_Authority,neighbour_dataset=GLD_neighbours_plot_data(),group = "All Other",show_all_LAs=show_all_LAs_selection,year=current_year,national=national_all_other,context="yes")
    
  })
  
  output$GLD_national_plot_all_other <- renderPlot({ 
    GLD_national_data_all_other() 
  })
  
  
  # Context Number of SEN and FSM ---------------------------------------------------
  

# Context FSM Data --------------------------------------------------------

  
  # FSM Context
  GLD_FSM_Numbers_plot_data <- reactive({
    
    latest_year<-current_year
    selected_LA<-input$Local_Authority
    
    #LA Number of FSM Eligible - retrieve data for the selected LA
    Num_FSM_LA_latest_year<-paste0("FSM_Number_GLD_",latest_year,"_LA")
    Num_all_LA_latest_year<-paste0("All_Number_GLD_",latest_year,"_LA")
    Num_all_other_number_GLD_latest_year<-paste0("All_Other_Number_GLD_",latest_year,"_LA")
    
   
    Num_FSM_LA_latest_year_sym<- sym(Num_FSM_LA_latest_year)
    Num_all_LA_latest_year_sym<- sym(Num_all_LA_latest_year)
    Num_all_other_number_GLD_latest_year_sym<- sym(Num_all_other_number_GLD_latest_year)
    
    number_FSM_data<- long_data_step1 %>% select("region"=LA_Name,Num_FSM_LA_latest_year ,  
                                                 Num_all_LA_latest_year,
                                                 Num_all_other_number_GLD_latest_year 
    ) %>%
      mutate_at(c(2,3),as.integer) %>%
      filter(region==selected_LA) %>%
      distinct()
    
    # calculate the percentage known to be receiving FSM
    number_percent_FSM_data<-  number_FSM_data %>% mutate(percent_elig_FSM=round2(!!Num_FSM_LA_latest_year_sym/!!Num_all_LA_latest_year_sym*100,n=1),
                                                          percent_elig_All_Other=round2(!!Num_all_other_number_GLD_latest_year_sym/!!Num_all_LA_latest_year_sym*100,n=1) )
    
    
    # get the percentages for the LA
    percent_FSM_LA<-number_percent_FSM_data %>% select(region,percent_elig_FSM,percent_elig_All_Other)
    
    
    #variable names
    Num_FSM_latest_year_National<-paste0("FSM_Number_GLD_",latest_year,"_National")
    Num_all_latest_year_National<-paste0("All_Number_GLD_",latest_year,"_National")
    Num_all_other_number_GLD_latest_year_National<-paste0("All_Other_Number_GLD_",latest_year,"_National")
    
    
    # Variable names as symbols for input into dplyr functions
    Num_FSM_latest_year_National_sym<- sym(Num_FSM_latest_year_National)
    Num_all_latest_year_National_sym<- sym(Num_all_latest_year_National)
    Num_all_other_number_GLD_latest_year_National_sym<- sym(Num_all_other_number_GLD_latest_year_National)
    
    #National Numbers of FSM are eligible
    national_FSM<- additional_nat_stats %>% select("region"=Country,Num_FSM_latest_year_National,
                                                   Num_all_latest_year_National,
                                                   Num_all_other_number_GLD_latest_year_National)
    
    number_percent_FSM_data_national<-  national_FSM %>% mutate(percent_elig_FSM=round2(!!Num_FSM_latest_year_National_sym/!!Num_all_latest_year_National_sym*100,n=1),
                                                                percent_elig_All_Other=round2(!!Num_all_other_number_GLD_latest_year_National_sym/!!Num_all_latest_year_National_sym*100,n=1) )
    
    percent_FSM_National<-number_percent_FSM_data_national %>% select(region,percent_elig_FSM,percent_elig_All_Other)
    
    
    # Combine LA and national
    percent_FSM_elig_data<- rbind(percent_FSM_LA,percent_FSM_National) %>% select(region,FSM=percent_elig_FSM,`All Other`=percent_elig_All_Other)
    
    # put in tidy format
    percent_FSM_elig_data_2<- percent_FSM_elig_data %>% gather(key="group",value="Percentage",-region)  
    percent_FSM_elig_data_2$group<-factor(percent_FSM_elig_data_2$group,levels=c("FSM","All Other"))
    
    ## return the table for the LA and National, Number and Percent
    names(number_percent_FSM_data)<- c("Region","Number of FSM","Number all","Number all other","Percentage FSM","Percentage all other")
    names(number_percent_FSM_data_national)<- c("Region","Number of FSM","Number all","Number all other","Percentage FSM","Percentage all other")  
    
    number_percent_FSM_data_LA_Nat<- rbind(number_percent_FSM_data_national,number_percent_FSM_data)
    number_percent_FSM_data_LA_Nat
    
    FSM_context_list<- list("FSM_context_table"=number_percent_FSM_data_LA_Nat,"FSM_context_graph_data"=percent_FSM_elig_data_2)
    FSM_context_list
    
  })
 

# Context SEN Table -------------------------------------------------------

   
  output$SEN_context_table <- renderTable({ 
    
    table<- GLD_SEN_Numbers_plot_data()[1] %>% as.data.frame()
    names(table)<- c("Region","Number of SEN","Number of non SEN","Percentage SEN","Percentage non SEN")
    table<- table %>% select(Region,`Number of SEN`,`Number of non SEN`)%>% 
    mutate_if(is.numeric, formatC, big.mark=",", format = "d") # this line ensures large numbers are displayed with commas
    table    

  }, align='rrr')
 

# Context FSM Plot --------------------------------------------------------

   
  output$FSM_Num_Perc_plot <- renderPlot({
    
    selected_LA<-input$Local_Authority
    percent_FSM_elig_data<-GLD_FSM_Numbers_plot_data()[[2]]  
    
    percent_FSM_elig_data<- percent_FSM_elig_data %>% 
      mutate(Percentage_2= formatC(Percentage, format="f", big.mark=",",digits=1))
    
    percent_FSM_elig_data$group<-factor(percent_FSM_elig_data$group,levels=c("All Other","FSM"))
    
    FSM_LA_National_Plot_1<- ggplot(percent_FSM_elig_data,aes(x=region,y=Percentage, fill=group) )+
      geom_col() +  
      #geom_text(aes(label=Percentage), position = position_stack(vjust = 0.5))+
      geom_text(aes(label=Percentage_2), position = position_stack(vjust = 0.5))+
      scale_fill_manual(name= "",
                        #control legend order
                        values = c("All Other"="grey90", "FSM"="grey50"),breaks=c("FSM","All Other"))+   
      
      scale_y_continuous(labels = scales::comma,breaks=seq(0,100,10),limits=c(0, 100))+
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            aspect.ratio = 1/3,
            legend.position="bottom")+
      #labs(x="",title=paste0("Graph comparing the percentage of children eligible for FSM in ",selected_LA," with national percentage.	"))+
      labs(x="")+
      coord_flip()
    
    FSM_LA_National_Plot_1
  })
  
  
  # Context FSM  Plot Title -------------------------------------
  output$FSM_context_graph_title <- renderUI({
    
    selected_LA_Name<-input$Local_Authority
    str1 <- get_graph_title(neighbour_gap_context="context",LA=input$Local_Authority,group="FSM")
    HTML(str1)
  })
 

# Context SEN Plot Data ---------------------------------------------------

  
  GLD_SEN_Numbers_plot_data <- reactive({
    
    selected_LA<-input$Local_Authority
    latest_year<-current_year
    Num_SEN_LA_latest_year<-paste0("SEN_Number_GLD_",latest_year,"_LA")
    Num_all_LA_latest_year<-paste0("All_Number_GLD_",latest_year,"_LA")
    Num_SEN_LA_latest_year_sym<- sym(Num_SEN_LA_latest_year)
    Num_all_LA_latest_year_sym<- sym(Num_all_LA_latest_year)
    
    Num_SEN_LA_latest_year_National<-paste0("SEN_Number_GLD_",latest_year,"_LA_National")
    Num_all_latest_year_National<-paste0("All_Number_GLD_",latest_year,"_LA_National")
    Num_SEN_LA_latest_year_National_sym<- sym(Num_SEN_LA_latest_year_National)
    Num_all_latest_year_National_sym<- sym(Num_all_latest_year_National)
    
    
    #Select fields needed  
    number_SEN_data<- long_data_step1 %>% select("region"=LA_Name, Num_SEN_LA_latest_year
                                                 ,Num_all_LA_latest_year)%>% 
      mutate_at(c(2,3),as.integer) %>% 
      filter(region==selected_LA)  %>% 
      distinct()
    
    # Calculate non SEN by subtracting SEN from all
    number_SEN_data<- number_SEN_data %>%   mutate(Num_non_sen_latest_year=!!(Num_all_LA_latest_year_sym)-!!(Num_SEN_LA_latest_year_sym))
    
    number_percent_SEN_data<-  number_SEN_data %>% mutate(percent_elig_SEN=round2(!!Num_SEN_LA_latest_year_sym/!!Num_all_LA_latest_year_sym*100,n=1),
                                                          percent_elig_non_SEN=round2((100-!!Num_SEN_LA_latest_year_sym/!!Num_all_LA_latest_year_sym*100),n=1)) %>% 
                                                        select(-!!(Num_all_LA_latest_year_sym)) 
    percent_SEN_LA<-number_percent_SEN_data %>% select(region,percent_elig_SEN,percent_elig_non_SEN)
    
    #National Numbers of SEN are eligible
    Num_SEN_LA_latest_year_National<-paste0("SEN_Number_GLD_",latest_year,"_National")
    Num_all_latest_year_National<-paste0("All_Number_GLD_",latest_year,"_National")
    Num_SEN_LA_latest_year_National_sym<- sym(Num_SEN_LA_latest_year_National)
    Num_all_latest_year_National_sym<- sym(Num_all_latest_year_National)
    
    
    national_SEN<- additional_nat_stats %>% select("region"=Country,Num_SEN_LA_latest_year_National 
                                                   ,Num_all_latest_year_National)
    
    # Calculate non SEN by subtracting SEN from all
    national_SEN<- national_SEN %>%   mutate(Num_non_sen_latest_year_National=!!(Num_all_latest_year_National_sym)-!!(Num_SEN_LA_latest_year_National_sym))
    number_percent_SEN_data_national<-  national_SEN %>% mutate(percent_elig_SEN=round2(!!Num_SEN_LA_latest_year_National_sym/!!Num_all_latest_year_National_sym*100,n=1),
                                                                percent_elig_non_SEN=round2((100-!!Num_SEN_LA_latest_year_National_sym/!!Num_all_latest_year_National_sym*100),n=1)) %>% 
                                                                  select(-!!(Num_all_latest_year_National_sym))  
    
    
    percent_SEN_National<-number_percent_SEN_data_national %>% select(region,percent_elig_SEN,percent_elig_non_SEN)
    
    
    # Combine LA and national
    percent_SEN_elig_data<- rbind(percent_SEN_LA,percent_SEN_National) %>% select(region,SEN=percent_elig_SEN,`Non SEN`=percent_elig_non_SEN)
    
    # put in tidy format
    percent_SEN_elig_data_2<- percent_SEN_elig_data %>% gather(key="group",value="Percentage",-region)  
    percent_SEN_elig_data_2$group<-factor(percent_SEN_elig_data_2$group,levels=c("SEN","Non SEN"))
    
    # return the table for the LA and National, Number and Percent
    names(number_percent_SEN_data)<- c("Region","Number of SEN","Number non SEN","Percentage SEN","Percentage non SEN")
    names(number_percent_SEN_data_national)<- c("Region","Number of SEN","Number non SEN","Percentage SEN","Percentage non SEN")  
    
    number_percent_SEN_data_LA_Nat<- rbind(number_percent_SEN_data_national,number_percent_SEN_data)
    number_percent_SEN_data_LA_Nat
    
    SEN_context_list<- list("SEN_context_table"=number_percent_SEN_data_LA_Nat,"SEN_context_graph_data"=percent_SEN_elig_data_2)
    SEN_context_list
  })

# Context FSM Table -------------------------------------------------------
  
  output$FSM_context_table <- renderTable({ 
    table<- GLD_FSM_Numbers_plot_data()[1] %>% as.data.frame()
    names(table)<- c("Region","Number of FSM","Number all","Number all other","Percentage FSM","Percentage all other")
    table<- table %>% select(Region,`Number of FSM`,`Number all other`,`Number all`) %>% 
      mutate_if(is.numeric, formatC, big.mark=",", format = "d") # this line ensures large numbers are displayed with commas
    table
  }, align='r') 
  

# Context SEN Plot --------------------------------------------------------

  
  output$SEN_Num_Perc_plot <- renderPlot({
    
    selected_LA<-input$Local_Authority
    percent_SEN_elig_data<-GLD_SEN_Numbers_plot_data()[[2]] %>% arrange(group) 
    percent_SEN_elig_data$group<-factor(percent_SEN_elig_data$group,levels=c("Non SEN","SEN"))
    
    
    # To get the zero after the decimal point in the graph
    
    percent_SEN_elig_data<- percent_SEN_elig_data %>%  mutate(Percentage_2= formatC(Percentage, format="f", big.mark=",",digits=1))
    # # end of section dded to get the zero after the decimal
    
    
    SEN_LA_National_Plot_1<- ggplot(percent_SEN_elig_data,aes(x=region,y=Percentage, fill=group) )+
      geom_col() +  
      #geom_text(aes(label=Percentage), position = position_stack(vjust = 0.5))+
      geom_text(aes(label=Percentage_2), position = position_stack(vjust = 0.5))+
      scale_fill_manual(name= "",
                        values = c("Non SEN"="grey90", "SEN"="grey50"),breaks=c("SEN","Non SEN"))+   
      scale_y_continuous(labels = scales::comma,breaks=seq(0,100,10),limits=c(0, 100))+
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            aspect.ratio = 1/3,
            legend.position="bottom")+
            labs(x="")+ #,title=paste0("The percentage of children eligible for SEN in ",selected_LA," compared with national percentage.	"))+
            coord_fixed(1/8)+
            coord_flip()
    
    SEN_LA_National_Plot_1
  })
  
  
  # Context SEN  Plot Title -------------------------------------
  output$SEN_context_graph_title <- renderUI({
    
    selected_LA_Name<-input$Local_Authority
    str1 <- get_graph_title(neighbour_gap_context="context",LA=input$Local_Authority,group="SEN")
    HTML(str1)
  })
  

# end context tab  

  

  
  # AoL Section -------------------------------------------------------------
  
  
  
  # AoL Combined Output Boxes --------------------------------------------------------
  
  
  output$AoL_comb_Nat <- renderValueBox({
    
    EYFSP_nat_variable<-colnames(AoL_comb_neighbours_plot_data()[4])
    
    national<- AoL_comb_neighbours_plot_data()[4] %>% select(!!EYFSP_nat_variable) %>% head(1) %>% as.numeric()
    
    valueBox(
      value = paste0(formatC(national, digits = 1, format = "f"),"%"),
      subtitle = "National Average",
      icon = icon("area-chart"),
      color = "yellow" 
    )
  })
  
  
  output$AoL_comb_LA <- renderValueBox({
    
    EYFSP_LA_variable<-colnames(AoL_comb_neighbours_plot_data()[3])
    
    LA_AoL_comb<-AoL_comb_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    
    valueBox(
      value = paste0(formatC(LA_AoL_comb, digits = 1, format = "f"),"%"),
      subtitle = "LA AoL CL and Lit",
      icon = icon("area-chart"),
      color = "yellow"
    )
  })
  
  
  output$AoL_comb_LA_Rank <- renderValueBox({
    EYFSP_LA_variable<-colnames(AoL_comb_neighbours_plot_data()[5])
    LA_AoL_comb<-AoL_comb_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    valueBox(
      value = formatC(LA_AoL_comb, digits = 0, format = "f"),
      subtitle = "LA AoL CL and Lit National Rank",
      icon = icon("area-chart"),
      color = "yellow" 
    )
  })
  
  
  output$AoL_comb_LA_SN_Rank <- renderValueBox({
    
    EYFSP_LA_variable<-colnames(AoL_comb_neighbours_plot_data()[6])
    LA_AoL_comb<-AoL_comb_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    valueBox(
      value = formatC(LA_AoL_comb, digits = 0, format = "f"),
      subtitle = "LA AoL Statistical Neighbour Rank",
      icon = icon("area-chart"), 
      color =  "yellow" 
    )
  })
  
  # Note the AoL Combined code was modelled on the code for single areas. Some commented out code has been retained for ease and flexibility in future developments
  
  
  # AoL Combined Plots Stats Neighbour Graphs- combined CL and Lit get data ----------------------------------------
 
  AoL_comb_neighbours_plot_data <- reactive({
    
    AoL_sub_type   <-"CL_and_Lit"
    
    # For combined CL and Lit, the all group has been published. So there are no further options.
    AoL_input_group<-"All"
    
    
    selected_LA<-input$Local_Authority
    level<-"LA"
    metric<-"AoL"
    
    
    # Retrieve data
    AoL_comb_data_subset_1<-get_data_subset_neighbour(long_data_step1,selected_LA,GLD_or_AoL_or_take_up="AoL",AoL_type="CL_and_Lit",group="All",Percent_or_Gap="Percent",latest_year = current_year)#selected_gap_type,latest_year = 2017)
   
    AoL_comb_data_subset_1<- AoL_comb_data_subset_1 %>% mutate_at(3,as.numeric)
    sort<-colnames(AoL_comb_data_subset_1[3])
    sort_sym<-  sym(sort) 
    AoL_comb_data_subset_2<- AoL_comb_data_subset_1 %>%  arrange(desc(!!sort_sym)) 
    AoL_comb_data_subset_2$LA_Name<-factor( AoL_comb_data_subset_2$LA_Name, levels =  AoL_comb_data_subset_2$LA_Name) #convert to factor to preserve sort order
    
    # Below the factor affects the order of the legend
    AoL_comb_data_subset_2$LA_selected_by_user<-factor(AoL_comb_data_subset_2$LA_selected_by_user,levels=c("yes","no"))
    AoL_comb_data_subset_2
    
  })
  
  

# AoL Combined Text -------------------------------------------------------

  output$AoL_comb_text <- renderUI({
    
    selected_LA_Name<-input$Local_Authority
    col<- 4#if_else(input$AoL_group_select %in% c("FSM","All"),5,4)
    EYFSP_nat_variable<-colnames(AoL_comb_neighbours_plot_data()[col])
    national_perc<- AoL_comb_neighbours_plot_data() %>% select(!!EYFSP_nat_variable) %>% head(1) %>% as.numeric()
    
    EYFSP_LA_variable<-colnames(AoL_comb_neighbours_plot_data()[3])
    LA_perc<-AoL_comb_neighbours_plot_data() %>% filter(LA_Name==selected_LA_Name) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    # Gaps
    gap_dataset<-AoL_comb_neighbours_gaps_plot_data()
    EYFSP_gap_variable<-colnames(AoL_comb_neighbours_gaps_plot_data()[3])
    selected_LA<- input$Local_Authority
    
    national_gap_perc<- gap_dataset%>% filter(LA_Name==selected_LA)%>%
      select(!!EYFSP_gap_variable) %>%
      as.numeric()
    
    above_below_equal<- if_else(national_gap_perc<0,"below",
                                if_else(national_gap_perc==0,"ie equal to","above"))
    
    
    AoL_input_group<-"All"#input$AoL_group_select 
    group_type<- "All" #if_else(AoL_input_group=="All Other","All_Other"
                        #,if_else(AoL_input_group=="All","All",AoL_input_group))
    title_group= "all children"
      
      # if_else(group_type=="FSM","children known to be eligible for FSM",
      #                   if_else(group_type=="All","all children",
      #                           if_else(group_type=="All Other","all other children",
      #                                   "children identified as SEN")))
      # 
    
    comparison_group= "all children"
      
      #if_else(group_type%in% c("FSM","All_Other","All"),"all other children",
      #                       "children identified as SEN") 
    
      selected_gap_type<- ""
    # selected_gap_type<- if_else(AoL_input_group=="FSM",input$AoL_gap_select,"")
    # selected_gap_type<- if_else(selected_gap_type=="Within the LA: gap with all other children","all other children in ",
    #                             if_else(selected_gap_type=="National average: gap with all other children","all other children",
    #                                     if_else(selected_gap_type=="National average: gap with FSM children","children known to be eligible for FSM","")))
    # 
      
      gap_comparison_group= "all children"
    # gap_comparison_group=if_else(group_type== "FSM" ,selected_gap_type,
    #                              if_else(group_type== "All_Other","all other children",
    #                                      if_else(group_type=="All", "all children", "children identified as SEN")))
    # 
    
    
    
   AoL_comb<-"communication and language and literacy (combined)"
   
   # need to ensure that numbers such as 72.0% are not reported as 72% 
   LA_perc_text<-sprintf('%.1f', LA_perc)
   national_perc_text<- sprintf('%.1f',national_perc)
   abs_national_gap_perc_text<- sprintf('%.1f',abs(national_gap_perc))
   
   str1 <- paste0("In ", input$Local_Authority,", ",LA_perc_text,"% of " , title_group," achieve the expected level of development for ",AoL_comb ," compared to  ",
                 national_perc_text,"% of ",comparison_group," nationally.")
   str2 <- paste0("In ", input$Local_Authority,", the gap for ",title_group," is ", abs_national_gap_perc_text," percentage points ",
                 above_below_equal, " the national average for ",gap_comparison_group," that achieve at least the expected level of development for ",AoL_comb,".")
   
   if(group_type=="FSM"&gap_comparison_group=="all other children in "){
     str2 <- paste("In ", input$Local_Authority," the gap for ",title_group," is ", abs_national_gap_perc_text," percentage points ",
                   above_below_equal, " ",gap_comparison_group, " in ",input$Local_Authority," that achieve at least the expected level of development for ",AoL_comb,".")
   }
    
     
    
    HTML(paste(str1, str2, sep = '<br/>'))
  })
 
  
  # AoL Combined Stats Neighbour Graph -----------------------------------------------
  
  output$AoL_comb_neighbour_plot <- renderPlot({
    
    dataset<-AoL_comb_neighbours_plot_data()
    EYFSP_variable<-colnames(AoL_comb_neighbours_plot_data()[3])
    selected_LA<- input$Local_Authority
    AoL_input_group<-"All"
    
    National_variable_name<- colnames(dataset[4])
    National_figure<- dataset %>% select(National_variable_name) %>% distinct %>% as.numeric()
    
    #For graph title/legend/labels
    group_type<- "All"
    title_group= "all children" 
 
    # To get the zero after the decimal point in the graph
    EYFSP_variable_2<-paste0(EYFSP_variable,"_2")
    EYFSP_variable_sym<-sym(EYFSP_variable)
    dataset<- dataset %>%  mutate(EYFSP_variable_2=sprintf('%.1f', !!EYFSP_variable_sym))
    # end of section dded to get the zero after the decimal
    
    
    plot<- ggplot(dataset,aes_string(x="LA_Name",y=EYFSP_variable, fill="LA_selected_by_user") )+ #NB aes_string as we are passing a variable rather than an actual name
      geom_col() +  
      
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.justification = "top")+
      scale_y_continuous(labels = scales::comma,breaks=seq(0,100,10),limits=c(0, 100))+
      labs(y="Percentage",x="Neighbour")+#,title=Plot_title)+
      geom_hline(aes(yintercept=National_figure,linetype="National percentage for all"),colour='grey50')+
      #geom_text(aes_string(label=EYFSP_variable,vjust = -0.25), position=position_dodge(width=0.9))+ 
      geom_text(aes(label=EYFSP_variable_2,vjust = -0.25), position=position_dodge(width=0.9))+
      scale_linetype_manual(name = "", values = "longdash", 
                            guide = guide_legend(override.aes =list(colour= "grey30")))+  
      scale_fill_manual(name= "",
                        labels = c(selected_LA, "Neighbours"),
                        values = c("yes"="cornflowerblue", "no"="darkseagreen3"))+ 
      guides(
        fill = guide_legend(order = 1))+ # change the order of the legend, as prefer the horizontal line to be after the bars label
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
    
    
    plot#+ scale_colour_manual(
    # manually add a legend https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r/24497113        
    # https://stackoverflow.com/questions/39119917/how-to-add-a-legend-to-hline       
    # values = c("selectedLA" = "cornflowerblue",Neighbour="darkseagreen3", National = "grey"),
    #                        name="",
    #                       breaks=c("LA","Neighbour", "National" ),
    #                      labels=c("selectedLA","Neighbour", "National percent achieving at least eld for CL&Lit")#c(LA_key, National_key))
    # )
  })
  output$AoL_comb_table <- renderTable({ # for debug
    AoL_comb_neighbours_plot_data()  
    
  })
  
  
  # AoL Combined Neighbour Plot Title -------------------------------------
  output$AoL_comb_neighbour_plot_graph_title <- renderUI({
    
    selected_LA_Name<-input$Local_Authority
    str1 <- get_graph_title(neighbour_gap_context="neighbour",LA=input$Local_Authority,group="All", GLD_or_AoL_or_take_up="AoL", AoL_type = "communication and language and literacy (combined)")
    HTML(str1)
  })
  
  
  # AoL Combined Trend Graph Data ---------------------------------------------------------
  AoL_comb_trend_plot_data <- reactive({
    
    AoL_comb_input_group<-input$AoL_comb_group_select
    AoL_comb_input_group<-if_else(AoL_comb_input_group=="All Other","All_Other"
                                  ,if_else(AoL_comb_input_group=="All","All",AoL_comb_input_group))
    
    
    selected_LA<-input$Local_Authority
    dataset<-get_trend_data_fn(three_years_data_step1,group="All",LA=selected_LA,GLD_or_AoL="AoL",AoL_type = "CL_and_Lit",latest_year=current_year)
    dataset
    
    #debug_dataset<-get_trend_data_fn(three_years_data_step1,group="All",LA_Name="Bury",GLD_or_AoL="AoL",AoL_type = "CL_and_Lit")
    #debug_dataset$trend_data
  })
  
  # Used for debug 
  
  output$AoL_comb_trend_table_data <- renderTable({#renderText({ # #depending on dbug need may renderText or renderTable
    
    #AoL_comb_neighbours_plot_data()
    #AoL_comb_national_plot_data()
    #AoL_comb_SEN_Numbers_plot_data()
    #SEN_table_data()
    AoL_comb_trend_plot_data() 
    
  })
  
  
  # AoL Comb Trend Data -----------------------------------------------------
  output$AoL_comb_trend_plot <- renderPlot({
    
    AoL_comb_trend_data_list<-AoL_comb_trend_plot_data()
    AoL_comb_trend_data<-AoL_comb_trend_plot_data()$trend_data

    LA_key<-AoL_comb_trend_plot_data()$LA_legend
    National_key<-AoL_comb_trend_plot_data()$National_legend
     
    AoL_comb_trend_data_table_1<- AoL_comb_trend_data %>%  spread(Year, Percentage) %>% select(-LA_Name)
    AoL_comb_trend_data_table_2<- AoL_comb_trend_data_table_1 %>%  mutate(Metric=if_else(Region=="LA",LA_key,
                                                                                         National_key)) %>% select(-Region) %>% select(Metric, everything())
    
    #theme_minimal()
    AoL_comb_trend_plot<-
      ggplot(data=AoL_comb_trend_data, aes(x= Year, y=Percentage, group=Region,color=Region)) +
      geom_line(linetype="dashed",size=0.8)+#,size=1.3
      geom_point(size=4)+
      
      scale_y_continuous(labels = scales::comma,breaks=seq(0,100,10),limits=c(0, 100))+
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.justification = "top")+
      labs(y="Percentage",x="Year")+
      theme(legend.position = "bottom")
   
    #overwriting legends http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
    AoL_comb_trend_plot+ scale_colour_manual(values = c(LA = "cornflowerblue", National = "grey"),
                                             name="",
                                             breaks=c("LA", "National" ),
                                             labels=c(LA_key, National_key))
  })
  
         
  output$AoL_comb_trend_plot_graph_title <- renderUI({
    str1 <- AoL_comb_trend_plot_data()$trend_graph_title
    HTML(str1)
  })  
  
  # AoL Combined Trend Table ------------------------------------------------
  
  output$AoL_comb_trend_table <- renderDT({
    
      selected_LA<- input$Local_Authority
      AoL_comb_trend_data<-AoL_comb_trend_plot_data()$trend_data
      AoL_comb_trend_data_spread <- AoL_comb_trend_data %>% spread(Year,Percentage) %>%
      mutate(Region=if_else(Region=="LA",selected_LA,Region)) %>%
      select(-LA_Name) %>% select(" "=Region, everything())%>% 
        # This line is to ensure that point zero ie 77.0 not 77 is displayed
        mutate_if(is.numeric,formatC,digits=1,format="f")
    
    datatable(AoL_comb_trend_data_spread, 
              options=list(
                
                paging=FALSE,
                searching=FALSE,
                info=FALSE#,
                #ordering=FALSE,
              ),
              rownames = FALSE,
              class = 'cell-border stripe'
    )
  })
  
  # AoL Combined National Data and Plot -------------------------------------
  
  #for debug of national graph
  output$AoL_comb_national_table <- renderTable({
    AoL_comb_national_plot_data()
  })
 
  
  output$AoL_comb_national_plot <- renderPlot({
    
    #National percentage
    EYFSP_nat_variable<-colnames(AoL_comb_neighbours_plot_data()[4])
    
    national_CL_Lit<- AoL_comb_neighbours_plot_data()[4] %>% select(!!EYFSP_nat_variable) %>% head(1) %>% as.numeric()
    
    areas_of_learning<- "CL_and_Lit" #input$AoL_select
    AoL_sub_type="CL_and_Lit"

    # AoL_input_group<-input$AoL_group_select
    # AoL_input_group<-if_else(AoL_input_group=="All Other","All_Other"
    #                          ,if_else(AoL_input_group=="All","All",AoL_input_group))
    AoL_input_group<-"All"
    show_all_LAs_selection<- if_else(input$show_all_LAs_AoL_comb_tab=="Yes",TRUE,FALSE)
    get_national_plot(selected_LA=input$Local_Authority,neighbour_dataset=AoL_neighbours_plot_data(),group = AoL_input_group,metric="AoL",AoL_type=AoL_sub_type,year=current_year,show_all_LAs=show_all_LAs_selection,national=national_CL_Lit) 
    
  })
  
  # AoL Combined Lit and Lang Areas Neighbours Gaps Graph Data   ---------------------------
  
  AoL_comb_neighbours_gaps_plot_data <- reactive({
  
    selected_LA<- input$Local_Authority
    AoL_input_group<-"All"#input$AoL_group_select
    AoL_input_group<-if_else(AoL_input_group=="All Other","All_Other"
                             ,if_else(AoL_input_group=="All","All",AoL_input_group))
    areas_of_learning<- "CL_and_Lit"#input$AoL_select
    
    #selected gap type not needed
    # selected_gap_type<- if_else(AoL_input_group=="FSM",input$AoL_gap_select,"")
    # selected_gap_type<- if_else(selected_gap_type=="Within the LA: gap with all other children","gap_within_LA",
    #                             if_else(selected_gap_type=="National average: gap with all other children","gap_All_Other_Nat_Av",
    #                                     if_else(selected_gap_type=="National average: gap with FSM children","gap_FSM_Nat_Av","")))
    # selected_gap_type<- if_else(AoL_input_group=="FSM",selected_gap_type,"")
    selected_gap_type=""
    AoL_sub_type= "CL_and_Lit"#if_else(areas_of_learning=="Communication and Language","CL",
    #      if_else(areas_of_learning=="Literacy","Lit","Mat"))
    
    dataset_1 <-          get_data_subset_neighbour(long_data_step1,LA=selected_LA,GLD_or_AoL_or_take_up="AoL",AoL_type=AoL_sub_type,group=AoL_input_group,Percent_or_Gap="gap",gap_type=selected_gap_type, latest_year = current_year)
    dataset_1<- dataset_1 %>% mutate_at(3,as.numeric)
    sort<-colnames(dataset_1[3])
    sort_sym<-  sym(sort)
    dataset_2<- dataset_1 %>%  arrange(!!sort_sym) 
    dataset_2$LA_Name<-factor( dataset_2$LA_Name, levels =  dataset_2$LA_Name) #convert to factor to preserve sort order
    dataset_2
  })
  
  output$AoL_comb_neighbours_gaps_table_data <- renderTable({   
    AoL_comb_neighbours_gaps_plot_data()
  })
  
  # AoL Combined Literacy and Language Neighbours Gaps Graph ----------------------------------

  output$AoL_comb_neighbours_gaps_plot<- renderPlot({ 
    
    dataset<-AoL_comb_neighbours_gaps_plot_data()
    EYFSP_variable<-colnames(AoL_comb_neighbours_gaps_plot_data()[3])
    selected_LA<- input$Local_Authority
    AoL_input_group<-"All"#input$AoL_group_select
    
    #For graph title/legend/labels
    group_type<-if_else(AoL_input_group=="All Other","All_Other"
                        ,if_else(AoL_input_group=="All","All",AoL_input_group))
    title_group=if_else(group_type=="FSM","children known to be eligible for FSM",
                        if_else(group_type=="All","all children",
                                if_else(group_type=="All_Other","all other children",
                                        "children identified as SEN")))
    
    
    dataset_2<- dataset %>% select("Percentage_Gap"=EYFSP_variable,everything())
    dataset_3<- dataset_2 %>% mutate(LA_selected_by_user=if_else(LA_Name==selected_LA& Percentage_Gap<0,"Chosen LA below national average",
                                                                 if_else(LA_Name==selected_LA& Percentage_Gap>=0,"Chosen LA above or equal to national average",
                                                                         if_else(Percentage_Gap<0,"Neighbour below national average","Neighbour above or equal to national average"))))
    
    
    # To get the zero after the decimal point in the graph
    gap_2<-paste0("Percentage_Gap","_2")
    gap_sym<-sym(gap_2)
    dataset_3<- dataset_3 %>%  mutate(gap_2=sprintf('%.1f', Percentage_Gap))
    # end of section dded to get the zero after the decimal
    
    #add two line to ensure the data is in descending order- consistent with the neighbours graph
    dataset_3 <- dataset_3 %>% arrange(desc(Percentage_Gap))
    dataset_3$LA_Name<- factor(dataset_3$LA_Name,levels=dataset_3$LA_Name)
    
    labels_for_graph<- dataset_3 %>% select(LA_selected_by_user) %>% distinct()
    
    plot<- ggplot(dataset_3,aes(x=LA_Name,y=Percentage_Gap, fill=LA_selected_by_user))+ #NB aes_string as we are passing a variable rather than an actual name
      geom_col()+  
      scale_fill_manual(name= "",
                        #,labels = c("Chosen LA above or equal to national average", "Neighbour above or equal to national average","Chosen LA below national average","Neighbour below national average"),
                        labels = labels_for_graph,
                        values = c("Chosen LA above or equal to national average"="cornflowerblue", "Neighbour above or equal to national average"="darkseagreen3",
                                   "Chosen LA below national average"="orange","Neighbour below national average"="navajowhite2"))+

      #geom_text(aes(label=Percentage_Gap,vjust = ifelse(Percentage_Gap >= 0, -0.25, 1)), position=position_dodge(width=0.9))+
      geom_text(aes(label=gap_2,vjust = ifelse(Percentage_Gap >= 0, -0.25, 1)), position=position_dodge(width=0.9))+#, vjust=-0.25)+
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank())+
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))+
      labs(x="",y="Percentage point gap")
    
    plot
    # how to put labels over each bar https://stackoverflow.com/questions/12018499/how-to-put-labels-over-geom-bar-for-each-bar-in-r-with-ggplot2
    # Solution for making labels appear above and below axis
    # https://stackoverflow.com/questions/11938293/how-to-label-a-barplot-bar-with-positive-and-negative-bars-with-ggplot2
   
  })
  
  
  
  # AoL Combined Neighbour Gap Plot Title -------------------------------------
  output$AoL_comb_neighbours_gaps_plot_graph_title <- renderUI({
    selected_LA_Name<-input$Local_Authority
    selected_gap_type<- if_else(input$AoL_group_select=="FSM",input$AoL_gap_select,"")
    str1 <- get_graph_title(neighbour_gap_context="neighbour_gap",LA=input$Local_Authority,group="All", GLD_or_AoL_or_take_up="AoL",AoL_type="communication and language and literacy (combined)", gap_type=selected_gap_type)
    HTML(str1)
  })
  

# AoL Combined Trend Gap Plot Data ----------------------------------------
  AoL_comb_trend_gap_plot_data <- reactive({
    AoL_input_group<-"All" #input$AoL_group_select
    # AoL_input_group<-if_else(AoL_input_group=="All Other","All_Other"
    #                          ,if_else(AoL_input_group=="All","All",AoL_input_group))
    
    areas_of_learning<- "CL_and_Lit"#input$AoL_select
    
    # edit this
    # selected_gap_type<- if_else(AoL_input_group=="FSM",input$AoL_gap_select,"")
    # selected_gap_type<- if_else(selected_gap_type=="Within the LA: gap with all other children","gap_within_LA",
    #                             if_else(selected_gap_type=="National average: gap with all other children","gap_All_Other_Nat_Av",
    #                                     if_else(selected_gap_type=="National average: gap with FSM children","gap_FSM_Nat_Av","")))
    # selected_gap_type<- if_else(AoL_input_group=="FSM",selected_gap_type,"")
    # 
    selected_gap_type<-""
    # AoL_sub_type=if_else(areas_of_learning=="Communication and Language","CL",
    #                      if_else(areas_of_learning=="Literacy","Lit","Mat"))
    # 
    AoL_sub_type<- areas_of_learning
    selected_LA<-input$Local_Authority
    get_trend_data_fn_v2(three_years_data_step1,group=AoL_input_group,LA=selected_LA,GLD_or_AoL_or_Take_up="AoL",AoL_type=AoL_sub_type,gap_type = selected_gap_type,gap="gap",latest_year = current_year) #for debug
  })
  

  # Used for debug  
  output$AoL_comb_trend_gap_table <- renderTable({
    
    #AoL_neighbours_plot_data()
    #AoL_national_plot_data()
    #AoL_SEN_Numbers_plot_data()
    #SEN_table_data()
    #AoL_neighbours_gaps_plot_data()
    # data<-AoL_trend_plot_data()    
    data<- AoL_comb_trend_gap_plot_data()
    data$trend_data_gaps
  })#
  
  
  # AoL Combined Trend Gap Plot ---------------------------------------- 
  output$AoL_comb_trend_gap_plot <- renderPlot({
    
    
    AoL_trend_data_list<-AoL_comb_trend_gap_plot_data()
    AoL_trend_data<-AoL_comb_trend_gap_plot_data()$trend_data_gaps
    
    gaps_data<-AoL_trend_data %>% mutate(above_or_below=if_else(`Percentage Gap`>=0,"above_or_equal","below"))
  
    plot<- ggplot(gaps_data,aes(x=Year,y=`Percentage Gap`, fill=above_or_below) )+ #NB aes_string as we are passing a variable rather than an actual name
      geom_col() +  
      scale_fill_manual(name= ""
                        ,labels = c("below", "above_or_equal"),
                        values = c("above_or_equal"="cornflowerblue",
                                   "below"="orange"),guide=FALSE)+ 
      
      geom_text(aes(label=`Percentage Gap`,vjust = ifelse(`Percentage Gap` >= 0, -0.25, 1)), position=position_dodge(width=0.9))+
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank())+
      labs(y="Percentage point gap")
    plot  
  })
  
  
  
  output$AoL_comb_trend_gap_plot_graph_title <- renderUI({
    str1 <- AoL_comb_trend_gap_plot_data()$trend_gaps_graph_title
    HTML(str1)
  })  
  
  
  # AoL Single Areas --------------------------------------------------------

  # AoL Single Areas Output Boxes --------------------------------------------------------
 
  output$AoL_Nat <- renderValueBox({
    group<- input$AoL_group_select
    #comparison_group<- if_else(group %in% c("FSM","All"),"All other",group)
    comparison_text<- if_else(group %in% c("FSM","All"),group,"All Other")
    col<- if_else(input$AoL_group_select %in% c("FSM","All"),4,4)# change later
    EYFSP_nat_variable<-colnames(AoL_neighbours_plot_data()[col])
    
    national<- AoL_neighbours_plot_data()[col] %>% select(!!EYFSP_nat_variable) %>% head(1) %>% as.numeric()
    
    valueBox(
      value = paste0(formatC(national, digits = 1, format = "f"),"%"),
      subtitle = paste0(comparison_text,": National Average"),
      icon = icon("area-chart"),
      color = "orange" 
    )
  })
  
  
  output$AoL_LA <- renderValueBox({
    EYFSP_LA_variable<-colnames(AoL_neighbours_plot_data()[3])
    
    LA_AoL<-AoL_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    valueBox(
      value = paste0(formatC(LA_AoL, digits = 1, format = "f"),"%"),
      subtitle = paste0(input$AoL_group_select,": LA AoL"),
      icon = icon("area-chart"),
      color = "orange"
    )
  })
  
  
  output$AoL_LA_Rank <- renderValueBox({
    
    # column 5 contains the rank if All Other chosen, otherwise it will be column 6
    col<- if_else(input$AoL_group_select %in% c("FSM","All"),6,5) 
    EYFSP_LA_variable<-colnames(AoL_neighbours_plot_data()[col])
    
    LA_AoL<-AoL_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    valueBox(
      value = formatC(LA_AoL, digits = 0, format = "f"),
      subtitle = paste0(input$AoL_group_select,":  LA AoL National Rank"),
      icon = icon("area-chart"),
      color = "orange" 
    )
  })
  
  
  output$AoL_LA_SN_Rank <- renderValueBox({
    col<- if_else(input$AoL_group_select %in% c("FSM","All"),7,6)
    EYFSP_LA_variable<-colnames(AoL_neighbours_plot_data()[col])
    
    LA_AoL<-AoL_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    valueBox(
      value = formatC(LA_AoL, digits = 0, format = "f"),
      subtitle = paste0(input$AoL_group_select,":  LA AoL Statistical Neighbour Rank"),
      icon = icon("area-chart"), 
      color =  "orange" 
    )
  })
  
  
  # AoL Single Areas Get Data -----------------------------------------------
  
  AoL_neighbours_plot_data <- reactive({
    AoL_input_group<-input$AoL_group_select
    level<-"LA"
    metric<-"AoL"
    areas_of_learning<- input$AoL_select
    
    AoL_sub_type=if_else(areas_of_learning=="Communication and Language","CL",
                         if_else(areas_of_learning=="Literacy","Lit","Mat"))
    
    # EYFSP_AoL_variable<- paste0(AoL_input_group,"_","Percent","_",metric,"_","AoL","_",current_year,"_",level)
    # EYFSP_AoL_variable_Nat<- paste0(AoL_input_group,"_","Percent","_",metric,"_","AoL","_",current_year,"_","National")
    # EYFSP_AoL_variable_Rank<- paste0(AoL_input_group,"_","Rank","_",metric,"_","AoL","_",current_year,"_",level)
    # EYFSP_AoL_variable_SN_Rank<- paste0(AoL_input_group,"_","SN_Rank","_",metric,"_","AoL","_",current_year,"_",level)
   
    selected_LA<-input$Local_Authority

    AoL_input_group<-if_else(AoL_input_group=="All Other","All_Other"
                             ,if_else(AoL_input_group=="All","All",AoL_input_group))
    

    selected_LA<-input$Local_Authority
    level<-"LA"
    metric<-"AoL"
    
    AoL_subset_1<-get_data_subset_neighbour(long_data_step1,selected_LA,GLD_or_AoL_or_take_up="AoL",AoL_type=AoL_sub_type,group=AoL_input_group ,Percent_or_Gap="Percent",latest_year = current_year)#selected_gap_type,latest_year = 2017)
    
    # debug_aol<-get_data_subset_neighbour(long_data_step1,"Bury",GLD_or_AoL_or_take_up="AoL",AoL_type="CL",group="All" ,Percent_or_Gap="Percent",latest_year = current_year)
    # debug_aol
    AoL_subset_1<- AoL_subset_1 %>% mutate_at(3,as.numeric)
    sort<-colnames(AoL_subset_1[3])
    sort_sym<-  sym(sort)
    AoL_subset_2<- AoL_subset_1 %>%  arrange(desc(!!sort_sym)) 
    AoL_subset_2$LA_Name<-factor( AoL_subset_2$LA_Name, levels =  AoL_subset_2$LA_Name) #convert to factor to preserve sort order
    # Below the factor affects the order of the legend
    AoL_subset_2$LA_selected_by_user<-factor(AoL_subset_2$LA_selected_by_user,levels=c("yes","no"))
    AoL_subset_2
  })
  
  
  # AoL Text Under Stat Neighbours Plot -------------------------------------
  
  
  output$AoL_text <- renderUI({
    
    selected_LA_Name<-input$Local_Authority
    col<- if_else(input$AoL_group_select %in% c("FSM","All"),5,4)
    EYFSP_nat_variable<-colnames(AoL_neighbours_plot_data()[col])
    national_perc<- AoL_neighbours_plot_data() %>% select(!!EYFSP_nat_variable) %>% head(1) %>% as.numeric()
    
    EYFSP_LA_variable<-colnames(AoL_neighbours_plot_data()[3])
    LA_perc<-AoL_neighbours_plot_data() %>% filter(LA_Name==selected_LA_Name) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    # Gaps
    gap_dataset<-AoL_neighbours_gaps_plot_data()
    EYFSP_gap_variable<-colnames(AoL_neighbours_gaps_plot_data()[3])
    selected_LA<- input$Local_Authority
    
    national_gap_perc<- gap_dataset%>% filter(LA_Name==selected_LA)%>%
      select(!!EYFSP_gap_variable) %>%
      as.numeric()
    
    above_below_equal<- if_else(national_gap_perc<0,"below",
                                if_else(national_gap_perc==0,"ie equal to","above"))
    
    
    AoL_input_group<-input$AoL_group_select 
    group_type<-if_else(AoL_input_group=="All Other","All_Other"
                        ,if_else(AoL_input_group=="All","All",AoL_input_group))
    title_group=if_else(group_type=="FSM","children known to be eligible for FSM",
                        if_else(group_type=="All","all children",
                                if_else(group_type=="All_Other","all other children",
                                        "children identified as SEN")))
    
    
    comparison_group=if_else(group_type%in% c("FSM","All_Other","All"),"all other children",
                             "children identified as SEN") 
    
    
    
    selected_gap_type<- if_else(AoL_input_group=="FSM",input$AoL_gap_select,"")
    selected_gap_type<- if_else(selected_gap_type=="Within the LA: gap with all other children","all other children in ",
                                if_else(selected_gap_type=="National average: gap with all other children","all other children",
                                        if_else(selected_gap_type=="National average: gap with FSM children","children known to be eligible for FSM","")))
    
    gap_comparison_group=if_else(group_type== "FSM" ,selected_gap_type,
                                 if_else(group_type== "All_Other","all other children",
                                         if_else(group_type=="All", "all children", "children identified as SEN")))
  
    
    LA_perc_text<-sprintf('%.1f', LA_perc)
    national_perc_text<- sprintf('%.1f',national_perc)
    abs_national_gap_perc_text<- sprintf('%.1f',abs(national_gap_perc))
    
    str1 <- paste0("In ", input$Local_Authority,", ",LA_perc_text,"% of " , title_group," achieve the expected level of development for ",tolower(input$AoL_select) ," compared to  ",
                  national_perc_text,"% of ",comparison_group," nationally.")
    str2 <- paste0("In ", input$Local_Authority,", the gap for ",title_group," is ", abs_national_gap_perc_text," percentage points ",
                  above_below_equal, " the national average for ",gap_comparison_group," that achieve at least the expected level of development for ",tolower(input$AoL_select),".")
    
    if(group_type=="FSM"&gap_comparison_group=="all other children in "){
      str2 <- paste0("In ", input$Local_Authority,", the gap for ",title_group," is ", abs(national_gap_perc)," percentage points ",
                    above_below_equal, " ",gap_comparison_group, " in ",input$Local_Authority," that achieve at least the expected level of development for ",tolower(input$AoL_select),".")
    }
    
    
    HTML(paste(str1, str2, sep = '<br/>'))
   
  })
  
  
  # AoL Single Areas  Stats Neighbour Graph -----------------------------------------------
  
  # This is only used for debug
  output$AoL_neighbour_table <- renderTable({ 
    AoL_neighbours_plot_data() 
  })
  
  output$AoL_neighbour_plot <- renderPlot({
    
    dataset<-AoL_neighbours_plot_data()
    EYFSP_variable<-colnames(AoL_neighbours_plot_data()[3])
    selected_LA<- input$Local_Authority
    AoL_input_group<-input$AoL_group_select
    AoL_input_group<-if_else(AoL_input_group=="All Other","All_Other"
                             ,if_else(AoL_input_group=="All","All",AoL_input_group))
    
    
    National_variable_name<- colnames(dataset[4])
    National_figure<- dataset %>% select(National_variable_name) %>% distinct %>% as.numeric()
    
    #For graph title/legend/labels
    group_type<-if_else(AoL_input_group=="All Other","All_Other"
                        ,if_else(AoL_input_group=="All","All",AoL_input_group))
    title_group=if_else(group_type=="FSM","children known to be eligible for FSM",
                        if_else(group_type=="All","all children",
                                "all other children"))
    
    # To get the zero after the decimal point in the graph
    EYFSP_variable_2<-paste0(EYFSP_variable,"_2")
    EYFSP_variable_sym<-sym(EYFSP_variable)
    dataset<- dataset %>%  mutate(EYFSP_variable_2=sprintf('%.1f', !!EYFSP_variable_sym))
    # end of section dded to get the zero after the decimal
    
    
    plot_output<- ggplot(dataset,aes_string(x="LA_Name",y=EYFSP_variable, fill="LA_selected_by_user") )+ #NB aes_string as we are passing a variable rather than an actual name
      geom_col() + 
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.justification = "top")+
      scale_y_continuous(labels = scales::comma,breaks=seq(0,100,10),limits=c(0, 100))+
      labs(y="Percentage",x="Neighbour")+
      #geom_text(aes_string(label=EYFSP_variable,vjust = -0.25), position=position_dodge(width=0.9))+ 
      geom_text(aes(label=EYFSP_variable_2,vjust = -0.25), position=position_dodge(width=0.9))+
      geom_hline(aes(yintercept=National_figure,linetype=paste0("National percentage for ",title_group)),colour='grey50')+ 
      scale_linetype_manual(name = "", values = "longdash", 
                            guide = guide_legend(override.aes =list(colour= "grey30")))+  
      scale_fill_manual(name= ""
                        
                        ,labels = c(selected_LA, "Neighbours"),
                        values = c("yes"="cornflowerblue", "no"="darkseagreen3"))+
      guides(
        fill = guide_legend(order = 1))+ # change the order of the legend, as prefer the horizontal line to be after the bars label
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
    
    if(group_type %in% c("FSM","All")){
      National_variable_name_2<- colnames(dataset[5])
      National_figure_2<- dataset %>% select(National_variable_name_2) %>% distinct %>% as.numeric()
      plot_output<- plot_output+ geom_hline(aes(yintercept=National_figure_2,linetype="National percentage for all other"),colour='black',show.legend = FALSE)+#show_guide=FALSE)+
        scale_linetype_manual(name = "", values = c(2,2), 
                              guide = guide_legend(override.aes =list(colour= c("black","grey50"))))
    }
    plot_output#+ scale_colour_manual(
    # manually add a legend https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r/24497113        
    # https://stackoverflow.com/questions/39119917/how-to-add-a-legend-to-hline       
    # values = c("selectedLA" = "cornflowerblue",Neighbour="darkseagreen3", National = "grey"),
    #                        name="",
    #                       breaks=c("LA","Neighbour", "National" ),
    #                      labels=c("selectedLA","Neighbour", "National percent achieving at least eld for CL&Lit")#c(LA_key, National_key))
    # )
  })
  
  
  
  # AoL Neighbour Plot Title -------------------------------------
  output$AoL_neighbour_plot_graph_title <- renderUI({
    
    selected_LA_Name<-input$Local_Authority
    AoL_input_group<-input$AoL_group_select
    AoL_input_group<-if_else(AoL_input_group=="All Other","All_Other"
                             ,if_else(AoL_input_group=="All","All",AoL_input_group))
    str1 <- get_graph_title(neighbour_gap_context="neighbour",LA=input$Local_Authority,group=AoL_input_group, GLD_or_AoL_or_take_up="AoL", AoL_type = input$AoL_select)
    HTML(str1)
  })
  
  # AoL Single Trend Graph Data ---------------------------------------------------------
  
  AoL_trend_plot_data <- reactive({
    AoL_input_group<-input$AoL_group_select
    AoL_input_group<-if_else(AoL_input_group=="All Other","All_Other"
                             ,if_else(AoL_input_group=="All","All",AoL_input_group))
    areas_of_learning<- input$AoL_select
    AoL_sub_type=if_else(areas_of_learning=="Communication and Language","CL",
                         if_else(areas_of_learning=="Literacy","Lit","Mat"))
    selected_LA<-input$Local_Authority
    dataset<-get_trend_data_fn(three_years_data_step1,group=AoL_input_group,LA=selected_LA,GLD_or_AoL="AoL",AoL_type = AoL_sub_type, latest_year = current_year)
    dataset
    
    #debug_dataset<-Get_trend_data_fn(three_years_data_step1,group="All",LA="Bury",GLD_or_AoL="AoL",AoL_type = "CL_and_Lit",latest_year=2018)
    #debug_dataset$trend_data
  })
  
  
  # AoL Trend Table ---------------------------------------------------------
  
  
  
  output$AoL_trend_table <- renderDT({
    
    selected_LA<- input$Local_Authority
    AoL_trend_data<-AoL_trend_plot_data()$trend_data
    AoL_trend_data_spread <- AoL_trend_data %>% spread(Year,Percentage) %>%
      mutate(Region=if_else(Region=="LA",selected_LA,Region)) %>%
      select(-LA_Name) %>% select(" "=Region, everything())%>% 
      # This line is to ensure that point zero ie 77.0 not 77 is displayed
      mutate_if(is.numeric,formatC,digits=1,format="f")
    
    datatable(AoL_trend_data_spread,
                  options=list(
                    
                    paging=FALSE,
                    searching=FALSE,
                    info=FALSE#,
                    #ordering=FALSE,
                  ),
              rownames = FALSE,
              class = 'cell-border stripe'
            )
  })
  
  # AoL Single Areas Neighbours Gaps Graph Data   ---------------------------
  
  AoL_neighbours_gaps_plot_data <- reactive({
    
    
    selected_LA<- input$Local_Authority
    AoL_input_group<-input$AoL_group_select
    AoL_input_group<-if_else(AoL_input_group=="All Other","All_Other"
                             ,if_else(AoL_input_group=="All","All",AoL_input_group))
    areas_of_learning<- input$AoL_select
    
    selected_gap_type<- if_else(AoL_input_group=="FSM",input$AoL_gap_select,"")
    selected_gap_type<- if_else(selected_gap_type=="Within the LA: gap with all other children","gap_within_LA",
                                if_else(selected_gap_type=="National average: gap with all other children","gap_All_Other_Nat_Av",
                                        if_else(selected_gap_type=="National average: gap with FSM children","gap_FSM_Nat_Av","")))
    selected_gap_type<- if_else(AoL_input_group=="FSM",selected_gap_type,"")
    
    AoL_sub_type=if_else(areas_of_learning=="Communication and Language","CL",
                         if_else(areas_of_learning=="Literacy","Lit","Mat"))
    
    dataset_1 <-    get_data_subset_neighbour(long_data_step1,LA=selected_LA,GLD_or_AoL_or_take_up="AoL",AoL_type=AoL_sub_type,group=AoL_input_group,Percent_or_Gap="gap",gap_type=selected_gap_type, latest_year = current_year)
    
    dataset_1<- dataset_1 %>% mutate_at(3,as.numeric)
    sort<-colnames(dataset_1[3])
    sort_sym<-  sym(sort)
    dataset_2<- dataset_1 %>%  arrange(!!sort_sym)#arrange(sort_sym)
    dataset_2$LA_Name<-factor( dataset_2$LA_Name, levels =  dataset_2$LA_Name) #convert to factor to preserve sort order
    dataset_2
  })
  
  
  output$AoL_neighbours_gaps_table_data <- renderTable({   
    AoL_neighbours_gaps_plot_data()
  })

  
  # AoL Single Areas Neighbours Gaps Graph ----------------------------------

  output$AoL_neighbours_gaps_plot<- renderPlot({ 
    
    dataset<-AoL_neighbours_gaps_plot_data()
    EYFSP_variable<-colnames(AoL_neighbours_gaps_plot_data()[3])
    selected_LA<- input$Local_Authority
    AoL_input_group<-input$AoL_group_select
    
    #For graph title/legend/labels
    group_type<-if_else(AoL_input_group=="All Other","All_Other"
                        ,if_else(AoL_input_group=="All","All",AoL_input_group))
    title_group=if_else(group_type=="FSM","children known to be eligible for FSM",
                        if_else(group_type=="All","all children",
                                if_else(group_type=="All_Other","all other children",
                                        "children identified as SEN")))
    
    selected_gap_type<- if_else(AoL_input_group=="FSM",input$AoL_gap_select,"")
    selected_gap_type<- if_else(selected_gap_type=="Within the LA: gap with all other children","gap_within_LA",
                                if_else(selected_gap_type=="National average: gap with all other children","gap_All_Other_Nat_Av",
                                        if_else(selected_gap_type=="National average: gap with FSM children","gap_FSM_Nat_Av","")))
    selected_gap_type<- if_else(AoL_input_group=="FSM",selected_gap_type,"")
    
    
    # Alternative way to do title- found issues with wrapping
    # Plot_title_1<-paste0("Graph showing the percentage gap ",title_group," within the LA achieving a AoL at EYFSP, compared with the 10 nearest statistical neighbours	.")
    # Plot_title_2<-wrapper(Plot_title_1, width = 90)
    # 
    # Plot_title<- Plot_title_2
    
   
    dataset_2<- dataset %>% select("Percentage_Gap"=EYFSP_variable,everything())
    dataset_3<- dataset_2 %>% mutate(LA_selected_by_user=if_else(LA_Name==selected_LA& Percentage_Gap<0,"Chosen LA below national average",
                                                                 if_else(LA_Name==selected_LA& Percentage_Gap>=0,"Chosen LA above or equal to national average",
                                                                         if_else(Percentage_Gap<0,"Neighbour below national average","Neighbour above or equal to national average"))))
    
    
    # To get the zero after the decimal point in the graph
    gap_2<-paste0("Percentage_Gap","_2")
    gap_sym<-sym(gap_2)
    dataset_3<- dataset_3 %>%  mutate(gap_2=sprintf('%.1f', Percentage_Gap))
    # end of section dded to get the zero after the decimal
    
    #add two line to ensure the data is in descending order- consistent with the neighbours graph
    dataset_3 <- dataset_3 %>% arrange(desc(Percentage_Gap))
    dataset_3$LA_Name<- factor(dataset_3$LA_Name,levels=dataset_3$LA_Name)
    
    labels_for_graph<- dataset_3 %>% select(LA_selected_by_user) %>% distinct() # labels will vary slightly depending on whether selected LA is above or below average
    
    plot<- ggplot(dataset_3,aes(x=LA_Name,y=Percentage_Gap, fill=LA_selected_by_user) )+ #NB aes_string as we are passing a variable rather than an actual name
      geom_col() + #fill="darkseagreen3"
      
      scale_fill_manual(name= "",
                        labels=labels_for_graph,
                        #labels = c("Chosen LA above or equal to national average", "Neighbour above or equal to national average","Chosen LA below national average","Neighbour below national average"),
                        values = c("Chosen LA above or equal to national average"="cornflowerblue", "Neighbour above or equal to national average"="darkseagreen3",
                                   "Chosen LA below national average"="orange","Neighbour below national average"="navajowhite2"))+#,guide=FALSE
      
     # geom_text(aes(label=Percentage_Gap,vjust = ifelse(Percentage_Gap >= 0, 0, 1)), position=position_dodge(width=0.9))+#, vjust=-0.25)+
      geom_text(aes(label=gap_2,vjust = ifelse(Percentage_Gap >= 0, -0.25, 1)), position=position_dodge(width=0.9))+ 
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank())+
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))+
      labs(x="",y="Percentage point gap")
    plot
    # how to put labels over each bar https://stackoverflow.com/questions/12018499/how-to-put-labels-over-geom-bar-for-each-bar-in-r-with-ggplot2
    # Solution for making labels appear above and below axis
    # https://stackoverflow.com/questions/11938293/how-to-label-a-barplot-bar-with-positive-and-negative-bars-with-ggplot2
    
    
    
  })
  

# AoL Singles Areas Gaps Plot Title ---------------------------------------

  
  output$AoL_neighbours_gaps_plot_graph_title <- renderUI({
    selected_LA_Name<-input$Local_Authority
    selected_gap_type<- if_else(input$AoL_group_select=="FSM",input$AoL_gap_select,"")
    str1 <- get_graph_title(neighbour_gap_context="neighbour_gap",LA=input$Local_Authority,group=input$AoL_group_select, GLD_or_AoL_or_take_up="AoL",AoL_type=input$AoL_select, gap_type=selected_gap_type)
    HTML(str1)
  })
  
  # for debug
  output$AoL_trend_table_data <- renderTable({#renderText({ # #depending on dbug need may renderText or renderTable
    #AoL_neighbours_plot_data()
    #AoL_national_plot_data()
    #AoL_SEN_Numbers_plot_data()
    #SEN_table_data()
    AoL_trend_plot_data() 
    
  })#
  
  
  # AoL Trend Plot ----------------------------------------------------------
  output$AoL_trend_plot <- renderPlot({
    
    
    AoL_trend_data_list<-AoL_trend_plot_data()
    AoL_trend_data<-AoL_trend_plot_data()$trend_data
    
    # Alternative plot title
    # Plot_title_1<- AoL_trend_plot_data()$trend_graph_title
    # 
    # Plot_title_2<-wrapper(Plot_title_1, width = 60) # To wrap title
    # 
    # Plot_title<- Plot_title_2
    
    
    LA_key<-AoL_trend_plot_data()$LA_legend
    National_key<-AoL_trend_plot_data()$National_legend
    
    AoL_trend_data_table_1<- AoL_trend_data %>%  spread(Year, Percentage) %>% select(-LA_Name)
    AoL_trend_data_table_2<- AoL_trend_data_table_1 %>%  mutate(Metric=if_else(Region=="LA",LA_key,
                                                                               National_key)) %>% select(-Region) %>% select(Metric, everything())
    
    #theme_minimal()
    AoL_trend_plot<-
      ggplot(data=AoL_trend_data, aes(x= Year, y=Percentage, group=Region,color=Region)) +
      geom_point(size=4)+
      geom_line(linetype="dashed",size=0.8)+
      scale_y_continuous(labels = scales::comma,breaks=seq(0,100,10),limits=c(0, 100))+
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.justification = "top")+
      #labs(y="Percentage",x="Year",title=Plot_title)+
      labs(y="Percentage",x="Year")+
      theme(legend.position = "bottom")
    
    
    #overwriting legends http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
    AoL_trend_plot+ scale_colour_manual(values = c(LA = "cornflowerblue", National = "grey"),
                                        name="",
                                        breaks=c("LA", "National" ),
                                        labels=c(LA_key, National_key))
  })
  

# AoL Single Areas Trend Plot Title ---------------------------------------

  
  output$AoL_trend_plot_graph_title <- renderUI({
    str1 <- AoL_trend_plot_data()$trend_graph_title
    HTML(str1)
  }) 
  
  
  # AoL National Data and Plot ----------------------------------------------
 

  output$AoL_national_plot <- renderPlot({
    
    
    #national figure
    #col<- if_else(input$AoL_group_select %in% c("FSM","All"),5,4)
    national_aol<- AoL_neighbours_plot_data()[4] %>%  head(1) %>% as.numeric()
    
    areas_of_learning<- input$AoL_select
    AoL_sub_type=if_else(areas_of_learning=="Communication and Language","CL",
                         if_else(areas_of_learning=="Literacy","Lit","Mat"))
    
    AoL_input_group<-input$AoL_group_select
    AoL_input_group<-if_else(AoL_input_group=="All Other","All_Other"
                             ,if_else(AoL_input_group=="All","All",AoL_input_group))
    show_all_LAs_selection<- if_else(input$show_all_LAs_AoL_tab=="Yes",TRUE,FALSE)
    get_national_plot(selected_LA=input$Local_Authority,neighbour_dataset=AoL_neighbours_plot_data(),group = AoL_input_group,metric="AoL",AoL_type=AoL_sub_type,year=current_year,show_all_LAs=show_all_LAs_selection,national = national_aol) 
    
  })
  
  # AoL Trend Gaps Graph ---------------------------------------------------------
  
  AoL_trend_gap_plot_data <- reactive({
    AoL_input_group<-input$AoL_group_select
    AoL_input_group<-if_else(AoL_input_group=="All Other","All_Other"
                             ,if_else(AoL_input_group=="All","All",AoL_input_group))
    
    areas_of_learning<- input$AoL_select
    
    # Selected gap type chosen depending on user input
    selected_gap_type<- if_else(AoL_input_group=="FSM",input$AoL_gap_select,"")
    selected_gap_type<- if_else(selected_gap_type=="Within the LA: gap with all other children","gap_within_LA",
                                if_else(selected_gap_type=="National average: gap with all other children","gap_All_Other_Nat_Av",
                                        if_else(selected_gap_type=="National average: gap with FSM children","gap_FSM_Nat_Av","")))
    selected_gap_type<- if_else(AoL_input_group=="FSM",selected_gap_type,"")
    
    AoL_sub_type=if_else(areas_of_learning=="Communication and Language","CL",
                         if_else(areas_of_learning=="Literacy","Lit","Mat"))
    
    selected_LA<-input$Local_Authority
    get_trend_data_fn_v2(three_years_data_step1,group=AoL_input_group,LA=selected_LA,GLD_or_AoL_or_Take_up="AoL",AoL_type=AoL_sub_type,gap_type = selected_gap_type,gap="gap",latest_year = current_year)  })
  
  
  output$AoL_trend_gap_plot_graph_title <- renderUI({
    str1 <- AoL_trend_gap_plot_data()$trend_gaps_graph_title
    HTML(str1)
  })  
 
  
  # for debug
  output$AoL_trend_gap_table <- renderTable({
    
    #AoL_neighbours_plot_data()
    #AoL_national_plot_data()
    #AoL_SEN_Numbers_plot_data()
    #SEN_table_data()
    #AoL_neighbours_gaps_plot_data()
   # data<-AoL_trend_plot_data()   - review later
    data<- AoL_trend_gap_plot_data()
    data$trend_data_gaps
  })#
 
  
  output$AoL_trend_gap_plot <- renderPlot({
    
    
    AoL_trend_data_list<-AoL_trend_plot_data()
    AoL_trend_data<-AoL_trend_gap_plot_data()$trend_data_gaps
    
    gaps_data<-AoL_trend_data %>% mutate(above_or_below=if_else(`Percentage Gap`>=0,"above_or_equal","below"))
    
    plot<- ggplot(gaps_data,aes(x=Year,y=`Percentage Gap`, fill=above_or_below) )+ #NB aes_string as we are passing a variable rather than an actual name
      geom_col() + #fill="darkseagreen3"
      scale_fill_manual(name= ""
                        ,labels = c("below", "above_or_equal"),
                        values = c("above_or_equal"="cornflowerblue",
                                   "below"="orange"),guide=FALSE)+#,guide=FALSE
      
      geom_text(aes(label=`Percentage Gap`,vjust = ifelse(`Percentage Gap` >= 0, -0.25, 1)), position=position_dodge(width=0.9))+#, vjust=-0.25)+
      
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank())+
            labs(y="Percentage point gap")
    
    plot  

  })
  
  
  # Take Up Section ---------------------------------------------------------
  
  # Take Up Neighbours Graph Data -------------------------------------------
  Take_up_neighbours_plot_data <- reactive({
    
    selected_LA<-input$Local_Authority
    selected_ages<-input$Ages
    
    level<-"LA"
    metric<-"Take_up"
    selected_ages_input<- if_else(selected_ages=="Two Year Olds","two_year_olds","three_and_four_year_olds")
    take_up_data<-get_data_subset_neighbour(long_data_step1,LA=selected_LA,GLD_or_AoL_or_take_up="Take_up",take_up_ages = selected_ages_input,latest_year = latest_year_take_up)
    
    # for debug
    # take_up_data<- get_data_subset_neighbour(long_data_step1,LA="Bury",GLD_or_AoL_or_take_up="Take_up",take_up_ages = selected_ages_input,latest_year = 2018)
    
    
    take_up_data_1<- take_up_data %>% mutate_at(3,as.numeric)
    sort<-colnames(take_up_data_1[3])
    sort_sym<-  sym(sort) 
    take_up_data_2<- take_up_data_1 %>%  arrange(desc(!!sort_sym))#arrange(sort_sym)
    take_up_data_2$LA_Name<-factor( take_up_data_2$LA_Name, levels =  take_up_data_2$LA_Name) #convert to factor to preserve sort order
    
    take_up_data_2$LA_selected_by_user<-factor(take_up_data_2$LA_selected_by_user,levels=c("yes","no"))
    take_up_data_2
    
    
  })
  
  
  # Take Up Combined Neighbour Plot Title -------------------------------------
  output$Take_up_neighbour_plot_graph_title <- renderUI({
    
    selected_LA_Name<-input$Local_Authority
    str1 <- get_graph_title(neighbour_gap_context="neighbour",LA=input$Local_Authority,take_up_ages = input$Ages, GLD_or_AoL_or_take_up="Take_up")
    HTML(str1)
  })
  
  
  # Take up Output Boxes ----------------------------------------------------
  
  
  output$Take_up_Nat <- renderValueBox({
    
    EYFSP_nat_variable<-colnames(Take_up_neighbours_plot_data()[10])
    national<- Take_up_neighbours_plot_data()[10] %>% select(!!EYFSP_nat_variable) %>% head(1) %>% as.numeric()
    
    valueBox(
      value = paste0(formatC(national, digits = 0, format = "f"),"%"),
      subtitle = "National Average",
      icon = icon("area-chart"),
      color = "purple" 
    )
  })
  
  
  output$Take_up_LA <- renderValueBox({ 
    
    EYFSP_LA_variable<-colnames(Take_up_neighbours_plot_data()[3])
    LA_Take_up<-Take_up_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    valueBox(
      value = paste0(formatC(LA_Take_up, digits = 0, format = "f"),"%"),
      subtitle = "LA Take up",
      icon = icon("area-chart"),
      color = "purple"
    )
  })
  
  
  output$Take_up_LA_Rank <- renderValueBox({
    
    EYFSP_LA_variable<-colnames(Take_up_neighbours_plot_data()[4])
    LA_Take_up<-Take_up_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    valueBox(
      value = formatC(LA_Take_up, digits = 0, format = "f"),
      subtitle = "LA Take up National Rank",
      icon = icon("area-chart"),
      color = "purple" 
    )
  })
  
  
  output$Take_up_LA_SN_Rank <- renderValueBox({

    EYFSP_LA_variable<-colnames(Take_up_neighbours_plot_data()[5])
    
    LA_Take_up<-Take_up_neighbours_plot_data() %>% filter(LA_Name==input$Local_Authority) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    valueBox(
      value = formatC(LA_Take_up, digits = 0, format = "f"),
      subtitle = "LA Take up Statistical Neighbour Rank",
      icon = icon("area-chart"), 
      color =  "purple" 
    )
  })
  
  
  
  output$Take_up_table <- renderTable({ # for debug
    Take_up_neighbours_plot_data ()  
  })
  
  
  # Take up Statistical Neighbours Plot -------------------------------------
 
  output$Take_up_neighbour_plot <- renderPlot({
    
    dataset<-Take_up_neighbours_plot_data()
    EYFSP_variable<-colnames(Take_up_neighbours_plot_data()[3])
    selected_LA<- input$Local_Authority
    
    selected_ages<-input$Ages
    selected_ages_input<- if_else(selected_ages=="Two Year Olds","two_year_olds","three_and_four_year_olds")
    
    National_variable_name<- colnames(dataset[10])
    National_figure<- dataset %>% select(National_variable_name) %>% distinct %>% as.numeric()
    
    title_group= tolower(selected_ages)
    
    
    
    # some code to make the y axis upper limit dynamic
    
    max_take_up<-max(dataset[3])
    max_take_up_rounded<-round(max_take_up,digits=-1)
    #logic is that if max_take_up is more than 100, then have the upper limit rounded to the nearest ten above
    upper_limit_take_up<- if_else(max_take_up>100,if_else(max_take_up_rounded<max_take_up,max_take_up_rounded+10,
                                                              max_take_up_rounded),100)
    
    plot<- ggplot(dataset,aes_string(x="LA_Name",y=EYFSP_variable, fill="LA_selected_by_user") )+ #NB aes_string as we are passing a variable rather than an actual name
      geom_col() + 
      
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.justification = "top")+
      scale_y_continuous(labels = scales::comma,breaks=seq(0,upper_limit_take_up,10),limits=c(0, upper_limit_take_up))+
      #scale_y_continuous(labels = scales::comma,breaks=seq(0,140,10),limits=c(0, 140))+
      #labs(y="Percentage",x="Neighbour",title=Plot_title)+
      labs(y="Percentage",x="Neighbour")+
      #geom_hline(yintercept=National_figure,colour='grey50',linetype="longdash",show.legend=TRUE)+
      geom_text(aes_string(label=EYFSP_variable,vjust = -0.25), position=position_dodge(width=0.9))+
      geom_hline(aes(yintercept=National_figure,linetype="National percentage for all"),colour='grey50')+#,linetype="longdash")+
      scale_linetype_manual(name = "", values = "longdash",
                            guide = guide_legend(override.aes =list(colour= "grey30")))+  #color = "blue"))+#,linetype="longdash"
      scale_fill_manual(name= ""
                        #,labels = c("Neighbours", selected_LA),
                        ,labels = c(selected_LA, "Neighbours"),
                        values = c("yes"="cornflowerblue", "no"="darkseagreen3"))+#,guide=FALSE)+
      guides(
        #color = guide_colorbar(order = 1),
        fill = guide_legend(order = 1))+ # change the order of the legend, as prefer the horizontal line to be after the bars label
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
    
    plot#+ scale_colour_manual(
    # manually add a legend https://stackoverflow.com/questions/24496984/how-to-add-legend-to-ggplot-manually-r/24497113
    #https://stackoverflow.com/questions/39119917/how-to-add-a-legend-to-hline
    # values = c("selectedLA" = "cornflowerblue",Neighbour="darkseagreen3", National = "grey"),
    #                        name="",
    #                       breaks=c("LA","Neighbour", "National" ),
    #                      labels=c("selectedLA","Neighbour", "National percent achieving at least eld for CL&Lit")#c(LA_key, National_key))
    #            )
  })
  
  
  # For debug
  #output$Take_up_neighbour_text <- renderText({
  
  # output$text1 <- renderText({paste("You have selected", input$Local_Authority)})
  # output$text2 <- renderText({paste("You have chosen a range that goes from",
  #                                   input$GLD_group_select, "to", input$GLD_gap_select)})
  
  # For debug
  output$text <- renderUI({
    str1 <- paste("In ", input$Local_Authority," the percentage of " , tolower(input$Ages)," benefitting from  funded early education places is ",
                  74,"% compared to ",78,"% of ",tolower(input$Ages)," nationally")
    str2 <- paste("You have chosen a range that goes from",
                  input$AoL_group_select, "to", input$Ages)
    HTML(paste(str1, str2, sep = '<br/>'))
  })
 
  
  # Trend Neighbours Text ---------------------------------------------------

  output$Take_up_text <- renderUI({
    
    selected_LA_Name<-input$Local_Authority
    EYFSP_nat_variable<-colnames(Take_up_neighbours_plot_data()[10])
    national_take_up<- Take_up_neighbours_plot_data()[10] %>% select(!!EYFSP_nat_variable) %>% head(1) %>% as.numeric()
    
    EYFSP_LA_variable<-colnames(Take_up_neighbours_plot_data()[3])
    LA_take_up<-Take_up_neighbours_plot_data() %>% filter(LA_Name==selected_LA_Name) %>% select(EYFSP_LA_variable) %>% as.numeric()
    
    
    str1 <- paste0("The percentage of ",tolower(input$Ages)," benefitting from funded early education places in ",input$Local_Authority," is ",LA_take_up,"% compared to ",national_take_up,"% nationally.")
    
    #str1 <- paste("For ", input$Local_Authority," the percentage of " , tolower(input$Ages)," benefitting from  funded early education places is ",
     #             LA_take_up,"% compared to ",national_take_up,"% of ",tolower(input$Ages)," nationally")
    HTML(str1)
  })

  # for debug
  output$Take_up_comb_table <- renderTable({ 
    Take_up_trend_plot_data()  
  })
  
   
  
  
  # Take up plot data -------------------------------------------------------
 
  Take_up_trend_plot_data <- reactive({
    selected_LA<- input$Local_Authority
    selected_ages<-input$Ages
    trend_dataset<-  Take_up_neighbours_plot_data ()  
    selected_ages_input<- if_else(selected_ages=="Two Year Olds","two_year_olds","three_and_four_year_olds")
    
    get_trend_take_up_data_fn(dataset=trend_dataset,LA=selected_LA,age_group = selected_ages_input, latest_year = current_year)
    
  })
  
  # Take Up Trend Plot ----------------------------------------------------------
  output$Take_up_trend_plot <- renderPlot({
    
    
    Take_up_trend_data_list<-Take_up_trend_plot_data()
    Take_up_trend_data<-Take_up_trend_plot_data()$trend_data
    # Plot_title_1<- Take_up_trend_plot_data()$trend_graph_title
    # 
    # Plot_title_2<-wrapper(Plot_title_1, width = 60) # To wrap title
    # 
    # Plot_title<- Plot_title_2
    
    LA_key<-Take_up_trend_plot_data()$LA_legend
    National_key<-Take_up_trend_plot_data()$National_legend
    
    Take_up_trend_data_table_1<- Take_up_trend_data %>%  spread(Year, Percentage) %>% select(-LA_Name)
    Take_up_trend_data_table_2<- Take_up_trend_data_table_1 %>%  mutate(Metric=if_else(Region=="LA",LA_key,                                                                                      National_key)) %>% select(-Region) %>% select(Metric, everything())
    
    
    max_take_up<-max(Take_up_trend_data$Percentage)
    max_take_up_rounded<-round(max_take_up,digits=-1)
    #logic is that if max_take_up is more than 100, then have the upper limit rounded to the nearest ten above
    upper_limit_take_up<- if_else(max_take_up>100,if_else(max_take_up_rounded<max_take_up,max_take_up_rounded+10,
                                                          max_take_up_rounded),100)
    
    
    
    #theme_minimal()
    Take_up_trend_plot<-
      ggplot(data=Take_up_trend_data, aes(x= Year, y=Percentage, group=Region,color=Region)) +
      geom_line(linetype="dashed",size=0.8)+
      geom_point(size=4)+
      scale_y_continuous(labels = scales::comma,breaks=seq(0,upper_limit_take_up,10),limits=c(0, upper_limit_take_up))+
     # scale_y_continuous(labels = scales::comma,breaks=seq(0,150,10),limits=c(0, 150))+
      theme_minimal()+
      theme(axis.ticks.x = element_line(colour = "grey30"),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.justification = "top")+
      labs(y="Percentage",x="Year")+#,title=Plot_title)+
      theme(legend.position = "bottom")
    
    #overwriting legends http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
    Take_up_trend_plot+ scale_colour_manual(values = c(LA = "cornflowerblue", National = "grey"),
                                            name="",
                                            breaks=c("LA", "National" ),
                                            labels=c(LA_key, National_key))
  })  
  
  output$Take_up_trend_plot_graph_title <- renderUI({
    str1 <- Take_up_trend_plot_data()$trend_graph_title
    HTML(str1)
  }) 
  
  
  # Take Up Trend Table -----------------------------------------------------
  
  output$Take_up_trend_table <- renderDT({
    
    selected_LA<- input$Local_Authority
    Take_up_trend_data<-Take_up_trend_plot_data()$trend_data
    Take_up_trend_data_spread <- Take_up_trend_data %>% spread(Year,Percentage) %>% 
      mutate(Region=if_else(Region=="LA",selected_LA,Region)) %>% 
      select(-LA_Name) %>% select(" "=Region, everything())
    #Take_up_trend_data_spread
      datatable(Take_up_trend_data_spread,
                options=list(
                  paging=FALSE,
                  searching=FALSE,
                  info=FALSE#,
                  #ordering=FALSE,
                ),
                rownames = FALSE,
                class = 'cell-border stripe'
      )
     
  })
  
  
  
  # Take Up national plot data ----------------------------------------------
  
  # for debug Take up national plot data
  # output$Take_up_national_table <- renderTable({
  #   Take_up_national_plot_data()
  #   
  # })
  
  # Take Up national graph --------------------------------------------------

  output$Take_up_national_plot <- renderPlot({
    
    national_take_up<- Take_up_neighbours_plot_data()[10] %>%  head(1) %>% as.numeric()
    
    selected_ages<-input$Ages
    selected_ages_input<- if_else(selected_ages=="Two Year Olds","two","three_and_four")
    show_all_LAs_selection<- if_else(input$show_all_LAs_take_up_tab=="Yes",TRUE,FALSE)
    get_national_plot(selected_LA=input$Local_Authority,neighbour_dataset=Take_up_neighbours_plot_data(),group = "",metric="Take_up",take_up_age=selected_ages_input,year=current_year,show_all_LAs=show_all_LAs_selection,national=national_take_up) 
    
  })
  
# For the notes tab, abbreviations and definitions, it seemed easier to format this by putting this into a table.
  output$Notes_table <- renderTable({
    notes_df
   }) 
})








# AMDG
