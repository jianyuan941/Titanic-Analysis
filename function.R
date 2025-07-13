#Statistic
Descriptive_statistic <- function(data_column){
  
  mean <- round(mean(data_column, na.rm=TRUE),2)
  median <- round(median(data_column, na.rm=TRUE),2)
  mode <- round(get_mode(data_column),2)
  
  Des1 <- if(median>mean){"(negative skewness)"} 
  else if(median<mean){"(positve skewness)"}
  else if(mean == median){"(no skew)"}
  
  
  sdv <- round(sd(data_column, na.rm= TRUE),4)
  variance <- round(var(data_column, na.rm=TRUE),4)
  
  #Quartile
  Q1 <- quantile(data_column, probs=0.25, na.rm=TRUE)
  Q3 <- quantile(data_column, probs=0.75, na.rm=TRUE)
  IQR <- Q3-Q1
  
  #Formula for outliner
  Upper_bound_base_IQR <- Q3 + 1.5*IQR
  Lower_bound_base_IQR <- Q1 - 1.5*IQR
  Upper_bound_base_Mean <- mean + 2*sdv
  Lower_bound_base_Mean <- mean - 2*sdv
  
  #Calculate Outliner
  Upper_outliner <- sum(data_column >Upper_bound_base_IQR,na.rm=TRUE)
  
  Part1 <- paste0("Mean = ",mean, " ;Median = ",median, " ;Mode = ",mode, "; ", Des1)
  
  results <- c(Part1,"-----",sdv,variance,Q1,Q3,IQR,"-----",Upper_bound_base_IQR,Lower_bound_base_IQR,"Outliner = ", Upper_outliner)
  results %>% result_display(.,"")
}
#function
join_word <- function(x){
          new_join <- c()
          new_x <- sort(x)
          for(n in 1:length(unique(new_x))){
            if(n == 1){
              join <- unique(new_x)[n]  
            }
            else if(n == length(unique(new_x))){
              join <- paste0("and ",unique(new_x)[n])
            }else {
              join <- paste0(", ", unique(new_x)[n])
            }
            new_join <- c(new_join, join) %>% paste0(., collapse =" ")
          }
          #Output: 1,2,3,...and n
          return(new_join)
}
link <- function( url, new_tab = FALSE) {
          target <- if (new_tab) " target='_blank'" else ""
          paste0("Result show in section - <a href='", url, "'", target, ">", "Direct", "</a>")
}
sig_multiple <- function (col, single, multiple){
          min_value <- min(unique(nchar(col))) 
          max_value <- max(unique(nchar(col))) 
          word <- if(max_value>1){multiple}else{single}
          combine <- if(max_value>1){
            paste0(min_value, " to ", max_value, word)}else{
            paste0(min_value, word)  
            }
          # Output: 1 to 2 values/ 1 value
          return(combine)
}
result_display <- function(x,cap){
          x %>%
            tibble("Output :" =.) %>% 
            kbl(caption=cap) %>% 
            kable_styling(position="left")
} 
h4_header <- function(id,title) {
          paste0(
            "<hr>\n",
            "<h4 id='", id, "'>", title, "</h4>\n"
          )
}

combined_function <- function(file_name, dataframe_name, folder_path){
  
          file_path <- paste0(folder_path, file_name)
          
          #adjust column name
          #trim data
          
          df <- read.csv(file_path) %>% 
            clean_names() %>% 
            mutate(passenger_id = as.character(passenger_id))
          df[] <- lapply(df, function(col){
            if(is.character(col)) trimws(col) 
            else (col)
          })
          
          assign(dataframe_name, df, envir = .GlobalEnv)
          
          #return as skim for futher checking
          return(skim(df))
}
merged <- function(name,dataset1,dataset2,col_name){
  df <- merge(dataset1,dataset2, by=col_name, all= TRUE) 
  assign(name, df, envir = .GlobalEnv)
}
#Form new dataset for analysis purpose
  new_data <- function(n){
    df <- data.frame(
      ticket = merged_data$ticket,
      age_group = merged_data$age_group,
      survived = merged_data$survived,
      fare = merged_data$fare, 
      sib_sp = merged_data$sib_sp, 
      parch = merged_data$parch,
      embarked = merged_data$embarked,
      pclass = merged_data$pclass)
    
    assign(n, df, envir = .GlobalEnv)
    
    #Find any fare column with NA value before remove
    df[which(rowSums(is.na(df)) > 0),] %>% 
      kbl() %>% 
      kable_styling()
    
  }
#=====================================================
#MERGED PLOT
  merged_plot<- function(graph1, graph2){
    plot_grid(
      NULL,graph1, graph2, NULL,
      ncol = 4,
      rel_widths = c(0.1, 1, 1, 0.1)
    )
  }
#MERGED PLOT FULL
  merged_plot_full <-function(graph1, graph2,graph_y_axis,tit, title_y_axis){ #default graph_y_axis = -0.1, title_y_axis = 0.8
    ggdraw() +
      draw_label(tit, 
                 fontface = 'bold', 
                 x = 0.5, 
                 y = title_y_axis, 
                 hjust = 0.5, size = 16) +
      draw_plot(
        merged_plot(
          graph1,graph2), 
        x = 0, 
        y = graph_y_axis, 
        width = 1, 
        height = 0.95)
  }
#====================================================
# RANKING LARGE TO SMALL
  ranking_large_small <- function(data,column,tit){
    col <- enquo(column)
    data %>% 
      count(!!col, name = "temporary_n") %>% 
      arrange(desc(temporary_n)) %>% 
      ggplot(aes(
        x=fct_reorder(!!col,temporary_n, .desc=FALSE),
        y=temporary_n,
        fill=!!col,
        label=temporary_n,
      ))+
      geom_col()+
      coord_flip()+
      labs(x=NULL,
           y=NULL,
           title=tit)
#      theme(legend.position ="none",
#            plot.title = element_text(hjust=-0.2,
#                                      margin=margin(b=30)
#            ))+
#      geom_text(size = 3, #need to have label only can use geom_text
#                hjust = 1.1,
#                vjust = 0.25,
#                col = "black")     
  }
# RANKING SMALL TO LARGE
  ranking_small_large <- function(data,column,tit){
    col <- enquo(column)
    data %>% 
      count(!!col, name = "temporary_n") %>% 
      arrange(desc(temporary_n)) %>% 
      ggplot(aes(
        x=fct_reorder(!!col,temporary_n, .desc=TRUE),
        y=temporary_n,
        fill=!!col,
        label=temporary_n,
      ))+
      geom_col()+
      coord_flip()+
      labs(x=NULL,
           y=NULL,
           title=tit)+
      theme(legend.position ="none",
            plot.title = element_text(hjust=-0.2,
                                      margin=margin(b=30)
            ))+
      geom_text(size = 3, #need to have label only can use geom_text
                hjust = 1.1,
                vjust = 0.25,
                col = "black")     
  }
#BAR CHART
  barchart_echart <- function(data, col_for_bar, col_for_xaxis, title) {
    x <- enquo(col_for_bar)
    y <- enquo(col_for_xaxis)
    
    data %>%
      count(!!x, !!y) %>%
      mutate(!!quo_name(y) := as.character(!!y)) %>%     # 🔹 Ensure numeric x-axis
      arrange(!!y) %>%                                 # 🔹 Sort in numeric order
      group_by(!!x) %>%
      e_charts_(x = quo_name(y)) %>%
      e_bar(n, stack = FALSE) %>%
      e_legend() %>% 
      e_title(title) %>% 
      e_legend(orient = "vertical", left = "right", top = "center") %>% #side table
      e_grid(right = 150, bottom = 30)  # Increase this value to add more space
    
      #e_tooltip(trigger = "axis")
  }
#BAR CHART - GGPLOT (SINGLE)
  barchart_gg <- function(data, column, name_for_title){
    x <- enquo(column)
    
    data %>%
      count(!!x, name = "temporary_n") %>%
      mutate(total = sum(temporary_n)) %>% 
      ggplot(aes(x = !!x,
                 y= temporary_n,
                 fill = !!x)) +
      
      #1st Layer: Apply type of graph
      geom_col()
    
  }
#BAR CHART - GGPLOT 3 LAYER
  barchart_gg_custom <- function(data, x_axis, bar_count, group){
    x <- enquo(x_axis)
    y <- enquo(bar_count)
    fill <- enquo(group)
    
    ggplot(data, aes(x = !!x,
                     y = !!y,
                     fill = !!fill,
                     label = !!y
    ))+
      geom_col()
      
      #layer 1: Apply type of graph => position dodge mean seperate bar base on fill      
      #geom_col(position = position_dodge(width = 1)) +
      
      #layer 2: Apply labels to graph => position dodge mean apply label base on fill;
      #                                  vjust is to adjust height of label        
      #geom_text(position = position_dodge(width = 1), vjust = -0.25, size = 3)
      
    #this is to adjust the y-axis to start with coordination (0,0)
      #scale_y_continuous(expand = c(0,0),
      #                   limits = c(0,120)) #this to set from y = 0 to y = 120
  }  
  
#PIE CHART
  piechart_echart <- function(dataset, col,tit){
    x <- enquo(col)
    x_name <- as_name(x)
    
    dataset %>% 
      count(!!x, name = "count") %>% 
      e_charts_(x = x_name) %>% 
      e_title(tit) %>% 
      e_pie_(
        serie = "count",
        label = list(
          formatter = "{b}: {d}%"  # 👈 b = label, d = percent, c = raw count
        )
      ) %>% 
      e_legend(orient = "vertical", left = "right", top = "top") %>% #side table
      e_grid(right = 150, bottom = 30)  # Increase this value to add more space
  }
#PIE CHART -GGPLOT (SINGLE ) => combine data manipulation + graphing
  piechart_gg <- function(data, column, name_for_legend, name_for_title){
    col <- enquo(column)
    
    data %>% 
      count(!!col, name = "temporary_n") %>% 
      mutate(
        temporary_n_perc = round(temporary_n / sum(temporary_n) * 100, 2),
        label = paste0(temporary_n," (",temporary_n_perc, "%)")
      ) %>% 
      ggplot(aes(x = "", 
                 y = temporary_n, 
                 fill = !!col)) +
      
      # 1st layer
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      
      # 2nd layer
      labs(fill = name_for_legend, title = name_for_title) +
      geom_text(aes(label = label), 
                position = position_stack(vjust = 0.5), 
                size = 4) +
      
      # 3rd layer
      theme_void() +
      theme(legend.position = "right")
  }
#PIE CHART -GGPLOTV2 (SINGLE)  => only graphing
  piechart_ggv2 <- function(data, column, col_count, label, leg_tit, tit){
    
    ggplot(data, aes(x = "", 
                     y = {{col_count}}, 
                     fill = {{column}})) +
    
      #apply 1st layer: type of graph
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      
      #apply 2nd layer: labs
      labs(fill = leg_tit , title = tit) +
      geom_text(aes(label = {{label}}), position = position_stack(vjust = 0.5), size = 4) +
      
      #apply 3rd layer: adjustment for label, title, legend
      theme_void() +
      theme(legend.position = "right")
  }

  
  
#TREE MAP
  treemap_echart <- function(data, column, tit) {
    df <- data %>%
      count({{ column }}, name = "value") %>%
      mutate(
        name = as.character({{ column }}),
        parent = NA_character_
      ) %>%
      select(name, parent, value)
    
    df %>%
      e_charts() %>%
      e_treemap_(name = "name", value = "value", parents = "parent")%>% 
      e_title(tit) %>% 
      e_legend() %>% 
      e_legend(orient = "vertical", left = "right", top = "center") %>% #side table
      e_grid(right = 150, bottom = 30)  # Increase this value to add more space
    
  }
  
#TREE MAP 3 LAYER
  treemap_3_layer_echart <- function(data, l1, l2, l3) {
    level1 <- data %>%
      count({{ l1 }}, name = "value") %>%
      mutate({{ l2 }} := NA, {{ l3 }} := NA)
    
    level2 <- data %>%
      count({{ l1 }}, {{ l2 }}, name = "value") %>%
      mutate({{ l3 }} := NA)
    
    level3 <- data %>%
      count({{ l1 }}, {{ l2 }}, {{ l3 }}, name = "value")
    
    df <- bind_rows(level3, level2, level1) %>%
      mutate(
        name = coalesce(
          as.character({{ l3 }}),
          as.character({{ l2 }}),
          as.character({{ l1 }})
        ),
        parent = case_when(
          !is.na({{ l3 }}) ~ as.character({{ l2 }}),
          !is.na({{ l2 }}) ~ as.character({{ l1 }}),
          TRUE ~ NA_character_
        )
      ) %>%
      select(name, parent, value)
    
    df %>%
      e_charts() %>%
      e_treemap_(name = "name", value = "value", parents = "parent") %>%
      e_title("Treemap: l1 → l2 → l3") %>%
      e_tooltip(trigger = "item") %>% 
      e_legend() %>% 
      e_legend(orient = "vertical", left = "right", top = "center") %>% #side table
      e_grid(right = 150, bottom = 30)  # Increase this value to add more space
    
  }
