#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(tibble)
library(tidyr)
library(rsconnect)


change_statistic = function (x){
  if(x == "FG"){
    y = "Field Goals"
  } else if( x == "FGA"){
    y = "Field Goal Attempts Per Game"
  } else if ( x =="X3P"){
    y = "3-Point Field Goals Per Game"
  } else if( x =="X3PA"){
    y = "3-Point Field Goal Attempts Per Game"
  }else if(x =="FT"){
    y = "Free Throws Per Game"
  }else if(x =="ORB"){
    y = "Offensive Rebounds Per Game"
  }else if(x =="DRB"){
    y = "Defensive Rebounds Per Game"
  }else if(x =="TRB"){
    y = "Total Rebounds Per Game"
  }else if(x =="AST"){
    y = "Assists Per Game"
  }else if(x =="STL"){
    y = "Steals Per Game"
  }else if(x =="BLK"){
    y = "Blocks Per Game"
  }else if(x =="TOV"){
    y = " Turnovers Per Game"
  }else if(x =="PF"){
    y = "Personal Fouls Per Game"
  }else if(x =="PTS"){
    y = "Points Per Game"
  }
  
  return(y)
}

change_name = function(x){
  if(x == "KB"){
    return("Kobe Bryant")
  } else if(x == "LJ"){
    return("LeBron James")
  } else if(x == "MJ"){
    return("Michael Jordan")
  } 
}


change_name2 = function(x){
  if(x == "KB"){
    return("Bryant")
  } else if(x == "LJ"){
    return("James")
  } else if(x == "MJ"){
    return("Jordan")
  } 
}


var_type_find = function(x){
  if(x == 1){
    imp_stat = "FG"
  } else if (x == 2){
    imp_stat = "X3P"
  } else if (x == 3){
    imp_stat = "FT"
  } else if (x == 4){
    imp_stat = "ORB"
  } else if (x == 5){
    imp_stat = "DRB"
  } else if (x == 6){
    imp_stat = "TRB"
  } else if (x == 7){
    imp_stat = "AST"
  } else if (x == 8){
    imp_stat = "STL"
  } else if (x == 9){
    imp_stat = "BLK"
  } else if (x == 10){
    imp_stat = "TOV"
  } else if (x == 11){
    imp_stat = "PF"
  } else if (x == 12){
    imp_stat = "PTS"
  }
  return(imp_stat)
  
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Plots for Shots Activity"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      
      
      
      ##-------------------------------------------------------------------
      ## 0. Select Season
      ##-------------------------------------------------------------------
      selectInput("Season", h1("Select Season:"),
                  choices = list("Season 1" = 1,
                                 "Season 2" = 2,
                                 "Season 3" = 3,
                                 "Season 4" = 4,
                                 "Season 5" = 5,selected = 2)),
      
      
      ##-------------------------------------------------------------------
      ## 1. Pick Type of Single Player comparison or Multiplayer Comparison
      ##-------------------------------------------------------------------
      selectInput("type_analysis", h1("Choose Type of Analysis:"),
                  choices = list("Multiple Players" = 1,
                                 "Single Player" = 2, selected = 2)),
      
      
      
      
      conditionalPanel(
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## Conditional: Type of Analysis - Multi-Player Analysis
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        condition = "input.type_analysis == 1",
        selectInput("graph_represent", h3("Graphical Representation:"),
                    choices = list("Heatmap" = 1,
                                   "Boxplot" = 3  ## One Stat at a time
                    ), selected = 1)),
      
      
      
      conditionalPanel(
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## Conditional: Bar Plot: Offensive vs. Defensive
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        condition = "input.graph_represent == 2 & input.type_analysis == 1",
        
        
        selectInput("barplot_type", "Choose the side of the ball:",
                    choices = list("Offensive Stats" = 1,
                                   "Defensive Stats" = 2  ), selected = 1)),
      
      
      conditionalPanel(
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## Conditional: Box Plot 12 variables to Choose from
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        condition = "input.graph_represent == 3 & input.type_analysis == 1",
        
        
        selectInput("boxplot_type", "Choose Statistic:",
                    choices = list("Field Goals Per Game" = 1,
                                   "3-Point Field Goals Per Game"=2,
                                   "Free Throws Per Game"=3,
                                   "Offensive Rebounds Per Game" = 4,
                                   "Defensive Rebounds Per Game"=5,
                                   "Total Rebounds Per Game"=6,
                                   "Assists Per Game" = 7,
                                   "Steals Per Game"=8,
                                   "Blocks Per Game" = 9,
                                   "Turnovers Per Game"=10,
                                   "Personal Fouls Per Game" = 11,
                                   "Points Per Game"=12), selected = 1)),
      
      
      
      
      conditionalPanel(
        
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## Conditional: Type of Analysis - Single Player Analysis
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        condition = "input.type_analysis == 2",
        
        
        
        ##~~~~~~~~~~~~~~~~~~~~~~
        ## A.1) Player
        ##~~~~~~~~~~~~~~~~~~~~~~
        selectInput("player_name", "Player of Interest:",
                    choices = list("LeBron James" = 1,
                                   "Michael Jordan" = 2,
                                   "Kobe Byrant" = 3), selected = 1),
        
        
        ##~~~~~~~~~~~~~~~~~~~~~~
        ## A.2) Number of Variables
        ##~~~~~~~~~~~~~~~~~~~~~~
        selectInput("number_variables", "How many variables:",
                    choices = list("One Variable" = 1, 
                                   "Two Variables" = 2),
                    selected = "One Variable"),
        
        
        
        
        
        
        
        
        
        ##~~~~~~~~~~~~~~~~~~~~~~
        ## B.2) Conditional: Number of Variables - One Variable
        ##~~~~~~~~~~~~~~~~~~~~~~
        conditionalPanel(
          condition = "input.number_variables == 1",
          ## Type of plot
          selectInput("one_vars", "Type of Plot:",
                      choices = list("Histogram" = 1,
                                     "Bar Plot" = 2), selected = 1),
          selectInput("single_player_1stat", "Choose Statistic",
                      choices = list("Field Goals Per Game" = 1,
                                     "3-Point Field Goals Per Game"=2,
                                     "Free Throws Per Game"=3,
                                     "Offensive Rebounds Per Game" = 4,
                                     "Defensive Rebounds Per Game"=5,
                                     "Total Rebounds Per Game"=6,
                                     "Assists Per Game" = 7,
                                     "Steals Per Game"=8,
                                     "Turnovers Per Game"=10,
                                     "Personal Fouls Per Game" = 11,
                                     "Points Per Game"=12), selected = 1)),
        
        ##~~~~~~~~~~~~~~~~~~~~~~
        ## B.3) Conditional: Number of Variables - Two Variable
        ##~~~~~~~~~~~~~~~~~~~~~~
        conditionalPanel(
          condition = "input.number_variables == 2",
          ## Type of plot
          selectInput("two_vars", "Type of Plot:",
                      choices = list("Scatter Plot" = 1,
                                     "Time Series" = 2), selected = 1),
          
          conditionalPanel(
            condition = "input.two_vars == 1",
            selectInput("single_player_2stats1", "Choose the 1st-statistic (x-axis):",
                        choices = list("Field Goals Per Game" = 1,
                                       "3-Point Field Goals Per Game"=2,
                                       "Free Throws Per Game"=3,
                                       "Offensive Rebounds Per Game" = 4,
                                       "Defensive Rebounds Per Game"=5,
                                       "Total Rebounds Per Game"=6,
                                       "Assists Per Game" = 7,
                                       "Steals Per Game"=8,
                                       "Blocks Per Game" = 9,
                                       "Turnovers Per Game"=10,
                                       "Personal Fouls Per Game" = 11,
                                       "Points Per Game"=12), selected = 1),
            
            
            selectInput("single_player_2stats2", "Choose the 2nd-statistic (y-axis):",
                        choices = list("Field Goals Per Game" = 1,
                                       "3-Point Field Goals Per Game"=2,
                                       "Free Throws Per Game"=3,
                                       "Offensive Rebounds Per Game" = 4,
                                       "Defensive Rebounds Per Game"=5,
                                       "Total Rebounds Per Game"=6,
                                       "Assists Per Game" = 7,
                                       "Steals Per Game"=8,
                                       "Blocks Per Game" = 9,
                                       "Turnovers Per Game"=10,
                                       "Personal Fouls Per Game" = 11,
                                       "Points Per Game"=12), selected = 2)),
          
          conditionalPanel(
            condition = "input.two_vars == 2",
            selectInput("single_player_ts", "Choose the a statistic:",
                        choices = list("Field Goals Per Game" = 1,
                                       "3-Point Field Goals Per Game"=2,
                                       "Free Throws Per Game"=3,
                                       "Offensive Rebounds Per Game" = 4,
                                       "Defensive Rebounds Per Game"=5,
                                       "Total Rebounds Per Game"=6,
                                       "Assists Per Game" = 7,
                                       "Steals Per Game"=8,
                                       "Blocks Per Game" = 9,
                                       "Turnovers Per Game"=10,
                                       "Personal Fouls Per Game" = 11,
                                       "Points Per Game"=12), selected = 1))
          
          
        )
        
        
        
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("plot"),
      verbatimTextOutput("event"),
      textOutput("text1"),
      tags$head(tags$style("#text1{color: red;
                                 font-size: 20px;
                                 font-family: 'Old Standard TT, serif';
                                 }"
      ))
      
    )
    
    
    
    
    
    
    
    
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$plot <- renderPlotly({
    
    
    
    
    
    ##-----------------------------------------------
    ## Choice 1: Multiple Players
    ##-----------------------------------------------
    
    
    
    
    setwd("C:/Users/james/OneDrive/Documents/Important_Files/Stat_ed_2018_papers/paper_0_bball_data/0_basketball_data")
    nba = read.csv('modern_nba_legends_5292019.csv')
    
    nba = nba %>%
      filter(Name != "SC") %>%
      filter(Season == paste0("season_",input$Season,sep=""))
    
    ##Remove unimportant variables-----------------------------------------------------------
    ## 
    ##-----------------------------------------------------------
    nba_data = nba %>%
      select(-Rk,-G,-Date,-Age,-Tm,-Opp,-GS,-MP,-X3P.,
             -GmSc,-FT.,-FGA,-X3PA,-FTA,-FG.,-Season)
    
    
    
    
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Font Type
    f <- list(
      family = "Courier New",
      size = 18,
      color = "#7f7f7f"
    )
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Font Type for x and y title
    f_xy_title <- list(
      family = "Old Standard TT, serif",
      size = 20,
      color = "black"
    )
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Font Type for x and y tick
    f_xy_tick <- list(
      family = "Old Standard TT, serif",
      size = 16,
      color = "black"
    )
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Font Type for title
    f_title <- list(
      family = "Old Standard TT, serif",
      size = 23,
      color = "black"
    )
    
    ##For Margins of Graphic
    m <- list( 
      l = 50,
      r = 50,
      b = 100,
      t = 80,
      pad = 4
    )
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ## State the player, color, player name based on selection
    if(input$player_name == 1){
      imp_player = 'LJ'
      imp_player_name = 'LeBron James'
      imp_color_plots = 'rgb(255,184,28)'
    } else if(input$player_name == 2){
      imp_player = 'MJ'
      imp_player_name = 'Michael Jordan'
      imp_color_plots = 'rgb(206,17,65)'
    }else if(input$player_name == 3){
      imp_player = 'KB'
      imp_player_name = 'Kobe Byrant'
      imp_color_plots = 'rgb(85,37,130)'
    } 
    
    
    
    
    
    
    
    if(input$type_analysis == 1){ ## Multiplayer analysis
      
      if(input$graph_represent == 1){ ## Heatmap
        ##-----------------------------------------------------------
        ## Heatmap 0: Data Curation for Heatmap
        ##-----------------------------------------------------------
        
        
        
        ##-----------------------------------------------------------
        ## Heatmap 0: Data Curation for Heatmap
        ##-----------------------------------------------------------
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## x and y for Heatmaps Multiplayer
        x_heatmaps <- list(
          title = "Statisics",
          titlefont = f_xy_title,
          tickfont = f_xy_tick
        )
        y_heatmaps <- list(
          title = "Players",
          titlefont = f_xy_title,
          tickfont = f_xy_tick
        )
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        ## Obtain only the quantitative variables
        nba_data_quant = nba_data %>%
          select(-Game_Location,-Game_Outcome,-Point_Margin,-DD,
                 -TD)
        
        ## Obtain only the Categorical Variables
        nba_data_categ = nba_data %>%
          select(Name,DD,TD)
        
        
        ## Find Means of quantitative variables
        summarize_data_nba_quant = nba_data_quant %>%
          group_by(Name) %>%
          summarise_all(funs(mean))
        
        ## Find Sums of categorical variables
        summarize_data_nba_categ = nba_data_categ %>%
          group_by(Name) %>%
          summarise_all(funs(sum))
        
        summarize_data_nba = inner_join(x = summarize_data_nba_categ,
                                        y = summarize_data_nba_quant,
                                        by = c("Name"))
        
        ## Make NA 0
        summarize_data_nba[is.na(summarize_data_nba)] = 0
        ## Get names for new matrix
        names_players = as.character(summarize_data_nba$Name)
        
        ## Make data matrix
        summarize_data_nba = summarize_data_nba %>%
          select(-Name) %>%
          as.matrix()
        ## Reverse Order
        data_nba = 4-apply(summarize_data_nba,2,rank,ties.method = "max")
        rownames(data_nba) = names_players
        my_palette <- colorRampPalette(c("green", "black","red"))(n = 299)
        
        
        
        ## Gather information in correct format for Heatmap
        stats_name = rep(colnames(data_nba),3)
        p_name = rep(rownames(data_nba),14)
        stats_name = matrix(paste0('Stat: ',stats_name,sep=''),3,14,byrow = T)
        p_name = matrix(paste0('Player: ',p_name,sep=''),3,14)
        r_name = matrix(paste0('Rank: ',data_nba,sep=''),3,14)
        stat_amount_categ = summarize_data_nba[,c(1,2)]
        stat_amount_quant = round(summarize_data_nba[,-c(1,2)],2)
        stat_info_categ = matrix(paste0('Total: ',stat_amount_categ,sep=''),3,2)
        stat_info_quant = matrix(paste0('Average: ',stat_amount_quant,sep=''),3,12)
        stat_info = cbind(stat_info_categ, stat_info_quant)
        hover_labels = matrix(paste0(stats_name,' \n ',p_name,' \n ',r_name,' \n ',
                                     stat_info),3,14)
        rownames(data_nba) = apply(as.matrix(rownames(data_nba)),1,change_name2)
        
        
        ##-----------------------------------------------------------
        ## Heatmap 1: Plot of Heatmap
        ##-----------------------------------------------------------
        
        ## Create ranking for Color
        interval.cols <- c('red','yellow','blue')
        names(interval.cols) <- c(3,2,1)
        interval.cols2 <- rep(interval.cols, each=ncol(data_nba))
        color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
        color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)
        for (i in 1:(2*length(interval.cols))) {
          color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
          color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
        }
        
        
        ## Plot Graphic  
        
        p <- plot_ly(x=colnames(data_nba),
                     y=rownames(data_nba),
                     z = data_nba,
                     type = "heatmap",
                     hoverinfo='text',
                     text=hover_labels,
                     colors=interval.cols2,
                     colorscale=color.df,
                     colorbar=list(tickmode='array',tickvals=c(1:3),ticktext=names(interval.cols),
                                   len=0.375,outlinecolor="white",bordercolor="white",borderwidth=5,
                                   bgcolor="white"))  %>%
          layout(xaxis = x_heatmaps,
                 yaxis = y_heatmaps, 
                 title = paste0("Season ",input$Season," Heatmap of Players Rank",sep=""),
                 font =f_title, margin=m )
        
        p
        
        
        
        
      }  else { ## Boxplot
        ##-----------------------------------------------------------
        ## Boxplot 0: Curate data for boxplot
        ##-----------------------------------------------------------
        
        
        
        ## Determine Important Statistic
        imp_stat = var_type_find(input$boxplot_type)
        
        
        ##
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ## x and y for Boxplots Multiplayer
        y_boxplot_mp <- list(
          title = change_statistic(imp_stat),
          titlefont = f_xy_title,
          tickfont = f_xy_tick
        )
        x_boxplot_mp <- list(
          title = 'Players',
          titlefont = f_xy_title,
          tickfont = f_xy_tick
        )
        
        
        
        
        
        
        
        
        ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        info = nba %>%
          select(Name,G,imp_stat) %>%
          spread(Name,imp_stat)
        ##-----------------------------------------------------------
        ## Boxplot 1:  Plot Boxplot
        ##-----------------------------------------------------------
        p <- plot_ly( y = ~info$KB,type = "box", name = 'Bryant',
                      marker = list(color ='rgb(85,37,130)'),
                      line = list(color = 'rgb(85,37,130)') ) %>%
          add_trace(y = ~info$LJ, name = 'James',
                    marker = list(color = 'rgb(255,184,28)'),
                    line = list(color = 'rgb(255,184,28)') ) %>%
          add_trace(y = ~info$MJ, name = 'Jordan',
                    marker = list(color = 'rgb(206,17,65)'),
                    line = list(color = 'rgb(206,17,65)') ) %>%
          layout(yaxis = y_boxplot_mp ,
                 xaxis = x_boxplot_mp ,
                 title = paste0("Season ",input$Season," ",change_statistic(imp_stat)," by Player", sep=""),
                 font =f_title, margin=m)
        p
        
        
      }
      
    } else { ## Single player analysis
      
      if(input$number_variables == 1){ ## Number of variables
        
        
        
        
        if(input$one_vars == 1){ ## Histogram
          
          
          
          
          ## Determine Important Statistic
          imp_stat = var_type_find(input$single_player_1stat)
          
          
          
          
          
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ## X, y & title for Histogram SP
          x_hist_sp <- list(
            title = paste0(change_statistic(imp_stat)," Bins"),
            titlefont = f_xy_title,
            tickfont = f_xy_tick
          )
          y_hist_sp <- list(
            title = "Frequency",
            titlefont = f_xy_title,
            tickfont = f_xy_tick
          )
          title_hist_sp <- paste("Season ",input$Season," of ",imp_player_name," ",change_statistic(imp_stat)," Histogram",sep ='')
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          
          player_hist_sp = nba %>%
            filter(Name == imp_player) %>%
            select(imp_stat)
          
          ##-----------------------------------------------------------
          ## Histogram 1: Show Histogram
          ##-----------------------------------------------------------
          p <- plot_ly(x = ~player_hist_sp[,1],
                       type = "histogram",
                       marker=list(color=imp_color_plots , opacity=1))%>%
            layout(xaxis =x_hist_sp , yaxis = y_hist_sp, title = title_hist_sp,
                   font =f_title, margin=m)
          
          p
          
          
        } else { ##Bar Plot
          
          ## Determine Important Statistic
          imp_stat = var_type_find(input$single_player_1stat)
          
          
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ## X, y & title for Barplot SP
          x_barplot_sp <- list(
            title = paste0(change_statistic(imp_stat)," Performance Type"),
            titlefont = f_xy_title,
            tickfont = f_xy_tick
          )
          y_barplot_sp <- list(
            title = "Frequency",
            titlefont = f_xy_title,
            tickfont = f_xy_tick
          )
          title_barplot_sp <- paste("Season ",input$Season," of ",imp_player_name," ", change_statistic(imp_stat)
                                    ," Performance Bar Plot",sep ='')
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          
          player_barplot_sp = nba %>%
            filter(Name == imp_player) %>%
            select(imp_stat)
          
          
          sum_bp = summary(player_barplot_sp[,1])
          
          q1_bp = as.numeric(sum_bp[2])
          q3_bp = as.numeric(sum_bp[5])
          
          ## Barplot: Discetize Field Goals
          class_player = ifelse(player_barplot_sp[,1]<=q1_bp,
                                'Poor',ifelse(player_barplot_sp[,1]<=q3_bp,'Decent','Great'))
          data_barplot = data.frame(table(class_player))
          colnames(data_barplot)[1] = c('group')
          
          ## Create Proper ordrer
          data_barplot$group <- factor(data_barplot$group, levels = c('Poor','Decent','Great'))
          
          ##-----------------------------------------------------------
          ## Barplot 1: Show Barplot
          ##-----------------------------------------------------------
          p = plot_ly(x = data_barplot$group,
                      y = data_barplot$Freq,
                      name = paste0(change_name(imp_player_name)," ",
                                    change_statistic(imp_stat)," Performance"),
                      type = "bar",
                      marker=list(color=imp_color_plots , opacity=1)
          ) %>%
            layout(xaxis = x_barplot_sp , yaxis = y_barplot_sp, title = title_barplot_sp,
                   font =f_title, margin=m)
          p
          
          
          
          
        }
        
        
        
        
        
        
        
      } else if(input$number_variables == 2){ ## Scatter Plot or Time Series
        
        if(input$two_vars == 1){ ## Scatter Plot
          
          
          ## Determine Important Statistics
          imp_stat_1 = var_type_find(input$single_player_2stats1)
          imp_stat_2 = var_type_find(input$single_player_2stats2)
          
          
          
          
          
          
          
          
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ## x,y and title Scatter Plot SP
          x_scatterplot_sp <- list(
            title = change_statistic(imp_stat_1),
            titlefont = f_xy_title,
            tickfont = f_xy_tick
          )
          y_scatterplot_sp  <- list(
            title = change_statistic(imp_stat_2),
            titlefont = f_xy_title,
            tickfont = f_xy_tick
          )
          title_scatterplot_sp <- paste("Season ",input$Season," of ",imp_player_name," ",
                                        change_statistic(imp_stat_1),
                                        " vs. ",
                                        change_statistic(imp_stat_2)," Scatter Plot",sep ='')
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          
          player_data = nba %>%
            filter(Name == imp_player)
          
          player_scatter = player_data %>%
            select(imp_stat_1,imp_stat_2)
          
          ##-----------------------------------------------------------
          ## Scatter Plot 1: Show Scatter Plot
          ##-----------------------------------------------------------
          p = plot_ly(x = player_scatter[,1], y = player_scatter[,2],
                      marker=list(color=imp_color_plots , opacity=1),type = 'scatter') %>%
            layout(xaxis =x_scatterplot_sp,
                   yaxis = y_scatterplot_sp,
                   title = title_scatterplot_sp,
                   font =f_title, margin=m)
          p
          
          
          
          
        } else { ## Time Series
          ## Determine Important
          imp_stat = var_type_find(input$single_player_ts)
          
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ## x,y and title Time Series SP
          x_timeseries_sp <- list(
            title = "Date",
            titlefont = f_xy_title,
            tickfont = f_xy_tick
          )
          y_timeseries_sp  <- list(
            title = change_statistic(imp_stat),
            titlefont = f_xy_title,
            tickfont = f_xy_tick
          )
          title_timeseries_sp <- paste("Season ",input$Season," of ",change_name(imp_player_name)," ",
                                       change_statistic(imp_stat), " Time Series",sep ='')
          ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          player_timeseries = nba %>%
            filter(Name == imp_player)
          
          imp_stat_data = select(player_timeseries,imp_stat)
          
          
          ##-----------------------------------------------------------
          ## Time Series 1: Show Time Series
          ##-----------------------------------------------------------
          p <- plot_ly(x = ~as.Date(as.character(player_timeseries$Date),
                                    format = '%m/%d/%Y'),
                       y = ~imp_stat_data[,1],
                       type = 'scatter',
                       mode = 'lines',
                       marker=list(color=imp_color_plots, opacity=1),
                       line=list(color='black', opacity=1)) %>%
            layout(xaxis =x_timeseries_sp,
                   yaxis = y_timeseries_sp,
                   title = title_timeseries_sp,
                   font =f_title, margin=m)
          p
          
          
          
          
        }
        
        
        
      }
      
      
      
      
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  # output$text2 <- renderText({
  # 
  #   #if(input$type_analysis == 1 ){
  #     paste("Note: MJ - Michael Jordan, LJ - LeBron James, KB - Kobe Byrant")
  #   #}
  # 
  # })
  
  output$text1 <- renderText({
    if(input$type_analysis == 1 ){
      paste("Note: MJ - Michael Jordan, LJ - LeBron James, KB - Kobe Byrant",input$n) 
    }
  })
  
}

shinyApp(ui, server)

