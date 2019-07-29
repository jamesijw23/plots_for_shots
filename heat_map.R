library(shiny)
library(dplyr)
library(plotly)
library(tibble)
library(tidyr)
library(rsconnect)
setwd("C:/Users/james/OneDrive/Documents/Important_Files/Stat_ed_2018_papers/paper_0_bball_data/0_basketball_data")
nba = read.csv('modern_nba_legends_5292019.csv')

nba = nba %>%
  filter(Name != "SC") %>%
  filter(Season == paste0("season_",1,sep=""))

##Remove unimportant variables-----------------------------------------------------------
## 
##-----------------------------------------------------------
nba_data = nba %>%
  select(-Rk,-G,-Date,-Age,-Tm,-Opp,-GS,-MP,-X3P.,
         -GmSc,-FT.,-FGA,-X3PA,-FTA,-FG.,-Season)





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



m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 80,
  pad = 4
)
rownames(data_nba) = apply(as.matrix(rownames(data_nba)),1,change_name2)
##-----------------------------------------------------------
## Heatmap 1: Plot of Heatmap
##-----------------------------------------------------------
interval.cols <- c('red','yellow','blue')
names(interval.cols) <- c(3,2,1)
interval.cols2 <- rep(interval.cols, each=ncol(data_nba))
color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),colors=c(0:(2*length(interval.cols)-1)))
color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)
for (i in 1:(2*length(interval.cols))) {
  color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
  color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
}

p <- plot_ly(x=colnames(data_nba),
             y=rownames(data_nba),
             z = data_nba,
             type = "heatmap",
             hoverinfo='text',
             text=hover_labels,
             colors=interval.cols2,
             colorscale=color.df,
             colorbar=list(tickmode='array',tickvals=c(1:3),ticktext=names(interval.cols),len=0.375,outlinecolor="white",bordercolor="white",borderwidth=5,bgcolor="white"))  %>%
  layout(xaxis = x_heatmaps,
         yaxis = y_heatmaps, 
         title = "Heatmap of Rank Players",
         font =f_title, margin=m )

p
