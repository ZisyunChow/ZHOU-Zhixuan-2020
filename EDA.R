##### Package #####
library(tidyverse)
library(feather)
library(data.table)
library(viridis)
library(DT)
library(lubridate)
library(magrittr)
options(tibble.print_max = 5, tibble.print_min = 5)

##### EDA #####

# TRAIN #
setwd("~/Desktop/6010S HW/6010S Project/data")
train<-read.csv("train.csv")
summary(train)

# ggplot setting for readable labels #
readable_labs <- theme(axis.text=element_text(size=12),
                       axis.title=element_text(size=14),
                       plot.title = element_text(hjust = 0.5))

# Function to dislpay count of each category of the column and plot how it affects target #
target_vs_column <-function(df, col_name, x , y, title)
{
  
  temp_df <- df %>% 
    group_by_(col_name) %>% 
    summarize(count = n(), mean_target = mean(target)) %>% 
    arrange(desc(mean_target)) 
  
  df_plot <- temp_df %>%  
    ggplot(aes_string(col_name, "mean_target")) + 
    geom_col(aes(fill=count)) +
    scale_fill_gradient(low='gray', high = 'black')+
    coord_flip() +
    labs(x = x,
         y = y,
         title= title) +
    readable_labs
  
  print(df_plot)
  return (temp_df)
  
}

# Function to group songs and user by count and check it agains mean_target
target_vs_colcount <- function(df, col_name, x, y, title)
{ 
  
  df %>% 
    group_by_(col_name) %>% 
    summarize(count = n(), mean_target = mean(target)) %>% 
    group_by(count) %>% 
    summarize(new_count = n(), avg_target = mean(mean_target)) %>% 
    rename(no_of_items = new_count, occurence = count) %>% 
    print %>% 
    arrange(desc(avg_target)) %>% 
    print %>% 
    ggplot(aes(occurence, avg_target)) +
    geom_line(color='dark gray') +
    geom_smooth(color='light yellow') +
    labs(x = x,
         y = y,
         title= title) +
    readable_labs
  
}

## Train column count and its effect on target ##

# source_system_tab #
target_vs_column(train, col_name = "source_system_tab",
                 x = 'Frequency',
                 y = 'Target',
                 title = 'Count of source_system_tab vs Target')

# source_screen_name #
target_vs_column(train, col_name = "source_screen_name",
                 x = 'Frequency',
                 y = 'Target',
                 title = 'Count of source_screen_name vs Target')

# source_type #
target_vs_column(train, col_name = "source_type",
                 x = 'Frequency',
                 y = 'Target',
                 title = 'Count of source_type vs Target')

## Song count and User count vs target ##
target_vs_colcount(train, "song_id", "Song Occurence", "Target", "Song Occurence vs Target")


## check the distribution of target ##
train %>% 
  group_by(target) %>% 
  count

