##### Data preparation #####
# load packages
rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
library(rjson)
library(xtable)

# add paths to files
path_csv = list.files(path=here('pilot/'), pattern="*.csv")
path_json = list.files(path=here('pilot/'), pattern="*.json")

# import data from csv
df_csv = NULL
for(i in seq(along=path_csv)){
  d = read.csv(paste(here('pilot/'), '/', path_csv[i], sep=''),
               header=T, stringsAsFactors = FALSE)
  df_csv = rbind(df_csv, d)
}
df_csv = filter(df_csv, subsession.round_number <= 8 & subsession.practice != 1)

# import data from json
df_json = NULL
for(i in seq(along=path_json)){
  d = fromJSON(file=paste(here('pilot/'), '/', path_json[i], sep=''))
  df_json = c(df_json, d)
}

# remove temporary dataset
rm(d)


##### Market-level Dynamics #####
# loop over to get data for each market
for (i in 1:length(df_json)){
  
  # get treatment info
  session = df_json[[i]]$session_code
  round = df_json[[i]]$round_number
  group = df_json[[i]]$id_in_subsession
  num_assets = filter(df_csv, session.code == session)$subsession.num_assets[1]
  num_states = filter(df_csv, session.code == session)$subsession.num_states[1]
  treatment = paste('A', num_assets, 'S', num_states, sep = '')
  
  # set up data containers
  df_a = data.frame()
  df_b = data.frame()
  df_trade_a = df_json[[i]]$exchange_data[[1]]$trades
  df_trade_b = df_json[[i]]$exchange_data[[2]]$trades
  
  # loop over asset A trade data
  if (length(df_trade_a)!=0){
    for (j in 1:length(df_trade_a)){
      temp = df_trade_a[[j]]
      df_a = rbind(df_a, c(temp$timestamp, temp$price))
    }
    colnames(df_a) = c('time','price')
  }
  
  # loop over asset B trade data
  if (length(df_trade_b)!=0){
    for (j in 1:length(df_trade_b)){
      temp = df_trade_b[[j]]
      df_b = rbind(df_b, c(temp$timestamp, temp$price))
    }
    colnames(df_b) = c('time','price')
  }
  
  # draw graphs
  title = paste(session, treatment, 'round', round, sep = '_')
  file = paste(here('figures/'), '/', title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 1000, height = 600)
  
  if (length(df_a)!=0 & length(df_b)!=0){
    pic = ggplot() +
      geom_hline(yintercept=100) + 
      geom_line(data = df_a, aes(x=time, y=price), colour = 'red', size = 1) +
      geom_point(data = df_a, aes(x=time, y=price), colour = 'red', size = 3) +
      geom_line(data = df_b, aes(x=time, y=price), colour = 'blue', size = 1) +
      geom_point(data = df_b, aes(x=time, y=price), colour = 'blue', size = 3) +
      scale_x_continuous(name='time stamp', waiver()) +
      scale_y_continuous(name='price', limits=c(20,180),
                         breaks = c(20,80,100,120,180)) +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  else if (length(df_a)!=0){
    pic = ggplot() +
      geom_hline(yintercept=100) + 
      geom_line(data = df_a, aes(x=time, y=price), colour = 'red', size = 1) +
      geom_point(data = df_a, aes(x=time, y=price), colour = 'red', size = 3) +
      scale_x_continuous(name='time stamp', waiver()) +
      scale_y_continuous(name='price', limits=c(20,180),
                         breaks = c(20,80,100,120,180)) +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  else if (length(df_b)!=0){
    pic = ggplot() +
      geom_hline(yintercept=100) + 
      geom_line(data = df_b, aes(x=time, y=price), colour = 'blue', size = 1) +
      geom_point(data = df_b, aes(x=time, y=price), colour = 'blue', size = 3) +
      scale_x_continuous(name='time stamp', waiver()) +
      scale_y_continuous(name='price', limits=c(20,180),
                         breaks = c(20,80,100,120,180)) +
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  
  print(pic)
  dev.off()
}

# remove temporary data
rm(df_a, df_b, df_trade_a, df_trade_b, pic, temp)


##### Summary Statistics #####
# create data matrix, first 3 rows for asset A, last 3 rows for asset B
mat = matrix(0, nrow = 10, ncol = 6)
rownames(mat) = c('# of groups', '# of trades', 'price mean', 'price variance',
                  'bid mean', 'bid variance', 'ask mean', 'ask variance',
                  'd(price,RE)', 'd(price,ST)')
colnames(mat) = c('A2S2-Asset A', 'A2S3-Asset A', '(1)-(2)',
                  'A2S2-Asset B', 'A2S3-Asset B', '(4)-(5)')

# create container for price, bid, and ask
a2s2_price_a = c()
a2s2_bid_a = c()
a2s2_ask_a = c()
a2s3_price_a = c()
a2s3_bid_a = c()
a2s3_ask_a = c()
a2s2_price_b = c()
a2s2_bid_b = c()
a2s2_ask_b = c()
a2s3_price_b = c()
a2s3_bid_b = c()
a2s3_ask_b = c()

# loop over to get data for each market
for (i in 1:length(df_json)){
  
  # get treatment info
  session = df_json[[i]]$session_code
  round = df_json[[i]]$round_number
  group = df_json[[i]]$id_in_subsession
  num_assets = filter(df_csv, session.code == session)$subsession.num_assets[1]
  num_states = filter(df_csv, session.code == session)$subsession.num_states[1]
  treatment = paste('A', num_assets, 'S', num_states, sep = '')
  
  # skip if it is round 1 (practice round)
  if (round == 1){next}
  
  # set up data containers
  df_trade_a = df_json[[i]]$exchange_data[[1]]$trades
  df_order_a = df_json[[i]]$exchange_data[[1]]$orders
  df_trade_b = df_json[[i]]$exchange_data[[2]]$trades
  df_order_b = df_json[[i]]$exchange_data[[2]]$orders
  
  # update the dataset if treatment is A2S2
  if (treatment == 'A2S2'){
    
    # update number of groups
    mat[1,1] = mat[1,1] + 1
    mat[1,4] = mat[1,4] + 1
    
    # update number of trades
    mat[2,1] = mat[2,1] + length(df_trade_a)
    mat[2,4] = mat[2,4] + length(df_trade_b)
    
    # record price info
    for (j in 1:length(df_trade_a)){
      a2s2_price_a = rbind(a2s2_price_a, df_trade_a[[j]]$price)
    }
    for (j in 1:length(df_trade_b)){
      a2s2_price_b = rbind(a2s2_price_b, df_trade_b[[j]]$price)
    }
    
    # record bid and ask info
    for (j in 1:length(df_order_a)){
      if (df_order_a[[j]]$is_bid == 'TRUE'){
        a2s2_bid_a = rbind(a2s2_bid_a, df_order_a[[j]]$price)
      }
      else{a2s2_ask_a = rbind(a2s2_ask_a, df_order_a[[j]]$price)}
    }
    for (j in 1:length(df_order_b)){
      if (df_order_b[[j]]$is_bid == 'TRUE'){
        a2s2_bid_b = rbind(a2s2_bid_b, df_order_b[[j]]$price)
      }
      else{a2s2_ask_b = rbind(a2s2_ask_b, df_order_b[[j]]$price)}
    }
  }
  
  # update the dataset if treatment is A2S3
  else if (treatment == 'A2S3'){
    
    # update number of groups
    mat[1,2] = mat[1,2] + 1
    mat[1,5] = mat[1,5] + 1
    
    # update number of trades
    mat[2,2] = mat[2,2] + length(df_trade_a)
    mat[2,5] = mat[2,5] + length(df_trade_b)
    
    # record price info
    for (j in 1:length(df_trade_a)){
      a2s3_price_a = rbind(a2s3_price_a, df_trade_a[[j]]$price)
    }
    for (j in 1:length(df_trade_b)){
      a2s3_price_b = rbind(a2s3_price_b, df_trade_b[[j]]$price)
    }
    
    # record bid and ask info
    for (j in 1:length(df_order_a)){
      if (df_order_a[[j]]$is_bid == 'TRUE'){
        a2s3_bid_a = rbind(a2s3_bid_a, df_order_a[[j]]$price)
      }
      else{a2s3_ask_a = rbind(a2s3_ask_a, df_order_a[[j]]$price)}
    }
    for (j in 1:length(df_order_b)){
      if (df_order_b[[j]]$is_bid == 'TRUE'){
        a2s3_bid_b = rbind(a2s3_bid_b, df_order_b[[j]]$price)
      }
      else{a2s3_ask_b = rbind(a2s3_ask_b, df_order_b[[j]]$price)}
    }
  }
}

# fill out the rest of the dataset
mat[3,1] = mean(a2s2_price_a)
mat[3,2] = mean(a2s3_price_a)
mat[3,4] = mean(a2s2_price_b)
mat[3,5] = mean(a2s3_price_b)

mat[4,1] = var(a2s2_price_a)
mat[4,2] = var(a2s3_price_a)
mat[4,4] = var(a2s2_price_b)
mat[4,5] = var(a2s3_price_b)

mat[5,1] = mean(a2s2_bid_a)
mat[5,2] = mean(a2s3_bid_a)
mat[5,4] = mean(a2s2_bid_b)
mat[5,5] = mean(a2s3_bid_b)

mat[6,1] = var(a2s2_bid_a)
mat[6,2] = var(a2s3_bid_a)
mat[6,4] = var(a2s2_bid_b)
mat[6,5] = var(a2s3_bid_b)

mat[7,1] = mean(a2s2_ask_a)
mat[7,2] = mean(a2s3_ask_a)
mat[7,4] = mean(a2s2_ask_b)
mat[7,5] = mean(a2s3_ask_b)

mat[8,1] = var(a2s2_ask_a)
mat[8,2] = var(a2s3_ask_a)
mat[8,4] = var(a2s2_ask_b)
mat[8,5] = var(a2s3_ask_b)

mat[9,] = mat[3,] - c(100,100,0,100,100,0)
mat[10,] = mat[3,] - c(113.3,109.4,0,86.7,90.6,0)

# calculate the difference column (3) and (6)
mat[,3] = mat[,1] - mat[,2]
mat[,6] = mat[,4] - mat[,5]
mat = round(mat, digits = 2)

# export table
xtable(mat, caption = 'Summary statistics', label = 'tab:summary')


##### Asset Holding by the end of each period #####
# collect player ID
uniqueplayer = unique(df_csv$participant.code)
min = min(df_csv$subsession.round_number)
max = max(df_csv$subsession.round_number)

# asset holding container
mat = matrix(0, nrow = length(uniqueplayer), 
              ncol = 2*(max-min+1))
rownames(mat) = uniqueplayer
colnames(mat) = c(2,2,3,3,4,4,5,5,6,6,7,7,8,8)

mata = matrix(0, nrow = length(uniqueplayer), 
             ncol = max-min+1)
rownames(mata) = uniqueplayer
colnames(mata) = seq(from=min, to=max)

matb = mata
matdiff = mata

# locate the specific player at the specific trading period
for (i in min:max){
  for (j in 1:length(uniqueplayer)){
    data = filter(df_csv, participant.code == uniqueplayer[j],
                          subsession.round_number == i)
    mat[j,2*(i-1)-1] = as.integer(substring(data$player.settled_assets,7,7))
    mat[j,2*(i-1)] = as.integer(substring(data$player.settled_assets,15,15))
    mata[j,i-1] = mat[j,2*(i-1)-1]
    matb[j,i-1] = mat[j,2*(i-1)]
    matdiff[j,i-1] = mata[j,i-1] - matb[j,i-1]
  }
}