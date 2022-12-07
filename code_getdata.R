##### Data preparation #####
# load packages
rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
library(rjson)
library(xtable)
library(tidyverse)

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
df_csv = filter(df_csv, subsession.round_number <= 8)
df_csv = df_csv %>% mutate(
  subsession.asset_type = ifelse(subsession.asset_endowments=='2 2', 'A and B',
                          ifelse(subsession.asset_endowments=='0 4', 'only B', 'only A'))
)

# import data from json
df_json = NULL
for(i in seq(along=path_json)){
  d = fromJSON(file=paste(here('pilot/'), '/', path_json[i], sep=''))
  df_json = c(df_json, d)
}

# remove temporary dataset
rm(d)

# export aggreagate data file
write.csv(df_csv, here("data_csv.csv"))
save(df_json, file="data_json.JSON")

# build the trade data csv from json data
# create data frame
df_reg = data.frame(session_code=character(), round_number=integer(), 
                    id_in_subsession=integer(), asset_id=character(), 
                    timestamp=double(), taker_id=integer(), maker_id=integer(),
                    price=double(), volume=double())

# loop over json data to fill out the new datasets
for (i in 1:length(df_json)){
  
  # trading period info
  temp = df_json[[i]]
  # asset A trade history
  temp_a = temp$exchange_data[[1]]$trades
  if (length(temp_a) > 0){
    for (j in 1:length(temp_a)){
      temp_tr = temp_a[[j]]
      df_reg = df_reg %>% add_row(session_code=temp$session_code, round_number=temp$round_number,
                         id_in_subsession=temp$id_in_subsession, asset_id='A',
                         timestamp=temp_tr$timestamp, taker_id=temp_tr$taking_order_id,
                         maker_id=temp_tr$making_order_id,
                         price=temp_tr$price, volume=temp_tr$volume)
    }
  }
  # asset B trade history
  temp_b = temp$exchange_data[[2]]$trades
  if (length(temp_b) > 0){
    for (j in 1:length(temp_b)){
      temp_tr = temp_b[[j]]
      df_reg = df_reg %>% add_row(session_code=temp$session_code, round_number=temp$round_number,
                         id_in_subsession=temp$id_in_subsession, asset_id='B',
                         timestamp=temp_tr$timestamp, taker_id=temp_tr$taking_order_id,
                         maker_id=temp_tr$making_order_id,
                         price=temp_tr$price, volume=temp_tr$volume)
    }
  }
}

# add treatment info to df_reg
df_treat = select(df_csv, c(session.code, subsession.round_number,
                            group.id_in_subsession, subsession.period_length,
                            subsession.num_assets, subsession.num_states,
                            subsession.asset_endowments, subsession.cash_endowment,
                            subsession.practice, subsession.x, subsession.G,
                            subsession.L, subsession.p1, subsession.p2, subsession.p3,
                            subsession.state_independent, subsession.salient_payoff,
                            subsession.asset_type, participant.id_in_session))
df_treat = rename(df_treat, session_code = session.code, 
                            round_number = subsession.round_number,
                            id_in_subsession = group.id_in_subsession)
df_treat = filter(df_treat, participant.id_in_session == 1)

df_reg = merge(df_reg, df_treat, all.x = TRUE,
               by = c('session_code', 'round_number', 'id_in_subsession'))
df_reg = rename(df_reg,
  period_length = subsession.period_length,
  num_assets = subsession.num_assets, 
  num_states = subsession.num_states,
  asset_endowments = subsession.asset_endowments, 
  cash_endowment = subsession.cash_endowment,
  practice = subsession.practice, 
  x = subsession.x, 
  G = subsession.G,
  L = subsession.L, 
  p1 = subsession.p1, 
  p2 = subsession.p2, 
  p3 = subsession.p3,
  state_independent = subsession.state_independent, 
  salient_payoff = subsession.salient_payoff,
  asset_type = subsession.asset_type)

df_reg = select(df_reg, -c(participant.id_in_session))

# remove temporary datasets
rm(df_treat, temp, temp_a, temp_b, temp_tr)

# export aggreagate data file
write_dta(df_reg, here("data_reg.dta"))
write.csv(df_reg, here('data_trade.csv'))