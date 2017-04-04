library(dplyr)

d.readout <- read.csv2('../Data/irr_readout_master.csv') %>% arrange(Study.ID) %>% 
  bind_cols(read.csv2('../Data/irr_readout_sanja.csv') %>% arrange(Study.ID) %>% .['BAT.R1']) %>% 
  bind_cols(read.csv2('../Data/irr_readout_caro.csv') %>% arrange(Study.ID) %>% .['BAT.R2']) %>% 
  bind_cols(read.csv2('../Data/irr_readout_khoschy.csv') %>% arrange(Study.ID) %>% .['BAT.R3']) 

colnames(d.readout)[3:5] <- c('BAT.R2', 'BAT.R3', 'BAT.R1')
