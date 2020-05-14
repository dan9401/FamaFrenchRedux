savePreprocess <- function() {
  save(data_price, file = "data/data_price.Rdata")
  save(data_delist, file = "data/data_delist.Rdata")
  save(data_idx, file = "data/data_idx.Rdata")
  save(data_fund, file = "data/data_fund.Rdata")
  save(data_link, file = "data/data_link.Rdata")
  save(data_names, file = "data/data_names.Rdata")
  
  save(preBeta, file = "data/preBeta.Rdata")
  save(nyse_breakpoints, file = "data/nyse_breakpoints.Rdata")
  save(beta_groups, file = "data/beta_groups.Rdata")
  save(data_beta, file = "data/data_beta.Rdata")
  
  save(data_groups, file = "data/data_groups.Rdata")
  save(data_processed, file = "data/data_processed.Rdata")
  save(beta_processed, file = "data/beta_processed.Rdata")
}

loadPreprocess <- function() {
  load("data/data_price.Rdata")
  load("data/data_delist.Rdata")
  load("data/data_idx.Rdata")
  load("data/data_fund.Rdata")
  load("data/data_link.Rdata")
  load("data/data_names.Rdata")
  
  load("data/preBeta.Rdata")
  load("data/nyse_breakpoints.Rdata")
  load("data/beta_groups.Rdata")
  load("data/data_beta.Rdata")
  
  load("data/data_groups.Rdata")
  load("data/data_processed.Rdata")
  load("data/beta_processed.Rdata")
}

saveData <- function() {
  save(data_63_90, file = "data/data_63_90.Rdata")
  save(data_80_15, file = "data/data_80_15.Rdata")
  save(data_63_15, file = "data/data_63_15.Rdata")
  save(data_63_18, file = "data/data_63_18.Rdata")
}

loadData <- function() {
  load(file = "data/data_63_90.Rdata")
  load(file = "data/data_80_15.Rdata")
  load(file = "data/data_63_15.Rdata")
  load(file = "data/data_63_18.Rdata")
}

saveRes <- function() {
  save(beta63_90, file = "data/beta63_90.Rdata")
  save(beta80_15, file = "data/beta80_15.Rdata")
  save(beta63_15, file = "data/beta63_15.Rdata")
  save(beta63_18, file = "data/beta63_18.Rdata")
  
  save(size63_90, file = "data/size63_90.Rdata")
  save(size80_15, file = "data/size80_15.Rdata")
  save(size63_15, file = "data/size63_15.Rdata")
  save(size63_18, file = "data/size63_18.Rdata")
  
  save(b2m63_90, file = "data/b2m63_90.Rdata")
  save(b2m80_15, file = "data/b2m80_15.Rdata")
  save(b2m63_15, file = "data/b2m63_15.Rdata")
  save(b2m63_18, file = "data/b2m63_18.Rdata")
  
  save(e2p63_90, file = "data/e2p63_90.Rdata")
  save(e2p80_15, file = "data/e2p80_15.Rdata")
  save(e2p63_15, file = "data/e2p63_15.Rdata")
  save(e2p63_18, file = "data/e2p63_18.Rdata")
}

loadRes <- function() {
  load(file = "data/beta63_90.Rdata")
  load(file = "data/beta80_15.Rdata")
  load(file = "data/beta63_15.Rdata")
  load(file = "data/beta63_18.Rdata")
  
  load(file = "data/size63_90.Rdata")
  load(file = "data/size80_15.Rdata")
  load(file = "data/size63_15.Rdata")
  load(file = "data/size63_18.Rdata")
  
  load(file = "data/b2m63_90.Rdata")
  load(file = "data/b2m80_15.Rdata")
  load(file = "data/b2m63_15.Rdata")
  load(file = "data/b2m63_18.Rdata")
  
  load(file = "data/e2p63_90.Rdata")
  load(file = "data/e2p80_15.Rdata")
  load(file = "data/e2p63_15.Rdata")
  load(file = "data/e2p63_18.Rdata")
}