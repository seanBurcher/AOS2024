library(celltracktech)
library(duckdb)
start <- Sys.time()

####SETTINGS#####
myproject <- "Meadows V2" #this is your project name on your CTT account
outpath <- "~/development/aos_test/" #where your downloaded files are to go 
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "~/development/aos_test/data/meadows.duckdb", read_only = FALSE)
my_token <- "d20922a756795c9857fb1dce6c4c3bb3be4c50cd3090e30a869ab647f031cb60"
################
get_my_data(my_token, outpath, con, myproject=myproject, begin=as.Date("2024-09-01"), end=as.Date("2024-12-01"), filetypes=c("raw", "node_health", "blu"))
update_db(con, outpath, myproject)
DBI::dbDisconnect(con)
time_elapse <- Sys.time() - start
print(time_elapse)