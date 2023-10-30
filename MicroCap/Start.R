source("R/dependencies.R")

# Create Starting Universe -----------------------------------------------------
tidyquant::tq_exchange_options()
stocks <- tidyquant::tq_exchange("AMEX")


# create research dataset ------------------------------------------------------
DB_FILE = "C:/0cap_2/MacroMan/SystematicMacro/data/MicroCap.db"
conn <- RSQLite::dbConnect(RSQLite::SQLite(), DB_FILE)
portfolio_db <- tbl(conn, "UNIVERSE") %>% as.data.frame()

# Update Rebalancing table
RSQLite::dbWriteTable(conn, "REBALANCING_DATES",
                      data.frame("date"=as.character(Sys.Date()),"SubPortfolio"=rebalance_subPort),
                      append = TRUE, overwrite = FALSE)


stocks <- jsonlite::fromJSON('https://api.nasdaq.com/api/screener/stocks?tableonly=true&limit=25&offset=0&exchange=AMEX&download=true')
stocks <- httr::GET('https://api.nasdaq.com/api/screener/stocks?tableonly=true&limit=25&offset=0&exchange=AMEX&download=true')
