library(data.table)
library(bit64)

load("Products.RData")

# distinguish between control and non-control bands
products[, is_PL := brand_descr %like% "CTL BR"]
table(products[,is_PL])
# TRUE       FALSE
# 1,031,950  2,910,990

table((products[is_PL==TRUE,brand_descr]))

# get rid of products data for General Merch dept, Magnet Data, and products w/o dept code
products <- products[product_module_descr != "MAGNET DATA" & is.na(department_code) == FALSE & department_descr != "GENERAL MERCHANDISE"]

# load purchase data and do calculations and shit
container = list()
index = 1

for (yr in 2004:2014) {
  
  load(paste("purchases_", yr, ".RData", sep=""))
  
  # add year and month variables
  purchases[, `:=` (year = year(purchase_date), month = month(purchase_date))]
  
  # merge in is_PL and dept_descr
  setkey(purchases, upc, upc_ver_uc)
  setkey(products, upc, upc_ver_uc)
  purchases <- merge(purchases, products[,.(upc, upc_ver_uc, is_PL, department_code)])
  
  # calculate total dollar spend for private and other by household/year/month
  purchases_PL <- purchases[, .(spend = sum(total_price_paid-coupon_value)), 
                                  keyby = .(is_PL, household_code, year, month)]
  
  # convert to percentage shares
  purchases_total <- purchases[, .(total = sum(total_price_paid-coupon_value)),
                               keyby= .(household_code, year, month)]
  purchases_PL <- merge(purchases_PL, purchases_total[,.(household_code, year, month, total)])
  purchases_PL[, perc_share := spend/total]
  
  # get rid of data on non-private label shares only
  purchases_PL <- purchases_PL[is_PL == TRUE]
  
  # insert year-level data into a list
  container[[index]] = purchases_PL
  index <- index + 1
  rm(purchases_PL, purchases_total, purchases)
  
}

# get it all into one data table
PL_purchases <- rbindlist(container)
PL_purchases2 <- PL_purchases[complete.cases(PL_purchases)]
