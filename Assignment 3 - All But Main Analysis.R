### Data Preparation

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
products2 <- products[product_module_descr != "MAGNET DATA"]
products3 <- products2[is.na(department_code) == FALSE]
products4 <- products3[department_descr != "GENERAL MERCHANDISE"]
products_orig <- products
products <- products4

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
PL_purchases <- PL_purchases[complete.cases(PL_purchases)]
rm(container)

setkey(PL_purchases, household_code, year, month)




### Data Analysis

library(psych)
library(lfe)
library(ggplot2)
library(stargazer)



## Prep Household Data

load("panelists.RData")

# translate income level factors to numerics
panelists[household_income == "-$5000", income := 2500]
panelists[household_income == "$5000-$7999", income := 6500]
panelists[household_income == "$8000-$9999", income := 9000]
panelists[household_income == "$10,000-$11,999", income := 11000]
panelists[household_income == "$12,000-$14,999", income := 13500]
panelists[household_income == "$15,000-$19,999", income := 17500]
panelists[household_income == "$20,000-$24,999", income := 22500]
panelists[household_income == "$25,000-$29,999", income := 27500]
panelists[household_income == "$30,000-$34,999", income := 32500]
panelists[household_income == "$35,000-$39,999", income := 37500]
panelists[household_income == "$40,000-$44,999", income := 42500]
panelists[household_income == "$45,000-$49,999", income := 47500]
panelists[household_income == "$50,000-$59,999", income := 55000]
panelists[household_income == "$60,000-$69,999", income := 65000]
panelists[household_income == "$70,000-$99,999", income := 85000]
panelists[household_income == "$100,000 - $124,999", income := 112500]
panelists[household_income == "$125,000 - $149,999", income := 132500]
panelists[household_income == "$150,000 - $199,999", income := 175000]
panelists[household_income == "$100,000 + ", income := 112500]
panelists[household_income == "$200,000 + ", income := 250000]

# change any income above $100k to $112,500 because info only exists for a few years within
# the data (want to be consistent across all years)
panelists[income >= 100000, income := 112500]


# income is shifted by 2 years because of how Nielsen collected data -- need to associate with
# correct year

# check if panelists is keyed correctly (household_code and panel_year)
key(panelists)
# shift
panelists[, income := shift(income, n = 2, type = "lead"), by = household_code]

# show justification of choosing male head as default because more female heads unemployed vs
# male heads
table(panelists[,male_head_employment == "Not Employed for Pay"])
# TRUE     FALSE
# 145,176  462,288
table(panelists[,female_head_employment == "Not Employed for Pay"])
# TRUE     FALSE
# 234,153  373,311


# recode age, unemployed, and education variables to reflect only 1 head of household

# note where household head is female
panelists[, female_head := male_head_age == "No Such Head"]

# set head of hh age by male head except when head is female
panelists[, age := male_head_birth]
panelists[female_head == TRUE,
          age := female_head_birth]

# convert age from character string of birth date to numeric by extracting year from string 
# and subtracting it from panel year
panelists[, age := panel_year - as.numeric(substr(age, 1, 4))]

# set unemployed by male head except when head is female
panelists[, unemployed := male_head_employment == "Not Employed for Pay"]
panelists[female_head == TRUE,
          unemployed := female_head_employment == "Not Employed for Pay"]

# set education by male head except when head is female
panelists[, education := male_head_education]
panelists[female_head == TRUE,
          education := female_head_education]

# add variable size that converts household_size from word string to numeric
panelists[household_size %like% "Single", size := 1]
panelists[household_size %like% "Two", size := 2]
panelists[household_size %like% "Three", size := 3]
panelists[household_size %like% "Four", size := 4]
panelists[household_size %like% "Five", size := 5]
panelists[household_size %like% "Six", size := 6]
panelists[household_size %like% "Seven", size := 7]
panelists[household_size %like% "Eight", size := 8]
panelists[household_size %like% "Nine", size := 9]

# add binary variable has_children
panelists[,has_children := 1]
panelists[age_and_presence_of_children == "No Children Under 18", has_children := 0] 



## Merge Zillow data, panelists data, and private label data (PL_purchases)
load("Zillow-Data.RData")

# rename panelists columns "panel_year" to "year" and "panelist_zip_code" to "zip_code" for
# consistency
setnames(panelists, c("panel_year","panelist_zip_code"), c("year","zip_code"))

# first merge panelist data with private label data
# shared columns = household_code, year
# use panelists columns: income, unemployed, education, age, size, has_children, female_head,
# marital_status, race, hispanic_origin (all former to be used in regression), zip_code,
# dma_code, and projection_factor
key(PL_purchases)
setkey(panelists, household_code, year)
key(panelists)

share_DT <- merge(PL_purchases, panelists[,.(household_code, year, income, unemployed, education,
                                             age, size, has_children, female_head, marital_status,
                                             race, hispanic_origin, zip_code, dma_code, projection_factor)])
                                            # FYI, this lost nearly 10,000 rows of data

# then merge Zillow data with share_DT
# shared columns = zip_code, year, month
key(zillow_DT)
setkey(share_DT, zip_code, year, month)
key(share_DT)

# Not all zip codes have Zillow home values, so need to include all.x=TRUE so data isn't gone
share_DT <- merge(share_DT, zillow_DT, all.x=TRUE)
# FYI, this lost no rows of data (yay!)

# reset share_DT key to household, year, month
setkey(share_DT, household_code, year, month)

# scale percent shares to 0-100 (vs 0-1)
share_DT[, perc_share := perc_share*100]



## Data Description

# Graph (histogram) distribution of private label shares across households per year (mean of mos)
pl_share_dist <- share_DT[,.(shares = mean(perc_share)), by=.(household_code, year)]
summary(pl_share_dist[,shares])
sd(pl_share_dist[,shares])
ggplot(data=pl_share_dist, aes(shares)) + geom_histogram(binwidth = 1)


# Evolution of private label shares over time

# calculate weighted average of private label shares per month (across households)
share_month <- share_DT[,.(shares = weighted.mean(perc_share, projection_factor)), by=.(month, year)]

# create date variable for X-axis
share_month[, date := as.Date(ISOdate(year, month, 1))]

# plot
ggplot(data=share_month, aes(y=shares, x=date)) +
  annotate("rect", xmin = as.Date("2007-12-1"), xmax = as.Date("2009-6-1"), # highlights recession
           ymin = -Inf, ymax = Inf, fill = "lightblue1", alpha = 0.4) +
  geom_line() +
  geom_point() +
  scale_x_date("Year", date_labels = "%Y", date_breaks = "1 years", minor_breaks = NULL)
# The recession is highlighted, yet I don't see anything drastic there. I do, however, see
# a huge drop from Dec 2011 to Jan 2012. Was there some translation error? Maybe when shifting
# the income level or something? Except the income was shifting by 2 years, and this is a 3-year
# shift (if that's what it is at all).

# Garaudy: I think that there's a delay to when people react to the recession. So even though the official
# recession ended in 2009, main street was still hurting and people take time to go back to old habits.
# 2012 drop may coincide with Obama reelection and average people's incomes finally return to normal?


# Change in home values

# calculate percentage change in zillow_index at zip code level
indexna <- (share_DT[is.na(zillow_index)==TRUE])
nrow(indexna)/nrow(share_DT)
# 30% of zip codes have no zillow_index. I guess he told us this above, but that's a lot!
# Oh, well, just ignore for this.

home_value_zip <- share_DT[,.(index = mean(zillow_index)), by=.(zip_code, year, month)]

setkey(home_value_zip, zip_code, year, month)
home_value_zip[, thirtysix_mo_lag := shift(index, n=36, type="lag"), by=zip_code]
home_value_zip[, value_change := (index-thirtysix_mo_lag)/thirtysix_mo_lag]

ggplot(data=home_value_zip[year==2009 & month==6], aes(value_change)) + 
  geom_histogram(binwidth = .01)

# checking this against zillow data
home_value_zip2 <- zillow_DT[,zillow_index, by=.(zip_code, year, month)]
setkey(home_value_zip2, zip_code, year, month)
home_value_zip2[, thirtysix_mo_lag := shift(zillow_index, n=36, type="lag"), by=zip_code]
home_value_zip2[, value_change := (zillow_index-thirtysix_mo_lag)/thirtysix_mo_lag]

ggplot(data=home_value_zip2[year==2009 & month==6], aes(value_change)) + 
  geom_histogram(binwidth = .01)
# seems similar enough!



## Main Analysis
