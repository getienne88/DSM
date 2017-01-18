setwd("C:/Users/Garau/OneDrive/BOOTH/Academic_Courses/37105_Data_Science_Mktg/Assignment_1")

library(bit64)
library(data.table)
library(ggplot2)

load(paste0("purchases_beverages_50_subsample.RData"))
load(paste0("products_beverages.RData"))


# sample data from all purchases
purchases_sub <- purchases[sample(.N, 4000000)]

# grab no. of households and sample 25% of them, then take all data from those households
# (b/c likely multiple purchases made per household)
N_households <- length(unique(purchases$household_code))
N_subsample <- round(.25*N_households)
household_code_sub <- sample(unique(purchases$household_code), N_subsample)
purchases_sub_hh <- purchases[household_code %in% household_code_sub]

# saving space because not using the samples
rm(list=c("purchases_sub", "purchases_sub_hh"))



## on to the real stuff, I guess

# calculate year corresponding to a purchase date
# not really sure what this does exactly
purchases[, year := year(purchase_date)]
# looks like := is the same as <-, so purchases[, year <- year(purchase_date)]
# converts the years as integers? I think so

# remove rows with year 2003
purchases <- purchases[year > 2003]

# identify soft drinks, diet soft drinks, and water (and other) as categories in new column
products[, category := "Other"]

products[product_module_code == 1484, category := "CSD"]
products[product_module_code == 1553, category := "Diet"]
products[product_module_code == 1487, category := "Water"]

table(products[,category])
# CSD     Diet    Water   Other
# 24,126  10,267  15,921  58,754

# merge category, size, unit type, and whether part of multipack with purchase data
purchases <- merge(purchases, products[, .(upc, upc_ver_uc, category)])
purchases <- merge(purchases, products[, .(upc, upc_ver_uc, size1_amount, size1_units, multi)])

table(purchases[,size1_units])
# CT       OZ          QT
# 234,989  19,268,697  855,140

# get rid of CT data
purchases <- purchases[size1_units != "CT"]

# convert all to gallons
purchases[, volume := 0]
purchases[size1_units == "OZ", volume := (size1_amount/128)*multi]
purchases[size1_units == "QT", volume := (size1_amount/4)*multi]

# number of households in the data
purchases[, no_households := length(unique(household_code)), by = year]

households_per_year <- aggregate(purchases[,no_households], by=list(year=purchases[,year]), FUN="mean")
households_per_year
# 2004 21,183
# 2005 19,432
# 2006 21,057
# 2007 32,716
# 2008 32,275
# 2009 32,133
# 2010 32,729
# 2011 30,785
# 2012 30,855
# 2013 31,420
# 2014 30,517


## Category level analysis

# Create total volume spend and total purchase volume for year year/category combination
purchases_category <- purchases[, .(spend = sum(total_price_paid-coupon_value),
                                    purchase_volume = sum(volume*multi), 
                                    no_households = head(no_households, 1)), 
                                keyby = .(category, year)]

# purchases_category1 <- purchases[, .(spend = sum(total_price_paid-coupon_value),
#                                     purchase_volume = sum(volume*multi), 
#                                     no_households = mode(no_households)), 
#                                 keyby = .(category, year)]

# purchases_category[1:20]

# calculate per capita spend and purchase volume(gal) for each category
purchases_category[, per_cap_spend := spend/no_households]
purchases_category[, per_cap_vol := purchase_volume/no_households]

# plot per capital spend and purchase volume
ggplot(purchases_category, aes(x=year, y=per_cap_vol, color=category)) + geom_line() + 
  facet_wrap(~category)
ggplot(purchases_category, aes(x=year, y=per_cap_spend, color=category)) + geom_line() + 
  facet_wrap(~category)

# normalize each category per cap spend and vol against 2004 per cap spend
purchases_category[purchases_category[year==2004,], `:=` 
                   (norm_per_cap_vol=per_cap_vol/i.per_cap_vol, 
                     norm_per_cap_spend=per_cap_spend/i.per_cap_spend), 
                   on="category"]
# graph it
ggplot(purchases_category, aes(x=year, y=norm_per_cap_spend, color=category)) + geom_line() + 
  facet_wrap(~category)

ggplot(purchases_category, aes(x=year, y=norm_per_cap_vol, color=category)) + geom_line() + 
  facet_wrap(~category)



## Brand-level analysis

# merge brand_descr with purchase data
purchases <- merge(purchases, products[, .(upc, upc_ver_uc, brand_descr)])

# calculate total dollar spend by each category/brand description 
brand_summary <- purchases[, .(spend = sum(total_price_paid-coupon_value)),
                           by = .(category, brand_descr)]

# rank brands by total dollar spend in each category separately
brand_summary[, rank := frankv(spend, order = -1), by = category]

#CSD_brand_summary <- brand_summary[category == "CSD"]
#CSD_brand_summary1 <- CSD_brand_summary[order(rank)]

# merge brand ranking into purchases
# need to set key by brand description AND category because there are blank brand descrs
# in Water and Diet
setkey(purchases,brand_descr, category)
setkey(brand_summary, brand_descr, category)

purchases <- merge(purchases, brand_summary[,.(brand_descr, category, rank)])


# Create total volume spend and total purchase volume for year year/brand combination
purchases_brand <- purchases[rank<=4, .(spend = sum(total_price_paid-coupon_value),
                                        purchase_volume = sum(volume*multi), 
                                        no_households = head(no_households, 1)), 
                             keyby = .(brand_descr, category, year)]

# calculate per capital spend and purchase volume(gal) for each brand
purchases_brand[, per_cap_spend := spend/no_households]
purchases_brand[, per_cap_vol := purchase_volume/no_households]

baller_alert <- purchases_brand[order(-per_cap_spend)]

# plot per capita purchase volume
ggplot(purchases_brand[category=="CSD"], aes(x=year, y=per_cap_vol, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr)
ggplot(purchases_brand[category=="Diet"], aes(x=year, y=per_cap_vol, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr)
ggplot(purchases_brand[category=="Water"], aes(x=year, y=per_cap_vol, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr, scales="free_y")
ggplot(purchases_brand[category=="Other"], aes(x=year, y=per_cap_vol, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr)

# normalize each brand per cap spend and vol against 2004 per cap spend
purchases_brand[purchases_brand[year==2004,], `:=` 
                (norm_per_cap_vol=per_cap_vol/i.per_cap_vol, 
                  norm_per_cap_spend=per_cap_spend/i.per_cap_spend), 
                on=c("brand_descr","category")]

# graph it
ggplot(purchases_brand[category=="CSD"], aes(x=year, y=norm_per_cap_spend, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr)
ggplot(purchases_brand[category=="Diet"], aes(x=year, y=norm_per_cap_spend, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr)
ggplot(purchases_brand[category=="Water"], aes(x=year, y=norm_per_cap_spend, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr, scales="free_y")
ggplot(purchases_brand[category=="Other"], aes(x=year, y=norm_per_cap_spend, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr)

# graph it
ggplot(purchases_brand[category=="CSD"], aes(x=year, y=norm_per_cap_vol, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr)
ggplot(purchases_brand[category=="Diet"], aes(x=year, y=norm_per_cap_vol, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr)
ggplot(purchases_brand[category=="Water"], aes(x=year, y=norm_per_cap_vol, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr, scales="free_y")
ggplot(purchases_brand[category=="Other"], aes(x=year, y=norm_per_cap_vol, color=brand_descr)) + 
  geom_point() + geom_smooth() + facet_wrap(~brand_descr)
