library(bit64)
library(data.table)
library(RcppRoll)
library(ggplot2)
library(lfe)
library(stargazer)

load("Brands.RData")
load("Stores-DMA.RData")
load("1484.RData")
load("1484-RMS.RData")

selected_module = 1484
selected_brand = 531429
model_name = "Soft-Drinks-Carbonated-Coca-Cola-R"


### Data Prep

## RMS Scanner Data (move)

# rename units to quantity and promo_percentage to promotion
setnames(move, c("units", "promo_percentage"), c("quantity", "promotion"))

# create variable brand name
move[,brand_name := "comp"]
move[brand_code_uc == 531429, brand_name := "own"]

# aggregate data by store/week--take mean of price and promotion, 
# sum over brand-level quantities
move <- move[, .(price = mean(price), promotion = mean(promotion), 
                     quantity = sum(quantity)), by=.(week_end, store_code_uc, brand_name)]

# merge dma_code by store onto RMS data
setkey(move, store_code_uc)
stores_dma <- unique(stores[,.(store_code_uc, dma_code)])
setkey(stores_dma, store_code_uc)
move <- merge(move, stores_dma)


## Ad Intel advertising data (adv_DT)

# Fill in missing gaps where no advertising data for particular brands during particular 
# weeks (i.e. set to 0 occurences)
brands <- unique(adv_DT$brand_code_uc)
dma_codes <- unique(adv_DT$dma_code)
weeks <- seq(from = min(adv_DT$week_start), to = max(adv_DT$week_start), by = "week")

setkey(adv_DT, brand_code_uc, dma_code, week_start)
adv_DT <- adv_DT[CJ(brands, dma_codes, weeks)]
adv_DT[is.na(adv_DT)] = 0

# create own and competitor brand names 
adv_DT[,brand_name := "comp"]
adv_DT[brand_code_uc == 531429, brand_name := "own"]

# aggregate at week/dma level -- sum local and national GRPs
adv_DT <- adv_DT[, .(national_grp = sum(national_grp), local_grp = sum(local_grp)),
                  by=.(week_start, dma_code, brand_name)]

# sum local and national GRPs across
adv_DT[, grp := sum(national_grp, local_grp), by=.(week_start, dma_code, brand_name)]


## Calculate adstock/goodwill

# I don't know what this is doing...
# define adstock parameters
N_lags <- 52
delta <- 0.9

# caculate geometric weights based on carry-over factor (delta)
geom_weights <- cumprod(c(1.0, rep(delta, times = N_lags)))
geom_weights <- sort(geom_weights)

# calculate adstock
setkey(adv_DT, brand_name, dma_code, week_start)
adv_DT[, adstock := roll_sum(log(1+grp), n = N_lags+1, weights = geom_weights,
                             normalize = FALSE, align = "right", fill = NA),
       by = .(brand_name, dma_code)]

# this is in relation to his "video game" where effect of advertising slowly trails off


## Merge scanner and ad data

# make dates line up (RMS is week_end, adv is week_start)
adv_DT[, week_end := week_start + 5]

setkey(adv_DT, brand_name, dma_code, week_end)
setkey(move, brand_name, dma_code, week_end)

move <- merge(move, adv_DT[,.(brand_name, dma_code, week_end, grp, adstock)])

# reshape the data
#### THIS IS WHERE I'M NOT SURE HOW TO INCORPORATE THE GRP
#### In the instructions we have to visualize the grp over time and normalize it
#### When data is recast, each row is a store/week, so either needs to be grp column
#### both competitor and for own brand or something else I don't know help
move_cast <- dcast(move, dma_code + store_code_uc + week_end ~ brand_name,
             value.var = c("quantity", "price", "promotion","adstock", "grp"))

head(move_cast)

# remove incomplete cases
move_cast <- move_cast[complete.cases(move_cast)]

# create time trend
move_cast[, `:=` (year = year(week_end), month = month(week_end))]
move_cast[, month_index := 12*(year - min(year)) + month]


### Data Inspection

## Time series of advertising levels

# DMA chosen: New York City -- dma_code = 501
ggplot(data=move[dma_code==501], aes(x=week_end, y=grp, color=brand_name)) +
  geom_line() + facet_wrap(~brand_name, scales="free_y")

# DMA chosen: Chicago -- dma_code = 601
ggplot(data=move[dma_code==602], aes(x=week_end, y=grp, color=brand_name)) +
  geom_line() + facet_wrap(~brand_name, scales="free_y")



## Overall advertising variation

# Create normalized GRP at DMA level
move_cast[, normalized_grp := 100*grp_own/mean(grp_own), by=dma_code]

# plot histogram of GRPs at DMA level
ggplot(data=move_cast, aes(x=normalized_grp)) +
  geom_histogram(binwidth=45, fill="darkgreen") + 
  scale_x_continuous(limit=c(-50,1200))



## Advertising effect estimation

# estimate base model of log(quantity) against price and promotion of own and competitors
fit_base <- felm(log(1+quantity_own) ~ log(price_own) + log(price_comp) + promotion_own +
                   promotion_comp | month_index + store_code_uc, data=move_cast)

stargazer(fit_base, 
          title = "Model Estimates",
          type = "text",
          column.labels = "Base",
          dep.var.labels.include = FALSE,
          model.numbers = FALSE)

# add adstock
fit_adstock <- felm(log(1+quantity_own) ~ log(price_own) + log(price_comp) + promotion_own +
                   promotion_comp + adstock_own + adstock_comp | month_index + 
                     store_code_uc, data=move_cast)

stargazer(fit_base, fit_adstock, 
          title = "Model Estimates",
          type = "text",
          column.labels = c("Base", "Adstock"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE)

# remove time
fit_timeless <- felm(log(1+quantity_own) ~ log(price_own) + log(price_comp) + promotion_own +
                      promotion_comp + adstock_own + adstock_comp | store_code_uc, 
                     data=move_cast)

stargazer(fit_base, fit_adstock, fit_timeless, 
          title = "Model Estimates",
          type = "text",
          column.labels = c("Base", "Adstock", "No Time"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE)

# how to interpret x on log y
# If you have a 10% discount on the product, you have a 3.64% increase in quantity bought

# adstock vs time
# If you don't control for time, might look like running ads works against you because
# you happen to be running ads in times that have low sales.
# By controlling for time, you take that out.




### Border Strategy


## merge border names

# change border_name to factor (from character)
stores[, border_name := as.factor(border_name)]

# merge move and border_name
move_cast <- merge(move_cast, stores[on_border==TRUE, .(store_code_uc, border_name)], 
                   allow.cartesian = TRUE)


## run regression to see how being on border affects quantity

# Advertising model with store fixed effects and border/time interactions
fit_border <- felm(log(1+quantity_own) ~ log(price_own) + log(price_comp) + promotion_own +
                      promotion_comp + adstock_own + adstock_comp | month_index + 
                      store_code_uc + as.factor(month_index):border_name + border_name, 
                   data=move_cast)

stargazer(fit_adstock, fit_border, 
          title = "Model Estimates",
          type = "text",
          column.labels = c("Adstock", "Border"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE)

# advertising works better than we'd think when we look at borders. Setting up borders
# is essentially creating the perfect experiment where everything is the same btwn
# populations except what advertising is being shown to them.

# standard error clusters
# Advertising model with store fixed effects and border/time interactions
fit_cluster <- felm(log(1+quantity_own) ~ log(price_own) + log(price_comp) + promotion_own +
                     promotion_comp + adstock_own + adstock_comp | month_index + 
                     store_code_uc + as.factor(month_index):border_name + border_name
                   |0| dma_code, data=move_cast)

stargazer(fit_adstock, fit_border, fit_cluster, 
          title = "Model Estimates",
          type = "text",
          column.labels = c("Adstock", "Border", "SE Cluster"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE)

# I didn't run this yet because ain't nobody got time for that