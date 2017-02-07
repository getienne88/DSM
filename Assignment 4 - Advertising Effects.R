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
             value.var = c("quantity", "price", "promotion","adstock"))
setkey(move, dma_code, store_code_uc, week_end)
move_cast <- merge(move_cast, move[,.(dma_code, store_code_uc, week_end, grp)])

head(move)

# remove incomplete cases
move_cast <- move_cast[complete.cases(move_cast)]

# create time trend
move_cast[, `:=` (year = year(week_end), month = month(week_end))]
move_cast[, month_index := 12*(year - min(year)) + month]


### Data Inspection

## Time series of advertising levels

# DMA chosen: New York City -- dma_code = 501
ggplot(data=move[dma_code==501], aes(x=week_end, y=grp, color=brand_name)) +
  geom_area()

## Overall advertising variation

# Create normalized GRP at DMA level
move_cast[, normalized_grp := 100*grp/mean(grp)]