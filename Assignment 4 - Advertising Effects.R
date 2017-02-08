---
title: "Assignment 4 - Advertising Effects"
author: "Team Revolution (Beslic, Etienne, Riso, Stonehill)"
date: "2/9/2017"
output:
  html_document: default
  word_document: default
---

```{r global_options, include=FALSE}
knitr::opts_chunk$set(warning=FALSE)
```

```{r, include = FALSE}


setwd("C:/Users/Garau/OneDrive/BOOTH/Academic_Courses/37105_Data_Science_Mktg/Assignment_4")


library(bit64) 
library(data.table) 
library(RcppRoll) 
library(ggplot2) 
library(lfe) 
library(stargazer)
```

<br>
**Coca-Cola Data Inspection **

```{r include=FALSE}
#selected_module = 7260
selected_module = 1484
#selected_brand = 526996
selected_brand = 531429
model_name = "Toilet-Tissue-Charmin"
#model_name = "Coca-Cola"

load("./Brands.RData")
load("./Stores-DMA.Rdata")
load("./RMS/1484.RData")
load("./AdIntel/1484.RData")


# RMS Scanner data (move)
setnames(move,c("units", "promo_percentage"), c("quantity", "promotion"))[]
move <- move[,brand_name := "own"]
move <- move[brand_code_uc != selected_brand, brand_name := "comp"]


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

# define adstock parameters
N_lags <- 52 #52 weeks
delta <- 0.9 # weekly residual effect (carryover factor)

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
move_cast <- dcast(move, dma_code + store_code_uc + week_end ~ brand_name,
                   value.var = c("quantity", "price", "promotion","grp", "adstock"))

#tail(move_cast)

# remove incomplete cases
move_cast <- move_cast[complete.cases(move_cast)]

# create time trend
move_cast[, `:=` (year = year(week_end), month = month(week_end))]
move_cast[, month_index := 12*(year - min(year)) + month]
```

```{r echo=FALSE}

## Time series of advertising levels

# Own
ggplot(data=move_cast[dma_code==501], aes(x=week_end, y=grp_own)) +
  geom_line(aes(group=1), colour="magenta2") +  # lines
  ggtitle("New York (DMA 501) Own GRP")

# Competitor
ggplot(data=move_cast[dma_code==501], aes(x=week_end, y=grp_comp)) +
  geom_line(aes(group=1), colour = "navyblue") + 
  ggtitle("New York (DMA 501) Competitors GRP")

# ChiTown
#ggplot(data=move_cast[dma_code==602], aes(x=week_end, y=grp_own), color = "red") +
 # geom_line()

## Overall advertising variation

# Create normalized GRP at DMA level
move_cast <- move_cast[, normalized_grp := 100*grp_own/mean(grp_own), by = dma_code]
move_cast <- move_cast[, comp_normalized_grp := 100*grp_comp/mean(grp_comp), by = dma_code]

# Own Normalized grp
ggplot(data=move_cast, aes(x=normalized_grp, color = "red")) +
  geom_histogram(binwidth = 45, color = "navyblue", fill = "magenta2") + scale_x_continuous(limit=c(-50,975)) + ggtitle("Normalized Own GRP")

# Comp Normalized grp
ggplot(data=move_cast, aes(x=comp_normalized_grp, color = "red")) +
  geom_histogram(binwidth = 45, color = "navyblue", fill = "lightblue2") + scale_x_continuous(limit=c(-50,500)) + ggtitle("Normalized Competitors GRP")
```

** **
**Advertising Effect Estimation**

```{r echo=FALSE}  
# Plot regression specification
fit_base <- felm(log(1+quantity_own) ~ log(price_own) + log(price_comp) 
                 + promotion_own + promotion_comp | store_code_uc + month_index, data = move_cast)

fit_adstock <- felm(log(1+quantity_own) ~ log(price_own) + log(price_comp) 
                 + promotion_own + promotion_comp + adstock_own + adstock_comp 
                 | store_code_uc + month_index, data = move_cast)

fit_timeless <- felm(log(1+quantity_own) ~ log(price_own) + log(price_comp) 
                  + promotion_own + promotion_comp + adstock_own + adstock_comp 
                  | store_code_uc, data = move_cast)

#Table 1
stargazer(fit_base, fit_adstock, fit_timeless,
          title = "Advertising Effect Estimation",
          type = "text",
          column.labels = c("Base Model", "Adstock", "No Time Fixed Effects"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE)
```

Increase in promotion by 0.1 = increase in sales quantity by 3.6%. Time Fixed effects allows you to discern seasonal trends that may be correlated with advertising. If you don't control for time, it might look like your ads don't work even though you may just be running ads at times that have low sales.


** **
**Border Strategy**

```{r echo=FALSE}
#stores[, border_name := as.factor(border_name)]

#move_cast <- merge(move_cast, stores[on_border == TRUE, .(store_code_uc, border_name)],
             #allow.cartesian = TRUE)
#move_cast[, month_index := as.factor(month_index)]

#fit_border <- felm(log(1+quantity_own) ~ log(price_own) + log(price_comp) 
#                    + promotion_own + promotion_comp + adstock_own + adstock_comp 
 #                   |  border_name:month_index + border_name + store_code_uc + month_index, data =move_cast)
```
