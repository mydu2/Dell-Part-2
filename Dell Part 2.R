library(readxl)
library(ggplot2)
library(reshape)
library(dplyr)
library(tidyverse)
library(fastDummies)
# function used for exploration of columns

density <- function(data, var) {
  ggplot(data, aes(var)) + geom_density(na.rm = TRUE)
}

box <- function(data, var) { 
  ggplot(data, aes(var)) + geom_boxplot(na.rm = TRUE) 
}

assess <- function(data) { 
  library(car)
  vif <- vif(data)
  plot <- plot(data)
  return(vif)
  return(plot)
}
# read in in data

Raw <- read_excel("Regression_Final_Raw.xlsx", sheet = "Raw-Data")
Calendar <- read_excel("Regression_Final_Raw.xlsx", sheet = "Calendar")


# add lead time and Man. Time column 

Raw <- Raw %>%
  mutate(`Lead Time` = Raw$`Receipt Date` - Raw$`Ship Date`)

Raw <- Raw %>%
  mutate(Man.Time = Raw$`Ship Date` - Raw$`PO Download Date`)

Raw$`Lead Time` <- as.numeric(Raw$`Lead Time`)
Raw$Man.Time <- as.numeric(Raw$Man.Time)

# check for NA's 

Columns <- names(Raw)
builder <- data.frame(Columns)
builder$NAs <- sapply(Raw, function(x) length(which(is.na(x))))
builder
#          Columns NAs
#1              LOB   0
#2           Origin   0
#3        Ship Mode   0
#4 PO Download Date   0
#5        Ship Date 150
#6     Receipt Date 150
#7        Lead Time 150
#8         Man.Time 150

# Lead Time, Man.Time and Date columns needs cleaning 
# but first Check other categorical columns for bad entries 

unique(Raw$LOB)
unique(Raw$Origin)
unique(Raw$`Ship Mode`)

# there are no typos in these variables so next check to see if 
# data contains any implausible entries. For example since Site B, 
# C and A are over seas they should not have a ship mode of Ground

nrow(Raw[Raw$`Ship Mode` == "Ground" & Raw$Origin == "Site A", ])
Raw[Raw$`Ship Mode` == "Ground" & Raw$Origin == "Site C", ]
Raw[Raw$`Ship Mode` == "Ground" & Raw$Origin == "Site B", ]
Raw[Raw$`Ship Mode` == "OCEAN" & Raw$Origin == "Site D", ]
Raw[Raw$`Ship Mode` == "FASTBOAT" & Raw$Origin == "Site D", ]

Raw$`Ship Mode` <- as.factor(Raw$`Ship Mode`)
Raw$Origin <- as.factor(Raw$Origin)
Raw$LOB <- as.factor(Raw$LOB)

# prop tables
round(prop.table(table(Raw$`Ship Mode`)),2)
#AIR   FASTBOAT   GROUND    OCEAN 
#0.42     0.06     0.27     0.26 
round(prop.table(table(Raw$Origin)),2)
#Site A Site B Site C Site D 
#0.38   0.17   0.18   0.27 
round(prop.table(table(Raw$LOB)),2)
#Product A Product B Product C 
#0.32      0.65      0.03

# The categorical columns look good 
# so we can focus on cleaning the Lead Time and Date columns

summary(Raw$`Lead Time`/86400)
sd(Raw$`Lead Time`, na.rm = T)/86400

#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -235.00    5.00    8.00   11.93   23.00   86.00     150 
# sd = 22.35

summary(Raw$Man.Time/86400)
sd(Raw$Man.Time, na.rm = T)/86400

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-28.000   4.000   6.000   9.262  10.000 237.000     150 
# sd = 18.92

# look at boxplot 


box(Raw, Raw$`Lead Time`/86400) 
box(Raw, Raw$Man.Time/86400)

# negatives need to be removed for both
# now look at high numbers 
Investigate <- Raw[Raw$`Lead Time` > 6480000, ]
# come from same predictors so can leave as is

Investigate <- Raw[Raw$Man.Time > 8640000, ]

# We can replace this with the mean

#Na <- Raw[is.na(Raw$Man.Time), ]
a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND" 
         , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND" , "Man.Time"] 
a <- a[a >= 0  , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT"  & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR"  & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site B" &
           Raw$`Ship Mode` == "OCEAN", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site B" &
      Raw$`Ship Mode` == "OCEAN"  & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site C" &
           Raw$`Ship Mode` == "AIR" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site C" &
      Raw$`Ship Mode` == "AIR"  & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site C" &
           Raw$`Ship Mode` == "AIR" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site C" &
      Raw$`Ship Mode` == "AIR" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN"  & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site C" &
           Raw$`Ship Mode` == "AIR" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site C" &
      Raw$`Ship Mode` == "AIR"  & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR" , "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site B" &
           Raw$`Ship Mode` == "AIR", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site B" &
      Raw$`Ship Mode` == "AIR" & is.na(Raw$`Man.Time`), "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Man.Time`), ]

# address negatives

negatives <- Raw[Raw$Man.Time < 0, ]
a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & Raw$Man.Time < 0 , "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
negatives <- Raw[Raw$Man.Time < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site B" &
           Raw$`Ship Mode` == "OCEAN", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site B" &
      Raw$`Ship Mode` == "OCEAN" & Raw$Man.Time < 0 , "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
negatives <- Raw[Raw$Man.Time < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & Raw$Man.Time < 0 , "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
negatives <- Raw[Raw$Man.Time < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site C" &
           Raw$`Ship Mode` == "AIR", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site C" &
      Raw$`Ship Mode` == "AIR" & Raw$Man.Time < 0 , "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
negatives <- Raw[Raw$Man.Time < 0, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & Raw$Man.Time < 0 , "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
negatives <- Raw[Raw$Man.Time < 0, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & Raw$Man.Time < 0 , "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
negatives <- Raw[Raw$Man.Time < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & Raw$Man.Time < 0 , "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
negatives <- Raw[Raw$Man.Time < 0, ]

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & Raw$Man.Time < 0 , "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
negatives <- Raw[Raw$Man.Time < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site B" &
           Raw$`Ship Mode` == "AIR", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site B" &
      Raw$`Ship Mode` == "AIR" & Raw$Man.Time < 0 , "Man.Time"] <- mean(a$`Man.Time`, na.rm = TRUE)
negatives <- Raw[Raw$Man.Time < 0, ]

# Address high outliers in Man Time 

toohigh <- Raw[Raw$Man.Time > 8640000, ]
a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & Raw$Man.Time > 8640000 , "Man.Time"] <- mean(a$`Man.Time`)
toohigh <- Raw[Raw$Man.Time > 8640000, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site B" &
           Raw$`Ship Mode` == "OCEAN", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site B" &
      Raw$`Ship Mode` == "OCEAN" & Raw$Man.Time > 8640000 , "Man.Time"] <- mean(a$`Man.Time`)
toohigh <- Raw[Raw$Man.Time > 8640000, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site C" &
           Raw$`Ship Mode` == "AIR", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site C" &
      Raw$`Ship Mode` == "AIR" & Raw$Man.Time > 8640000 , "Man.Time"] <- mean(a$`Man.Time`)
toohigh <- Raw[Raw$Man.Time > 8640000, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & Raw$Man.Time > 8640000 , "Man.Time"] <- mean(a$`Man.Time`)
toohigh <- Raw[Raw$Man.Time > 8640000, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & Raw$Man.Time > 8640000 , "Man.Time"] <- mean(a$`Man.Time`)
toohigh <- Raw[Raw$Man.Time > 8640000, ]

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & Raw$Man.Time > 8640000 , "Man.Time"] <- mean(a$`Man.Time`)
toohigh <- Raw[Raw$Man.Time > 8640000, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & Raw$Man.Time > 8640000 , "Man.Time"] <- mean(a$`Man.Time`)
toohigh <- Raw[Raw$Man.Time > 8640000, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN", "Man.Time"] 
a <- a[a >= 0 & a < 8640000 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & Raw$Man.Time > 8640000 , "Man.Time"] <- mean(a$`Man.Time`)
toohigh <- Raw[Raw$Man.Time > 8640000, ]

# all Na, negatives, and unreasonably high man times are removed so we can move on to Lead Time

Na <- Raw[is.na(Raw$`Lead Time`), ]
a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND" 
         , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT"  & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR"  & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site B" &
           Raw$`Ship Mode` == "OCEAN", "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site B" &
      Raw$`Ship Mode` == "OCEAN"  & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site C" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site C" &
      Raw$`Ship Mode` == "AIR"  & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site C" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site C" &
      Raw$`Ship Mode` == "AIR" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT", "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR", "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN"  & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site C" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site C" &
      Raw$`Ship Mode` == "AIR"  & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ] 

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site B" &
           Raw$`Ship Mode` == "AIR", "Lead Time"] 
a <- a[a > 0 , ] 
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site B" &
      Raw$`Ship Mode` == "AIR" & is.na(Raw$`Lead Time`), "Lead Time"] <- mean(a$`Lead Time`, na.rm = TRUE)
Na <- Raw[is.na(Raw$`Lead Time`), ]

Columns <- names(Raw)
builder <- data.frame(Columns)
builder$NAs <- sapply(Raw, function(x) length(which(is.na(x))))
builder
#          Columns NAs
#1              LOB   0
#2           Origin   0
#3        Ship Mode   0
#4 PO Download Date   0
#5        Ship Date 150
#6     Receipt Date 150
#7        Man.Time    0
#9        Lead Time   0

# No more Na's for Lead Time now work on negatives 

negatives <- Raw[Raw$`Lead Time` < 0, ]
a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site B" &
           Raw$`Ship Mode` == "OCEAN", "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site B" &
      Raw$`Ship Mode` == "OCEAN" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site C" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site C" &
      Raw$`Ship Mode` == "AIR" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND", "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT", "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site C" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site C" &
      Raw$`Ship Mode` == "AIR" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND", "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT", "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT", "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN", "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND" , "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN", "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT" , "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN" , "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site C" &
           Raw$`Ship Mode` == "AIR" , "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site C" &
      Raw$`Ship Mode` == "AIR" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN" , "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product A" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "OCEAN" , "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "OCEAN" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

a <- Raw[Raw$LOB == "Product C" & 
           Raw$Origin == "Site A" &
           Raw$`Ship Mode` == "FASTBOAT", "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product C" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "FASTBOAT" & Raw$`Lead Time` < 0, "Lead Time"] <- mean(a$`Lead Time`)
negatives <- Raw[Raw$`Lead Time` < 0, ]

negatives
# address two 0 rows 

Raw[Raw$`Lead Time` == 0 , ]
a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND", "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product A" & 
      Raw$Origin == "Site A" &
      Raw$`Ship Mode` == "AIR" & Raw$`Lead Time` == 0, "Lead Time"] <- mean(a$`Lead Time`)

a <- Raw[Raw$LOB == "Product B" & 
           Raw$Origin == "Site D" &
           Raw$`Ship Mode` == "GROUND", "Lead Time"] 
a <- a[a > 0, ]
Raw[Raw$LOB == "Product B" & 
      Raw$Origin == "Site D" &
      Raw$`Ship Mode` == "GROUND" & Raw$`Lead Time` == 0, "Lead Time"] <- mean(a$`Lead Time`)
Raw[Raw$`Lead Time` == 0 , ]
# 0 

# Next we have to address the NA's In the ship date and receipt date column

Raw <- Raw %>%
  mutate("New Ship Date" = Raw$`PO Download Date` + Raw$Man.Time) 

Raw <- Raw %>%
  mutate("New Receipt Date" = Raw$`New Ship Date` + Raw$`Lead Time`)

Raw$`New Ship Date` <- trunc(Raw$`New Ship Date`, "day")

Raw$`New Receipt Date` <- trunc(Raw$`New Receipt Date`, "day")

Raw <- Raw[ , -(5:6)]

Columns <- names(Raw)
builder <- data.frame(Columns)
builder$NAs <- sapply(Raw, function(x) length(which(is.na(x))))
builder

#           Columns NAs
#1              LOB   0
#2           Origin   0
#3        Ship Mode   0
#4 PO Download Date   0
#5        Lead Time   0
#6         Man.Time   0
#7    New Ship Date   0
#8 New Reciept Date   0

# add quarter and year columns and get rid of Man.Time and Date columns

Raw$Quarter <- NA
Raw$Year <- NA

i <- 1
while( i <= nrow(Calendar)) {
  Raw$Quarter <-  ifelse(Raw$`New Receipt Date` >= Calendar$Start_Date[i] & 
                           Raw$`New Receipt Date` <= Calendar$End_date[i], 
                         Calendar$Quarter[i], Raw$Quarter) 
  Raw$Year  <- ifelse(Raw$`New Receipt Date` >= Calendar$Start_Date[i] & 
                        Raw$`New Receipt Date` <= Calendar$End_date[i], 
                      Calendar$Year[i], Raw$Year) 
  i <- i + 1
}

Raw$Quarter <- as.factor(Raw$Quarter)
round(prop.table(table(Raw$Quarter)),2)
#Q1   Q2   Q3   Q4 
#0.43 0.40 0.17 0.00 

Columns <- names(Raw)
builder <- data.frame(Columns)
builder$NAs <- sapply(Raw, function(x) length(which(is.na(x))))
builder

#            Columns NAs
#1               LOB   0
#2            Origin   0
#3         Ship Mode   0
#4  PO Download Date   0
#5         Lead Time   0
#6          Man.Time   0
#7     New Ship Date   0
#8  New Receipt Date   0
#9           Quarter   0
#10             Year   0

# recheck boxplots and summary stats

summary(Raw$`Lead Time`/86400)
sd(Raw$`Lead Time`)/86400

#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#     1.00    5.00    8.00   13.86   23.00   86.00 
# sd = 12.32

summary(Raw$Man.Time/86400)
sd(Raw$Man.Time)/86400

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
#0.000   4.000   6.000   8.011  10.000  51.000 

# sd = 5.85

# look at boxplot 


box(Raw, Raw$`Lead Time`/86400) 
box(Raw, Raw$Man.Time/86400)

# remove all columns except Lead Time, Man.Time, Ship Mode, LOB, Orgin 
# Quarter and Year


Raw <- Raw[ , -c(4,7,8)]



# all good now convert  Lead Time to days

Raw$`Lead Time` <- Raw$`Lead Time`/86400
Raw$Man.Time <- Raw$Man.Time/86400

# now check skewness of Lead Time


density(Raw, Raw$`Lead Time`)

# It appears to be skewed right but Lead Time is based off of 
# different categorical variables so lets explore
# the distribution of lead time with in those variables

# lets run bivariate analyisis to explore if it needs transformation

ggplot(Raw, aes(`Lead Time`, `Ship Mode`)) + geom_boxplot()
ggplot(Raw, aes(`Lead Time`, Origin)) + geom_boxplot()
ggplot(Raw, aes(`Lead Time`, LOB)) + geom_boxplot()

# normal within ship modes should be good no transformation 


# dummy one-encode categorical variables for correlations

Raw <- dummy_cols(Raw, select_columns = c("LOB", "Origin", 
                                          "Ship Mode", "Quarter", "Year"))



# build correlation table with predictors and Lead time 
Cor_data <- Raw[ , c(4, 8:24)]
Columns <- names(Cor_data)
builder <- data.frame(Columns)
builder$Correlation <- sapply(Cor_data, function(x) cor(x, Raw$`Lead Time`))

builder <- builder %>%
  arrange(desc(abs(Correlation)))

# find correlations between predictors and build correlation plot

cor_matrix  <- round(cor(Cor_data),2)   


ut <- function(cor_matrix){ 
  cor_matrix[lower.tri(cor_matrix)] <- NA
  return(cor_matrix) }

lt <- ut(cor_matrix)
mc <- melt(lt, na.rm = TRUE)

# The correlation matrix

cor_matrix

# the correlation plot 
ggplot(data = mc, aes(X2, X1, fill = value)) + geom_tile(color = "white") +
  theme(axis.text.x = element_text(angle = 90))


# find correlations between predictors greater than .6 (absolute)
cor_matrix  <- round(cor(Cor_data),4)
cor_matrix <- as.data.frame(cor_matrix)

cor_matrix <- gather(cor_matrix, varX, Cor, `Lead Time`:Year_2020)


cor_matrix <- cor_matrix %>%
  mutate( varY = rep(Columns, 18)) 

Cor_significant <- cor_matrix %>%
  filter(abs(Cor) > .6) %>%
  arrange(desc(abs(Cor))) 

Cor_significant <- Cor_significant[Cor_significant$varX
                                   != Cor_significant$varY, ]

Cor_significant <- Cor_significant[!grepl("Lead Time", 
                                          Cor_significant$varX), ]

Cor_significant <- Cor_significant[!grepl("Lead Time", 
                                          Cor_significant$varY), ]

Cor_significant <- Cor_significant[Cor_significant$varY == "Ship Mode_GROUND" |
                                     Cor_significant$varY == "Year_2019" |
                                     Cor_significant$varY == "Year_2020" |
                                     Cor_significant$varY == "Origin_Site A" |
                                     Cor_significant$varY == "LOB_Product A" |
                                     Cor_significant$varY == "Ship Mode_OCEAN" |
                                     Cor_significant$varY == "Quarter_Q2" , ]

Cor_significant <- Cor_significant[-c(4, 9), ]

Cor_significant

#            varX     Cor             varY
#9  Origin_Site D  1.0000 Ship Mode_GROUND
#19    Quarter_Q4  1.0000        Year_2019
#20    Quarter_Q4 -1.0000        Year_2020
#25     Year_2020 -1.0000        Year_2019
#27 LOB_Product B -0.9410    Origin_Site A
#30 LOB_Product B -0.9380    LOB_Product A
#31 LOB_Product A  0.8826    Origin_Site A
#35 Origin_Site B  0.7635  Ship Mode_OCEAN
#37    Quarter_Q1 -0.7038       Quarter_Q2


# Address the highly correlated variables 

# create modeldata
modeldata <- Raw[ , c(1:4,6:7)]

# select the predictors I want in model 

# model 1  - Product A is highly correlated with B so drop that
# Q2 is highly correlated with Q1 so drop that 
# Site D is high correlated with Ground so drop that 
# OCEAN is highly correlated with Site B so drop that 

modeldata$LOB <- relevel(modeldata$LOB, "Product A")
modeldata$Quarter <- relevel(modeldata$Quarter, "Q2")
modeldata$Origin <- relevel(modeldata$Origin, "Site D")
modeldata$`Ship Mode` <- relevel(modeldata$`Ship Mode`, "OCEAN")


summary(model <- lm(`Lead Time` ~ `Ship Mode` + LOB + Origin + Quarter
                    , data = modeldata))


# Model 2 - combined Origin and Ship Mode to solve singularity issue above 


modeldata$OriginShipMode <- paste(Raw$Origin, Raw$`Ship Mode`)

# check to see which level to drop for OriginShipMode

modeldata$OriginShipMode <- as.factor(modeldata$OriginShipMode)

modeldata <- dummy_cols(modeldata, select_columns = "OriginShipMode")

cor  <- round(cor(modeldata[ , c(4, 8:14)]), 2)

# worst correlation is Site B Air

# relevel 

modeldata$OriginShipMode <- relevel(modeldata$OriginShipMode, "Site B AIR")


unique(modeldata$OriginShipMode)


summary(model <- lm(`Lead Time` ~ LOB + OriginShipMode + Quarter
                    , data = modeldata))

assess(model)

#                    GVIF Df GVIF^(1/(2*Df))
#LOB            18.208933  2        2.065718
#OriginShipMode 25.237666  6        1.308692
#Quarter         1.502133  3        1.070167


# Model 3 - To solve high multicollinearity in Model 2 combine
# LOB and Origin and Ship Mode 

modeldata$LOBOriginShipMode <- paste(Raw$`Ship Mode`, Raw$Origin, Raw$LOB)

# Check to see which level to drop

modeldata$LOBOriginShipMode <- as.factor(modeldata$LOBOriginShipMode)

modeldata <- dummy_cols(modeldata, select_columns = "LOBOriginShipMode")

cor  <- round(cor(modeldata[ , c(4, 16:26)]), 2)

# Worst correlation is AIR Site A Product C so drop that 

# check plot to justify dropping that level

ggplot(modeldata, aes(LOBOriginShipMode, `Lead Time`)) + geom_boxplot() + theme(axis.text.x = element_text(angle=90))

# relevel 

modeldata$LOBOriginShipMode <- relevel(modeldata$LOBOriginShipMode, "AIR Site A Product C")

unique(modeldata$LOBOriginShipMode)


summary(model <- lm(`Lead Time` ~ LOBOriginShipMode + Quarter
                    , data = modeldata))

assess(model)

#                     GVIF Df GVIF^(1/(2*Df))
#LOBOriginShipMode 1.519539 10        1.021141
#Quarter           1.519539  3        1.072223

# All good model three is good and plots look okay 



#### Questions 


#Has the logistics lead time reduced since Q2, when compared with 
#Q4 and Q1 for AIR and GROUND ship mode?

Q2AG <- Raw[Raw$Quarter == "Q2" & (Raw$`Ship Mode` == "AIR" | 
                                     Raw$`Ship Mode` == "GROUND"), ]

round(mean(Q2AG$`Lead Time`),0)

# 7

Q1AG <- Raw[Raw$Quarter == "Q1" & (Raw$`Ship Mode` == "AIR" | 
                                     Raw$`Ship Mode` == "GROUND"), ]

round(mean(Q1AG$`Lead Time`),0)

# 7

Q4AG <- Raw[Raw$Quarter == "Q4" & (Raw$`Ship Mode` == "AIR" | 
                                     Raw$`Ship Mode` == "GROUND"), ]

round(mean(Q4AG$`Lead Time`),0)

# 3 , but there was only one observation

Q3AG <- Raw[Raw$Quarter == "Q3" & (Raw$`Ship Mode` == "AIR" | 
                                     Raw$`Ship Mode` == "GROUND"), ]

round(mean(Q3AG$`Lead Time`),0)

# 7


AG <- Raw[Raw$`Ship Mode` == "AIR" | Raw$`Ship Mode` == "GROUND", ]

round(mean(AG$`Lead Time`),0)

# 7 

### Seasonality is not present between Air or Ground between the Quarters

#How many in-transit days can be saved if we bring inventory by 
#AIR from Site A compared to Site C?



AA <- Raw[Raw$`Ship Mode` == "AIR" & Raw$Origin == "Site A", ]
round(mean(AA$`Lead Time`),0)

# 8

AC <- Raw[Raw$`Ship Mode` == "AIR" & Raw$Origin == "Site C", ]
round(mean(AC$`Lead Time`),0)

# 9 

# According to model 

# Coefficient for Site C AIR is 2.2 but Site A product A AIR is 1.6
# so the average between 2.2 and .6 is 1.4 or 1 to 2 days 


# Overall, you save about one to two days on average 


#### Optimization Problem 


#Assuming we have a customer deal 100K units of product A, for which 
#Purchase Orders(PO) are set to be provided to site A between 21st 
#Sept and 25th Sept at a daily rate of 20K units (20K*5days = 100K)
#and we must the meet deadline of 27th Oct to receive all units at the 
#destination facility/warehouse in US in order to meet customer due date
#. We have only $1.5M allocated on freight budget to realize positive
#margin on customer sales (The higher the logistics cost, it lowers the 
#profit margin). The product is sold at $333 to the customer with 7% 
#margin. Considering all the above factors, the supply chain analytics
#team needs to optimize and evaluate the following:

Cost <- read_excel("Regression_Final_Raw.xlsx", sheet = "Cost Details")

#Can we fulfil all 100K units before the deadline with the available 
#budget based on the current manufacturing and logistics lead times?

Avg.Man.Time <- Raw[Raw$LOB == "Product A" & Raw$Origin == "Site A" & 
                      Raw$Quarter == "Q3", ]
round(mean(Avg.Man.Time$Man.Time),0)

# 5 so Ship Dates would be :

# Sept 26th, Sept 27th, Sept 28th, Sept 29th, and  Sept 30th

Fastboat <- Raw[Raw$LOB == "Product A" & Raw$Origin == "Site A" & 
                  Raw$`Ship Mode` == "FASTBOAT" & Raw$Quarter == "Q3", ]
nrow(Fastboat)

# 14 

Air <- Raw[Raw$LOB == "Product A" & Raw$Origin == "Site A" & 
             Raw$`Ship Mode` == "AIR" & Raw$Quarter == "Q3", ]
nrow(Air)

#206

Ocean <- Raw[Raw$LOB == "Product A" & Raw$Origin == "Site A" & 
               Raw$`Ship Mode` == "OCEAN" & Raw$Quarter == "Q3", ]
nrow(Ocean)

#57

14 + 206 + 57  #277

14/277   # 5%

206/277 #  74%

57/277   # 21%

# each day 

20000*.05   # 1000 shipped by Fastboat

20000*.74 # 14800 shipped by Air

20000*.21  #4200 shipped by Ocean

# Average lead time for Fastboat using model

6.6932 + 20.6628 + .42# or 27 days

mean(Fastboat$`Lead Time`) # 22 days taking into account Q3

# Average lead time for Air using model

6.6932 + 1.6458 +.42 # or 9 days 

mean(Air$`Lead Time`) # 8 days taking into account Q3

# Average lead time for Ocean using model 

6.6932 + 27.0124 + .42 # or 34 days 

mean(Ocean$`Lead Time`) # or 31 days taking into account Q3

# With current manufacturing and logistics lead times the product 
# will not be fulfilled if the product is shipped by Ocean any time
# after September 26th

# Total cost for shipping modes currently 

1000*13*5 # cost for FastBoat $65000

14800*22*5  # cost for Air $1628000 

4200*10*5  # coast for ocean $210000

65000 + 1628000 + 210000 #Total cost across the 5 days $1,903,000

# it is $400,000 over budget with current distributions 


#If not, please come up with an optimized volume of product A needs
#to be lifted by air/Ocean/Fast Boat to meet maximum customer demand 
#depending on the ship mode lead times and budget available?

# Based on the Ship Mode Lead Times and Budget:

# Sept 26th 20k units need to be shipped via Ocean 
# Sept 27th- Sept 30th the remaining 80k units need to be shipped via Fastboat

# The Ocean shipment will arrive October 27th for $200,000
# The FastBoat shipment will arrive  Oct 19th - Oct. 22 for $1,040,000


# We are $260,000 under budget and the products are delivered before
# the October 27th deadline

#Create a proposal to request for additional budget to fulfil all 100K 
#units by the deadline?

# Assuming we still only ship the Sept 26 Products via Ocean to fulfill
# deadline. But keep same proportions from previous data in terms of 
# shipping tendencies. The shipping structure would go as follows:

# Sept 26th 20k units need to be shipped via Ocean 
# Sept 27th- Sept 30th the remaining 80k units will be 
# shipped via Fastboat and air at a rate of 18500 via Air per day
# and 1500 via FastBoat per day 

# The Ocean shipment will arrive October 27th for $200,000
# The FastBoat shipments will arrive  Oct 19th - Oct. 22 
# and the Air Shipments will arrive Oct 5th - Oct 8th for a combined
# cost of $1,706,000

# the new requested budget is $1,950,000 to fulfill 100k order while
# keeping the same shipping structure. 

#Provide an analysis to leadership team on total logistics cost vs 
#total revenue vs profit margin with the initial budget provided and 
#proposed budget for them to decide what trade-offs to make?


# Total logistics cost for optimized volume is $1,240,000

# Total logistics cost for new proposed budget is $1,906,000


333*100000 # Total revenue is $33,300,000

333*.07*100000 # Total profit margin is $2,331,000

2331000-1240000 # Total profit is $1,091,000 for optimized volume

2331000-1906000 # Total profit is $425,000 for new proposed budget


# If you went with the the optimized model you'd gain $666,000 more money
# than the other model. However the other shipping structure follows the 
# historical pattern of how products are typically shipped from that site 
# so in order to fulfill the new optimized model you'd have to ship 80k 
# units via FastBoat. Both structures yield a positive profit so you'd have
# to assess if you are able to ship 80k units via FastBoat. 

