require("quantmod");require("data.table"); require("pbapply");require("ggplot2")
# get E-mini data
# ES <- readRDS("ES_2020.rds")
# Thurs = ES[weekdays(index(ES)) == "Thursday",]
# saveRDS(Thurs,"ES_THURS.rds")

# read in E-mini S&P "Thursday" Data 
Thurs <- readRDS("ES_THURS.rds")
# extract unique days
DAYS = unique(as.Date(index(Thurs)))
DAYS = DAYS[weekdays(DAYS) == "Thursday"]

# extract S&P points at different time intervals
BT = pblapply(as.list(DAYS), function(dd){
  # subset by Days passed in
  tmp = Thurs[paste0(dd)]
  # 08:30AM EST release time to 09:30AM EST before market opens
  tmp = suppressWarnings(tmp["T08:30/T09:30"]) 
  # opening Price at  08:30 AM
  OpPRC = as.numeric(Op(tmp)[1])
  # First  5 minutes
  Cl05 = Cl(tmp[paste(dd, "08:35:00")]) %>% as.numeric
  # First 10 minutes
  Cl10 = Cl(tmp[paste(dd, "08:40:00")]) %>% as.numeric
  # first 15 minutes
  Cl15 = Cl(tmp[paste(dd, "08:45:00")]) %>% as.numeric
  # first 60 minutes
  Cl60 = Cl(tmp[paste(dd, "09:30:00")]) %>% as.numeric
  # Max Price
  MAX = max(Hi(tmp))
  # Min Price
  MIN = min(Lo(tmp))
  # Range
  RANGE = MAX - MIN
  # calculate Returns from Open
  Cl05 = round(Cl05-OpPRC,2)
  Cl10 = round(Cl10-OpPRC,2)
  Cl15 = round(Cl15-OpPRC,2)
  Cl60 = round(Cl60-OpPRC,2)
  toMAX = round(MAX-OpPRC,2)
  toMIN = round(MIN-OpPRC,2)
  
  bt = cbind(paste(dd),Cl05,Cl10,Cl15,Cl60,toMAX,toMIN,RANGE)
  colnames(bt)[1] = "Date"
  bt = as.data.frame(bt)
  
})

# row bind all data
BT= rbindlist(BT,use.names = TRUE,fill = TRUE)

# Convert columns to numeric 
BT$Cl05 <- BT$Cl05 %>% as.numeric
BT$Cl10 <- BT$Cl10 %>% as.numeric
BT$Cl15 <- BT$Cl15 %>% as.numeric
BT$Cl60 <- BT$Cl60 %>% as.numeric
BT$toMAX <- BT$toMAX %>% as.numeric
BT$toMIN <- BT$toMIN %>% as.numeric
BT$RANGE <- BT$RANGE %>% as.numeric

# convert to xts
BT <- xts(BT[,c(2:8)], order.by = as.Date(BT$Date, format = "%Y-%m-%d"))


# get Initial Claims data 
IC <- read.csv("WeeklyClaims.csv",header = TRUE, sep=",")
IC <- as.data.frame(IC)
# remove commas in numbers
IC$Actual <- gsub("\\,","",IC$Actual)
IC$Forecast <- gsub("\\,","",IC$Forecast)
IC$Previous <- gsub("\\,","",IC$Previous)
# remove "K"
IC$Actual <- gsub("K","",IC$Actual)
IC$Forecast <- gsub("K","",IC$Forecast)
IC$Previous <- gsub("K","",IC$Previous)
# convert to numeric 
IC$Actual   <- IC$Actual   %>% as.numeric
IC$Forecast <- IC$Forecast %>% as.numeric
IC$Previous <- IC$Previous %>% as.numeric
# convert to xts
IC <- xts(IC[,c(2:4)], order.by = as.Date(IC$Date, format = "%Y-%m-%d"))
# If Weekly Claims > Forecast then Bearish (-1) otherwise Bullish (1)
IC$Sig <- NA
IC$Sig <- ifelse(IC$Actual > IC$Forecast, -1, 1)

# combine Returns with Weekly Claims data
df <- merge(BT,IC) 

#complete cases
df <- na.omit(df)

# Ranges for negative/positive signals
ggplot(df, aes(y=as.factor(df$RANGE))) +
  geom_boxplot() + 
  facet_wrap(as.factor(df$Sig))

# BEARISH Signals
BEARISH <- subset(df, df$Sig == -1)

# BULLISH Signals
BULLISH <- subset(df, df$Sig == 1)

# BEARISH QUANTILES: 50%
quantile(BEARISH$Cl05*BEARISH$Sig)
quantile(BEARISH$Cl10*BEARISH$Sig)
quantile(BEARISH$Cl15*BEARISH$Sig)
quantile(BEARISH$Cl60*BEARISH$Sig)

# BULLISH QUANTILES
quantile(BULLISH$Cl05*BULLISH$Sig)
quantile(BULLISH$Cl10*BULLISH$Sig)
quantile(BULLISH$Cl15*BULLISH$Sig)
quantile(BULLISH$Cl60*BULLISH$Sig)

# Quantiles to Maximum
quantile(BEARISH$toMIN)
quantile(BULLISH$toMAX)
