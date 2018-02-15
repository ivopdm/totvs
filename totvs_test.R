library(jsonlite) # to read json
library(xts) # to handle time serie
library(forecast) # to perform prediction

# Reading json file and converting to data frame format
sampleDF <- fromJSON("sample.txt", simplifyDataFrame = TRUE)

# Check missing values
if(any(is.na(sampleDF$complemento$valorTotal))){
  # Missing value row index
  indexNA = which(is.na(sampleDF$complemento$valorTotal))
  sampleDF$complemento$valorTotal[indexNA] <- NULL
  sampleDF$ide$dhEmi[indexNA] <- NULL
}

if(any(is.na(sampleDF$ide$dhEmi))){
  # Missing value row index
  indexNA = which(is.na(sampleDF$ide$dhEmi))
  sampleDF$complemento$valorTotal[indexNA] <- NULL
  sampleDF$ide$dhEmi[indexNA] <- NULL
}

# New data frame with Date and Total Value
date = as.Date(sampleDF$ide$dhEmi)
value = sampleDF$complemento$valorTotal
dateValue = data.frame(date,value)

# Procedure to aggreate revenue per day
by1 = factor(dateValue$date)
dateValue = aggregate(dateValue$value, by=list(by1), FUN = sum)

colnames(dateValue)[1] = "date"
colnames(dateValue)[2] = "value"

dateValue$date = as.Date(dateValue$date)

# Visualizing time serie
timeSerie = xts(x=dateValue$value, order.by=dateValue$date)
plot(timeSerie)

# Transforming data to include historical and to be predicted dates
ahead = 6
dateAux = (1:ahead) + dateValue$date[length(dateValue$date)]
datePred = c(dateValue$date,dateAux)

#Procedure to predict next week revenue
y = ts(dateValue$value, frequency=6)
fit <- tslm(y ~ trend + season)
pred = forecast(fit, h=ahead)

plot(pred, xaxt = "n", xlab = "Business days (Monday-Saturday)", ylab= "Revenue per day ($)")
axis(1, at=1:4, labels=datePred[c(1,7,13,20)])
grid(lty=1)
legend("bottomleft", 
       c("Historical data", "Prediction","80% confidence","95% confidence"), 
       fill=c("black","blue","light gray","dark gray"), horiz=TRUE, cex=0.8)
