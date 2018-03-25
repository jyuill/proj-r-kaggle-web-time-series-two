## from: http://r-statistics.co/Loess-Regression-With-R.html

library(tidyverse)
library(stats)

data(economics, package="ggplot2")  # load data
economics$index <- 1:nrow(economics)  # create index variable
economics <- economics[1:80, ]  # retail 80rows for better graphical understanding

summary(economics)

ggplot(economics, aes(x=date, y=uempmed))+geom_line()
## same but y starts at 0
plot1 <- ggplot(economics, aes(x=date, y=uempmed))+geom_line()+
  scale_y_continuous(limits=c(0,10), expand=c(0,0))+
  scale_x_date(expand=c(0,0))+theme_classic()
plot1

## apply loess with different spans (precision)
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10) # 10% smoothing span
loessMod25 <- loess(uempmed ~ index, data=economics, span=0.25) # 25% smoothing span
loessMod50 <- loess(uempmed ~ index, data=economics, span=0.50) # 50% smoothing span

summary(loessMod10)

## Predict with Loess
# get smoothed output
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 

summary(smoothed10)

plot1 + geom_line(aes(y=smoothed10, x=economics$date), color='green')

## Predict forward periods with Loess
## ??



