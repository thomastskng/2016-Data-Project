package <- c("quantmod","ggplot2","reshape2","MASS","TTR","forecast","dplyr")
lapply(package,require,character.only = T)
hsi <- getSymbols("^HSI", auto.assign=F)

hsi_09_14  <- hsi['2009::2014']
#hsi_11_15 <- hsi['2011::']
sma10      <- ma(hsi_09_14$HSI.Adjusted, order=10,centre=F)
sma20      <- ma(hsi_09_14$HSI.Adjusted, order=20)
sma50      <- ma(hsi_09_14$HSI.Adjusted, order=50)
sma100     <- ma(hsi_09_14$HSI.Adjusted, order=100)
sma250     <- ma(hsi_09_14$HSI.Adjusted, order=250)
SMA10      <- SMA(hsi_09_14$HSI.Adjusted,n=10)

ema10      <- EMA(hsi_09_14$HSI.Adjusted, n=10)
ema20      <- EMA(hsi_09_14$HSI.Adjusted, n=20)
ema50      <- EMA(hsi_09_14$HSI.Adjusted, n=50)
ema100     <- EMA(hsi_09_14$HSI.Adjusted, n=100)
ema150     <- EMA(hsi_09_14$HSI.Adjusted, n=150)

rsi10      <- RSI(hsi_09_14$HSI.Adjusted, n=10)
rsi14      <- RSI(hsi_09_14$HSI.Adjusted, n=14)
rsi20      <- RSI(hsi_09_14$HSI.Adjusted, n=20)
macd       <- MACD(hsi_09_14$HSI.Adjusted)
bband      <- BBands(hsi_09_14$HSI.Adjusted, n=10,sd=2)
ggplot(data =hsi_09_14) +geom_line( data = hsi_09_14, aes(x=time(hsi_09_14), y=HSI.Adjusted)) + geom_line( aes(x=time(hsi_09_14),y= sma10),colour="red") + 
geom_line( aes(x=time(hsi_09_14),y= sma20),colour="yellow") + 
geom_line( aes(x=time(hsi_09_14),y= sma50),colour="purple") + 
geom_line( aes(x=time(hsi_09_14),y= sma100),colour="cyan") + 
geom_line( aes(x=time(hsi_09_14),y= sma250),colour="blue")

ggplot(data =hsi_09_14) +geom_line( data = hsi_09_14, aes(x=time(hsi_09_14), y=HSI.Adjusted)) + geom_line( data=ema10 ,aes(x=time(ema10),y= HSI.Adjusted.EMA.10),colour="red") + 
geom_line( data=ema20, aes(x=time(ema20),y= HSI.Adjusted.EMA.20),colour="yellow") + 
geom_line( data=ema50, aes(x=time(ema50),y= HSI.Adjusted.EMA.50),colour="purple") + 
geom_line( data=ema100, aes(x=time(ema100),y= HSI.Adjusted.EMA.100),colour="blue") + 
geom_line( data=ema150, aes(x=time(ema150),y= HSI.Adjusted.EMA.150),colour="green")

df <- data.frame(hsi_09_14,SMA10=sma10, SMA20=sma20, SMA100=sma100,SMA250=sma250,EMA10=ema10,EMA20=ema20, EMA50=ema50,EMA100=ema100, EMA150=ema150,RSI10=rsi10,RSI14=rsi14,RSI20=rsi20,macd,bband)
df <- tbl_df(data.frame(df))
df <- rename(df, EMA10=HSI.Adjusted.EMA.10, EMA20=HSI.Adjusted.EMA.20, EMA50=HSI.Adjusted.EMA.50, EMA150=HSI.Adjusted.EMA.150, RSI10=HSI.Adjusted.EMA.10.1, RSI14=HSI.Adjusted.EMA.14, RSI20=HSI.Adjusted.EMA.20.1)

set_indicators_threshold <- function(rsi_overbought, rsi_oversold){
	return(
		df %>% mutate(RSI10_OB=RSI10> rsi_overbought, RSI10_OS = RSI10<rsi_oversold,RSI14_OB= RSI14> rsi_overbought, RSI14_OS= rsi_oversold, RSI20_OB = rsi_overbought, RSI20_OS = rsi_oversold)
	)
}

df <- set_indicators_threshold(70,30)

#########################################################################################

fit <- Arima(hsi_09_14-mean(hsi_09_14),order=c(0,0,1),transform.pars=T, include.mean= F,method="ML", xreg=1:length(oshort_ts))



fit10 <- Arima(hsi_11_15$HSI.Adjusted -mean(hsi_11_15$HSI.Adjusted),order=c(0,0,10),transform.pars=T, include.mean= F,method="ML")
fit20 <- Arima(hsi_11_15$HSI.Adjusted -mean(hsi_11_15$HSI.Adjusted),order=c(0,0,20),transform.pars=T, include.mean= F,method="ML")
#fit50 <- Arima(hsi_11_15$HSI.Adjusted -mean(hsi_11_15$HSI.Adjusted),order=c(0,0,50),transform.pars=T, include.mean= F,method="ML")

par(mfrow=c(1,2))
plot.ts(hsi_11_15$HSI.Adjusted,type="l")
lines(mean(hsi_11_15$HSI.Adjusted)+fitted(fit10), col="blue", type="l")