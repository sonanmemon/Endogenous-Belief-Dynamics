library(dslabs)
library(dplyr)
library(ggplot2)
options(stringsAsFactors = FALSE)
library(tidyverse)
library(gridExtra)
library(Lahman)
library(dslabs)
library(AER)
library(tseries)
library(dynlm)
library(stargazer)
library(forecast)
library(mFilter)
library(data.table)
library(caTools)
ds_theme_set()




data <- read_csv("F:/IBA Economics Department/IBA Teaching/Intermediate Macroeconomics ECON 204/Assignments/Assignment 1/US_GDP_NX.csv")

head(data)

data <- ts(data, start = c(1947,1), frequency = 3)


data <- data.frame(data)









data$DATE <- seq(from = as.Date("1947-01-01"), to = as.Date("2020-12-01"), by = 'quarter')


head(data)


data



colnames(data) <- c("GDP", "NX", "time_index", "DATE")


head(data)


graph <- ggplot(data) +
  geom_line(aes(x=DATE, y=GDP), color = "red") +
  #geom_hline(yintercept=mean(data$GDP), linetype="dashed", color = "black") +
  #scale_y_continuous(breaks = c(50, 70, 86, 90,  110)) +
  labs(x = "Time", y = "Nominal GDP", title = "Evolution of Nominal GDP for US")

graph


adf.test(data$GDP) # p-value < 0.05 indicates the TS is stationary

data$logGDP <- log10(data$GDP)

adf.test(data$logGDP) # p-value < 0.05 indicates the TS is stationary



diff1_logGDP <- diff(data$logGDP, differences= 1)

data[-1, 6] <- diff1_logGDP

colnames(data) <- c("GDP", "NX", "time_index", "DATE", "logGDP", "diff1_logGDP")

head(data)


adf.test(data[-1, 6]) # p-value < 0.05 indicates the TS is stationary


growthrate <- ggplot(data) +
  geom_line(aes(x=DATE, y=diff1_logGDP), color = "red") +
  geom_hline(yintercept= mean(data$diff1_logGDP, na.rm = TRUE), linetype="dashed", color = "black") +
  scale_y_continuous(breaks = c(-0.04, -0.02, 0, 0.01,  0.02)) +
  labs(x = "Time", y = " First Difference of Log Nominal GDP", title = "Growth Rate of Nominal GDP (USA)")

growthrate

#Moving Average Growth


growth_centered_roll_mean <- runmean(data[-1,6], 3, alg=c("fast"), endrule=c("NA"), align = c("center"))
growth_forward_roll_mean <- runmean(data[-1,6], 3, alg=c("fast"), endrule=c("NA"), align = c("left"))
growth_backward_roll_mean <- runmean(data[-1,6], 3, alg=c("fast"), endrule=c("NA"), align = c("right"))



growth_centered_roll_mean
growth_forward_roll_mean
growth_backward_roll_mean

plot(growth_backward_roll_mean)

head(data)

#Volatility (SD)

vol_growth <- sd(data$diff1_logGDP, na.rm = TRUE)

vol_growth

#Moving Volatility/SD Measures

growth_centered_rollsd <- runsd(data[-1,6], 3, center = runmean(data$diff1_logGDP,3),  endrule=c("NA"), align = c("center"),na.remove = TRUE)

growth_forward_rollsd <- runsd(data[-1,6], 3, center = runmean(data$diff1_logGDP,3),  endrule=c("NA"), align = c("left"), na.remove = TRUE)

growth_backward_rollsd <- runsd(data[-1,6], 3, center = runmean(data$diff1_logGDP,3),  endrule=c("NA"), align = c("right"), na.remove = TRUE)

growth_centered_rollsd
growth_forward_rollsd
growth_backward_rollsd

plot(growth_centered_rollsd)




#corr(x,y)

corr <- cor(data$GDP, data$NX)

corr

ccf(data$GDP, data$NX)

df <- data

df_x <- eval(substitute(data$GDP),df)
df_y <- eval(substitute(data$NX),df)
ccf.object <- ccf(df_x,df_y,plot=FALSE)
output_table <- cbind(lag=ccf.object$lag, x.corr=ccf.object$acf) %>% as_tibble() %>% mutate(cat=ifelse(x.corr>0,"green","red"))
output_table %>% ggplot(aes(x=lag,y=x.corr)) + geom_bar(stat="identity",aes(fill=cat))+scale_fill_manual(values=c("#339933","#cc0000"))+ylab("Cross correlation")+scale_y_continuous(limits=c(-1,1))+theme_bw()+theme(legend.position = "none", plot.title=element_text(size=10))+ggtitle(title) -> p
#tikz(file = "PCEbusinessinventories.tex", width = 6, height = 3.7)
dynamic_corr <- ggplot(output_table, aes(x=lag,y=x.corr)) + geom_bar(stat="identity",aes(fill=cat))+scale_fill_manual(values=c("#339933","#cc0000"))+ geom_hline(yintercept=0.15) + geom_hline(yintercept = -0.15) + ggtitle("Nominal GDP and Net Exports") + ylab("Cross correlations")+scale_y_continuous(limits=c(-1,1))+theme_bw()+theme(legend.position = "none", plot.title=element_text(size=10))

print(dynamic_corr)
#Necessary to close or the tikxDevice .tex file will not be written





#dev.off()
#Linear trend separation




trend <- lm(data$logGDP ~ c(1:length(data$logGDP)))


plot(resid(trend), type="l")  # resid(trModel) contains the de-trended series.


#HP Filter
#Use lamda = 129600 for monthly, 1600 for quarterly data and 6.25 for annual data.


hpindex <- hpfilter(data$logGDP, freq = 1600, type=c("lambda","frequency"),drift=FALSE)

plot(hpindex)




















