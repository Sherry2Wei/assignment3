require(data.table)
require(dplyr)
require(feather)
require(lubridate)
require(ggplot2)
setwd("c:/Users/ChonWai/Desktop/quantitive_trading/Data")
# Note:
# The following code is the simple version of our assignment 3, basicly it cover all parts expect 
# the part of creating the indicator for the datatable. Alan has created a feather file, 
# replication_dateset_feather, which already calculate the indicator for us. So the indicator part 
# is just for impression or show off only. Plus, I am lazy, forgive me. 
# So , if you guys finish off reading my code and have some free
# time, u guys can try to recreate the indicator by using msf file and the peer file which are the 
# two files that i block in the following. But be careful with the file peer.Feather since it is huge 
# and it may break your ram or disk usage when you try to open it. It take me more than 5 minute to open it.
# Alternative way is to use the separate peer files which is the zip file inside the hw3 zip. But you will 
# need to do the calculation multiple time and merge it together. 

# file that will be used for the creation of the indicator
#msf <- "msf_with_gvkey.feather" %>% read_feather %>% setDT
#peer <- "tnic3_peers.feather" %>% read_feather %>% setDT

# assignment code (simple version)
# def quantiles and end_of_month functions
apply_quantiles=function(x,include_in_quantiles=NULL,bins=10){
  
  if(is.null(include_in_quantiles)) include_in_quantiles=rep(TRUE,length(x))
  quantiles=quantile(ifelse(include_in_quantiles,x,NA),probs=seq(0,1,length.out=bins+1),na.rm=TRUE)
  quantiles['0%']=min(x,na.rm=TRUE)-1 ; quantiles['100%']=max(x,na.rm=TRUE)+1
  #x=ifelse(include_in_quantiles,x,NA)
  return(cut(x,breaks=quantiles,labels=FALSE))
}


end_of_month=function(x){
  require(lubridate)
  x=as.Date(x)
  day(x)=days_in_month(x)
  
  x %>% as.character
}

replication <- "replication_dataset.feather" %>% read_feather %>% setDT
# set the date so that the date can match the ff4 yearmonth
#replication$date <- end_of_month(replication$date)
ff4 <- "ff4_monthly.feather" %>% read_feather %>% setDT
# impose filter condition (please check whether the vol filter done it correctly or not, bc from 
# my understanding at least 25 million per day and there is 20 days in a month
# so 25 million  times 20 will be the filter)

replication <- replication %>% subset(prc_lag1 >= 5 & 
                                      dollarvol_lastmonth >= 25000000*20) 
setkey(replication,permno,date)

# bin assignment of the comeptitor ret, not sure whether bin is 5 or 10
# bc in the customer supplier html, he uses 5 and he never point that out in 
# the homework document. 
replication[, bins:=apply_quantiles(competitor_ret, bins=5), by=list(date)]
# form an overall portfolio and impose the normal filtering whihc mcap and ret is not na
# note: i am not sure whether we need to filter out the bottom 20% percentile mcap or not
# since he does not specific on that, but if u guys think the result is not right
# u guys can try to add that filter on. i try it on my own, it only make minimal different

# code that generate a return of true or false base of whether the mcap_lag1 is smaller 
# than the bottom 20 % mcap_lag1 on each date
#replication[, is_small_declie := mcap_lag1 <= quantile(mcap_lag1, na.rm = TRUE, probs = (0.2)), by = list(date)]
#is_small_declie == FALSE # this will remove the bottom 20 

# on the other hand, i am not sure whether we use ret or ret_lag1 to 
# to calculate the vwret and the ewret, when i try to use the ret as the mean to
# calcualte those 2 variables, the cumlate return across the horizon is so small if you compare 
# to the last assignment cumlative return on both portfolio and somehow
# the vwret is even negative in the first few rows. Furthermore, the barplot for portfolio 1 is so 
# wired which bin 3 and 4 have higher vwret than bin 5. However, if you use the ret_lag1 
# the cum return will become so large and that is too good to be true. Plus, i don't think useing
# the ret_lag1 would make sense since we are using our peer ret this month to predict ours. So, in conlcude, 
# either i do something wrong or this strategy is basically shit. please help me on this one. see the comparsion 
# graph which i draw for both portfolio base on using ret_lag1 and ret

# task 1.B
port <- replication[!is.na(mcap_lag1) & !is.na(ret),
                    list(vwret = weighted.mean(ret, mcap_lag1),
                         ewret = mean(ret)),
                    by = list(date,bins)]
# task 1.C.I
# portfolio one (weight average)
# remember to change the ifelse(bins == ) if u guys change the bin number from 5 to 10
portfolio1 <- port[ bins %in% c(1,5), 
                    list(vwret=sum(ifelse(bins==5,1,-1)*vwret)),
                    by=list(date)]
setkey(portfolio1,date)
portfolio1$cumulativeReturns=cumprod(1+portfolio1$vwret)
# merge the yearmonth column from replication to portfolio1 so that we can do regression 
# on ff4 since ff4 only have yearmonth instead of date
yearmonth_block <- replication[,list(date, yearmonth)] %>% unique
portfolio1 <- merge(yearmonth_block, portfolio1, by ="date", all= FALSE)
# plot cumlative return on portfolio 1
ggplot(portfolio1,aes(x=as.Date(date),y=log(cumulativeReturns,base=10))) + geom_line() +
  ggthemes::theme_stata() + ggtitle('log cumulative return of portfolio 1')+
  xlab("Date")+ ylab("Log Cumulative Return of Portfolio") +
  theme(axis.title = element_text(face="bold"),title = element_text(face="bold.italic"))
# plot the bar chart of weighted average return according to bin size
quintile_1=port[!is.na(bins),list(vwret=mean(vwret)),by=list(bins)]
quintile_1=arrange(quintile_1,bins)
barplot(quintile_1$vwret ,main="Weight Average Mean of Return Base on Bins Size",
        xlab="Bin Size", ylab="Weight Average Mean of Return" , names.arg = c(1:5),
        font.main=4, font.lab=2) 
# portfolio two (equal weight)
# remember to change the ifelse(bins == ) if u guys change the bin number from 5 to 10
portfolio2 <- port[ bins %in% c(1,5), 
                    list(ewret=sum(ifelse(bins==5,1,-1)*ewret)),
                    by=list(date)]
setkey(portfolio2,date)
portfolio2$cumulativeReturns=cumprod(1+portfolio2$ewret)
# merge the yearmonth column from replication to portfolio1 so that we can do regression 
# on ff4 since ff4 only have yearmonth instead of date
portfolio2 <- merge(yearmonth_block, portfolio2, by ="date", all= FALSE)
# plot cumlative return on portfolio 1
ggplot(portfolio2,aes(x=as.Date(date),y=log(cumulativeReturns,base=10))) + geom_line() +
  ggthemes::theme_stata() + ggtitle('log cumulative return of portfolio 2')+
  xlab("Date")+ ylab("Log Cumulative Return of Portfolio") +
  theme(axis.title = element_text(face="bold"),title = element_text(face="bold.italic"))
# plot the bar chart of weighted average return according to bin size
quintile_2=port[!is.na(bins),list(ewret=mean(ewret)),by=list(bins)]
quintile_2=arrange(quintile_2,bins)
barplot(quintile_2$ewret ,main="Simple Average Mean of Return Base on Bins Size",
        xlab="Bin Size", ylab="Simple Average Mean of Return" , names.arg = c(1:5),
        font.main=4, font.lab=2) 
# comparsion between portfolio1 and portfolio2
ggplot() + 
  geom_line(data = portfolio1, aes(x =as.Date(date),y=log(cumulativeReturns,base=10),
                                               color ="Portfolio 1"))+
  geom_line(data = portfolio2, aes(x =as.Date(date),y=log(cumulativeReturns,base=10),
                                   color= "Portfolio 2"))+
  scale_color_manual(name = "Portfolio", values = c("blue","red"))+
  ggthemes::theme_stata() + 
  ggtitle('Log Cumulative Return Comparsion \nBetween Portfolio 1 and Portfolio 2') +
  xlab("Date")+ ylab("Log Cumulative Return")+
  scale_y_continuous(name="Log Cumulative Return of Portfolio") + 
  theme(axis.title = element_text(face="bold"),title = element_text(face="bold.italic"),
        legend.position = c(0.2,0.9))

# comparsion between using ret_lag1 and ret for both portfolio
# you can see the result is ridiculous
# port that use ret_lag to calculate vwret and ewret
port_lag <- replication[!is.na(mcap_lag1) & !is.na(ret_lag1),
                    list(vwret = weighted.mean(ret_lag1, mcap_lag1),
                         ewret = mean(ret_lag1)),
                    by = list(date,bins)]
# form portfolio 1 using lag
portfolio1_lag <- port_lag[ bins %in% c(1,5), 
                            list(vwret=sum(ifelse(bins==5,1,-1)*vwret)),
                            by=list(date)]
setkey(portfolio1_lag,date)
portfolio1_lag$cumulativeReturns=cumprod(1+portfolio1_lag$vwret)
# form portfolio 2 using lag
portfolio2_lag <- port_lag[ bins %in% c(1,5), 
                    list(ewret=sum(ifelse(bins==5,1,-1)*ewret)),
                    by=list(date)]
setkey(portfolio2_lag,date)
portfolio2_lag$cumulativeReturns=cumprod(1+portfolio2_lag$ewret)
# plot comparsion graph
ggplot() + 
  geom_line(data = portfolio1_lag, aes(x =as.Date(date),y=log(cumulativeReturns,base=10),
                                               color ="Portfolio 1 lag"), linetype="dashed")+
  geom_line(data = portfolio1, aes(x =as.Date(date),y=log(cumulativeReturns,base=10),
                                   color= "Portfolio 1"), linetype="longdash")+
  geom_line(data = portfolio2_lag, aes(x =as.Date(date),y=log(cumulativeReturns,base=10),
                                               color= "Portfolio 2 lag"),linetype="dotted")+
  geom_line(data = portfolio2, aes(x =as.Date(date),y=log(cumulativeReturns,base=10),
                                   color= "Portfolio 2"), linetype="dotdash")+
  scale_color_manual(name = "Portfolio", values = c("blue","darkblue","red","darkred"))+
  ggthemes::theme_stata() + 
  ggtitle('Log Cumulative Return Comparsion \nBetween Portfolios and Portfolios lag') +
  xlab("Date")+ ylab("Log Cumulative Return")+
  scale_y_continuous(name="Log Cumulative Return of Portfolio") + 
  theme(axis.title = element_text(face="bold"),title = element_text(face="bold.italic"),
        legend.position = c(0.2,0.8))
# task 1.C.II running regression of portfolio with Fama French Four Factor
port1_ff4 <- merge(portfolio1, ff4, by = "yearmonth", all = FALSE)
lm(data = port1_ff4,vwret~mkt_rf+smb+hml+mom)%>%summary
port2_ff4 <- merge(portfolio2, ff4, by = "yearmonth", all = FALSE)
lm(data = port2_ff4,ewret~mkt_rf+smb+hml+mom)%>%summary
# task 1.C.III
# comparsion graph cross all portfolio + benchmark 
# assume all factor equal
ff4[,ew_ff_r:= (mkt_rf+smb+hml+mom)/4]
# filter out na and date that is not in the portfolios investment horizon 
ew_ff_strategy <- ff4[!is.na(ew_ff_r)]
ew_ff_strategy <- merge(ew_ff_strategy, yearmonth_block, by ="yearmonth", all = TRUE) %>% 
                  subset(!is.na(date))
ew_ff_strategy$cumulativeReturns=cumprod(1+ew_ff_strategy$ew_ff_r)
# plot graph 
ggplot() + 
  geom_line(data = portfolio1_lag, aes(x =as.Date(date),y=log(cumulativeReturns,base=10),
                                       color ="Portfolio 1 lag"), linetype="dashed")+
  geom_line(data = portfolio1, aes(x =as.Date(date),y=log(cumulativeReturns,base=10),
                                   color= "Portfolio 1"), linetype="longdash")+
  geom_line(data = portfolio2_lag, aes(x =as.Date(date),y=log(cumulativeReturns,base=10),
                                       color= "Portfolio 2 lag"),linetype="dotted")+
  geom_line(data = portfolio2, aes(x =as.Date(date),y=log(cumulativeReturns,base=10),
                                   color= "Portfolio 2"), linetype="dotdash")+
  geom_line(data = ew_ff_strategy, aes(x =as.Date(date),y=log(cumulativeReturns,base=10),
                                       color= "Benchmark"), linetype="solid") +
  scale_color_manual(name = "Portfolio", values = c("gold","blue","darkblue","red","darkred"))+
  ggthemes::theme_stata() + 
  ggtitle('Log Cumulative Return Comparsion \nBetween Portfolios and Portfolios lag') +
  xlab("Date")+ ylab("Log Cumulative Return")+
  scale_y_continuous(name="Log Cumulative Return of Portfolio") + 
  theme(axis.title = element_text(face="bold"),title = element_text(face="bold.italic"),
        legend.position = c(0.2,0.8))









