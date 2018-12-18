############################
# Summary   : Replicates Jegadeesh and Titman (JF, 1993) Momentum Portfolios        
# Date      : November 2018                                
# Author    : Zhenghui Ni                                
# Variables : - J: # of Months in Formation Period to Create Momentum Portfolios    
#             - K: # of Months in Holding Period to Buy and Hold Mom. Ports.      
#             - BEGDATE: Sample Start Date  1963.01                                        
#             - ENDDATE: Sample End Date 1989.12

#neccesary setup
install.packages("dplyr")
install.packages("rio")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("sqldf")

# import data  
#Alternative way to import data <- import(filepath)  read.csv(filepath)
file0 <- "/Users/nizhenghui1/Desktop/Github/Momentum-Seasonality/Data&PDF/"
file1 <- "crsp_63018912.csv"
filepath <- paste0(file0, file1)
library(tidyverse)
data <- read_csv(filepath,na=c("","A","B","C","S","T","P"),col_types=("nncnnnnn"))
str(data)  
View(data)

# Step 1. specifying options
J <- 6 #Formation Period Length: J can be between 3 to 12 months
K <- 6 #Holding Period Length: K can be between 3 to 12 months

# Step 2. Extract CRSP Data for NYSE Nasdaq and AMEX Common Stocks
data1 <- data %>% filter(SHRCD == 10|11 & EXCHCD == 1|2|3 ) %>%
                 select(PERMNO, date, DLRET,RET) %>% 
                 group_by(PERMNO) %>%
                 mutate(ret=ifelse(date != max(date), RET,
                 ifelse(date == max(date) & is.na(DLRET),-0.5*lag(RET), DLRET)))  %>%     
                 filter(is.na(ret) !=1)


# Step 3. Create Momentum Port. Measures Based on Past (J) Month Compounded Returns
library(zoo)
data2 <- data1 %>%group_by(PERMNO)%>% 
                mutate(seq=row_number())%>% 
                mutate(R=ret+1)%>% 
                mutate(cum_ret=ifelse(seq>=J,rollapply(R,J,FUN=prod,fill=NA,align='right'), 0 ) )%>%
                filter(seq>=J)
               
# Formation of 10 Momentum Portfolios Every Month 
data_10portfolio <- data2 %>% filter( is.na(cum_ret)==0) %>%
                        group_by(date) %>%
                         arrange(date,cum_ret) %>%
                        mutate(rank=(row_number()-1)%/%(n()/10)+1)

# Step 4. Assign Ranks to the Next 6 (K) Months After Portfolio Formation
# rank is the portfolio rank variable taking values between 1 and 10: 
# 1 - the lowest  momentum group: Losers   
# 10 - the highest momentum group: Winners 
JK_portfolio <- data_10portfolio  %>% mutate(last_holding_date=ifelse(date%%10000>700,date+9450, date+650)) %>%
                                      mutate( first_holding_date=date+10) %>%
                                      select(PERMNO, last_holding_date, first_holding_date,rank,date)
colnames(JK_portfolio)[5] <- "D"
colnames(data2)[5] <- "return"

library(sqldf)
rm(data_10portfolio, data1)

JK_portfolio_full <- sqldf("select a.* , b.return,  b.date
                           from JK_portfolio as a
                           left outer join 
                           data2 as b
                           on  a.PERMNO = b.PERMNO
                           and  a.first_holding_date < b.date 
                           and b.date<a.last_holding_date")


# Step 5. Calculate Equally-Weighted Average Monthly Returns
library("PerformanceAnalytics")
library("lubridate")
EW_mean_return <- JK_portfolio_full %>% filter((rank==1 |rank==10) & !is.na(date)) %>% 
                   group_by(date, rank) %>% 
                  summarise(ew_return=mean(return)) %>% 
                  mutate(date1=ymd(date)) 

# Compute Equally-Weighted Average Monthly Cumulative Returns 
EW_mean_return <- EW_mean_return %>% group_by(rank) %>% 
                  mutate(cum_ew_return=Return.cumulative(ew_return))%>% 
                  mutate(cum_ew_return1=cumprod(ew_return+1)-1 ) 

figure1 <- ggplot(EW_mean_return, aes(x = date1, y = cum_ew_return1,colour=rank)) +
  geom_line() + labs(title ="Time Series of Cumulative Momentum Portfolio Returns Based on Jegadeesh and Titman (1993)", x = "date", y = "cumulative return", colour="Winner/Loser") 

# Step 6. Calculate Long-Short Portfolio Returns
EW_mean_return1 <- EW_mean_return %>% filter(rank==1)
EW_mean_return10 <- EW_mean_return %>% filter(rank==10)
colnames(EW_mean_return10)[3] <- "ew_return10"

long_short <- sqldf("select a.ew_return , b.ew_return10,  b.date1
                    from EW_mean_return1 as a
                    left outer join 
                    EW_mean_return10 as b
                    on  a.date = b.date")

long_short <- long_short %>% mutate(long_short=ew_return10-ew_return)%>% 
  mutate(cum_long_short=cumprod(long_short+1)-1 ) 

figure2 <- ggplot(long_short, aes(x = date1, y = cum_long_short)) +
  geom_line() + ggtitle("Long/Short Momentum Strategy Based on Jegadeesh and Titman (1993)") +
  xlab("date") + ylab("cumulative return")







#useful code stocked for future use
#test <- sqldf("select *
#              from data1   ") 
#i <- 1
#JK_portfolio1 <- JK_portfolio
#while (i<J){  JK_portfolio1 <- rbind(JK_portfolio1,JK_portfolio)
#  i=i+1
#}
#JK_portfolio2 <- merge(JK_portfolio1,JK_portfolio)
# %>%  group_by(date) %>% 
#group_by(date) %>%
#mutate(JK_portfolio, FormDate = group_indices(JK_portfolio))
#sessionInfo()




