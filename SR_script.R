

#Installing libraries and preparing the database:


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(readxl)
library(openxlsx)


#Import dataset

url <-"https://github.com/miguelboto/sales_report/blob/master/SR.xlsx?raw=true"

download.file(url, destfile = "SR.xlsx", mod="wb")

data <- read.xlsx("SR.xlsx")

#Converting date numeric rows to date format and including a new row with time in days (difference between open date an close date data):

sales_report <- data %>% mutate(open_date = as.Date(open_date, origin = "1899-12-30"), 
                                close_date = as.Date(close_date, origin = "1899-12-30"), 
                                last_date = as.Date(last_date, origin = "1899-12-30"),
                                time = close_date - open_date)


# Validation: 10% of Sales Report data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_index <- createDataPartition(y = sales_report$price, times = 1, p = 0.1, list = FALSE)
edx <- sales_report[-test_index,]
temp <- sales_report[test_index,]

# Opportunity_ID and count_id in validation set must be also in temp set

validation <- temp %>% 
  semi_join(edx, by = "opportunity_name") %>%
  semi_join(edx, by = "count_id")

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#EXPLORATORY ANALYSIS AND DATA VISUALIZATION

#Resume

dim(sales_report)


head(sales_report)

summary(sales_report)


#Opportunities value per time and status

sales_report %>% mutate(time = as.numeric(sales_report$time)) %>% 
  ggplot(aes(time, price, color = status)) + 
  geom_point(aes(shape = market_class)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

sales_report %>% group_by(open_date, sale_class)  %>% 
  ggplot(aes(open_date, price, color = sale_class)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  facet_grid(~status)


#Histogram Price Distribution All Opportunities per status
sales_report %>% 
  ggplot(aes(price)) +
  geom_histogram(bins = 30, fill = "pink", col = "black") +
  ggtitle("Price Distribution per Opportunity Status") +
  facet_grid(~status) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Average Price per status opportunity and Success Rate

dat <- sales_report %>% group_by(status) %>% 
  summarize(Opps = n(), Ticket = mean(price))


dat %>% mutate(Rate = (dat[, 2:2]) / sum(dat[, 2])) %>% knitr::kable()


#Resume Prospects Opportunities 

sales_resume <- sales_report %>% 
  summarize(
    'Counts'=n_distinct(count_id),
    'Opportunities'=n_distinct(opportunity_ID),
    'Min_Price'=min(price),
    'Max_Price'=max(price), 
    'Average_Price'=mean(price),
    'Total_Values'=sum(price)
  ) 
sales_resume %>% knitr::kable()

sales_resume_won <- sales_report %>%  filter(status ==  "Won") %>% 
  summarize(
    'Counts'=n_distinct(count_id),
    'Won'=n_distinct(opportunity_ID),
    'Min Price'=min(price),
    'Max Price'=max(price), 
    'Average Price'=mean(price),
    'Total Values Won'=sum(price),
    'Success Rate'=(Won/sales_resume$Opportunities)
  ) 
sales_resume_won %>% knitr::kable()

sales <- sales_report %>% group_by(sale_class) %>%
  summarize(totalopp = n(), averageprice = mean(price)) %>%
  arrange(desc(averageprice))
sales %>% mutate(sale_class = reorder(sale_class, totalopp)) %>%
  ggplot(aes(x= sale_class, y = averageprice, color = sale_class)) +
  geom_point(aes(size=totalopp)) +
  ggtitle("Medium Ticket per Sale Class") +
  xlab("Sale Class")+
  ylab("Medium Ticket") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(size=FALSE)

#Boxplot Opportunities and prices distribution per Market Class

sales_report %>% 
  group_by(market_class) %>% arrange(desc(price)) %>%
  ggplot(aes(market_class, price)) +
  scale_y_continuous(trans = "log10") +
  geom_boxplot(coef=3) +
  geom_jitter(width = 0.1, alpha = 0.2)  +
  ggtitle("Opportunities and Prices distribution per Market Class and Status")

type <- sales_report %>% group_by(market_class) %>%
  summarize(totalopp = n(), averageprice = mean(price)) %>%
  arrange(desc(averageprice))
type %>% mutate(market_class = reorder(market_class, totalopp)) %>%
  ggplot(aes(x= market_class, y = averageprice, color = market_class)) +
  geom_point(aes(size=totalopp)) +
  ggtitle("Medium Ticket per Market Class") +
  xlab("Market Class")+
  ylab("Medium Ticket") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(size=FALSE)


#Histogram Price Distribution per Market Class
sales_report %>% 
  ggplot(aes(price)) +
  geom_histogram(bins = 30, fill = "orange", col = "black") +
  ggtitle("Price distribution per Market Class (All Opportunities)") +
  facet_grid(~market_class) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Boxplot Opportunities per Market Class and Status
sales_report %>% 
  group_by(status) %>% arrange(desc(price)) %>%
  ggplot(aes(status, price, color = status)) +
  scale_y_continuous(trans = "log10") +
  geom_boxplot(coef=3) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  facet_grid(~market_class) +
  ylab("Oportunities per Market Class ") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Histogram Price Won Distribution per Market Class
sales_report %>% filter(status == "Won") %>%
  ggplot(aes(price)) +
  geom_histogram(bins = 30, fill = "green", col = "black") +
  ggtitle("Price distribution of Opportunities Won per Market Class") +
  facet_grid(~market_class) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Histogram Won Price Distribution per vendor 
sales_report %>% filter(status != "won") %>%
  ggplot(aes(price)) +
  geom_histogram(bins = 30, fill = "yellow", col = "black") +
  ggtitle("Price distribution of Won Opportunities per vendor") +
  facet_grid(~vendor) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Histogram Lost Price Distribution per vendor 
sales_report %>% filter(status != "Lost") %>%
  ggplot(aes(price)) +
  geom_histogram(bins = 30, fill = "red", col = "black") +
  ggtitle("Price distribution of Lost Opportunities per vendor") +
  facet_grid(~vendor) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Overall accuracy
                                              
y_hat <- sample(c("Won", "Lost"), length(sales_report), replace = TRUE) 
                                              
accuracy <- mean(y_hat == sales_report$status)
                                              
accuracy
                                              
#Price Mean & SD 
                                              
sales_report %>% group_by(status) %>% summarize(mean(price), sd(price)) 
                                              

#PREDICTIONS AND RESULTS
  
#Naive by Mean Method
  
  mu <- mean(edx$price)
  naive_RMSE <- RMSE(temp$price, mu) 
  
  RMSE_results = data.frame(Method = "Naive Model", RMSE = naive_RMSE) 
  RMSE_results
  
#Time effect model
  
  time_avg <- edx %>% group_by(time) %>%
    summarise(ti = mean(price - mu))
  
  qplot(ti, data = time_avg, bins = 10, color = I("blue"))
  
  
  prediction1 <- mu + temp %>% 
    left_join(time_avg, by="time") %>%
    pull(ti)
  
  RMSE_time_effect_model <- RMSE(prediction1, temp$price)
  
  RMSE_time_effect_model
  
  RMSE_results <-  bind_rows(RMSE_results, 
                             data.frame(Method = "Time Effect Model", 
                                        RMSE = RMSE_time_effect_model))
  RMSE_results %>% knitr::kable()
  
  
  #Time and Sale Class effect model
  
  sale_avg <- edx %>%
    left_join(time_avg, by='time') %>%
    group_by(sale_class) %>%
    summarize(sc = mean(price - mu - ti))
  
  qplot(sc, data = sale_avg, bins = 10, color = I("blue"))
  
  prediction2 <- temp %>%
    left_join(time_avg, by='time') %>%
    left_join(sale_avg, by='sale_class') %>%
    mutate(pred = mu + ti + sc) %>%
    pull(pred)
  
  RMSE(prediction2, temp$price)
  
  RMSE_time_and_sale_class_effect_model <- RMSE(prediction2, temp$price)
  
  RMSE_results <- bind_rows(RMSE_results, 
                            data.frame(Method="Time & Sale Class Effect Model", 
                                       RMSE = RMSE_time_and_sale_class_effect_model))
  RMSE_results %>% knitr::kable()
  
  
  #Time, Sale and Market Class Effect model
  
  market_avg <- edx %>%
    left_join(time_avg, by='time') %>%
    left_join(sale_avg, by='sale_class') %>%
    group_by(market_class) %>%
    summarize(ma = mean(price - mu - ti - sc))
  
  prediction3 <- temp %>%
    left_join(time_avg, by='time') %>%
    left_join(sale_avg, by='sale_class') %>%
    left_join(market_avg, by="market_class") %>%
    mutate(pred = mu + ti + sc + ma) %>%
    pull(pred)
  
  RMSE(prediction3, temp$price)
  
  RMSE_time_and_sale_and_market_class_effect_model <- RMSE(prediction3, temp$price)
  
  RMSE_results <- bind_rows(RMSE_results, 
                            data.frame(Method="Time, Sale & Market Class Effect Model", 
                                       RMSE = RMSE_time_and_sale_and_market_class_effect_model))
  RMSE_results %>% knitr::kable()                                             

#Time, Sale & Market Class and Status Effect Model


status_avg <- edx %>%
  left_join(time_avg, by='time') %>%
  left_join(sale_avg, by='sale_class') %>%
  left_join(market_avg, by='market_class') %>%
  group_by(status) %>%
  summarize(st = mean(price - mu - ti - sc - ma))

prediction4 <- temp %>%
  left_join(time_avg, by='time') %>%
  left_join(sale_avg, by='sale_class') %>%
  left_join(market_avg, by="market_class") %>%
  left_join(status_avg, by="status") %>%
  mutate(pred = mu + ti + sc + ma + st) %>%
  pull(pred)

RMSE(prediction4, temp$price)

RMSE_time_sale_market_class_and_status_effect_model <- RMSE(prediction4, temp$price)

RMSE_results <- bind_rows(RMSE_results, 
                          data.frame(Method="Time, Sale, Market Class & Status Effect Model", 
                                     RMSE = RMSE_time_sale_market_class_and_status_effect_model))
RMSE_results %>% knitr::kable()


#Time, Vendor, Sale & Market Class and Status Effect Model

vendor_avg <- edx %>%
  left_join(time_avg, by='time') %>%
  left_join(sale_avg, by='sale_class') %>%
  left_join(market_avg, by='market_class') %>%
  left_join(status_avg, by="status") %>%
  group_by(vendor) %>%
  summarize(ven = mean(price - mu - ti - sc - ma - st))


prediction5 <- temp %>%
  left_join(time_avg, by='time') %>%
  left_join(sale_avg, by='sale_class') %>%
  left_join(market_avg, by="market_class") %>%
  left_join(status_avg, by="status") %>%
  left_join(vendor_avg, by="vendor") %>%
  mutate(pred = mu + ti + sc + ma + st + ven) %>%
  pull(pred)

RMSE(prediction5, temp$price)

RMSE_time_sale_market_class_vendor_and_status_effect_model <- RMSE(prediction5, temp$price)

RMSE_results <- bind_rows(RMSE_results, 
                          data.frame(Method="Time, Sale, Market Class, Status & Vendor Effect Model", 
                                     RMSE = RMSE_time_sale_market_class_vendor_and_status_effect_model))
RMSE_results %>% knitr::kable()


#Regularization


lambdas <- seq(-0, 10, 0.25)
RMSES <- sapply(lambdas, function(l){
  mu <- mean(edx$price)
  ti <- edx %>%
    group_by(time) %>%
    summarize(ti = sum(price - mu)/(n()+l))
  sc <- edx %>%
    left_join(ti, by="time") %>%
    group_by(sale_class) %>%
    summarize(sc = sum(price - ti - mu)/(n()+l))
  ma <- edx %>%
    left_join(ti, by="time") %>%
    left_join(sc, by="sale_class") %>%
    group_by(market_class) %>%
    summarize(ma = sum(price - ti - sc - mu)/(n()+l))
  st <- edx %>%
    left_join(ti, by="time") %>%
    left_join(sc, by="sale_class") %>%
    left_join(ma, by="market_class") %>%
    group_by(status) %>%
    summarize(st = sum(price - ti - sc - ma - mu)/(n()+l))
  ven <- edx %>%
    left_join(ti, by="time") %>%
    left_join(sc, by="sale_class") %>%
    left_join(ma, by="market_class") %>%
    left_join(st, by="status") %>%
    group_by(vendor) %>%
    summarize(ven = sum(price - ti - sc - ma - st - mu)/(n()+l))
  
  predicted_prices <- temp %>%
    left_join(ti, by="time") %>%
    left_join(sc, by="sale_class") %>%
    left_join(ma, by="market_class") %>%
    left_join(st, by="status") %>%
    left_join(ven, by="vendor") %>%
    mutate(pred = ti + sc + ma + st + ven + mu) %>%
    pull(pred) 
  return(RMSE(predicted_prices, temp$price))
})

qplot(lambdas, RMSES)

lambda <- lambdas[which.min(RMSES)]
lambda

RMSE_results <- bind_rows(RMSE_results, 
                          data.frame(Method="Regularized Status, Market, Sale Class, Vendor & Time Effect Model", 
                                     RMSE = min(RMSES)))
RMSE_results %>% knitr::kable()

#Boxplot comparative effect models 

RMSE_results %>%  mutate(Method = reorder(Method, RMSE)) %>% 
  ggplot() + 
  geom_boxplot(aes(Method, RMSE, size= RMSE, color= Method)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

