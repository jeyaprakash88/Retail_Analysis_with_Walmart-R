# installing packages
install.packages("car") # for VIF ( Variance Inflation Factor)
install.packages("corrplot") # for corrplot
install.packages("caTools") # for splitting dataset
install.packages("ggplot2")
install.packages("MLmetrics")
install.packages("magrittr")
install.packages("dplyr")
install.packages('lubridate')
install.packages("tidyr")
install.packages('DMwR')

# calling library
library(car) # for VIF ( Variance Inflation Factor)
library(corrplot) # for corrplot
library(caTools) # for splitting dataset
library(ggplot2)
library(MLmetrics)
library(magrittr)
library(tidyr)
library(dplyr)
library(lubridate)

# import data from excel
getwd()
df <- read.csv("Walmart_Store_sales.csv")

# Visualize the dataset
View(df)
dim(df)
str(df)
summary(df)

#converting Store and Holiday flag from integer to factor value
df$Store <- as.factor(df$Store)
df$Holiday_Flag <- as.factor(df$Holiday_Flag)
str(df)

#checking for NA values
df[is.na(df)] <- 0

# finding total sales by each store
Total_sales = df %>% group_by(Store) %>% summarise(totalsales = sum(Weekly_Sales))
View(Total_sales)

# Findind max value of 
max_sales <- Total_sales[Total_sales$totalsales == max(Total_sales$totalsales),]
paste("The maximum sales done by store ", max_sales$Store," and total sale is", max_sales$totalsales)

#creating data frame to mean, SD and variance
df_mean = df %>% group_by(Store) %>% summarise(mean = mean(Weekly_Sales))
df_sd = df %>% group_by(Store) %>% summarise(standard_deviation = sd(Weekly_Sales))
df_variance = df %>% group_by(Store) %>% summarise(variance = var(Weekly_Sales))

#Merging of all the data frames
df1= merge(Total_sales, df_mean, by="Store", overwrite=F)
df1= merge(df1, df_sd, by="Store", overwrite=F)
df1= merge(df1, df_variance, by="Store", overwrite=F)
df1$co_eff <- df1$standard_deviation/df1$mean 
#finding coeff of variance sd/mean
View(df1)


#finding max standard deviation
max_SD <- df1[df1$standard_deviation == max(df1$standard_deviation),]
paste("The store with maximum standard deviation is", max_SD$Store,"and SD of", round(max_SD$standard_deviation, digits = 2), "with coefficient of mean to standard deviation is", round(max_SD$co_eff, digits = 2))


#store with good quarterly growth in Q3-2012
mart <- df
mart$Date <- dmy(mart$Date)
mart <- mart %>% mutate(Year=year(Date),Quarter=quarter(Date), Month=month(Date),Semester=semester(Date))
mart <- mart %>% mutate(Qt=paste(Year,'Q',Quarter, sep = ''))
View(mart)
q3_sum = mart %>% group_by(Store, Qt) %>% summarise(Quarter_sales = sum(Weekly_Sales))
View(q3_sum)
q3_sum <- q3_sum %>% mutate(Lagsales=lag(Quarter_sales,4))
q3_sum <- q3_sum %>% mutate(Comparison=(Quarter_sales/Lagsales)*100) %>% arrange(desc(Comparison))
View(q3_sum)
q3_sum <- q3_sum %>% filter(Qt=='2012Q3') %>% mutate(Comparison=(Quarter_sales/Lagsales)*100) %>% arrange(desc(Comparison))
View(q3_sum)

paste("The store which has good quarterly growth rate in Q3’2012 is", q3_sum[1,]$Store,"with sale of", q3_sum[1,]$Quarter_sales,"and highest percentage of", round(q3_sum[1,]$Comparison),"%")


#holidays which have higher sales than the mean sales in non-holiday season for all stores together
df_non_holiday = df %>% group_by(Holiday_Flag) %>% summarise(mean = mean(Weekly_Sales)) %>% filter(Holiday_Flag==0)
higher_sales <- filter(df, Weekly_Sales > df_non_holiday$mean & Holiday_Flag==1)
View(higher_sales)


#monthly and semester view of sales in units
monthly_sale <- mart %>% group_by(Month) %>% summarise(Mean = mean(Weekly_Sales))
View(monthly_sale) # December month has the highest average sales compared to other months followed by november

semester_sale <- mart %>% group_by(Semester) %>% summarise(Mean = mean(Weekly_Sales))
View(semester_sale)
paste("The total sale for first semester is",round(semester_sale[1,]$Mean, digits = 2),"The total sale for second semester is",round(semester_sale[2,]$Mean, digits = 2))
# from above we conclude that semester 2 has highest average sale since november and december contribute most


# 1.Data exploration and data cleaning

#Date split into Year, Month and Day. 
#Retrieving Week# from Date

#Statistical Model
View(df)

# Check for NA and missing values and dropping it
numberOfNA = dim(df[is.na(dataset),])[1]
numberOfNA
if(numberOfNA > 0)
{
  cat('Number of missing values: ', numberOfNA)
  cat('\nRemoving missing values...')
  df = df[complete.cases(df), ]
}


# Converting dates (Year - Month - Day) to 3 separate column 
wal_mart <- df
wal_mart$Date <- dmy(wal_mart$Date)
wal_mart <- wal_mart %>% mutate(Day=day(Date),Month=month(Date),Year=year(Date), Week=week(Date))
wal_mart$Date <- NULL
wal_mart$store <- as.numeric(wal_mart$Store)
wal_mart$Store <- NULL
View(wal_mart)
str(wal_mart)
par(mfrow=c(3,2))
hist(wal_mart$store, col = 'light green', main = "Stores")
hist(wal_mart$Temperature, col = 'light green', main = "Temperature")
hist(wal_mart$Fuel_Price, col = 'light green', main = "Fuel Price")
hist(wal_mart$CPI, col = 'light green', main = "CPI")
hist(wal_mart$Unemployment, col = 'light green', main = "Unemployment")

#2.Data Visualisation
par(mfrow = c(1,2))
hist(wal_mart$Weekly_Sales, col = 'light green', main = "Weekly Sales Original", xlab = "Weekly Sales")
hist(log(wal_mart$Weekly_Sales), col = 'light green', main = "Weekly Sales Transformed", xlab ='log(Weekly Sales)')

par(mfrow = c(1,1))
ggplot(wal_mart, aes(x = Month,y = Weekly_Sales )) + 
  geom_col() +
  facet_wrap(~Year) + 
  ggtitle("Weekly Sales Distribution")
#Training Data is missing for January in 2010, November and December in 2012.There are weeks when Sales peaks in the festive months of November and December.

# Checking correltaion of price with other variables
wal_mart$Holiday_Flag <- as.numeric(wal_mart$Holiday_Flag)
str(wal_mart)
corr = cor(wal_mart[, 1:11])
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))


#3.Data Modeling
# Ho: There is no linear relationship between Weekly_Sales with  CPI, unemployment, and fuel price 
# Ha: There is linear relationship between Weekly_Sales with  CPI, unemployment, and fuel price 

model1 <- lm (Weekly_Sales ~ CPI, data=df)
summary(model1)
# from above P<alpha, TRUE ,so their is relationship
model2 <- lm (Weekly_Sales ~ Unemployment, data=df)
summary(model2)
# from above P<alpha, TRUE ,so their is relationship
model3 <- lm (Weekly_Sales ~ Fuel_Price, data=df)
summary(model3)
# from above P<alpha, FALSE ,so their is no relationship


# Creating New coulmn for model building By Droping Store and Date Coulmn
col.vars <- c('Holiday_Flag','Temperature', 'Fuel_Price', 'CPI', 'Unemployment','Weekly_Sales')
datamodel <- df[,col.vars]

# Model Building
model4 <- lm (Weekly_Sales ~ ., wal_mart)
summary(model4) 
Rsqd1 <- summary(model4)$r.squared
Rsqd1 # 0.9274344

predicted_y1 <- predict(model4, wal_mart)
RMSE1 = sqrt(mean((df$Weekly_Sales - predicted_y1)^2))
RMSE1 # 152017.3

#Summary-In this model we use all varriables to check the impact 

# considering those independent where the value is higher

model5 <- lm(Weekly_Sales ~ Unemployment + CPI+ Temperature, datamodel)
summary(model5)
Rsqd2 <- summary(model5)$r.squared
Rsqd2 # 0.02423897

predicted_y2 <- predict(model5, datamodel)

RMSE2 = sqrt(mean((datamodel$Weekly_Sales - predicted_y2)^2)) 
RMSE2 # 557441.5

#Summary in model5 we are taken the high value that the co-related to Weekly_Sales like Unemployment & CPI & Temperature

model6 <- lm(Weekly_Sales ~ log(Unemployment) + CPI+ Temperature, datamodel)
summary(model6)
Rsqd3 <- summary(model6)$r.squared
Rsqd3 # 0.02270446

predicted_y3 <- predict(model6, datamodel)

RMSE3 = sqrt(mean((datamodel$Weekly_Sales - predicted_y3)^2))
RMSE3 # 557879.7

#Summary- In model6 we take the log of Unemployment & CPI & Temperature for better linearity

model7 <- lm(Weekly_Sales ~ Unemployment + CPI+ Temperature + Holiday_Flag , datamodel)
summary(model7)
Rsqd4 <- summary(model7)$r.squared
Rsqd4 # 0.02538059
predicted_y2 <- predict(model7, datamodel)

RMSE4 = sqrt(mean((datamodel$Weekly_Sales - predicted_y2)^2)) 
RMSE4 # 557115.3
#Summary- In model7 we take Unemployment & CPI & Temperature & Holiday_Flag for linear model  
#==============================================================================
## Comparing all models
#==============================================================================

Rsqd_ <- c(Rsqd1,Rsqd2,Rsqd3,Rsqd4)
RMSE_ <- c(RMSE1,RMSE2,RMSE3,RMSE4)

Model_Validation <- cbind(Rsqd_,RMSE_)
rownames(Model_Validation) <- c("model4 - (all variables)",
                                "model5 - (Weekly_Sales on  CPI  & Temperature)",
                                "model6 - (Weekly_Sales on log(Unemployment) & CPI  & Temperature)",
                                "model7 - (Weekly_Sales on Unemployment + CPI+ Temperature + Holiday_Flag)")

Model_Validation
#Summery- By model validation technique we can see which model would perfome best between all models
# from above model 4 is the best fit with high R square and low error.