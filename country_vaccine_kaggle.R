install.packages("lubridate")
library(lubridate)
library(forecast)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)



par(mfrow=c(2,2))


###reading the raw data from the dataset
raw_country_vaccines = read.csv("D:\\Datasetssss\\archive\\country_vaccinations.csv")

###checking the column names in the raw dataset
colnames(raw_country_vaccines)
View(raw_country_vaccines)

str(raw_country_vaccines)

###Identifiers the main attributes from the raw dataset which is Country ISO code and removed the NA values 
iso_country_vaccines = (raw_country_vaccines$iso_code)
iso_total_vaccinations = (raw_country_vaccines$total_vaccinations)
iso_country_name = (raw_country_vaccines$country)
###complete.cases(iso_total_vaccinations) ### to check any NA values available
iso_clean = which(is.na(iso_total_vaccinations)) 
###iso_clean[which(is.na(iso_clean))] = 0 
iso_date = (raw_country_vaccines$date)

test_date = iso_date

test_vaccinationation = iso_total_vaccinations
test_country = iso_country_vaccines

mean(test_vaccinationation)
sd(test_vaccinationation)

quantile(test_vaccinationation)

t.test(test_vaccinationation)
is.na(test_date)
is.na(test_country)

test_date_year = format(as.Date(test_date), "%Y-%m")

is.na(test_vaccinationation)
test_vaccinationation[which(is.na(test_vaccinationation))] = 0

iso_date_new = month(iso_date)


###Cross verified whether any NA values available still?
is.na(iso_country_vaccines)

is.na(test_vaccinationation)

df_total_vaccines = data.frame(iso_date_new,test_vaccinationation)
str(df_total_vaccines)


str(iso_date)

install.packages("ggplot2")
library(ggplot2)

ggplot(data = df_total_vaccines, aes(x = iso_date, y = test_vaccinationation)) + 
  geom_col() + labs(title = "Vaccinated Data Across the Globe Till 2nd Feb-2021" , 
                    subtitle = "Graph of Vaccinated Data", x = "Dates" , y = "Total Vaccinated") +
  scale_y_continuous(labels = scales::comma)

iso_date_month_yr <- format(as.Date(iso_date), "%Y-%m")

df_total_vaccines = data.frame(iso_date_month_yr,test_vaccinationation)

ggplot(data = df_total_vaccines, aes(x = iso_date, y= test_vaccinationation)) + 
  geom_col() + labs(title = "Vaccinated Data Across the Globe Till 2nd Feb-2021" , 
            subtitle = "Graph of Vaccinated Data", x = "Date and Year" , y = "Total Vaccinated") +
  scale_y_continuous(labels = scales::comma) 

par(mfrow=c(2,2))

#####Alternate View - Bargraph - Country Vs Vaccination

iso_country_name = (raw_country_vaccines$country)
iso_total_vaccinations = (raw_country_vaccines$total_vaccinations)

df_total_country_vaccines = data.frame(iso_country_name,test_vaccinationation)


View(df_total_country_vaccines)

country_vaccine = ggplot(df_total_country_vaccines, aes(y = iso_country_name, 
                                      fill =iso_country_name )) + 
                labs(x = "Test Vaccinated" , y = "Vaccinated Countries",
                title = " Vaccinated Countries Descriptive Analysis" , 
                subtitle = "Countries where Vaccination is available and people got vaccinated")

######scale_x_continuous(labels = scales::comma) 
                             
country_vaccine = country_vaccine + geom_bar(width=1, colour="white")


country_vaccine = country_vaccine + geom_text(aes(y = iso_country_name , 
                                              label=iso_country_name),
                   stat="count", color="white",
                   hjust=1.0, size=3) 


country_vaccine = country_vaccine + theme(legend.position="none")
country_vaccine = country_vaccine + coord_flip()




#######Inferential Statistics #####

# library required for decimal_date() function  
install.packages("lubridate")
library(lubridate)
iso_date = (raw_country_vaccines$date)
iso_date[which(is.na(iso_date))] = 0
test_vaccinationation = iso_total_vaccinations
test_vaccinationation[which(is.na(test_vaccinationation))] = 0


vaccine_time_series <- ts(test_vaccinationation, start = decimal_date(ymd("2020-01-22")),  
          frequency = 365.25 / 4)  

options(scipen=999)

test_vaccinationation = format(test_vaccinationation,scientific=FALSE)

vaccine_time_series <- ts(test_vaccinationation, start = decimal_date(ymd("2020-12-22")),  
                          frequency = 365.25 / 4)  

View(vaccine_time_series)

plot(vaccine_time_series, xlab ="Monthly Data of vaccinated",  
     ylab ="Total Vaccinated",  
     main ="Vaccinated vs Monthly Data",   
     col.main ="darkgreen") 


??barplot



###Predicting the data using forecast function : 

library(forecast)
library(lubridate)

vaccine_time_series <- ts(test_vaccinationation, 
                          start = decimal_date(ymd("2020-12-22")),  
                          frequency = 365.25 / 12)  


fit <- auto.arima(vaccine_time_series)  

fit = auto.arima(test_vaccinationation,seasonal = TRUE, 
                 approximation = FALSE)

forecast(fit,5)

vaccination_seasonal = !is.null()
seasonal <- !is.null(vaccination_seasonal)


plot(forecast(fit, 5), xlab ="Monthly Data of vaccinated",  
     ylab ="Total Vaccinated",  
     main ="Vaccinated vs Monthly Data", 
     col.main ="darkgreen")  



###Alternative View do not refer below code:



iso_date_month_yr <- format(as.Date(iso_date), "%Y-%m")

df_predict_vaccines = data.frame(iso_date_month_yr,test_vaccinationation)
str(df_predict_vaccines)
View(df_predict_vaccines)

relation_predict <- lm(test_vaccinationation~iso_date_month_yr)

print(summary(relation_predict))


a <- data.frame(iso_date_month_yr = 2021-12)
result <-  predict(relation_predict,a)

### Plot the chart.
plot(test_vaccinationation,iso_date_month_yr,col = "blue",main = "Country and Year Regression",
     abline(lm(test_vaccinationation~iso_date_month_yr)),cex = 1.3,pch = 16,xlab = "Total Vaccinated",
     ylab = "Date and Month")




# output to be created as png file  
png(file ="predictiveAnalysis.png")

mts = ts(data = df_total_vaccines$test_vaccinationation, start = decimal_date(ymd("2020-12-13")), 
         frequency = 365 /7 )

View(mts)

plot(mts, xlab ="Weekly Vaccination Details",  
     ylab ="Total Vaccination",  
     main ="Days vs Vaccination",   
     col.main ="darkgreen")




