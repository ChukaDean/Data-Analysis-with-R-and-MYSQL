install.packages("odbc"):require("odbc")
install.packages("dbplyr")
install.packages("RMYSQL")
library(dplyr)
library(odbc)
library(dbplyr)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={MySQL ODBC 8.0 Unicode Driver};",
                 Server = "localhost",Database = "data", UID = "root", PWD = passwords, Port = 3306)
library(RMySQL)
#list the fields present in cars_info
dbListFields(con, 'cars_info')
#Gets the first ten rows of the imported table
dbGetQuery(con , "SELECT * FROM cars_info LIMIT 10;")
#Gets all eight cylinder cars with miles per gallon greater than 18
dbGetQuery(con, "SELECT ID, cylinders, mpg FROM cars_info WHERE cylinders = 8 AND mpg > 18
            LIMIT 10")
#Gets the average horsepower and mpg by number of cylinder groups
dbGetQuery(con, "SELECT ID, AVG(horsepower),AVG (mpg) , cylinders FROM cars_info GROUP BY cylinders LIMIT 10")
#Gets all cars with less than eight cylinders and acceleration from 11 to 13 inclusive
dbGetQuery(con, "SELECT ID, car_name, cylinders , acceleration FROM cars_info WHERE cylinders < 8 AND acceleration
            BETWEEN 11 AND 13 LIMIT 10")
#Get the car names and horsepower of the car with three cylinders
dbGetQuery(con, "SELECT ID, car_name, horsepower, cylinders FROM cars_info WHERE cylinders = 3 Limit 10")
#Using the dplyr methodology
carss <- dplyr::tbl(con, "cars_info")
#Gets the first ten rows
head(carss , n =10)
#Gets all eight cylinder cars with miles per gallon greater than 18
carss %>% dplyr::select(ID, cylinders, mpg) %>% 
  dplyr::filter(cylinders == 8) %>% 
  dplyr::filter(mpg > 18) 
#Get the average horsepower and mpg by number of cylinder groups
carss %>% 
  dplyr::select(ID,horsepower, mpg , cylinders) %>% 
  dplyr::group_by(cylinders) %>% 
  dplyr::summarise(mean(horsepower), mean (mpg))
#Get all cars with less than eight cylinders and acceleration 11 to 13 inclusive
carss %>% 
   dplyr::select(ID, cylinders, acceleration) %>% 
   dplyr::filter(cylinders < 8) %>% 
   dplyr::filter(between(acceleration, 11,13))
#Get the car names and horsepower of the car with three cylinders
carss %>% 
  dplyr::select(ID, cylinders, car_name, horsepower) %>% 
  dplyr::filter(cylinders == 3) 

    
#Extracting the data from my database to R and Examining the distribution of MPG and Cylinders
extracted <- carss %>% 
          collect()
hist(extracted$mpg) 
hist(extracted$cylinders)
#Scatter Plot showing the relationship between weight and mpg
plot(extracted$weight, extracted$mpg, main="Scatterplot Between weight and MPG",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
boxplot(mpg~origin,data=extracted, main="MPG measurement per year",
        xlab="Year", ylab="Miles Per Gallon")
