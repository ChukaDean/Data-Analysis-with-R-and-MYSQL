#Setting up of the dimension tables
cities_table <- data.frame(key= c("Fran", "LAS", "SYD", "SO", "CAP"),
                           city = c ("Frankfurt", " LOS Angeles", " Sydney", " Seoul", " Cape Town"),
                           country = c("Germany", "USA", "Australia", "S.Korea", " Cape Town"))
month_table <- data.frame(key = 1:12,
                          month =c("JANUARY", "FEBUARY", "MARCH", "APRIL;", "MAY", "JUNE", "JUYL", 
                                   "AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER"))
prod_table <- data.frame(key = c("Washing machine", "Fridge", "Vacuum Cleaner", "Microwave Oven"),
                         price = c(200,500, 400, 150))
#Generate Sales Table
sales_data <- function(recs){
     #Generate random transactions
    loc <- sample(cities_table$city, recs, replace = T, prob = c(2,1,1,2,1))
    time_month <- sample(month_table$key, recs, replace = T)
    time_year <- sample(c(2015,2016,2017,2018,2019,2020),recs, replace = T )
    prod <- sample(prod_table$key,recs, replace = T , prob = c(2,1,2,1))
    unit <- sample(c(1,2,3), recs, replace = T, prob = c(7,5,3) )
    price <- 1*prod_table[prod(),]$price
    total_amount <- unit*prod_table[prod(),]$price
    
    sales <- data.frame (Month = time_month,
                         Year = time_year,
                         City = loc,
                         Product = prod,
                         UnitSold = unit,
                         Price = price,
                         amount = total_amount)
    #Sort records by time
    sales <- sales[order (sales$Year , sales$Month),]
    row.names(sales) <- NULL
    return(sales)
}

#Creating 500 records of sales data
sales_transaction <- sales_data(500)
#View the sales transaction data
head (sales_transaction)
                         
#Building up the hyper cube (The so called revenue cube)
revenue_cube <- 
  tapply(sales_transaction$amount, sales_transaction[, c("Product", "Month", "Year", "City")],
         FUN = function(x){return(sum(x , na.rm = TRUE))})
revenue_cube
dimnames(revenue_cube)

#Rollup OLAP operation
#Here we will look at the annual revenue for each product irrespective of which city it was sold
apply(revenue_cube, c("Year","Product"),
      FUN = function (x) {return(sum(x, na.rm = TRUE))})
#Here we get even more specific; focusing not only on the annual, but on the monthly revenue for each product
#regardless of the location
apply(revenue_cube, c("Year","Month", "Product"),
      FUN = function (x) {return(sum(x, na.rm = TRUE))})
#Slice and Dice
#In slice we fix certain dimensions, in a bid to analyze the other dimensions
revenue_cube["Washing machine", "9", "2018",]
#In Dice, we limit each dimension to a certain range of values, Here let us focus on the first month of sale for 
#Microwave Oven and Washing machine.
revenue_cube[c("Washing machine", "Microwave Oven"), c("1","2","3"), ,]
#Pivot
#This involves the combination of a pair of selected dimensions
#Here, we will analyze revenue by year and month
apply (revenue_cube, c("Year", "Month"), FUN = function (x) {return (sum (x , na.rm = TRUE))})
