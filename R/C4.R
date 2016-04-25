C4 <- function(){
    ##Importing data.
    x <- gather_data()

    ##Subsetting Friday's.
    x <- x %>% filter(wday(date)==6)

    ##Creating a stock price vector.
    priceVector <- x$price
    add.nas <- c(NA,NA,NA,NA)

    ##Adding NAs to price vectors. NAs are for the days where this metric
    ##can't be calculated.
    priceVector <- c(priceVector, add.nas)

    ##subsetting vector to align the 4th week price with the current price.
    priceVector <- priceVector[c(5:length(priceVector))]
    x$wk4price <- priceVector

    ##Calculating the C4 metric.
    x <- x %>% mutate(c4 = wk4price/price)

    ##Cleaning and arranging data so that stocks are ordered according to their
    ##relative strengths for each week.
    x <- x[c(1:(nrow(x)-4)),]
    x <- x %>% select(id, symbol, name, wk, date, price, c4)

    ##Ranking stocks based on their relative strength.
    x <- x %>% group_by(wk) %>% mutate(ranks = row_number(-c4))
    x <- x %>% group_by(wk) %>% arrange(ranks)
    return(x)
}
