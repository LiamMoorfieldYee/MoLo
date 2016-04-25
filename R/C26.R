C26 <- function(){
    ##Gathering data.
    x <- gather_data()

    ##Subsetting Fridays.
    x <- x %>% filter(wday(date)==6)

    ##Creating price vector.
    priceVector <- x$price

    ##Adding NAs to price vectors. NAs are for the days where this metric
    ##can't be calculated (i.e. the last 26 days).
    add.nas <- rep(NA, 26)
    priceVector <- c(priceVector, add.nas)

    ##subsetting vector to align the 4th week price with the current price.
    priceVector <- priceVector[c(27:length(priceVector))]
    x$wk26price <- priceVector

    ##Calculating the C26 ratio.
    x <- x %>% mutate(c26 = wk26price/price)

    ##Cleaning and arranging data so that stocks are ordered according to their
    ##relative strengths for each week.
    x <- x[c(1:(nrow(x)-26)),]
    x <- x %>% select(id, symbol, name, wk, date, price, c26)

    ##Ranking stocks based on their relative strength.
    x <- x %>% group_by(wk) %>% mutate(ranks = row_number(-c26))
    x <- x %>% group_by(wk) %>% arrange(ranks)


    return(x)
}
