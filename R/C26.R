C26 <- function(){
    ##Gathering data.
    x <- gather_data()

    ## Subsetting Fridays. wday is a lubridate function that turns the date into
    ## a factor (Sunday = 0 and Saturday = 7). Filtering by wday(date) == 6 subsets
    ## data so that there is only one day per week.

    x <- x %>% filter(wday(date)==6)

    ## Grabbing the price of each stock 4 weeks in advance using the lead
    ## function. Where price is the variable being stored, "n" is the number
    ## of weeks being jumped (really number of rows being jumped) and date
    ## is how the id groups are being ordered.

    x <- x %>% group_by(id) %>%
        mutate(wk26price = lead(price, n = 26, order_by = date))

    ##Calculating the C26 ratio.

    x <- x %>% mutate(c26 = wk26price/price)

    ##Cleaning and arranging data so that stocks are ordered according to their
    ##relative strengths for each week.

    x <- x[!is.na(x$c26),]
    x <- x %>% select(id, symbol, name, wk, date, price, c26)

    ##Ranking stocks based on their relative strength.

    x <- x %>% group_by(wk) %>% mutate(ranks = row_number(-c26))
    x <- x %>% group_by(wk) %>% arrange(ranks)


    return(x)
}
