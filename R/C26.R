C26 <- function(){
    ##Gathering data.
    x <- gather_data()

    ## Subsetting Fridays. wday is a lubridate function that turns the date into
    ## a factor (Sunday = 0 and Saturday = 7). Filtering by wday(date) == 6 subsets
    ## data so that there is only one day per week.

    x <- x %>% group_by(id, wk) %>% filter(wday(date)==max(wday(date)))

    ## Grabbing the price of each stock 4 weeks in advance using the lead
    ## function. Where price is the variable being stored, "n" is the number
    ## of weeks being jumped (really number of rows being jumped) and date
    ## is how the id groups are being ordered.

    x <- x %>% group_by(id) %>%
        mutate(wk26price = lead(price, n = 26, order_by = date))

    ##Calculating the C26 ratio.

    x <- x %>% mutate(c26 = wk26price/price)

    stopifnot(
        round(subset(x, wk == "1-2004" & symbol == "IBM", select = "c26"), 6) == .950737,
        round(subset(x, wk == "5-2005" & symbol == "GOOG", select = "c26"), 6) == 1.430564
    )
    ##Cleaning and arranging data so that stocks are ordered according to their
    ##relative strengths for each week.

    x <- x[!is.na(x$c26),]
    x <- x %>% select(id, symbol, name, wk, date, price, wk26price, c26)

    ##Ranking stocks based on their relative strength.

    x <- x %>% group_by(wk) %>% mutate(ranks = row_number(-c26))
    x <- x %>% group_by(wk) %>% arrange(ranks)


    return(x)
}
