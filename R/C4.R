
C4 <- function(){
    ## Importing data.
    x <- gather_data()

    ## Subsetting Friday's. If Friday is not available the next closest day
    ## is used.

    x <- x %>% group_by(id, wk) %>% filter(wday(date)==max(wday(date)))

    ## Grabbing the price of each stock 4 weeks in advance using the lead
    ## function. Where price is the variable being stored, "n" is the number
    ## of weeks being jumped (really number of rows being jumped) and date
    ## is how the id groups are being ordered.

    x <- x %>% group_by(id) %>%
        mutate(wk4price = lead(price, n = 4, order_by = date))


    ##Calculating the C4 metric.
    x <- x %>% mutate(c4 = wk4price/price)

    stopifnot(
        round(subset(x, date == "1998-01-02" & symbol == "SPLN.", select = "c4"), 6) == 1.808511,
        round(subset(x, date == "2004-09-24" & symbol == "GOOG", select = "c4"), 6) == 1.438955
    )

    ##Cleaning and arranging data so that stocks are ordered according to their
    ##relative strengths for each week.
    x <- x[!is.na(x$c4),]
    x <- x %>% select(id, symbol, name, wk, date, price, wk4price, c4)

    ##Ranking stocks based on their relative strength.
    x <- x %>% group_by(wk) %>% mutate(ranks = row_number(-c4))
    x <- x %>% group_by(wk) %>% arrange(ranks)
    return(x)
}
