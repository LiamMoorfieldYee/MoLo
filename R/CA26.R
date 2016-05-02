CA26 <- function() {
    ## Loading up data.
    x <- gather_data()

    ## Calculating average weekly price for each stock.

    x <- x %>% group_by(id, wk) %>% mutate(avg.price = mean(price))



    ## Calculating prior 26 week moving average and C/A26 daily ratio. The variation
    ## rank is also computed here. The "roll" functions are looking at the past
    ## 26 weeks, ending with the current price,  to calculate the average mean and
    ## standard deviation. If a day's mean or standard deviation cannot be
    ## calculated then NA's are introduced. Align = "right" includes the current
    ## data point in the calculations. The daily26.ratio is calculated by taking
    ## the current weekly average (avg.price) and dividing it by the past 26 week price
    ## average (avg26). Calculating price of the stock four
    ## weeks (20 days) in the future, and the price of the stock 26 weeks (130 days)
    ## into the future. Then calculating the C4 ratio, which is the stocks price 4
    ## weeks into the future divided by the current price, and the C26 ratio which
    ## is calculated by dividing the current price into the stocks price 26 weeks
    ## into the future.

    x <- x %>% group_by(id) %>% arrange(date) %>%
        mutate(avg26 = roll_mean(price, 130, fill = NA, align = "right")) %>%
        mutate(std26 = roll_sd(price, 130, fill = NA, align = "right")) %>%
        mutate(ca26.daily = avg.price/avg26) %>%
        mutate(variation26 = std26/avg26) %>%
        mutate(wk4price = lead(price, n = 20)) %>%
        mutate(wk26price = lead(price, n = 130)) %>%
        mutate(c4 = wk4price/price) %>%
        mutate(c26 = wk26price/price)


    stopifnot(
        round(subset(x, date == "1998-01-02" & symbol == "SPLN.", select = "c4"), 6) == 2.016000,
        round(subset(x, date == "2004-09-24" & symbol == "GOOG", select = "c4"), 6) == 1.438955,
        round(subset(x, date == "1998-01-02" & symbol == "IBM", select = "c26"), 6) == 1.121893,
        round(subset(x, date == "2004-09-01" & symbol == "GOOG", select = "c26"), 6) == 1.808978
    )






    ## Creating the C/A26 ratio by taking the mean of the weekly
    ## C/A26 daily ratio. This ratio is the average daily ca26 ratios for the week.

    x <- x %>% group_by(id, wk) %>% mutate(ca26.ratio = mean(ca26.daily))

    ## Calculating the overall market performance in the last 6 months measurement
    ## by summing all the ca26.ratios over the past 26 weeks. This is done by using
    ## the summarise command for each week. The sum for each week is assigned to y
    ## and then y is mergerd back to x using left_join.

    y <- x %>% group_by(wk) %>% summarise(market.rank = sum(ca26.ratio, na.rm = TRUE))
    x <- left_join(x, y, by = "wk")

    ## Removing the dates that aren't being used and subsetting data
    ## only keeping Fridays. If Friday is not a trading day then the last trading
    ## day of the week is used.

    x <- x[!is.na(x$ca26.ratio),]
    x <- x %>% group_by(id, wk) %>% filter(wday(date)==max(wday(date)))

    ## Ranking stocks based on their relative strength for each week. The
    ## stocks are grouped by week then each stock is ranked based on its
    ## ca26.ratio using the row_number function. Default method for the row_number
    ## function is to assign the lowest values the lowest rank, but we want the
    ## opposite of this so that we call the function with -ca26.ratio as its
    ## argument.

    x <- x %>% group_by(wk) %>% mutate(CA26.rank = row_number(-ca26.ratio))
    x <- x %>% group_by(wk) %>% arrange(ranks)

    ## Cleaning and arranging data so that stocks are ordered according to their
    ## relative strengths for each week.

    x <- x %>% select( symbol, name, date, wk, ca26.ratio, price, variation26,
                      ranks, wk4price, wk26price, c4, c26, CA26.rank)
    x <- x %>% group_by(wk) %>% arrange(ranks)


    return(x)
}
