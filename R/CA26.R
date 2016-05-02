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
    ## average (avg26).

    x <- x %>% group_by(id) %>%
        mutate(avg26 = roll_mean(price, 135, fill = NA, align = "right")) %>%
        mutate(std26 = roll_sd(price, 135, fill = NA, align = "right")) %>%
        mutate(ca26.daily = avg.price/avg26) %>%
        mutate(variation26 = std26/avg26)



    ## Creating the C/A26 ratio by taking the mean of the weekly
    ## C/A26 daily ratio. This ratio is the average daily ca26 ratios for the week.

    x <- x %>% group_by(id, wk) %>% mutate(ca26.ratio = mean(ca26.daily))

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

    x <- x %>% group_by(wk) %>% mutate(ranks = row_number(-ca26.ratio)) %>%
        mutate()
    x <- x %>% group_by(wk) %>% arrange(ranks)

    ## Cleaning and arranging data so that stocks are ordered according to their
    ## relative strengths for each week.

    x <- x %>% select(id, symbol, name, date, wk, ca26.ratio, price, variation26,
                      ranks)
    x <- x %>% group_by(wk) %>% arrange(ranks)


    return(x)
}
