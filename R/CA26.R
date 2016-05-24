#' Loading, cleaning, and calculating performance ratios for data analysis.
#' Returns a clean data set with relevant metrics for analysis.
#'
#'
#' Merges all datasets from ws.data.
#'
#'
#'
#' Calculates
#'
#'



CA26 <- function() {

    library(ws.data)
    library(lubridate)
    library(dplyr)
    library(devtools)
    ##function for gathering data
    ##Bring in the data from 1998
    data("daily.1998")
    data("daily.1999")
    data("daily.2000")
    data("daily.2001")
    data("daily.2002")
    data("daily.2003")
    data("daily.2004")
    data("daily.2005")
    data("daily.2006")
    data("daily.2007")
    data("secref")
    data("yearly")

    all.daily <- rbind(daily.1998, daily.1999, daily.2000, daily.2001,
                       daily.2002, daily.2003, daily.2004, daily.2005,
                       daily.2006, daily.2007)


    all.daily <- mutate(all.daily, year = year(v.date),
                        wk = paste(week(v.date), year, sep = "-"))

    all.daily <- left_join(all.daily, select(yearly, -symbol),
                           by = c("year", "id"))

    all.daily <- left_join(all.daily, select(secref, -symbol), by = "id")

    all.daily <- tbl_df(all.daily)

    all.daily <- all.daily %>% rename( date = v.date)
    ## Loading up data.
    x <- all.daily

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
    ## weeks (20 days) in the future, the price of the stock 26 weeks (130 days), and the price
    ## of the stock 52 weeks (260 days) into the future.
    ##  Then calculating the C4 ratio, which is the stocks price 4
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
        mutate(wk52price = lead(price, n = 260)) %>%
        mutate(c4 = wk4price/price) %>%
        mutate(c26 = wk26price/price) %>%
        mutate(c52 = wk52price/price)


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

    y <- x %>% group_by(wk) %>% summarise(market.performance = sum(ca26.ratio, na.rm = TRUE))
    y <- y %>% mutate(market.rank = row_number(-market.performance))
    x <- left_join(x, y, by = "wk")

    ## Removing the dates that aren't being used and subsetting data

    x <- x[!is.na(x$ca26.ratio),]
    x <- x[!is.na(x$c4),]
    x <- x[!is.na(x$c26),]


    ## Cleaning and arranging data so that stocks are ordered according to their
    ## relative strengths for each week.

    y <- x %>% select( symbol, date, wk, ca26.ratio, price, variation26,
                       wk4price, wk26price, c4, c26, market.performance, market.rank,
                       wk52price, c52)




    ## Returning cleaned data set.

    if(!file.exists("/data/y.rda")){

    devtools::use_data(y)
    }
    else{}

    return(y)

}
