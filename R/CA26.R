CA26 <- function() {
    ##Loading up data.
    x <- gather_data()

    ##Calculating average weekly price.
    x <- x %>% group_by(id, wk) %>% mutate(avg.price = mean(price))

    ##Calculating prior 26 week moving average and C/A26 daily ratio.
    x <- x %>% group_by(id) %>%
        mutate(avg26 = roll_mean(price, 130, fill = NA)) %>%
        mutate(ca26.daily = avg.price/avg26)




    ## Creating the C/A26 ratio by taking the mean of the weekly
    ## C/A26 daily ratio.
    x <- x %>% group_by(id, wk) %>% mutate(ca26.ratio = mean(ca26.daily))

    ## Removing the dates that aren't being used and subsetting data
    ## only keeping Fridays.
    x <- x[!is.na(x$ca26.ratio),]
    x <- x %>% filter(wday(date)==6)

    ## Ranking stocks based on their relative strength.

    x <- x %>% group_by(wk) %>% mutate(ranks = row_number(-ca26.ratio))
    x <- x %>% group_by(wk) %>% arrange(ranks)

    ## Cleaning and arranging data so that stocks are ordered according to their
    ## relative strengths for each week.

    x <- x %>% select(id, symbol, name, date, wk, ca26.ratio, price, ranks)
    x <- x %>% group_by(wk) %>% arrange(ranks)


    return(x)
}
