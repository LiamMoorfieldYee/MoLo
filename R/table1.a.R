table1 <- function() {
    x <- CA26()

    ## Removing a couple outliers in the data.
    x <- x[!is.na(x$c52),]
    x <- x %>% filter(!symbol=="CKXE")
    x <- x %>% filter(!symbol=="3STTCE")

    ## only keeping Fridays. If Friday is not a trading day then the last trading
    ## day of the week is used.

    x <- x %>% group_by(id, wk) %>% filter(wday(date)==max(wday(date)))

    ## Ranking stocks based on their relative strength for each week. The
    ## stocks are grouped by week then each stock is ranked based on its
    ## ca26.ratio using the row_number function. Default method for the row_number
    ## function is to assign the lowest values the lowest rank, but we want the
    ## opposite of this so that we call the function with -ca26.ratio as its
    ## argument.

    x <- x %>% group_by(wk) %>% mutate(CA26.rank = row_number(-ca26.ratio)) %>%
        mutate(c52.rank = row_number(-c52))



    ## Splitting data up into 10 groups based on decile ranks of CA26 rankings.

    x <- x %>% ungroup() %>% arrange(CA26.rank) %>%
        mutate(groupings = ntile(CA26.rank, 10))

    ## Creating 4C and 26C summary averages based on 26AC group rankings.
    ## Creating table 1.

    group.avgs <- x %>% group_by(groupings) %>% summarize(avg.c52.ratios = mean(c52),
                                                          avg.c52.rank = mean(c52.rank))

    all.stock.avg <- x %>% summarize(avg.c52.ratios = mean(c52),
                                     avg.c52.rank = mean(c52.rank))

    all.stock.avg <- all.stock.avg %>% mutate(groupings = "all stocks")

    table.data <- rbind(group.avgs, all.stock.avg)

    return(table.data)

    ## x <- x %>% group_by(wk) %>% arrange()
}
