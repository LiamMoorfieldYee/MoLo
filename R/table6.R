#' Computing future performance metrics for table2
#'
#' Calculating and grouping stocks based on their historical relative strengths, sub-grouping
#' by market performance, and comparing 52 week future returns.
#'
#' Resulting table is saved in the data file.
#'


table6 <- function() {


    data(y)

    x <- y


    ## Getting rid of two outliers that are skewing data.

    x <- x %>% filter(!symbol=="CKXE")
    x <- x %>% filter(!symbol=="3STTCE")


    ## only keeping Fridays. If Friday is not a trading day then the last trading
    ## day of the week is used.

    x <- x %>% group_by(id, wk) %>% filter(wday(date)==max(wday(date)))


    ##Removing NAs

    x <- x[!is.na(x$c52),]



    ## Ranking stocks based on their relative strength for each week. The
    ## stocks are grouped by week then each stock is ranked based on its
    ## ca26.ratio using the row_number function. Default method for the row_number
    ## function is to assign the lowest values the lowest rank, but we want the
    ## opposite of this so that we call the function with -ca26.ratio as its
    ## argument. The same is done for c4, c26, and market.performance variables.

    x <- x %>% group_by(wk) %>% mutate(CA26.rank = row_number(-ca26.ratio)) %>%
        mutate(c52.rank = row_number(-c52))


    ## Splitting data up into 3 groups based on market ranks and arranging based
    ## on those groups. Group 1 consists of the stocks within the strongest performing weeks,
    ## group 2 consists of the stocks in the average performing weeks, and group 4
    ## consists of the stocks in the weekest markets. Setting 3 == 2,
    ## so that group 2 consists of the middle 50% of stocks.

    x <- x %>% ungroup() %>% arrange(market.rank) %>%
        mutate(groupings = ntile(market.rank, 4))
    x <- x %>% mutate(groupings=replace(groupings, groupings==3, 2))

    ## Subsetting data into separate dataframes based on their groupings for analysis.

    x1 <- subset(x, groupings==1)
    x2 <- subset(x, groupings==2)
    x4 <- subset(x, groupings==4)


    ## Within each market rank group, breaking up stocks into deciles based on their CA26 ranks.

    x1 <- x1 %>% ungroup() %>% arrange(CA26.rank) %>%
        mutate(ca26groupings = ntile(CA26.rank, 10))
    x2 <- x2 %>% ungroup() %>% arrange(CA26.rank) %>%
        mutate(ca26groupings = ntile(CA26.rank, 10))
    x4 <- x4 %>% ungroup() %>% arrange(CA26.rank) %>%
        mutate(ca26groupings = ntile(CA26.rank, 10))


    ## Creating 52C summary averages based on 26AC group rankings. Done for each market rank
    ## group and then for the entire dataset. The results are then merged back into a final
    ## dataframe. Creating table 6.

    group1.avgs <- x1 %>% group_by(ca26groupings) %>% summarize(avg.c52.ratios = mean(c52),
                                                                avg.c52.rank = mean(c52.rank))

    all.stocks.x1 <- x1 %>% ungroup() %>% summarize(avg.c52.ratios = mean(c52),
                                                    avg.c52.rank = mean(c52.rank))
    all.stocks.x1$ca26groupings <- "all stocks"
    x1 <- rbind(group1.avgs, all.stocks.x1)

    group2.avgs <- x2 %>% group_by(ca26groupings) %>% summarize(avg.c52.ratios = mean(c52),
                                                                avg.c52.rank = mean(c52.rank))

    all.stocks.x2 <- x2 %>% ungroup() %>% summarize(avg.c52.ratios = mean(c52),
                                                    avg.c52.rank = mean(c52.rank))
    all.stocks.x2$ca26groupings <- "all stocks"
    x2 <- rbind(group2.avgs, all.stocks.x2)

    group4.avgs <- x4 %>% group_by(ca26groupings) %>% summarize(avg.c52.ratios = mean(c52),
                                                                avg.c52.rank = mean(c52.rank))

    all.stocks.x4 <- x4 %>% ungroup() %>% summarize(avg.c52.ratios = mean(c52),
                                                    avg.c52.rank = mean(c52.rank))
    all.stocks.x4$ca26groupings <- "all stocks"
    x4 <- rbind(group4.avgs, all.stocks.x4)

    x1.x2 <- left_join(x1, x2, by = "ca26groupings")
    tbl6 <- left_join(x1.x2, x4, by = "ca26groupings")



    ## Return and save the data frame

    return(devtools::use_data(tbl6))


}
