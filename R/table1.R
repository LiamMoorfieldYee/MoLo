#' Computing future performance metrics for table1
#'
#' Calculating and grouping stocks based on their historical relative strengths
#' and comparing 4 and 26 week future returns.
#'
#' Resulting table is saved in the data file.
#'
#'
#'
#'
#'
#'


table1 <- function() {



    data(y)

    x <- y
    ## Getting rid of two outliers that are skewing data.

    x <- x %>% filter(!symbol=="CKXE")
    x <- x %>% filter(!symbol=="3STTCE")


    ## only keeping Fridays. If Friday is not a trading day then the last trading
    ## day of the week is used.

    x <- x %>% group_by(id, wk) %>% filter(wday(date)==max(wday(date)))



    x <- x %>% group_by(wk) %>% mutate(CA26.rank = row_number(-ca26.ratio)) %>%
        mutate(c4.rank = row_number(row_number(-c4))) %>%
        mutate(c26.rank = row_number(-c26))



    ## Splitting data up into 10 groups based on decile ranks of CA26 rankings.

    x <- x %>% ungroup() %>% arrange(CA26.rank) %>%
        mutate(groupings = ntile(CA26.rank, 10))

    ## Creating 4C and 26C summary averages based on 26AC group rankings.
    ## Creating table 1.

    group.avgs <- x %>% group_by(groupings) %>% summarize(avg.C4.ratios = mean(c4),
                                                          avg.C4.rank = mean(c4.rank),
                                                          avg.c26.ratios = mean(c26),
                                                          avg.c26.rank = mean(c26.rank))

    all.stock.avg <- x %>% summarize(avg.C4.ratios = mean(c4),
                                     avg.C4.rank = mean(c4.rank),
                                     avg.c26.ratios = mean(c26),
                                     avg.c26.rank = mean(c26.rank))
    all.stock.avg <- all.stock.avg %>% mutate(groupings = 11)

    tbl1 <- rbind(group.avgs, all.stock.avg)

    if(!file.exists("data/tbl1.rda")){
        devtools::use_data(tbl1)
    }
    else{
    }

    return(tbl1)

    ## x <- x %>% group_by(wk) %>% arrange()
}
