gather_data <- function() {
    library(ws.data)
    library(lubridate)
    library(dplyr)
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

    return(all.daily)
    ##x <- merge(daily.1998, secref, by = names(daily.1998))
    ##x <- tbl_df(x)
    ##x <- x %>% select(-symbol.y)
    ##x <- x %>% mutate(year = year(v.date))
    ##x <- merge(x, yearly, by = c("year", "id"))
    ##x <- select(x, -symbol.x)
}

