plotCompetitiveness <- function(playersDf,rankingsDf){

    try_my_func <- function(x) {
        tryCatch(my_func(x), error = function(err){FALSE})
    }


    fullDf <- rankingsDf |>
        left_join(playersDf, by = c('player' = 'player_id')) |>

        filter(is.na(name_first)==F) |>
        filter(is.nan(name_first)==F) |>
        filter(is.na(name_last)==F) |>
        filter(is.na(name_last)==F) |>
        filter(is.na(dob)==F) |>
        filter(is.na(dob)==F) |>

        filter( tryCatch(ymd(dob),error=FALSE ) != FALSE) |>

        mutate(ranking_date = ymd(ranking_date), dob = ymd(dob),
               year = year(ranking_date),
               age = as.period(interval(start = dob, end = ranking_date))$year)


    bestInYear <- fullDf |> group_by(name_last,year,age) |>
        summarise(meanRank = mean(rank)) |> arrange(year,meanRank)|>
        ungroup() |>
        group_by(year) |> slice(1:2) |>
        mutate(rank = ifelse(meanRank==min(meanRank),1,2)) |> ungroup() |>
        mutate(rank = as.character(rank)) |>
        arrange(year) |>
        mutate(label = if_else(year == max(year),
                               as.character(rank), NA_character_))

    pacman::p_load(ggrepel)
    library(ggrepel)

    g <- bestInYear |>
        ggplot() +
        geom_smooth(aes(x=year, y = meanRank,color=rank),
                  alpha = 0.8, size = 1,n=150,se=F) +
        geom_point(aes(x=year,y=meanRank,colour = name_last))+
        theme_bw() + theme(legend.position = "bottom") + labs(x = "",
         y = "Smoothed mean rank of top ranked players through out the year",
         title = "Comparing mean rank with actual rank",
         subtitle = "Years in which mean rank is higher are years in which Tennis was more competitive",
        caption = "Source: Jeff Sackmann")+
        geom_label_repel(aes(x=year,y=meanRank,label = label),
                         nudge_x = 1,
                         na.rm = TRUE)


    g




}
