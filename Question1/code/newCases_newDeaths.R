deathVsCasesPlot <- function(df){

    library(ggplot2)

    rm(africaDf)
    rm(worldDf)

    nonAfrica = c("Asia","Europe","North America" ,"South America" ,"Oceania")


    africaDf <-  CovidDf |>  filter(continent == 'Africa') |>
        filter(is.na(new_cases_smoothed_per_million) ==FALSE ) |>
        filter(is.nan(new_cases_smoothed_per_million) == FALSE) |>
        filter(is.na(new_deaths_smoothed_per_million) ==FALSE ) |>
        filter(is.nan(new_deaths_smoothed_per_million) == FALSE) |>
        group_by(date,continent) |>
        summarise(newCasesPerMil = mean(new_cases_smoothed_per_million),
                  newDeathsPerThou = 100*mean(new_deaths_smoothed_per_million))


    worldDf <-  CovidDf |>  filter(continent %in% nonAfrica) |>
        filter(is.na(new_cases_smoothed_per_million) ==FALSE ) |>
        filter(is.nan(new_cases_smoothed_per_million) == FALSE) |>
        filter(is.na(new_deaths_smoothed_per_million) ==FALSE ) |>
        filter(is.nan(new_deaths_smoothed_per_million) == FALSE) |>
        mutate(continent = 'Non-Africa') |>
        group_by(date,continent) |>
        summarise(newCasesPerMil = mean(new_cases_smoothed_per_million),
                  newDeathsPerThou = 100*mean(new_deaths_smoothed_per_million))

    plotDf <- africaDf |> rbind(worldDf) |>
        mutate(date = as.Date(date, format = '%Y-%m-%d')) |>
        gather(Label,Value, -date, -continent) |>
        arrange(date)


    g <- plotDf |>
        ggplot() +
        geom_line(aes(x=date, y = Value, color = Label),
                  alpha = 0.8, size = 1) +
        theme_bw() + theme(legend.position = "bottom") + labs(x = "",
        y = "New deaths per 10 000/ new cases per million ", title = "New deaths and cases in and out of Africa",
        subtitle = "Notice Africa''s deaths stray further above the cases line than in non-Africa",
        caption = "Source: National government reports") +
        facet_wrap(~continent, scales = 'free_y')



    g



}