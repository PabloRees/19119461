vaccinePlot <- function(df){

    library(ggplot2)

    nonAfrica = c("Asia","Europe","North America" ,"South America" ,"Oceania")


    africaDf <-  CovidDf |>  filter(continent == 'Africa') |>
        filter(is.na(total_vaccinations_per_hundred) ==FALSE ) |>
        filter(is.nan(total_vaccinations_per_hundred) == FALSE) |>
        group_by(date,continent) |>
        summarise(vaxPerHund = mean(total_vaccinations_per_hundred))


    worldDf <-  CovidDf |>  filter(continent %in% nonAfrica) |>
        filter(is.na(total_vaccinations_per_hundred) ==FALSE ) |>
        filter(is.nan(total_vaccinations_per_hundred) == FALSE) |>
        group_by(date,continent) |>
        summarise(vaxPerHund = mean(total_vaccinations_per_hundred))

    plotDf <- africaDf |> rbind(worldDf) |>
        mutate(Date = as.Date(date, format = '%Y-%m-%d')) |>
        arrange(date)


    g <- plotDf |>
        ggplot() +
        geom_line(aes(x=Date, y = vaxPerHund, color = continent),
                  alpha = 0.8, size = 1) +
        theme_bw() + theme(legend.position = "bottom") + labs(x = "",
        y = "Vaccines per hundred people", title = "Total number of vaccinations delivered per hundred people",
        subtitle = "Notice Africa is significantly lower",
        caption = "Source: National government reports")



    g





}

