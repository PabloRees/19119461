monthlyAvTemp <- function(weatherDf){

    library(plotly)


    tempMonthly <- weatherDf |>
        filter(is.na(max_temp)==F) |>
        filter(is.nan(max_temp)==F) |>
        filter(is.na(min_temp)==F) |>
        filter(is.nan(min_temp)==F) |>

        group_by(month,year) |> summarise(
                                     mean_temp = mean(c(max_temp,min_temp)),
                                     max_temp = mean(max_temp),
                                     min_temp = mean(min_temp),
                                     na.rm=T)


    g <- tempMonthly |>
        ggplot() +
        geom_point(aes(x =month, y = mean_temp,color = month,frame = year), size = 2, alpha = 0.8) +
        geom_errorbar(aes(x = month, ymin = min_temp, ymax = max_temp, color = month,frame = year),) +
        theme_bw()+
        labs(title = 'Average monthly temperatures in London',
             caption = 'Data source: My friend in London',x = 'Months',
             y = 'Temperature (celcius)') +
        theme(legend.position = "top", legend.title = element_blank()) +
        theme(plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 12),
              axis.text.x = element_text(size = 12)) +
         theme(legend.position = "none")


    ggplotly(g)



}