precip_cloud_plot <- function(weatherDf){

    plotDf <- weatherDf |>
        filter(date > ymd(20100101), date < ymd(20220101)) |>
        mutate(rainDay = ifelse(precipitation > 0,1,0)) |>
        mutate(cloudDay = ifelse(cloud_cover > 2,1,0)) |>
        group_by(month,year) |>
        summarise(rainDays = sum(rainDay), cloudDays = sum(cloudDay))


    g <- plotDf |>
        ggplot() +
        geom_point(aes(x =rainDays, y = cloudDays, color = month, size=rainDays,frame=year), alpha = 0.8) +

        theme_bw()+
        labs(title = 'Cloudy and rainy days in London',
             caption = 'Data source: My friend in London',x = 'Rainy days per month',
             y = 'Cloudy days per month') +
        theme(legend.position = "top", legend.title = element_blank()) +
        theme(plot.title = element_text(size = 14),
              plot.subtitle = element_text(size = 12),
              axis.text.x = element_text(size = 12)) +
        guides(color = F)

    ggplotly(g)


}