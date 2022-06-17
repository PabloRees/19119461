hospitalizationDist <- function(CovidDf){

    plotDf <- CovidDf |> group_by(location) |>
        arrange("hospital_beds_per_thousand" )

    ping <- CovidDf |> group_by(location) |> summarise(hosp = sd(hospital_beds_per_thousand))


    hospDf <- CovidDf |>
        filter(is.na(hospital_beds_per_thousand) == FALSE ) |>
        filter(is.nan(hospital_beds_per_thousand) == FALSE) |>
        filter(is.na(icu_patients_per_million) ==FALSE ) |>
        filter(is.nan(icu_patients_per_million) == FALSE) |>

        filter(max(hospital_beds_per_thousand) != min (hospital_beds_per_thousand)) |>

        mutate(globalHospQuart =
        ifelse(hospital_beds_per_thousand < quantile(hospital_beds_per_thousand,0.25),1,
               ifelse(hospital_beds_per_thousand < median(hospital_beds_per_thousand),2,
                ifelse(hospital_beds_per_thousand < quantile(hospital_beds_per_thousand,0.75),3,
                       4)))) |>

        group_by(location) |>
        summarize(avgHospQuart = mean(globalHospQuart)) |>

        arrange(desc(avgHospQuart))

    top2 <-  hospDf |> head(2)
    middle2 <- hospDf |> slice((round(nrow(hospDf)/2)):(round(nrow(hospDf)/2)+1))
    bottom2 <-  hospDf |> tail(2)

    distBeds <- top2 |>  rbind(middle2) |> rbind(bottom2)

    rm(top2,middle2,bottom2,hospDf)

    distBeds


}

hospBedsPlot <- function(CovidDf){

    distBeds <- hospitalizationDist(CovidDf)

    plotDf <- CovidDf |> filter(location %in% distBeds$location) |>
        select(c(date,location,hospital_beds_per_thousand,
        icu_patients_per_million))|>

        mutate(hospital_beds_per_hundred = hospital_beds_per_thousand*10) |>

        mutate(date = as.Date(date, format = "%Y-%m-%d")) |>

        gather(Label,Value, -date, -location, -hospital_beds_per_thousand) |>

        arrange(date)



    g <- plotDf |>
    ggplot() +
    geom_line(aes(x=date, y = Value, color = Label),
        alpha = 0.8, size = 1) +
    theme_bw() + theme(legend.position = "bottom") + labs(x = "",
     y = "Hospital beds and ICU patients  ", title = "New hospital beds and ICU admissions",
     subtitle = "Top 2=Aus & Bulg, mid 2=Cyp & Swed, low 2 = Swi & UK",
     caption = "Only 36 countries had data") +
        facet_wrap(~location, scales = 'free_y')

    g




}