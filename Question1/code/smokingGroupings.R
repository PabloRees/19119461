smoking <- function(CovidDf){

    library(fixest)

    smokersDf <- CovidDf |> group_by(location) |>

        filter(is.na(total_deaths_per_million) ==FALSE ) |>
        filter(is.nan(total_deaths_per_million) == FALSE) |>
        filter(is.na(icu_patients_per_million) ==FALSE ) |>
        filter(is.nan(icu_patients_per_million) == FALSE) |>
        mutate(totalDeathsPerMil = max(total_deaths_per_million),
                  icuPerMil = max(icu_patients_per_million)) |>
        group_by(location,male_smokers,female_smokers) |>
        summarize(totalDeathsPerMil = max(total_deaths_per_million),
        icuPerMil = max(icu_patients_per_million)) |>
        mutate(meanSmokers = (male_smokers + female_smokers)/2)

    smokersDf


}
