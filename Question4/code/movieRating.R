ratings1 <- function(fullDf){

    standardise <- function(series){

        stdSeries = (series - mean(series))/sd(series)

        stdSeries
    }


    actorsType <- fullDf |> filter(role == 'ACTOR') |>
        filter(is.nan(tmdb_score)==F) |>
        filter(is.na(tmdb_score)==F) |>
        filter(is.nan(imdb_score)==F) |>
        filter(is.na(imdb_score)==F) |>

        mutate(tmdb_score = standardise(tmdb_score)) |>
        mutate(imdb_score = standardise(imdb_score)) |>
        mutate(combinedScore = tmdb_score+imdb_score) |>
        group_by(name,type) |> summarise(meanScore = mean(combinedScore))


    good <- actorsType |>  head(25000) |> mutate(standard = 'good')
    great <- good |> head (12500) |> mutate(standard = 'great')
    good <- good |> tail(12500)


    bad <- actorsType |> tail(25000) |> mutate(standard = 'bad')
    terrible <- bad |> tail(12500) |> mutate(standard = 'terrible')
    bad <- bad |> head(12500)

    plotDf <- great |> rbind(good) |>  rbind(bad) |> rbind(terrible)

    f <- plotDf |>
        ggplot()+
        geom_bar(aes(x=type,colour = type))+
        theme_bw() + theme(legend.position = "bottom") + labs(x = "",
        y = "Number of times being rated as great or terrible", title = "Movies vs shows in terms of score",
        subtitle = "There appears to be almost no rating difference between movies and shows - analysis should focus elsewhere",
        caption = "Source: Supplier") +
        facet_wrap(~standard, scales = 'free_y')

    f



}


ratings2 <- function(fullDf){

    standardise <- function(series){

        stdSeries = (series - mean(series))/sd(series)

        stdSeries
    }


    actorsType <- fullDf |> filter(role == 'ACTOR') |>
        filter(is.nan(tmdb_score)==F) |>
        filter(is.na(tmdb_score)==F) |>
        filter(is.nan(imdb_score)==F) |>
        filter(is.na(imdb_score)==F) |>

        mutate(tmdb_score = standardise(tmdb_score)) |>
        mutate(imdb_score = standardise(imdb_score)) |>
        mutate(combinedScore = tmdb_score+imdb_score) |>
        group_by(name,type) |> summarise(meanScore = mean(combinedScore))


    good <- actorsType |>  head(25000) |> mutate(standard = 'good')
    great <- good |> head (12500) |> mutate(standard = 'great')
    good <- good |> tail(12500)


    bad <- actorsType |> tail(25000) |> mutate(standard = 'bad')
    terrible <- bad |> tail(12500) |> mutate(standard = 'terrible')
    bad <- bad |> head(12500)

    plotDf <- great |> rbind(good) |>  rbind(bad) |> rbind(terrible)


    g <- plotDf |>
        ggplot()+
        geom_bar(aes(x=standard,colour = standard))+
        theme_bw() + theme(legend.position = "bottom") + labs(x = "",
                                                              y = "Combined normalized IMDB and TMDB scores", title = "Number of movie and show ratings",
                                                              subtitle = "",
                                                              caption = "Source: Supplier") +
        facet_wrap(~type, scales = 'free_y')

    g


}
