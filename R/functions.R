#' -----Top teams by win percentage
#'
#' A function returning top 10 teams by win percentage
#'
#' @param data a data.frame with W et G columns
#' @return ordered tibble by win percentage
#' @import dplyr
#' @export
best_10_winpct<-function(data){
  x<-data |>
    select(TEAM,G,W) |>
    mutate(win_pct=(W/G)*100) |>
    arrange(desc(win_pct)) |>
    slice_head(n=10)

  return(x)
}

#'------Top offense by metrics
#'
#' A function retunring the best teams by offensive metric chosen
#'
#' @param data dataset cbb
#' @param n number of teams
#' @param metric options of offensive metrics between ("ADJOE","EFG_O","2P_O","3P_O")
#'
#' @return ordered tibble
#' @import dplyr
#' @export
top_offense<-function(data,n=10,metric=c("ADJOE","EFG_O","2P_O","3P_O")){

  metric<-match.arg(metric)

  x<-data |>
    select(TEAM,all_of(metric)) |>
    arrange(desc(.data[[metric]])) |>
    slice_head(n=n)

  return(x)
}


#'------Top defense by metrics
#'
#' A function returning the best teams by defensive metric chosen
#'
#' @param data dataset cbb
#' @param n number of teams
#' @param metric options of offensive metrics between ("ADJDE","EFG_D","TORD")
#'
#' @return ordered tibble
#' @import dplyr
#' @export

top_defense<-function(data,n=10,metric=c("ADJDE","EFG_D","TORD")){

  metric<-match.arg(metric)

  x<-data |>
    select(TEAM,all_of(metric)) |>
    arrange((.data[[metric]])) |>
    slice_head(n=n)

  return(x)
}


#'------Team profile
#'
#' A function retunring a polygon plot of the chosen team overall performance
#'
#' @param data dataset cbb
#' @param team any of the 365 ncaa teams the 2025-26 season

#'
#' @return polygon chart
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @export

team_profil<-function(data,team){

  valid_team<-unique(data$TEAM)

  if (!(team %in% valid_team)) {
    stop(paste("Erreur : L'équipe '", team, "' n'existe pas dans le dataset."))}

  df <- data|>
    filter(TEAM == team) |>
    select(TEAM,ADJOE,ADJDE, ADJ_T,TORD,ORB,DRB,`2P_O`,`3P_O`) |>
    pivot_longer(-TEAM, names_to = "metric", values_to = "value")

  x<-ggplot(df, aes(x = metric, y = value, group = TEAM)) +
    geom_polygon(fill = "skyblue", alpha = 0.4) +
    geom_point(size = 3) +
    coord_polar() +
    labs(
      title = paste("Team Profil —", team),
      x = "",
      y = ""
    ) +
    theme_minimal()

  return(x)
}

#'------Filter conference
#'
#' A function filtering the dataset by conference
#'
#' @param data dataset cbb
#' @param conf the desired conference (ex: "ACC","B12","SEC",...)
#'
#' @return tibble with teams from the selected conference
#' @import dplyr
#' @export

filter_conference<-function(data,conf="ACC"){
  valid_conference<-unique(data$CONF)

  if (!(conf %in% valid_conference)) {
    stop(paste("Erreur : La conférence '", conf, "' n'existe pas dans le dataset."))}

  x<- data |>
    filter(CONF==conf)

  return(x)
}

#'-------Top conference
#'
#' A function to rank conference by desired metrics
#'
#' @param data dataset cbb
#' @param metric the desired metric for the ranking from this list:c("ADJOE","ADJDE","EFG_O","EFG_D")
#' @param desc boolean TRUE/FALSE, TRUE by default,
#'
#' @return tibble with the 31 conferences ranked
#' @import dplyr
#' @export

top_conference<-function(data,metric=c("ADJOE","ADJDE","EFG_O","EFG_D"),desc=TRUE){

  metric<-match.arg(metric)

  x<-data |>
    group_by(CONF) |>
    summarize(ADJOE=mean(ADJOE),
              ADJDE=mean(ADJDE),
              EFG_O=mean(EFG_O),
              EFG_D=mean(EFG_D)
    )

  if (desc==TRUE){
    x<-x|>arrange(desc(.data[[metric]]))
  } else {
    x<-x|>arrange(.data[[metric]])
  }


  return(x)
}

#' Plot conference ranking based on a selected metric
#'
#' This function creates a simple horizontal bar chart showing the average
#' value of a selected performance metric for each NCAA conference.
#' It is designed to be used with the output of `top_conference()`,
#' but works with any dataframe containing `CONF` and the chosen metric.
#'
#' @param data A dataframe containing at least two columns:
#'   \code{CONF} (conference names) and the selected metric.
#' @param metric A character string indicating which metric to plot c("ADJOE","ADJDE","EFG_O","EFG_D")
#'
#'
#' @return A \code{ggplot} object representing a horizontal bar chart.
#'
#'
#' @export
#' @import dplyr
#' @import ggplot2
plot_top_conference <- function(data, metric = "ADJOE") {

  # Vérification basique
  if (!metric %in% names(data)) {
    stop("Metric not found in dataframe")
  }

  ggplot2::ggplot(data) +
    ggplot2::aes(
      x = reorder(CONF, .data[[metric]]),
      y = .data[[metric]]
    ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = paste("Conference ranking by", metric),
      x = "Conference",
      y = metric
    )
}
