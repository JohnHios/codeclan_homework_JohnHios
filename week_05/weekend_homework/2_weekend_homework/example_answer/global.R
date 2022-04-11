library(ggplot2)
library(dplyr)
library(CodeClanData)

make_scores_scatter_plot <- function(chosen_data){
  ggplot(chosen_data) +
    aes(x = user_score, y = critic_score) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(
      x = "User Score",
      y = "Critic Score"
    )
}

make_sales_histogram <- function(chosen_data){
  ggplot(chosen_data) +
    aes(x = sales) +
    geom_histogram(bins = 30) +
    labs(
      x = "Total global sales, in millions",
      y = "Count"
    )
}

make_rating_bar_plot <- function(chosen_data){
  ggplot(chosen_data) +
    aes(x = rating, fill = rating) +
    geom_bar() +
    guides(fill = FALSE) +
    labs(
      x = "Rating",
      y = "Count"
    )
}

