library(dtw)
library(torch)

label_data <- function(mean_curves) {
  m_l <- mean_curves %>%
    group_by(keyword) %>%
    summarise(d = list(value), keyword = first(keyword))
  dist_matrix <- dist(m_l %>% select(d) %>% deframe, method = "DTW")
  hc <- hclust(dist_matrix, method = "average")
  clus <- cutree(hc, 3)
  return(tibble(
    keyword = m_l %>% select(keyword) %>% deframe,
    class = clus
  ))
}

df_dataset <- torch::dataset(
  "df_dataset",
  initialize = function(data) {
    self$x <- data[,1:-2]
    self$y <- data[,-1]
  },
  .length = function() {
    self$x %>% dim %>% first
  },
  .getitem = function(index) {
    x <- self$x[index,] %>% torch_reshape(c(-1, 1))
    y <- self$y[index] %>% torch_tensor(dtype = torch_long())
    list(x=x, y=y)
  },
)

rnn <- torch::nn_module(
  initialize = function(hidden_size = 256, num_layers = 2, bidirectional = FALSE) {
    self$rnn  <- nn_lstm(1, hidden_size = hidden_size, num_layers = num_layers, batch_first = TRUE, bidirectional = bidirectional)
    if (bidirectional) {
      hidden_size_after <- hidden_size*2
    } else {
      hidden_size_after <- hidden_size
    }
    self$proj <- nn_sequential(
      nn_linear(hidden_size_after, hidden_size),
      nn_relu(),
      nn_linear(hidden_size, hidden_size),
      nn_relu(),
      nn_linear(hidden_size, 3)
    )
  },
  forward = function(input) {
    output <- self$rnn(input)[[1]][,-1,]
    self$proj(output)
  }
)



prep_by_week <- function(data) {
  data %>%
    select(keyword, week, hour, day, search_index, class) %>%
    group_by(keyword, week) %>%
    pivot_wider(names_from = c(day, hour), names_glue="h-{day}-{hour}", values_from = search_index) %>%
    ungroup() %>%
    select(starts_with("h"), class) %>%
    unlist(use.names = FALSE) %>%
    matrix(ncol = 169) %>%
    torch_tensor
}

normalise <- function(data, minv, maxv) {
  data %>%
    mutate(search_index = (search_index-minv)/(maxv-minv))
}
