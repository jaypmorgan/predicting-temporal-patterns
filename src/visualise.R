library(tidyverse)
library(ggplot2)

visualise_mean_curve <- function(data, keyword_name)  {
    data %>%
        filter(keyword == keyword_name) %>%
        ggplot(aes(x = hour, y = value)) +
        geom_line()
}

hierarchical_clustering <- function(mean_curves) {

    m_l <- mean_curves %>%
        group_by(keyword) %>%
        summarise(d = list(value), keyword = first(keyword))
    dist_matrix <- dist(m_l %>% select(d) %>% deframe, method="DTW")
    hc <- hclust(dist_matrix, method="average")
    dend <- as.dendrogram(hc)

    clus <- cutree(hc, 3)

    keywords <- m_l %>% pull(keyword)

    dend <- color_branches(dend, k = 3)
    dend <- color_labels(dend, k = 3)
    dend <- set_labels(dend, keywords[order.dendrogram(dend)])

    par(mar = c(0, 0, 0, 6))
    return(plot(dend, horiz = T, xaxt="n", yaxt="n"))
}
