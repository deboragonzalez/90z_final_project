library(tidyverse)
library(rlang)

p2star <- function(x){
  case_when(between(x, 0.05, 1) ~ "",
            between(x, 0.01, 0.05) ~ "*",
            between(x, 0.001, 0.01) ~ "**",
            between(x, 0, 0.001) ~ "***")
}

combine <- function(estimate, std.error, star, format, digits){
  paste0(formatC(estimate, format = format, digits = digits), star) %>%
    paste0(., " (", formatC(std.error, format = format, digits = digits), ")")
}

regtable <- function(..., format = "f", digits = 3){
  cl <- match.call()
  cl$format <- cl$digits <-  cl[[1]] <- NULL
  eval_tidy(expr(map(list2(!!!cl), broom::tidy))) %>%
  map(~ mutate(.x,
      star = p2star(p.value),
      out = combine(estimate, std.error, star,
                    format = format, digits = digits))) %>%
  map(~ dplyr::select(.x, term, out)) %>%
  reduce(full_join, by = "term") %>%
  mutate_all( ~ tidyr::replace_na(.x, "")) %>%
  set_names(c("term", map_chr(ensyms(...), as_string)))
}
