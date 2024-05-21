library(tidyverse)
library(psych)

messagef <- function(...) message(sprintf(...))

mus_vars <- master %>%
  select(starts_with("MUS")) %>%
  select(where(is.numeric)) %>%
  names()

final_vars_orig <- c("often has the desire to make music.",
                     "has great enthusiasm for music.",
                     "enjoys making music a part of his/her life.",
                     "has a good sense of timing and rhythm.",
                     "has a feeling for the beat.",
                     "has good hearing ability, e.g. for melodies and rhythms.",
                     "shows difficulties in producing or reproducing music.",
                     "pays little attention while making music, so he/she does not realize if it sounds as intended.",
                     "has issues reproducing melodies he/she has heard before.")
final_vars <- c("MUS.often_has_the_desire_to_m", 
"MUS.has_great_enthusiasm_for_", 
"MUS.enjoys_making_music_a_par", 
"MUS.has_a_good_sense_of_timin", 
"MUS.has_a_feeling_for_the_bea", 
"MUS.has_good_hearing_ability_", 
"MUS.shows_difficulties_in_pro", 
"MUS.pays_little_attention_whi", 
"MUS.has_issues_reproducing_me")

final_cfa <- "
  F1 =~ MUS.often_has_the_desire_to_m + MUS.has_great_enthusiasm_for_ + MUS.enjoys_making_music_a_par 
  F2 =~ MUS.has_a_good_sense_of_timin + MUS.has_a_feeling_for_the_bea + MUS.has_good_hearing_ability_ 
  F3 =~ MUS.shows_difficulties_in_pro + MUS.pays_little_attention_whi + MUS.has_issues_reproducing_me
"

neutral_var_names <- function(data, prefix = "V"){
  data %>% set_names(sprintf("%s%02d", prefix, 1:length(.)))
}
iterative_fa <- function(data,
                         vars = NULL,
                         min_loading = .3,
                         min_comm = .4,
                         min_good = 1,
                         max_complexity = 2,
                         plot = F,
                         mode = c("vars", "efa")) {
  if(is.null(vars)){
    vars <- data %>% select(where(is.numeric)) %>% names()
    messagef("Using all %d numerical columns", length(vars))
  }
  data <- data %>% select(all_of(vars))
  n_obs <- nrow(data)
  mode <- match.arg(mode)
  parallel <- psych::fa.parallel(data,
    fm = "minres",
    fa = "both",
    nfactors = 1,
    main = "Parallel Analysis Scree Plots",
    n.iter = 200,
    error.bars = FALSE,
    se.bars = FALSE,
    SMC = FALSE,
    ylabel = NULL,
    show.legend = TRUE,
    sim = TRUE,
    quant = .95,
    cor = "cor",
    use = "pairwise",
    plot = plot,
    correct = .5
  )
  
  n_factors <- parallel[["nfact"]]
  efa <- psych::fa(data, nfactors = n_factors)
  
  if (mode == "efa") {
    return(efa)
  }
  
  loadings <- efa$loadings %>%
    unclass() %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    as_tibble()
  communalities <- names(efa$communalities[efa$communalities >= min_comm])
  complexities <- names(efa$complexity[efa$complexity <= max_complexity])
  
  good_loadings <- loadings %>%
    pivot_longer(-var) %>%
    filter(abs(value) >= min_loading) %>%
    count(var) %>%
    filter(n >= min_good)
  
  n_before <- length(data)
  good_vars <- intersect(
    good_loadings %>% pull(var),
    intersect(communalities, complexities)
  )
  messagef("Before: %d, after: %d, removed: %d", n_before, length(good_vars), n_before - length(good_vars))
  good_vars
}

find_best_fa <- function(data,
                         vars = NULL,
                         min_loading = .3,
                         min_comm = .4,
                         min_good = 1,
                         max_complexity = 2) {
  if(is.null(vars)){
    vars <- data %>% select(where(is.numeric)) %>% names()
    messagef("Using all %d numerical columns", length(vars))
  }
  change <- 999
  cur_vars <- vars
  last_vars <- vars
  while (change > 0) {
    cur_vars <- iterative_fa(data, cur_vars,
      min_loading = min_loading, min_comm = min_comm, min_good = min_good,
      max_complexity = max_complexity, mode = "vars"
    )
    change <- length(last_vars) - length(cur_vars)
    last_vars <- cur_vars
  }
  list(
    vars = cur_vars,
    efa = iterative_fa(data, cur_vars,
      min_loading = min_loading, min_comm = min_comm, min_good = min_good,
      max_complexity = max_complexity, mode = "efa"
    )
  )
}

get_cfa_from_efa <- function(efa, min_loading = .7){
  loadings <- efa$loadings %>%
    unclass() %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    as_tibble() %>% 
    pivot_longer(-var) 

  n_factors <- n_distinct(loadings$name)
  
  loadings <- loadings %>% 
    filter(abs(value) >= min_loading)
  n_factors_new <- loadings %>% count(name) %>% filter(n > 1) %>% pull(name) %>% n_distinct()
  if(n_factors_new != n_factors){
    message("Not all factors represented with at least to variables. Consider lowering min_loadings")
  }
  map_chr(unique(loadings$name), function(fact){
    def <- loadings %>% filter(name == fact) %>% pull(var) %>% paste(collapse = " + ")
    sprintf("%s =~ %s", fact, def)
  }) %>% 
    paste(collapse = "\n")
}