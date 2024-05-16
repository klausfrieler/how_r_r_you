library(tidyverse)
group_labels <-
  c(
    "1" = "GEN",
    "2" = "REL",
    "3" = "MUS",
    "4" = "MUS",
    "5" = "CHM",
    "6" = "GMS",
    "0" = "META"
  )

inverted_vars <- c(
  "G3Q00001[SQ009]. The child... [... won't let him-/herself become immersed in sounds.]",
  "G3Q00001[SQ010]. The child... [... shows little interest in memorising patterns.]",
  "G3Q00001[SQ011]. The child... [... often moves out of sync with music.]",
  "G3Q00001[SQ014]. The child... [... does not enjoy spending time in musical environments.]",
  "G3Q00001[SQ015]. The child... [... does not seek to acquire music related objects.]",
  "G3Q00001[SQ016]. The child... [... does not mimic or imitate musicians or singers.]",
  "G3Q00001[SQ017]. The child... [... doesn't seem to pick up on emotions conveyed by music.]",
  "G3Q00001[SQ40126]. The child... [... does not associate music with characters and stories.]",
  "G3Q00001[SQ018]. The child... [... has no association between music and routines (e.g. does not have a song for washing his/her hands).]",
  "G3Q00001[SQ023]. The child... [... does not insist on his/her own will when making and enjoying music.]",
  "G3Q00001[SQ024]. The child... [... doesn't repurpose music-neutral objects into musical instruments.]",
  "G3Q00001[SQ025]. The child... [...does not seem curious to explore sounds in different environments.]",
  "G4Q00001[A15]. The child...  [... does not recognise melodies.]",
  "G4Q00001[A16]. The child...  [... is easily distracted when engaged with musical activities.]",
  "G4Q00001[A17]. The child...  [... demonstrates little motivation to make music with others.]",
  "G4Q00001[A20]. The child...  [... has difficulties in recognizing melodies and tone progressions.]",
  "G4Q00001[A21]. The child...  [... has issues reproducing melodies he/she has heard before.]",
  "G4Q00001[A22]. The child...  [... does not distinguish between different musical instruments.]",
  "G4Q00001[A25]. The child...  [... does not recognise composers or singers.]",
  "G4Q00001[A3]. The child...  [... shows difficulties in producing or reproducing music.]",
  "G4Q00001[A4]. The child...  [... pays little attention while making music, so he/she does not realize if it sounds as intended.]",
  "G4Q00001[A6]. The child...  [... displays challenges in experiencing music with an open mind.]"
)


first_split <- function(var_name) {
  map_dfr(var_name, function(x) {
    original <- x
    x <- original %>%
      str_remove("\\[Please indicate a single number, different from 0.\\]") %>%
      str_remove("\\[multiple answers possible\\]") %>%
      stringr::str_split("[.]") %>% pluck(1)
    ids <- str_split_fixed(x[[1]], "\\[", 2) %>% str_remove("\\]")
    question_group <-
      str_extract(ids[1], "G[0-9]Q") %>% str_extract("[0-9]+") %>% as.numeric()
    question_id <- str_extract(ids[1], "[0-9]+$") %>% as.numeric()
    answer_id <- str_extract(ids[2], "[0-9]+$") %>% as.numeric()
    
    if (is.na(question_group)) {
      question_group <- 0
    }
    
    question_group <- group_labels[as.character(question_group)]
    # if(ids[[1]] == "G2Q00005"){
    #   browser()
    # }
    if (ids[[1]] %in% c("G2Q00006", "G4Q00002", "G2Q00002") ||
        question_group == "CHM") {
      rest <- trimws(paste(x[2:length(x)], collapse = "."))
    }
    else{
      rest <- trimws(paste(x[2:length(x)], collapse = ".")) %>%
        str_split_fixed("\\[", 2) %>%
        str_remove("\\]") %>%
        pluck(2)
    }
    #browser()
    if (nzchar(rest)) {
      rest <- rest %>%
        str_remove("I am a") %>%
        str_replace(
          "teacher for a musical instrument outside of school/kindergarten/pre-school",
          "music teacher outside school"
        ) %>%
        str_replace(
          "non-musical educator outside of school/kindergarten/pre-school",
          "non-musical educator outside school"
        ) %>%
        str_remove("can") %>%
        str_remove("Further training") %>%
        str_remove("^He/She")
      
      rest <- janitor::make_clean_names(rest) %>% substr(1, 25)
      if (question_group == "GMS" && ids[2] == "other") {
        new_name <- sprintf("%s.q%02d_other", question_group, question_id)
      }
      else{
        new_name <- sprintf("%s.%s", question_group, rest)
      }
    }
    else{
      if (!is.na(answer_id)) {
        new_name <- sprintf("%s.q%02d", question_group, answer_id)
      }
      else if (question_group == "META") {
        new_name <-
          sprintf("%s.%s",
                  question_group,
                  janitor::make_clean_names(ids[1]))
      }
      else{
        new_name <- sprintf("%s.q%02d", question_group, question_id)
      }
    }
    tibble(
      id = ids[1],
      sub_id = ids[2],
      question_group = question_group,
      question_id = question_id,
      answer_id = answer_id,
      rest = rest,
      original = original,
      new_name = new_name
    )
  })
}

remove_all_na_cols <- function(data) {
  bad_cols <- data %>%
    summarise(across(everything(), function(x)
      mean(is.na(x)))) %>%
    pivot_longer(everything()) %>%
    filter(value == 1) %>% pull(name)
  data %>% select(-all_of(bad_cols))
}

parse_col <- function(col) {
  if (!is.character(col)) {
    return(col)
  }
  col <- str_replace(col, "-oth-", "A0")
  firsts <- na.omit(unique(substr(col, 1, 1)))
  if (length(firsts) == 1 && firsts == "A") {
    return(str_extract(col, "[0-9]+") %>% as.numeric())
  }
  col
}

filter_bad_ids <- function(data, threshold = 1) {
  bad_ids <- data %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(-c(p_id, META.startlanguage)) %>%
    group_by(p_id) %>%
    summarise(m = mean(is.na(value))) %>%
    filter(m >= threshold) %>%
    pull(p_id) %>%
    as.numeric()
  data %>% filter(!(p_id %in% bad_ids))
}

clean_and_filter_var_names <- function(data){
  data <- data %>% select(-ends_with("raw")) 
  browser()
  old_names <- names(data)

  new_names <- (data %>% 
    names() %>% 
    str_split_fixed("[.]", 2))[,1]
  # new_names <- data %>% 
  #   names() %>% 
  #   str_extract_all("^G[0-9]Q[0-9]+[\\[ a-zA-Z0-9\\]]+") 
  # new_names <- sapply(1:length(old_names), 
  #                     function(x) ifelse(length(new_names[[x]]) == 0, old_names[x], new_names[[x]])) 
  new_names <- new_names  %>% 
    str_replace("[\\[]", "_") %>% 
    str_remove("[\\]]")  %>% 
    str_remove("000")  
  stopifnot(all(table(new_names)== 1),
            length(new_names) == length(data))
  data %>% set_names(new_names)
}

clean_var_names <- function(data) {
  names_info <- data %>% names() %>% first_split()
  assign("names_info", names_info, globalenv())
  names_info[names_info$new_name == "META.id", ]$new_name <- "p_id"
  recoding <- names_info$new_name
  names(recoding) <- names_info$original
  old_names <- names(data)
  new_names <- recoding[old_names]
  data %>%
    set_names(new_names) %>%
    select(-starts_with(
      c(
        "META.seed",
        "META.startdate",
        "META.datestamp",
        "META.lastpage",
        "META.refurl",
        "META.submitdate",
        "META.prolific_id"
      )
    ))
}

rescore_mus_vars <- function(data) {
  data <- data %>%
    mutate(across(starts_with("MUS") & where(is.numeric),
                  function(x)
                    x - min(x, na.rm = T)))
  inverted_vars <-  names_info %>%
    filter(original %in% inverted_vars) %>%
    pull(new_name)
  data <- data %>%
    mutate(across(all_of(inverted_vars), function(x)
      max(x, na.rm = T)  - x))
}

filter_age <- function(data) {
  data %>%
    filter(CHM.how_old_is_he_she > 2,
           CHM.how_old_is_he_she < 11)
}

read_survey <- function(fname, sep = ",") {
  if (sep == ",") {
    data <- readr::read_csv(fname, col_types = cols())
  }
  else{
    data <- readr::read_csv2(fname, col_types = cols())
  }
  data %>%
    rename(total_duration = `interviewtime. Total time`) %>%
    remove_all_na_cols() %>%
    select(-intersect(names(.), c("G1Q00005.", "G8Q00001."))) %>%
    select(-contains("Please check your")) %>%
    select(-contains("Question time")) %>%
    select(-contains("Group time")) %>%
    clean_var_names() %>%
    mutate(across(everything(), parse_col))
}

setup_workspace <- function() {
  survey1 <-
    read_survey(fname = "data/survey_321192_R_data_file_run1.csv", sep = ",") %>%
    mutate(p_id = sprintf("A%s", p_id))
  
  survey2 <-
    read_survey(fname = "data/survey_795331_R_data_file.csv", sep = ";") %>%
    mutate(p_id = sprintf("B%s", p_id))
  
  survey3 <-
    read_survey(fname = "data/survey_321192_R_data_file.csv", sep = ";") %>%
    mutate(p_id = sprintf("C%s", p_id))
  
  #browser()
  master <- bind_rows(survey1, survey2, survey3) %>%
    filter_bad_ids() %>%
    filter_age() %>%
    filter(REL.how_many_children_in_that < 500) %>%
    rescore_mus_vars()
  
  assign("master", master, globalenv())
  master
  
}
