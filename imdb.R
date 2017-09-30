# devtools::install_github("hrbrmstr/omdbapi")
# library(omdbapi)
library(feather)
library(rvest)
library(tidyverse)
library(stringr)
library(purrr)

best_movies_raw <- read_html("http://www.imdb.com/chart/top")

best_movies <- tibble(
  movie = best_movies_raw %>%
    html_nodes(".titleColumn a") %>%
      html_text(),
  year = best_movies_raw %>%
    html_nodes(".secondaryInfo") %>%
    html_text() %>%
      str_extract("[0-9]{4}"),
  rating = best_movies_raw %>%
    html_nodes("strong") %>%
      html_text(),
  votes = best_movies_raw %>%
    html_nodes("strong") %>%
      html_attr("title") %>%
        str_sub(14) %>%
          str_split(" user") %>%
            map_chr(~.[1]) %>% 
              str_replace_all(",","") %>%
                as.integer(),
  url = best_movies_raw %>%
    html_nodes(".titleColumn a") %>%
      html_attr("href")
)


(best_movies <- 
  best_movies %>%
    mutate(rank_imdb = row_number(),
           imdb_url = str_c("http://imdb.com",url),
           imdb_id = str_extract(imdb_url,"tt[0-9]{7}")))





validate_values <- function(values){
  if(length(values) == 0) values = NA
  if(length(values) == 1){
    if( values %>% str_replace_all("[^0-9A-Za-z///' ]","") %>% iconv("latin1", "ASCII", sub="") %>% length(.) == 0){
      values = NA
    }
  }
  values
}



movie_info <- 
  map(best_movies$imdb_url, function(url){
    html_page <- read_html(url)
    id <- str_extract(url,"tt[0-9]{7}")
    
    fetch_values <- function(identifier){
      html_page %>%
        html_nodes(identifier) %>%
        html_text() %>%
        str_trim()
    }
    
    cast <- 
      tibble(names = 
               html_page %>% 
               html_nodes(".itemprop .itemprop , .character div , .itemprop .itemprop") %>% 
               html_text() %>%
               str_trim()) %>%
      mutate(nature = rep(c("actor", "role"),nrow(.)/2),
             id = sort(rep(1:(nrow(.)/2),2))) %>%
      spread(nature,names)
    
    
    (summary <- fetch_values(".summary_text") %>%
        validate_values())
    
    (genres <- fetch_values(".txt-block~ .canwrap a") %>%
        validate_values())
    
    (language <- fetch_values("#titleDetails .txt-block:nth-child(5) a") %>%
        validate_values())
    
    (key_words <- fetch_values("#titleStoryLine .itemprop") %>%
        validate_values())
    
    (release_date <- fetch_values(".txt-block:nth-child(6)") %>%
        str_sub(15) %>% str_split(" \\(") %>% .[[1]] %>% .[1] %>%
        validate_values())
    
    
    (gross <- fetch_values("#titleDetails .txt-block:nth-child(13)") %>%
        str_replace_all(" ","") %>%
        str_replace_all("[A-z]","") %>%
        str_replace_all("[$&+,:;=?@#|'<>.^*()%!-]","") %>%
        str_trim() %>%
        validate_values())
    
    (budget <- fetch_values(".txt-block:nth-child(11)") %>%
        str_replace_all(" ","") %>%
        str_replace_all("[A-z]","") %>%
        str_replace_all("[$&+,:;=?@#|'<>.^*()%!-]","") %>%
        str_trim() %>%
        validate_values())
    
    
    (runtime <- fetch_values("time"))
    
    (parental_rating <- fetch_values("hr+ .txt-block h4+ span"))
    
    (metascore <- fetch_values(".score_favorable span") %>%
        validate_values())
    
    (director <- fetch_values(".summary_text+ .credit_summary_item .itemprop") %>%
        validate_values())
    
    result <- list(id = id,
                   director = director,
                   metascore = metascore,
                   cast = cast,
                   parental_rating = parental_rating,
                   runtime = runtime,
                   budget = budget,
                   gross = gross, 
                   release_date = release_date,
                   key_words = key_words,
                   language = language,
                   genres = genres,
                   summary = summary)
    
    print(result)
    
    return(result)
    
  })



best_movies <- 
  best_movies %>%
    mutate(addtional_info = movie_info)


saveRDS(best_movies, "best_movies.rds")
read

best_movies %>%
  mutate(director = map_chr(addtional_info, ~.$director %>% str_c(collapse = ", ")))


map(movie_info,"cast") %>% bind_rows() %>%
  count(role, sort = T)

# parent_func <- function(){
#   x <- 1:5
#   # print(environment())
#   print(child_func())
# }
# 
# child_func <- function(){
#   x <- parent.frame()$x
#   x <- x[1:2]
# }
# 
# parent_func()
# 
# 
