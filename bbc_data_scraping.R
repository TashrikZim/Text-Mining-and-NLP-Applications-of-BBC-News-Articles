library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

sleep_a_bit <- function() {
  Sys.sleep(runif(1, 1.0, 2.0))
}


scrape_article <- function(link, category) {
  sleep_a_bit()
  message("Scraping: ", link)
  
  doc <- tryCatch(read_html(link), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  
  title <- doc %>% html_element("h1") %>% html_text2()
  text <- doc %>% html_elements("div[data-component='text-block'] p") %>% 
    html_text2() %>% paste(collapse = " ")
  date <- doc %>% html_element("time") %>% html_attr("datetime")
  

  text <- text %>%
    str_squish() %>%            
    str_to_lower()             
  
  if (is.na(title) || nchar(text) < 200) return(NULL)
  
  tibble(
    Category = category,
    Headline = str_squish(title),
    Date = date,
    URL = link,
    Text = text
  )
}

get_category_links <- function(base_url, category_name, pattern, max_links = 100) {
  message("Getting links for: ", category_name)
  
  page <- tryCatch(read_html(base_url), error = function(e) NULL)
  if (is.null(page)) return(character(0))
  
  links <- page %>%
    html_elements("a[href]") %>%
    html_attr("href") %>%
    url_absolute(base_url) %>%
    unique()
  
  relevant_links <- links[grepl(pattern, links, ignore.case = TRUE)]
  head(relevant_links, max_links)
}



categories <- list(
  list(url = "https://www.bbc.com/news/politics",
       pattern = "/news/", category = "Politics"),
  list(url = "https://www.bbc.com/news/world",
       pattern = "/news/", category = "World"),
  list(url = "https://www.bbc.com/news/business",
       pattern = "/news/", category = "Business"),
  list(url = "https://www.bbc.com/news/science_and_environment",
       pattern = "/news/", category = "Science & Environment"),
  list(url = "https://www.bbc.com/news/health",
       pattern = "/news/", category = "Health"),
  list(url = "https://www.bbc.com/innovation/technology",
       pattern = "/innovation/technology-", category = "Technology"),
  
  list(url = c(
    "https://www.bbc.com/sport/football",
    "https://www.bbc.com/sport/cricket"
  ),
  pattern = "/sport/", category = "Sports"),
  

  list(url = c(
    "https://www.bbc.com/",
    "https://www.bbc.com/future-planet",
    "https://www.bbc.com/innovation/artificial-intelligence"
  ),
  pattern = "/news/", category = "News")
)


all_articles <- list()

for (cat in categories) {
  urls_to_scrape <- cat$url
  if (!is.vector(urls_to_scrape)) urls_to_scrape <- c(urls_to_scrape)
  
  cat_articles <- map_df(urls_to_scrape, function(u) {
    links <- get_category_links(u, cat$category, cat$pattern, max_links = 100)
    map_df(links, ~ scrape_article(.x, cat$category))
  })
  
  all_articles[[cat$category]] <- cat_articles
}


final_dataset <- bind_rows(all_articles) %>%
  filter(!is.na(Text), Text != "") %>%
  distinct(URL, .keep_all = TRUE)


write_csv(final_dataset, "bbc_dataset.csv")
message("Success! Saved ", nrow(final_dataset), " unique articles to 'bbc_dataset.csv'")
