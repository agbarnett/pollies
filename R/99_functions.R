# 99_functions.R

# function for getting Wikipedia link from https://stackoverflow.com/questions/32889136/how-to-get-google-search-results
get_first_google_link <- function(name, root = TRUE) {
  url = URLencode(paste0("https://www.google.com/search?q=",name))
  page <- xml2::read_html(url)
  # extract all links
  nodes <- rvest::html_nodes(page, "a")
  links <- rvest::html_attr(nodes,"href")
  # extract first link of the search results
  link <- links[startsWith(links, "/url?q=")][1]
  # clean it
  link <- sub("^/url\\?q\\=(.*?)\\&sa.*$","\\1", link)
  # get root if relevant
  if(root) link <- sub("^(https?://.*?/).*$", "\\1", link)
  link
}