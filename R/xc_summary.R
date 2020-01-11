#' Cross-country paragliding data web scraping
#'
#' @param limit_run either TRUE or FALSE(default) specifies whether to pull
#' only 3 pages if TRUE or all pages if FALSE
#'
#' @return a dataframe containing the number, full_name, nickname, country,
#' num_flights, open_score, fail_score, registered, and info from the website
#'   \url{https://www.xcontest.org/world/en/pilots/}.
#'
#' @examples
#' pilot_df <- xc_summary()
#' small_pilot_df <- xc_summary(TRUE)
#'
#' @export
xc_summary <- function(limit_run = FALSE){
  url_pg <- "https://www.xcontest.org/world/en/pilots/" #first page url
  page <- xml2::read_html(url(url_pg))
  df_pg1 <- xc_get_pilot_tbl(page)
  last_num <- xc_get_last_pg_num(page)
  if(limit_run) {
    x <- seq(50, 100, 50)
  }else{
    x <- seq(50, last_num, 50)
  }

  out_all <- purrr::map2(x, url_pg, xc_get_pilots)
  pilot_data <- dplyr::bind_rows(out_all)
  pilot_data <- df_pg1 %>% dplyr::bind_rows(pilot_data)
}

#' Get pilot table from website
#'
#' @param page xml object resulting from html_read
#'
#' @return tidy dataframe containing num, full_name, nickname, country, num_flights,
#' open_score, fail_score, registered and info variables for each pilot on the page
#' @export
#'
#' @examples
#' url_pg <- "https://www.xcontest.org/world/en/pilots/"
#' page <- xml2::read_html(url(url_pg))
#' df_pg1 <- xc_get_pilot_tbl(page)
xc_get_pilot_tbl <- function(page){
  # reads in from a page the table of pilot data
  df <- rvest::html_table(page, fill = TRUE)[[2]]
  names(df) <- c("Num", "name", "country", "num_flights", "open_score",
                 "fail_score" ,"registered", "info")
  # df <- as_tibble(df)
  df <- df %>%
    tidyr::separate(name,
                    into = c("full_name", "nickname"),
                    sep = "\\[") %>%
    dplyr::mutate(full_name = stringr::str_squish(full_name)) %>%
    dplyr::mutate(nickname = stringr::str_remove(nickname,"\\]")) %>%
    dplyr::mutate(open_score = readr::parse_number(open_score))
}

#' Retrieve Pilot Data Given a start point for a url
#'
#' @param x numeric value for starting point in url
#' @param url_pg character vector containing url base
#'
#' @return dataframe of pilot data from page url_pg pasted with "?list[start]=",x
#' @export
#'
#' @examples
#' df <- xc_get_pilots(50, "https://www.xcontest.org/world/en/pilots/")
xc_get_pilots <- function(x, url_pg){
  # pastes together a url and then reads in table of pilot data
  url_pg_i <- paste0(url_pg,"?list[start]=",x)
  page <- xml2::read_html(url(url_pg_i))
  df <- xc_get_pilot_tbl(page)
}

#' Scrape x value of last page number
#'
#' @param page xml object resulting from html_read
#'
#' @return numeric scalar for last x value needed
#' @export
#'
#' @examples
#' url_pg <- "https://www.xcontest.org/world/en/pilots/"
#' page <- xml2::read_html(url(url_pg))
#' last_num <- xc_get_last_pg_num(page)
xc_get_last_pg_num <- function(page) {
  # figure out where the last page of pilot data lives
  paging_nodeset <- page %>%
    # get the tag which has the number of the last page in the tag
    rvest::html_nodes('strong~ .pg-edge+ .pg-edge') %>%
    rvest::html_attr("href")
  # retrieve ending page number
  last_num <- as.numeric(stringr::str_extract(paging_nodeset[1], "\\d+$") )
}
