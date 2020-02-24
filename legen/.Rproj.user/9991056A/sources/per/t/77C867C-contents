# 'locsent
#
#' This function takes in google trend search keywords, beginning year, ending year as the keys and outputs annual time series data for each region within US.
#' @param key the input keywords that you would like to get the google trends for
#' @param begYr the year as the starting point for your time frame
#' @param endYr the year as the ending point for your time frame
#' @return a dataframe
#' @import gtrendsR stringr
#' @export
#' @examples
#' # obtain the google trends data for "corruption" from 2006 to 2016.
#' corruption <- locsent("corruption", 2006, 2016)

###
locsent <- function(key, begYr, endYr) {

  ## construct time vector
  begin <- seq(as.Date(paste(begYr, "-01-01", sep = "")), as.Date(paste(endYr, "-01-01", sep = "")), by = "years")
  end <- seq(as.Date(paste(begYr, "-12-31", sep = "")), as.Date(paste(endYr, "-12-31", sep = "")), by = "years")
  timeseries <- paste(begin, end, sep =" ")

  ## extract data series
  localsentiment  <- NULL;
  for(t in timeseries) {
    loc <- gtrends(keyword = key, geo = "US", time = t,
                            gprop = c("web", "news", "images", "froogle", "youtube"),
                            category = 0, hl = "en-US", low_search_volume = FALSE,
                            cookie_url = "http://trends.google.com/Cookies/NID", tz = 0,
                            onlyInterest = FALSE)
    location <- loc[["interest_by_region"]][["location"]]
    hits <- loc[["interest_by_region"]][["hits"]]
    date <- substr(t, 1, 4)
    localsentiment  = rbind(localsentiment, data.frame(date, location, hits))
  }
  localsentiment
}


#' statepop
#
#' This function takes in the user's API key, beginning year, ending year of the data and whether or not excluding puerto ricos.
#' @param APIkey users need to first obtain the free API authentication key from US Census Bureau
#' @param exclude_puerto_rico return the population estimates without puerto rico population estimates (\code{TRUE}) or #' with (\code{FALSE})
#' @param begYr the year as the starting point for your time frame
#' @param endYr the year as the ending point for your time frame
#' @return a dataframe
#' @import httr dplyr
#' @export
#' @examples
#' # obtain the state population from 2014 to 2016 excluding puerto ricos
#' statepopulation <- statepop("CENSUS_KEY", 2014, 2016, TRUE)
#' # obtain the state population from 2000 to 2018 including puerto ricos
#' statepopulation <- statepop("CENSUS_KEY", FALSE)
###

statepop <- function(APIkey, begYr = 2000, endYr = 2018, exclude_puerto_rico = TRUE) {
###2000-2010
  url1 <- paste("https://api.census.gov/data/", 2000,
                "/pep/int_population", "?get=GEONAME,POP,DATE_DESC&for=state:*&key=",
                Sys.getenv(APIkey), sep = "")
  pop1 <- content(GET(url1))
  lp1<-length(pop1)
  popdf1  <- NULL;
  for (i in 2:lp1)
  {
    state <-c(pop1[[i]][[1]]) ###put state names in a vector
    popest <-c(pop1[[i]][[2]]) ###put population estimates in a vector
    year <- c(pop1[[i]][[3]]) ###put estimation date in a vector
    popdf1  = rbind(popdf1, data.frame(state, popest, year))
  }

  ###2010-2018
  url2 <- paste("https://api.census.gov/data/", 2018,
                "/pep/population", "?get=GEONAME,POP,DATE_DESC,DATE_CODE&for=state:*&key=",
                Sys.getenv(APIkey), sep = "")
  pop2 <- content(GET(url2))
  lp2<-length(pop2)
  popdf2  <- NULL;
  for (i in 2:lp2)
  {
    state <-c(pop2[[i]][[1]]) ###put state names in a vector
    popest <-c(pop2[[i]][[2]]) ###put population estimates in a vector
    year <- c(pop2[[i]][[3]]) ###put estimation date in a vector
    popdf2  = rbind(popdf2, data.frame(state, popest, year))
  }
  popdf = rbind(popdf1, popdf2)
  ###Drop Census data
  popdff <- popdf[!grepl("Census", popdf$year), ]
  popdff <- popdff[!grepl("base", popdff$year), ]
  ###Clean the year column
  popdff$year <- substr(popdff$year, 5, 8)
  ###Drop non-US states if stated TRUE
  if(exclude_puerto_rico == TRUE){
    popdff <-  popdff[!grepl("Puerto Rico", popdff$state),]
  }
  ###Select the year range indicated in the command
  popdff <- popdff %>%
    dplyr::filter(begYr <= year & year <= endYr)
}

#' HdDataverse
#
#' This function takes in the File Persistent ID available for each data file in the Harvard Dataverse and user's API key.
#' @param ID the File Persistent ID available in the Metadata section for each data file
#' @param APIkey users need to first obtain the free API authentication key from Harvard Dataverse
#' @return a dataframe
#' @import httr
#' @export
#' @examples
#' # obtain the state_share data in the Startup Cartography Project with File Persistent ID
#' #' doi:10.7910/DVN/BMRPVH/0WQQMT
#' HarvardData <- HdDataverse("doi:10.7910/DVN/BMRPVH/0WQQMT", "HARVARDDATA_KEY")
#' # people could save the file in RData format
#' saveRDS(HarvardData, file = "Harvard_Dataverse_data.rds")

###

HdDataverse <- function(ID, APIkey) {
  url <- paste("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=", ID, sep = "")
  HarvardData <- as.data.frame(content(GET(url, query = list(key = Sys.getenv(APIkey)))))
  HarvardData
}

#' patent
#
#' This function takes in the beginning year, ending year of the data and outputs the number of patent granted for each state from the time period specified.
#' @param begYear the year as the starting point for your time frame
#' @param endYear the year as the ending point for your time frame
#' @return a dataframe
#' @import rvest stringr tibble tidyr hydrostats tibble dplyr gtools xml2
#' @importFrom rlang .data
#' @export
#' @examples
#' # obtain the number of patent granted for each state from 2004 to 2016
#' patentts <- patent(2004, 2016)

patent <- function(begYear = 1993, endYear = 2018) {
  ### Get urls for each year
  urlsf <- read_html("https://www.uspto.gov/web/offices/ac/ido/oeip/taf/reports_stco.htm") %>%
    xml_find_all("//tr/td/a") %>%
    xml_attr("href")
  urls_clean <- substring(urlsf, 2, str_length(urlsf))

  ###1993-2001
  urls_pt1 <- urls_clean[18:26]
  ###2002-2010
  urls_pt2 <- urls_clean[9:17]
  ###2011-2018
  urls_pt3 <- urls_clean[1:8]

  ### For part 1
  patentdf1 <- NULL
  for (u in urls_pt1){
    ### Extracting text inside a <pre> tag
    url <- paste("https://www.uspto.gov/web/offices/ac/ido/oeip/taf", u, sep = "")
    body<-html_nodes(read_html(url), "pre")
    text= sapply(1:length(body), function(i) {xml_text(body[i])})
    table1<-unlist(strsplit(text, "\n"))
    ### Split data frame string column into multiple columns
    df <- as.data.frame(table1)
    df1 <- df %>%
      separate(table1, c("CODE", "STATE/COUNTRY", "UTILITY", "DESIGN", "PLANT", "REISSUE", "TOTALS"))
    df2 <- df1[38:92,]
    patent_year <- df2 %>%
      add_column(year = substring(u, 8, 9))
    patentdf1 <- rbind(patentdf1, patent_year)
  }

  ### For part 2
  patentdf2 <- NULL
  for (i in urls_pt2){
    url <- paste("https://www.uspto.gov/web/offices/ac/ido/oeip/taf", i, sep = "")
    patent_html <- read_html(url)
    patent_node <- html_node(x = patent_html,
                             xpath = "//center/table")
    patent_table <- html_table(patent_node)
    patent_table <- patent_table[,1:7]
    colnames(patent_table) <- c("CODE", "STATE/COUNTRY", "UTILITY", "DESIGN", "PLANT", "REISSUE", "TOTALS")
    patent_year <- patent_table %>%
      add_column(year = substring(i, 8, 9))
    patent_year <- patent_year[1:51,]
    patentdf2 <- rbind(patentdf2, patent_year)
  }


  patentdf3 <- NULL
  for (i in urls_pt3){
    url <- paste("https://www.uspto.gov/web/offices/ac/ido/oeip/taf", i, sep = "")
    patent_html <- read_html(url)
    patent_node <- html_node(x = patent_html,
                             xpath = "//center/table")
    patent_table <- html_table(patent_node)
    patent_table <- patent_table[,1:8]
    colnames(patent_table) <- c("USFOREIGN","CODE", "STATE/COUNTRY", "UTILITY", "DESIGN", "PLANT", "REISSUE", "TOTALS")
    patent_years <- add_column(patent_table, year = substring(i, 8, 9))
    patent_year <- patent_years %>%
      dplyr::filter(.data$USFOREIGN == "US")
    patent_year <- patent_year[1:51,]
    patentdf3 <- rbind(patentdf3, patent_year)
  }

  patentdf1 <- patentdf1 %>%
    select(.data$year, everything())
  patentdf2 <- patentdf2 %>%
    select(.data$year, everything())
  patentdf3 <- patentdf3 %>%
    select(.data$year, everything()) %>%
    select(-.data$USFOREIGN)

  patentdf <- rbind(patentdf1, patentdf2, patentdf3)
  patentdf$year <- as.POSIXct(patentdf$year, format="%y")
  patentdf$year <- four.digit.year(patentdf$year, year=1968)
  patentdf_select <- patentdf %>%
    dplyr::filter(.data$year >= begYear & year <= endYear)
  patentdf_select
}

patentts <- patent(2006, 2016)
