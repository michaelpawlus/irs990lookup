#' Lookup IRS 990 Data Using EINs
#'
#' @param x user-supplied csv file via the file.choose() function 
#'
#' @return csv file with data from IRS 990 xml files corresponding with user-supplied EINs
#' 
#' @author Michael Pawlus, \email{michael.pawlus@@gmail.com}
#'
#' @examples
#' # simple run the following function call and select a csv file with one column of EINs
#' # please include a hearder row such as "eins"
#' get_990_data() 
#' 
#' @export

get_990_data  <- function(x) {
  
  # prompt user to choose a file
  x <- file.choose()
  
  # check if the file is a csv file
  if(!str_detect(x,".csv$")){
    
    # if not, stop with the following error message
    stop('Cannot read uploaded file. The uploaded file must be in csv format.')
    
    # if it is a csv file then continue
  } else {
    
    # possibly include a message here that only the first column will be used
    
    # read in the file
    eins <- read_csv(x)
    
    # select just the first column
    eins <- eins %>% select(1)
    
  }
  
  # if that first column is not all integers...
  if(!is_integer(eins[[1]])) {
    
    # Stop with error message below
    stop('Only the first column is used and must contain only EIN numbers and a header row')
    
  } else {
    
    # load required packages if not already loaded
    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(tidyverse, xml2, jsonlite, stringr)
    
    # read in the JSON index for all 990 filings for tax year 2017
    index <- fromJSON("https://s3.amazonaws.com/irs-form-990/index_2017.json")[[1]]
    
    # convert the TaxPeriod column to data type: integer
    index$TaxPeriod <- as.integer(index$TaxPeriod)
    
    # keep on the most recent filing as there are some duplicates
    index <- index %>% 
      arrange(desc(TaxPeriod)) %>% 
      distinct(EIN, .keep_all = TRUE)
    
    # read in the csv file uploaded by the user
    eins <- read_csv(x)
    
    # pad all eins with leading zeros so that all have a length of 9
    eins <- map_df(eins, ~str_pad(.x, 9, pad = "0"))
    
    # match up EINs from csv with those from the JSON index
    found_ids <- eins %>%
      left_join(index)
    
    # extract the urls from the index for all matching cases
    urls <- found_ids$URL
    
    # function to get name and amount given out in contributions for each url
    get_name_and_grants <- function(url){
      # read in the xml file for each url
      xf <- read_xml( x=url, options=NULL )
      # strip out the name spaces (I'm not sure what this means exactly but these have to be removed)
      xml_ns_strip( xf )
      # follow the xml path and extract value as text
      name <- xf %>% xml_find_all("//Return/ReturnHeader/Filer/BusinessName/BusinessNameLine1Txt") %>% xml_text()
      # follow the xml path and extract the value as integer
      fy_gifts <- xf %>% xml_find_all(".//CYGrantsAndSimilarPaidAmt") %>% xml_integer()
      # create a tibble with these two values
      tibble(org_name = name, 
             grants = fy_gifts)
    }
    
    # create a list of foundations based on matches as well as an error list using purrr::safely()
    found_list <- urls %>%
      map(safely(get_name_and_grants)) %>%
      transpose()
    
    # create a logical vector where the error list is null to get only successful matches
    is_ok <- found_list$error %>% 
      map_lgl(is_null)
    
    # create the results table by keeping only succesful matches with org names
    res <- found_list$result[is_ok] %>%
      keep(~ length(.x$org_name) > 0) %>%
      {
        tibble(
          `Foundation Name` = map_chr(., "org_name"),
          `Current FY Giving` = map_int(., "grants")
        )
      }
    
    # let the user know that the a portion of their data is printed in the console
    cat("Below are the first ten rows of your output file\n ")
    
    # print the top ten rows
    print(head(res, n=10))
    
    # write the file to a csv (possibly make naming the file a user option in the future)
    write_csv(res,"output_990_data.csv")
    
    # put name of csv in a file to prep for making this a user option
    output_file_name <- "output_990_data.csv"
    
    # print message the entire file has been saved as: output_file_name
    cat(paste0("Output file saved as: ",output_file_name))
    
  }
  
}
