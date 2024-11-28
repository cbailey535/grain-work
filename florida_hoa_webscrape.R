library(rvest)

#https://r4ds.hadley.nz/webscraping#extracting-data
#https://rvest.tidyverse.org/articles/selectorgadget.html

html <- read_html("https://search.sunbiz.org/Inquiry/CorporationSearch/SearchResultDetail?inquirytype=EntityName&directionType=Initial&searchNameOrder=CYPRESSFALLSATWOODLANDSHOMEOWN%20N050000061500&aggregateId=domnp-n05000006150-b273e9d3-92ab-4c70-b2f2-d0a0a6592bc5&searchTerm=CYPRESS%20FALLS%20AT%20THE%20WOODLANDS%20INC&listNameOrder=CYPRESSFALLSATWOODLANDSHOMEOWN%20N050000061500")
html <- read_html("https://search.sunbiz.org/Inquiry/CorporationSearch/SearchResultDetail?inquirytype=EntityName&directionType=Initial&searchNameOrder=VERONAWOODSHOMEOWNERSASSOCIATI%20N136380&aggregateId=domnp-n13638-ac5346f6-4fb2-460e-a935-bbae642b91d3&searchTerm=VERONAWALK%20OF%20NAPLES%2C%20INC.&listNameOrder=VERONAWALKPROPERTIES%20L200000285390")

hoa <- html %>% html_element("#content , h2") %>% 
  html_text2()
hoa

library(stringr)

# Assuming 'text' contains the text you got from html_text2()
# clean_text <- str_replace_all(hoa, "\\\\[[:alnum:]]+", "")

# If you want to remove the sequence without replacing with a space, use:
clean_text <- str_replace_all(hoa, "[\\r\\n\\t]", "")

# Print the cleaned text
print(clean_text)

library(tidyverse)

# Given text
text <- "Your provided text here"

# Regular expression to extract the Principal Address and the following address
# Regular expression to extract the Company Name, Principal Address, and Registered Agent
company_pattern <- "Florida\\s+([\\w .,-]+?)\\s+Filing Information"
address_pattern <- "Principal Address\\s+([\\dA-Za-z ,.-]+)\\s+([A-Za-z ,.-]+)\\s+(FL\\s+[0-9]{5})"
address_pattern <- "Principal Address\\s+([C/O\\w .,-]+)\\s+([\\dA-Za-z ]+)\\s+([A-Za-z ]+),\\s+(FL\\s+[0-9]{5})"

# Find the matches using str_match for company name
company_matches <- str_match(clean_text, company_pattern)

# Find the matches using str_match for address
address_matches <- str_match(clean_text, address_pattern)

# Check if matches are found
if (!is.na(company_matches[1,1]) & !is.na(address_matches[1,1])) {
  # Extract and combine the matched components
  company_name <- company_matches[1, 2]
  principal_address <- paste(address_matches[1, 2], address_matches[1, 3], address_matches[1, 4], sep = " ")
  
  # Create a data frame with the extracted information
  df <- data.frame(
    Company_Name = company_name,
    Principal_Address = principal_address,
    stringsAsFactors = FALSE
  )
  
  print(df)
} else {
  print("No matches found.")
}