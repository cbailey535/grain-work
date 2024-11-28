Sys.setenv(BEARER_TOKEN = "AAAAAAAAAAAAAAAAAAAAANqevQAAAAAAfKziWdhr5U0Acv6aN%2BOf639q1HE%3DD2gEgGsv3zQe0vwjTLNgbXizf50GgLWoewYGYlm2wisbMNoKtX")

#install.packages("httr")
#install.packages("jsonlite")
#install.packages("dplyr")

require(httr)
require(jsonlite)
require(dplyr)

bearer_token <- Sys.getenv("BEARER_TOKEN")
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

params <- list(`user.fields` = 'description',
               `expansions` = 'pinned_tweet_id')



handle <- "hunterfiber"
url_handle <-
  sprintf('https://api.x.com/2/users/by?usernames=%s', handle)

response <-
  httr::GET(url = url_handle,
            httr::add_headers(.headers = headers),
            query = params)
obj <- httr::content(response, as = "text")
print(obj)

handle <- "hunterfiber"
url_handle <- sprintf('https://api.twitter.com/2/users/by?usernames=%s', handle)

# Set up your Bearer Token
bearer_token <- "YOUR_BEARER_TOKEN"  # Replace with your actual token

# Set the headers to include the Bearer Token
headers <- c(
  `Authorization` = paste("Bearer", bearer_token)
)

response <- httr::GET(url = url_handle,
                      httr::add_headers(.headers = headers))

# Check the content of the response
obj <- httr::content(response, as = "text")
print(obj)