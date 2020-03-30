# Link to puzzle:
# https://fivethirtyeight.com/features/how-many-more-palindrome-dates-will-you-see/

library(stringi)

start <- as.Date("02/03/2020", format = "%m/%d/%Y")
end <- as.Date("12/31/2099", format = "%m/%d/%Y")

dates <- seq.Date(from = start, to = end, by = "day")
dates_string <- as.character(dates)

dates_formatted <- paste0(substr(dates_string, 6, 7), substr(dates_string, 9, 10), substr(dates_string, 1, 4))

# There is a nice function in the "stringi" package to reverse a string
is_palindrome <- function(date_string){
  if (date_string == stri_reverse(date_string)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

answer <- sapply(dates_formatted, is_palindrome)

print(length(answer[answer == TRUE]))