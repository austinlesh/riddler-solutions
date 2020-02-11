# Link to puzzle:
# https://fivethirtyeight.com/features/how-many-more-palindrome-dates-will-you-see/

library(stringi)

start <- as.Date("02/03/2020", format = "%m/%d/%Y")
end <- as.Date("12/31/2099", format = "%m/%d/%Y")

dates <- seq.Date(from = start, to = end, by = "day")
dates.str <- as.character(dates)

formatted.dates <- paste0(substr(dates.str, 6, 7), substr(dates.str, 9, 10), substr(dates.str, 1, 4))

# There is a nice function in the "stringi" package to reverse a string
is_palindrome <- function(date.string){
  if (date.string == stri_reverse(date.string)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

ans <- sapply(formatted.dates, is_palindrome)

print(length(ans[ans == TRUE]))