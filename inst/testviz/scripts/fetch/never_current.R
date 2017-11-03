fetch.never_current <- function(viz) {
  # formatTimestamp is usually used within fetchTimestamp methods; just using it
  # here as a convenient way to create always-new information in an always-new
  # file
  writeLines(formatTimestamp(Sys.time()), viz$location)
}

fetchTimestamp.never_current <- neverCurrent
