# modified sample function as per help documents; needed to
#   avoid sample.int behaviour when x is a single integer
safe_sample <- function(x, ...) x[sample.int(length(x), ...)]
