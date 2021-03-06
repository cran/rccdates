## ------------------------------------------------------------------------
library(rccdates)

## ------------------------------------------------------------------------
as.Dates(d)

## ------------------------------------------------------------------------
as.Dates(c("", NA, "2000-01-01", "20000101", "20000000", "7403"))

## ------------------------------------------------------------------------
df1 <- df2 <- data.frame(
  important_date = "1985-05-04",
  another_date = "2001-09-11",
  something_else = "halleluja!"
)
str(df1)
dts <- grepl("dat", names(df1))
df1[dts] <- lapply(df1[dts], as.Date)
str(df1)

## ------------------------------------------------------------------------
df2 <- as.Dates(df2)
str(df2)

## ------------------------------------------------------------------------
# Let's make some random dates
x <- Sys.Date() - sample(365:(5 * 365), 5)

# The year is usually treated as a string in one of two ways:
(y1 <- substr(x, 1, 4))
(y2 <- format(x, format = "%Y"))

## ------------------------------------------------------------------------
y1 <- as.numeric(y1)
log(y1)
y1 ^ 3

