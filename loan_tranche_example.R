library(data.table)

#create the months for the fake data
month_labels <- data.frame(a = (c("2015-08",
                                  "2015-09",
                                  "2015-10",
                                  "2015-11",
                                  "2015-12",
                                  "2016-01",
                                  "2016-02",
                                  "2016-03",
                                  "2016-04",
                                  "2016-05",
                                  "2016-06",
                                  "2016-07")),
                           b = c(1:12))

set.seed(12345)


#create the fake data
t1 <- function(x) {
  lt1 <- data.frame(loan_month = sort(rep(month_labels$a, length(month_labels$a))),
                    loan_amount = sort(rep(c((sample(c(160000:2600000), 1, FALSE) + 
                                                cumsum(sample(c(-100000, 100000), 12, TRUE)))*
                                               sample(c(1+.01*(5:15)), 12, TRUE)), length(month_labels$a))),
                    durational_factor = rep(month_labels$b, length(month_labels$a)),
                    rate = rep(c(.10, .12, .18, .14, .07, .05, .03, .03, .02, .01, .005, .0025), 
                               length(month_labels$a)^2)*sample(c(1+.01*(5:30)), length(month_labels$a)^2, TRUE),
                    loan_type = "A",
                    region = x)
}
loan_type1 <- do.call("rbind", lapply(c("Northeast","Midwest","Southeast","West"), function(x) t1(x))) 


t2 <- function(x) {
  lt2 <- data.frame(loan_month = sort(rep(month_labels$a, length(month_labels$a))),
                    loan_amount = sort(rep(c((sample(c(4500000:6500000), 1, FALSE) + 
                                                cumsum(sample(c(-200000, 200000), 12, TRUE)))*
                                               sample(c(1+.01*(4:10)), 12, TRUE)), length(month_labels$a))),
                    durational_factor = rep(month_labels$b, length(month_labels$a)),
                    rate = rep(c(.03, .05, .10, .12, .18, .14, .07, .03, .02, .01, .005, .0025), 
                               length(month_labels$a)^2)*sample(c(1+.01*(5:30)), length(month_labels$a)^2, TRUE),
                    loan_type = "B",
                    region = x)
}
loan_type2 <- do.call("rbind", lapply(c("Northeast","Midwest","Southeast","West"), function(x) t2(x))) 

t3 <- function(x) {
  lt3 <- data.frame(loan_month = sort(rep(month_labels$a, length(month_labels$a))),
                    loan_amount = sort(rep(c((sample(c(750000:1750000), 1, FALSE) + 
                                                cumsum(sample(c(-50000, 50000), 12, TRUE)))*
                                               sample(c(1+.01*(2:15)), 12, TRUE)), length(month_labels$a))),
                    durational_factor = rep(month_labels$b, length(month_labels$a)),
                    rate = rep(c(.09, .09, .09, .09, .09, .08, .07, .05, .05, .03, .02, .005), 
                               length(month_labels$a)^2)*sample(c(1+.01*(5:30)), length(month_labels$a)^2, TRUE),
                    loan_type = "C",
                    region = x)
}
loan_type3 <- do.call("rbind", lapply(c("Northeast","Midwest","Southeast","West"), function(x) t3(x))) 

df <- rbind(loan_type1, loan_type2, loan_type3)
df <- unique(df)

month_labels$a <- as.character(month_labels$a)
df$loan_month <- as.character(df$loan_month)
recov_month <- function(x) {
  y <- month_labels[nrow(month_labels[month_labels$a <= df[x,1], ]) + df[x,3]-1, 1]
}
z <- c(1:nrow(df))
recovery_month <- do.call("rbind", lapply(z, function(x) recov_month(x)))
df <- cbind(df, recovery_month)
df <- df[!is.na(df$recovery_month), ]
df$dollars_issued <- df$loan_amount * df$rate 
df$dollars_recovered <- -df$dollars_issued
df$rate <- NULL 
rm(recovery_month); gc()

df1 <- unique(df[c("loan_month", "loan_amount", "loan_type", "region")])
df2 <- data.table(df)
df2 <- data.frame(df2[, list(subtract = sum(dollars_recovered)),
           by = c("loan_month", "loan_type", "region")])
df2 <- merge(df1, df2, by = c("loan_month", "loan_type", "region"))
df2$dollars_issued <- df2$loan_amount + df2$subtract
df2$dollars_recovered <- 0
df2$recovery_month <- ""
df2$durational_factor <- 0
df2 <- df2[c("loan_month", "loan_amount", "durational_factor", "loan_type", "region", "recovery_month", "dollars_issued", "dollars_recovered")]
df <- rbind(df, df2)
rm(df1, df2); gc()
