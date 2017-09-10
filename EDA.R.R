#
# Spark Funds Case Study Group Submission by:
# 1. Sudeep Upadhya
# 2. Rishab Nigam
# 3. Vikash Prasad
# 4. Amit Mankikar
#
# Date: 25-06-2017
#

#install.packages("RH2")
#install.packages("sqldf")
#install.packages("gridExtra")
require(gridExtra)

library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
library(sqldf)
library(RH2)

#
# Set working directory
# 
setwd("C:/UpGrade Training/Group Case Study 2")

#
# This is our master frame and will be used throughout the analysis
#
loan<-read.csv("loan.csv",stringsAsFactors = F, na.strings=c("", " ", "NA", "N/A"))


#
# Data Frame cleaning goes here
#

#
# We need only 2 classes, FULLY PAID and CHARGED OFF in our analysis
# We dont need CURRENT because we dont know if they will be FULLY PAID/ CURRENT in future
# Get rid of all loan_status those are still on going
# i.e. loan_status = "CURRENT"
#
loan<-loan[-which(toupper(loan$loan_status) == "CURRENT"), ]

#
# Get rid of all columns having only 1 unique value
# Ref: https://stackoverflow.com/questions/30544282/how-to-remove-columns-with-same-value-in-r
#
loan <- loan[vapply(loan, function(x) length(unique(x)) > 1, logical(1L))]

#
# URL is useless without a login/ passwd, remove url column
#
loan <- subset(loan, select = -c(url) )

#
# We are not going to do any kind of text analysis/ NLP, get rid of the desc, title column
# we will not lose any important information by removing title, as the column purpose seems
# to be a drop down and so, the same information is captured there
loan <- subset(loan, select = -c(desc, title) ) 

#
# The following columns contain only 0, NA. If we impute NA to 0, they boil down to
# columns having only 1 value = 0. So get rid of them as they fail to provide any value
#
# collections_12_mths_ex_med,
# chargeoff_within_12_mths,
# tax_liens
loan <- subset(loan, select = -c(collections_12_mths_ex_med, chargeoff_within_12_mths, tax_liens) )

#
# We can get rid of member_id as we can clearly see that all loans are individual loans
# and so just the loan Id will suffice. Lets get rid of the redundant member_id as there is a 1 - 1
# mapping from loan Id to member Id
#
loan <- subset(loan, select = -c(member_id) )

#
# In fact, since we dont analyze at a single loan borrower's level
# Every analysis is done on the collection, so we dont need id as well
# We can uncomment this whenever we need to do detailed analysis on User
#
loan <- subset(loan, select = -c(id) )

#
# Let us find out which columns are non-numeric
# Ref: https://stackoverflow.com/questions/13706188/importing-csv-file-into-r-numeric-values-read-as-characters
#
num_data <- data.frame(data.matrix(loan))
numeric_columns <- sapply(num_data,function(x){mean(as.numeric(is.na(x)))<0.5})
rm(num_data)

#
# Convert all non-numeric columns to upper
# Ref: https://stackoverflow.coam/questions/16516593/convert-from-lowercase-to-uppercase-all-values-in-all-character-variables-in-dat
#
loan[,!numeric_columns] <- mutate_each(loan[,!numeric_columns], funs(toupper))
rm(numeric_columns)

#
# Employer Name Cleaning based on discrepencies seen in manual sifting
#
loan$emp_title <- gsub(" AND ", "&", loan$emp_title)
loan$emp_title <- gsub("LLC", "", loan$emp_title)
loan$emp_title <- gsub("LLP", "", loan$emp_title)
loan$emp_title <- gsub("INC", "", loan$emp_title)
loan$emp_title <- gsub("\\UNITED STATES", "US", loan$emp_title)

loan$emp_title <- gsub("\\.", "", loan$emp_title)
loan$emp_title <- gsub("\\,", "", loan$emp_title)
loan$emp_title <- gsub("\\'", "", loan$emp_title)
loan$emp_title <- gsub("\\:", "", loan$emp_title)
loan$emp_title <- gsub("\\-", "", loan$emp_title)
loan$emp_title <- gsub("\\)", "", loan$emp_title)
loan$emp_title <- gsub("\\(", "", loan$emp_title)
loan$emp_title <- gsub("\\(", "", loan$emp_title)

loan$emp_title <- gsub("[[:space:]]", "", loan$emp_title)

loan$emp_title[which(loan$emp_title == "USAIRFORCE")] <- "USAF"
loan$emp_title[which(loan$emp_title == "USARMY")] <- "ARMY"
loan$emp_title[which(loan$emp_title == "USNAVY")] <- "NAVY"
loan$emp_title[which(loan$emp_title == "USPOSTALSERVICE")] <- "USPS"
loan$emp_title[which(loan$emp_title == "UNITEDPARCELSERVICE")] <- "UPS"
loan$emp_title[which(loan$emp_title == "VERIZONWIRELESS")] <- "VERIZON"
loan$emp_title[which(loan$emp_title == "SELFEMPLOYED")] <- "SELF"
loan$emp_title[which(loan$emp_title == "THEHOMEDEPOT")] <- "HOMEDEPOT"

#
# Convert columns containing % to numeric
#
loan$int_rate <- as.numeric(gsub("%", "", loan$int_rate))
loan$revol_util <- as.numeric(gsub("%", "", loan$revol_util))

#
# On understanding the loan grade system, it was found that loan grade determines the 
# interest rate. Which means we already have nicely categorized loan rate of interest
# based on loan grades. Therefore we may get rid of the int_rate column as well
#
loan <- subset(loan, select = -c(int_rate) )

#
# Order the data frame so we can read and understand the data dictionary in its order
#
loan <- loan[c(sort(colnames(loan)))]


#
# Data Frame cleaning ends here
#

write.csv(loan, file = "cleanLoan.csv")

chargedoffloan<-loan[which(toupper(loan$loan_status) == "CHARGED OFF"), ]
fullypaidloan<-loan[-which(toupper(loan$loan_status) == "CHARGED OFF"), ]

#
# uni-variate analysis starts here
#
#boxplot(loan$annual_inc[loan$annual_inc < 82000])
#summary(loan$annual_inc)

#
# Address state and zip_code analysis
#
ggplot(data = chargedoffloan, aes(reorder(addr_state, -table(addr_state)[addr_state]))) + geom_bar(stat="count") + scale_y_continuous(trans='log2') + geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ xlab("State as per the Address") + ylab("Count of Applicants in each State") + ggtitle("Applicants on the basis of States ")
ggplot(data = chargedoffloan, aes(reorder(zip_code, zip_code)))  + geom_point(stat="count") +  xlab("Zip Codes") + ylab("Count of Applicants in each zip code") + ggtitle("Applicants Across the Zip Codes ")


#
# Home ownership univariate analysis
#

ggplot(data = chargedoffloan, aes(reorder(home_ownership, -table(home_ownership)[home_ownership]))) + geom_bar(stat="count") + scale_y_continuous(trans='log2') + geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ xlab("Home Ownership")+ylab("Applicants Count") + ggtitle("Home Ownership of the Applicants") 

tmp1 <- count(chargedoffloan, home_ownership)
tmp1$home_ownership <- paste( tmp1$home_ownership, ": ", percent(tmp1$n/sum(tmp1$n)))
bp <- ggplot(tmp1, aes(x="", y=tmp1$n, fill=home_ownership)) + geom_bar(width = 1,stat = "identity") 
bp+ coord_polar("y", start=0) + ggtitle("Home Ownership of the Applicants") 
rm(bp)
rm(tmp1)


#
# Check Verification status
#
ggplot(data = chargedoffloan, aes(reorder(verification_status, -table(verification_status)[verification_status]))) + geom_bar(stat="count") +geom_text(stat='count',aes(label=..count..),vjust=-0.5)+xlab("Verification Status of Applicant ")+ ylab("Applicants Count")+ ggtitle("Verification Status of Applicant's Income")

#
# Check Home loan purpose
#
ggplot(data = chargedoffloan, aes(reorder(purpose, -table(purpose)[purpose]))) + geom_bar(stat="count") + scale_y_continuous(trans='log2') +geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ scale_x_discrete(labels = abbreviate)+ xlab("Purpose given by applicant") + ylab("Count of Applicants") +ggtitle("Reasons for which the Loan was taken")

#
# CheckTerm
#
ggplot(data = chargedoffloan, aes(term)) + geom_bar(stat="count") + geom_text(stat='count',aes(label=..count..), vjust=-0.25)+xlab("Term of Payments") + ylab("Count of Applicants") + ggtitle("No of payments on the loan (Values are in months and can be either 36 or 60)")

#
# Grade & Sub-Grade
#
p1 <- ggplot(data = chargedoffloan, aes(grade)) + geom_bar(stat="count")+ geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) + xlab("Grade") + ylab("Count of Applicants") + ggtitle("No of Applicants in each Grade")
p2 <- ggplot(data = chargedoffloan, aes(sub_grade)) + geom_bar(stat="count") +geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25)+ xlab("Sub Grade") + ylab("Count of Applicants") + ggtitle("No of Applicants in each Sub Grade")
grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)
#
# Employee title and length analysis
#
tmp <- chargedoffloan[ chargedoffloan$emp_length != "N/A", ]
tmp$emp_length <- gsub("< 1 YEAR", "0 YEARS", tmp$emp_length)
tmp$emp_length <- gsub("10\\+ YEARS", "BEYOND 10 YEARS", tmp$emp_length)
ggplot(data = tmp, aes(emp_length)) + geom_bar(stat="count") + scale_y_continuous(trans='log2') + geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ xlab("Number of Years") + ylab("Count of Applicants") + ggtitle("No of Years for which the Applicants are employed")

rm(tmp)

emp_frame <- count(chargedoffloan, emp_title)
emp_frame <- emp_frame[ -which(is.na(emp_frame$emp_title)), ]
emp_frame <- head(arrange(emp_frame, desc(n)), 10)
ggplot(data = emp_frame, aes(reorder(emp_title, -n), n)) + geom_bar(stat="identity") + geom_text(aes(label=n),vjust=-0.5) +xlab("Employee Title") + ylab("Count of Applicants") + ggtitle("Companies in which the Applicants are working") 
rm(emp_frame)


#
# Earliest Credit Line Analysis
# Time starts from 1970-01-01, since our dates are before that, we have a problem
# so, lets not use the Date format, instead make our own Year format and see the 
# distribution and boxplot
#
t1 <- loan # make a copy
t1$yr <- as.numeric(substr(loan$earliest_cr_line, 5, 6)) #extract the year 
t1$yr <- ifelse(t1$yr >20 & t1$yr <=99, t1$yr+1900, t1$yr+2000) #add 1900 or 2000
t2 <- group_by(t1, yr) # group by yr
t2 <- summarise(t2, cnt = length(yr)) # get no of instances

ggplot(t2, aes(x=t2$yr, y=t2$cnt)) + geom_bar(stat = "identity") + geom_text(aes(label=t2$cnt),vjust=-0.5) +xlab("Inquries") + ylab("No. of Inquiries") + ggtitle("Freq of Inquiries in last 6 months") 
boxplot(t2$cnt)
summary(t2)

rm(t1)
rm(t2)


#
# Loan Issue Date Analysis
#
t1 <- loan # make a copy
t1$yr <- as.numeric(substr(loan$issue_d, 5, 6)) #extract the year 
t1$yr <- ifelse(t1$yr >20 & t1$yr <=99, t1$yr+1900, t1$yr+2000) #add 1900 or 2000
t2 <- group_by(t1, yr) # group by yr
t2 <- summarise(t2, cnt = length(yr)) # get no of instances

ggplot(t2, aes(x=t2$yr, y=t2$cnt)) + geom_bar(stat = "identity") + geom_text(aes(label=t2$cnt),vjust=-0.5) +xlab("Years") + ylab("Count") + ggtitle("Freq of Issued Loans binned by Year") 

summary(t2)

rm(t1)
rm(t2)

#
# Last Credit Pull Date Analysis
#
t1 <- loan # make a copy
t1$yr <- as.numeric(substr(loan$last_credit_pull_d, 5, 6)) #extract the year 
t1$yr <- ifelse(t1$yr >20 & t1$yr <=99, t1$yr+1900, t1$yr+2000) #add 1900 or 2000
t2 <- group_by(t1, yr) # group by yr
t2 <- summarise(t2, cnt = length(yr)) # get no of instances

ggplot(t2, aes(x=t2$yr, y=t2$cnt)) + geom_bar(stat = "identity") + geom_text(aes(label=t2$cnt),vjust=-0.5) +xlab("Years") + ylab("Count") + ggtitle("Freq of Last Credit Pulls binned by Year") 

summary(t2)

rm(t1)
rm(t2)

#
# Last Payment Date Analysis
#
t1 <- loan # make a copy
t1$yr <- as.numeric(substr(loan$last_pymnt_d, 5, 6)) #extract the year 
t1$yr <- ifelse(t1$yr >20 & t1$yr <=99, t1$yr+1900, t1$yr+2000) #add 1900 or 2000
t1 <- t1[-which(is.na(t1$yr)),]
t2 <- group_by(t1, yr) # group by yr
t2 <- summarise(t2, cnt = length(yr)) # get no of instances

ggplot(t2, aes(x=t2$yr, y=t2$cnt)) + geom_bar(stat = "identity") + geom_text(aes(label=t2$cnt),vjust=-0.5) +xlab("Years") + ylab("Count") + ggtitle("Freq of Last Payment Dates binned by Year") 

summary(t2)

rm(t1)
rm(t2)



#
# Funded Amount & Funded Amount invested 
#
nrow(loan[loan$funded_amnt == loan$funded_amnt_inv, ])
boxplot(loan$funded_amnt)
summary(loan$funded_amnt)
boxplot(loan$funded_amnt_inv)
summary(loan$funded_amnt_inv)

#
# Months since last delinquency 
#
loan$mths_since_last_delinq <- as.numeric(loan$mths_since_last_delinq)
nrow(loan[which(is.na(loan$mths_since_last_delinq)), ])
nrow(loan[loan$mths_since_last_delinq ==0,])

boxplot(loan$mths_since_last_delinq)
summary(loan$mths_since_last_delinq)
#
# There are too many NAs, and we also have 0s
# we must remove them at the time of analysis
#

#
# Months since last record 
#
loan$mths_since_last_record <- as.numeric(loan$mths_since_last_record)
nrow(loan[which(is.na(loan$mths_since_last_record)), ])
nrow(loan[loan$mths_since_last_record ==0,])

boxplot(loan$mths_since_last_record)
summary(loan$mths_since_last_record)
#
# There are too many NAs, and we also have 0s
# we must remove them at the time of analysis
#

#
# Open Account
#
boxplot(loan$open_acc)
summary(loan$open_acc)
ggplot(loan, aes(x=loan$open_acc)) + geom_bar(stat = "count") + xlab("Open Accounts") + ylab("Count") + ggtitle("Histogram of Open Accounts")

#
# Public Records & Public Record Bankruptcies
#
boxplot(loan$pub_rec)
summary(loan$pub_rec)
ggplot(loan, aes(x=loan$pub_rec)) + geom_bar(stat = "count") + xlab("No. of Public Records") + ylab("Count") + ggtitle("Histogram of Public Records")

boxplot(loan$pub_rec_bankruptcies)
summary(loan$pub_rec_bankruptcies)
ggplot(loan, aes(x=loan$pub_rec_bankruptcies)) + geom_bar(stat = "count") + xlab("No. of Public Records of Bankruptcy") + ylab("Count") + ggtitle("Histogram of Public Record Bankruptcies")






#
# No of inquiries in last 6 months
#
boxplot(loan$inq_last_6mths)
summary(loan$inq_last_6mths)
ggplot(loan, aes(x=loan$inq_last_6mths)) + geom_bar(stat = "count") +scale_y_continuous(trans='log2') +  geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) +xlab("TBD") + ylab("TBD") + ggtitle("TBD") 

#
# Total Acc
#
boxplot(loan$total_acc)
summary(loan$total_acc)

#
# Total Payment & payment inverse
#
boxplot(loan$total_pymnt)
summary(loan$total_pymnt)

boxplot(loan$total_pymnt_inv)
summary(loan$total_pymnt_inv)

#
# Total interest, late fee and principal recd. 
#
boxplot(loan$total_rec_int)
summary(loan$total_rec_int)

boxplot(loan$total_rec_late_fee)
summary(loan$total_rec_late_fee)


boxplot(loan$total_rec_late_fee)
summary(loan$total_rec_late_fee)


#
# Recovery
#
boxplot(loan$recoveries)
summary(loan$recoveries)

#
# Commenting below lines as it takes about 30 seconds to complete
# To test it, please uncomment and try
#
#t1 <- sqldf("SELECT CASE
#            WHEN recoveries BETWEEN 0 AND 3999 THEN '1. 0K-4K'
#            WHEN recoveries BETWEEN 4000 AND 7999 THEN '2. 4K-8K'
#             WHEN recoveries BETWEEN 8000 AND 11999 THEN '3. 8K-12K'
#             WHEN recoveries BETWEEN 12000 AND 15999 THEN '4. 12K-16K'
#             WHEN recoveries BETWEEN 16000 AND 19999 THEN '5. 16K-20K'
#             WHEN recoveries BETWEEN 20000 AND 23999 THEN '6. 20K-24K'
#             WHEN recoveries BETWEEN 24000 AND 27999 THEN '7. 24K-28K'
#             WHEN recoveries BETWEEN 28000 AND 31999 THEN '8. 28K-32K'
#             ELSE '9. 36K+' END AS bins,
#             COUNT(*) AS total
#             FROM loan
#             GROUP BY bins")
# 
# 
# ggplot(t1, aes(x=t1$bins, 
#                y = t1$total)) + geom_bar(stat = "identity") + xlab("Bins of size $4000") + ylab("Frequency") + ggtitle("Binned Recoveries Freq.") +scale_y_continuous(trans='log2')
# 
# rm(t1)

#
# Revolving Balance
#
boxplot(loan$revol_bal)
summary(loan$revol_bal)

boxplot(loan$revol_util)
summary(loan$revol_util)

#
# Commenting below lines as it takes about 30 seconds to complete
# To test it, please uncomment and try
#

# t1 <- sqldf("SELECT CASE
#             WHEN recoveries BETWEEN 0 AND 3999 THEN '1. 0K-4K'
#             WHEN recoveries BETWEEN 4000 AND 7999 THEN '2. 4K-8K'
#             WHEN recoveries BETWEEN 8000 AND 11999 THEN '3. 8K-12K'
#             WHEN recoveries BETWEEN 12000 AND 15999 THEN '4. 12K-16K'
#             WHEN recoveries BETWEEN 16000 AND 19999 THEN '5. 16K-20K'
#             WHEN recoveries BETWEEN 20000 AND 23999 THEN '6. 20K-24K'
#             WHEN recoveries BETWEEN 24000 AND 27999 THEN '7. 24K-28K'
#             WHEN recoveries BETWEEN 28000 AND 31999 THEN '8. 28K-32K'
#             ELSE '9. 36K+' END AS bins,
#             COUNT(*) AS total
#             FROM loan
#             GROUP BY bins")
# 
# 
# ggplot(t1, aes(x=t1$bins, 
#                y = t1$total)) + geom_bar(stat = "identity") + xlab("Bins of size $4000") + ylab("Frequency") + ggtitle("Binned Recoveries Freq.") +scale_y_continuous(trans='log2')
# 
# rm(t1)



#
# Installment
#
boxplot(loan$installment)
summary(loan$installment)

#
# Commenting below lines as it takes about 30 seconds to complete
# To test it, please uncomment and try
#

# t1 <- sqldf("SELECT CASE
#             WHEN installment BETWEEN 0 AND 199 THEN '1. 0-200'
#             WHEN installment BETWEEN 200 AND 399 THEN '2. 200-400'
#             WHEN installment BETWEEN 400 AND 599 THEN '3. 400-600'
#             WHEN installment BETWEEN 600 AND 799 THEN '4. 600-800'
#             WHEN installment BETWEEN 800 AND 999 THEN '5. 800-1K'
#             WHEN installment BETWEEN 1000 AND 1199 THEN '6. 1K-1.2K'
#             WHEN installment BETWEEN 1200 AND 1399 THEN '7. 1.2K-1.4K'
#             ELSE '8. 1.4K+' END AS bins,
#             COUNT(*) AS total
#             FROM loan
#             GROUP BY bins")
# 
# ggplot(t1, aes(x=t1$bins, 
#                y = t1$total)) + geom_bar(stat = "identity") + xlab("Bins of size $200") + ylab("Frequency") + ggtitle("Binned Instalment Freq.") +scale_y_continuous(trans='log2')
# 
# rm(t1)

#
# Last Payment Amount
#
boxplot(loan$last_pymnt_amnt)
summary(loan$last_pymnt_amnt)

#
# Commenting below lines as it takes about 30 seconds to complete
# To test it, please uncomment and try
#

# t1 <- sqldf("SELECT CASE
#             WHEN last_pymnt_amnt BETWEEN 0 AND 3999 THEN '1. 0K-4K'
#             WHEN last_pymnt_amnt BETWEEN 4000 AND 7999 THEN '2. 4K-8K'
#             WHEN last_pymnt_amnt BETWEEN 8000 AND 11999 THEN '3. 8K-12K'
#             WHEN last_pymnt_amnt BETWEEN 12000 AND 15999 THEN '4. 12K-16K'
#             WHEN last_pymnt_amnt BETWEEN 16000 AND 19999 THEN '5. 16K-20K'
#             WHEN last_pymnt_amnt BETWEEN 20000 AND 23999 THEN '6. 20K-24K'
#             WHEN last_pymnt_amnt BETWEEN 24000 AND 27999 THEN '7. 24K-28K'
#             WHEN last_pymnt_amnt BETWEEN 28000 AND 31999 THEN '8. 28K-32K'
#             ELSE '9. 36K+' END AS bins,
#             COUNT(*) AS total
#             FROM loan
#             GROUP BY bins")
# 
# ggplot(t1, aes(x=t1$bins, 
#                y = t1$total)) + geom_bar(stat = "identity") + xlab("Bins of size $2000") + ylab("Frequency") + ggtitle("Binned Last Payment Freq.") 
# 
# rm(t1)

#
# Loan Amount
#
boxplot(loan$loan_amnt)
summary(loan$loan_amnt)

#
# Commenting below lines as it takes about 30 seconds to complete
# To test it, please uncomment and try
#

# 
# t1 <- sqldf("SELECT CASE
#             WHEN loan_amnt BETWEEN 0 AND 3999 THEN '1. 0K-4K'
#             WHEN loan_amnt BETWEEN 4000 AND 7999 THEN '2. 4K-8K'
#             WHEN loan_amnt BETWEEN 8000 AND 11999 THEN '3. 8K-12K'
#             WHEN loan_amnt BETWEEN 12000 AND 15999 THEN '4. 12K-16K'
#             WHEN loan_amnt BETWEEN 16000 AND 19999 THEN '5. 16K-20K'
#             WHEN loan_amnt BETWEEN 20000 AND 23999 THEN '6. 20K-24K'
#             WHEN loan_amnt BETWEEN 24000 AND 27999 THEN '7. 24K-28K'
#             WHEN loan_amnt BETWEEN 28000 AND 31999 THEN '8. 28K-32K'
#             WHEN loan_amnt BETWEEN 32000 AND 36000 THEN '9. 32K-36K'
#       ELSE '10. 36K+' END AS bins,
#       COUNT(*) AS total
#       FROM loan
#       GROUP BY bins")
# 
# ggplot(t1, aes(x=t1$bins, y = t1$total)) + geom_bar(stat = "identity") + xlab("Bins of size $4000") + ylab("Frequency") + ggtitle("Binned Loan Amounts Freq.")
# 
# rm(t1)

#
# Annual Income univariate analysis
#
boxplot(loan$annual_inc)
summary(loan$annual_inc)
boxplot(loan$annual_inc[loan$annual_inc<150000])
summary(loan$annual_inc[loan$annual_inc<150000])

#
# Collection Recovery Fee univariate analysis
#

#
# How does the box plot look vis-a-vis the loan status?
#
boxplot(loan$collection_recovery_fee)
summary(loan$collection_recovery_fee)
boxplot(loan$collection_recovery_fee~loan$loan_status)

#
# What percentage of rows in loan df have a non zero value? 
#
nrow(loan[loan$collection_recovery_fee != 0, ])
nrow(loan)

#
# What percentage of rows in chargedoffloan df have a non zero value? 
#
nrow(chargedoffloan[chargedoffloan$collection_recovery_fee != 0, ])
nrow(chargedoffloan)

#
# What percentage of rows in fullypaidloan df have a non zero value? 
#
nrow(fullypaidloan[fullypaidloan$collection_recovery_fee != 0, ])
nrow(fullypaidloan)


#
# Doesnt make sense to keep this column in our data frames,
# Get rid of it.
#
loan <- subset(loan, select = -c(collection_recovery_fee) )
fullypaidloan <- subset(fullypaidloan, select = -c(collection_recovery_fee) )
chargedoffloan <- subset(chargedoffloan, select = -c(collection_recovery_fee) )

#
# No of delinquincy incidents in past 2 years
#
boxplot(loan$delinq_2yrs)
summary(loan$delinq_2yrs)
boxplot(loan$delinq_2yrs~loan$loan_status)

#
# What percentage of rows in loan df have a non zero value? 
#
nrow(loan[loan$delinq_2yrs != 0, ])
nrow(loan)

#
# What percentage of rows in chargedoffloan df have a non zero value? 
#
nrow(chargedoffloan[chargedoffloan$delinq_2yrs != 0, ])
nrow(chargedoffloan)

#
# What percentage of rows in fullypaidloan df have a non zero value? 
#
nrow(fullypaidloan[fullypaidloan$delinq_2yrs != 0, ])
nrow(fullypaidloan)

#
# DTI Ratio Analysis
#
boxplot(loan$dti)
summary(loan$dti)






#
# bi-variate for home ownership
#
ggplot(loan, aes(x= loan_status,  group=home_ownership)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Loan Status") +
  facet_grid(~home_ownership) +
  scale_y_continuous(labels = scales::percent)


#
# bi-variate for Purpose
#

ggplot(loan, aes(x= loan_status,  group=purpose)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") +
  facet_grid(~purpose) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)

#
# bi-variate for Grade
#

ggplot(loan[loan$emp_length != "N/A",], aes(x= loan_status,  group=grade)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") +
  facet_grid(~grade) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)


#
# bi-variate for Emp_length
#

ggplot(loan[loan$emp_length != "N/A",], aes(x= loan_status,  group=emp_length)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") +
  facet_grid(~emp_length) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)

#
# bi-variate for Verification
#

ggplot(loan, aes(x= loan_status,  group=verification_status)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") +
  facet_grid(~verification_status) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)



#
# bi-variate for emp_title
#

emp_frame <- count(loan, emp_title)
emp_frame <- emp_frame[ -which(is.na(emp_frame$emp_title)), ]
emp_frame <- head(arrange(emp_frame, desc(n)), 10)

tmp <- loan
tmp1 <- tmp[which(tmp$emp_title == emp_frame$emp_title), ]

ggplot(tmp1, aes(x= loan_status,  group=emp_title)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Status") +
  facet_grid(~emp_title) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=abbreviate)

rm(emp_frame)
rm(tmp)
rm(tmp1)


#
# bi-variate for Address State
#
p1 <- ggplot(loan, aes(x=loan$addr_state, y = (..count..)/sum(..count..), fill=factor(loan_status))) + geom_bar(position="fill") + scale_y_continuous(labels = percent) + xlab("Address State") + ylab("Applicants  Percentage")+ ggtitle("Status of Loan on basis of State") + geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) 
p2 <- ggplot(loan, aes(x=loan$purpose, fill=factor(loan_status))) + geom_bar(position="dodge",stat="count") + geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25)+ xlab("Home Ownership") + ylab("Applicants Count") + ggtitle("Status of the loan on the Basis of the type of Home") 
grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

#
# bi-variate for Employee
#

p1 <-ggplot(loan, aes(x=loan$emp_length, y = (..count..)/sum(..count..), fill=factor(loan_status))) + geom_bar(position="fill") + scale_y_continuous(labels = percent) + xlab("Home Ownership") + ylab("Applicants  Percentage")+ ggtitle("Status of the loan on the Basis of the type of Home") + geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) 
p2 <- ggplot(loan, aes(x=loan$emp_length, fill=factor(loan_status))) + geom_bar(position="dodge",stat="count") + geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25)+ xlab("Home Ownership") + ylab("Applicants Count") + ggtitle("Status of the loan on the Basis of the type of Home") 
grid.arrange(p1, p2, ncol=2)
rm(p1)
rm(p2)

#
# Scatter plot for multivariate 
#
ggplot(loan, aes(x = loan_amnt, y = purpose, col = loan_status )) + geom_point( alpha = 0.2 ) + geom_jitter()

#ggplot(loan, aes(x = addr_state, y = emp_title, col = loan_status, shape = home_ownership)) + geom_point( alpha = 0.3 )


rm(chargedoffloan)
rm(fullypaidloan)
rm(loan)
