# -----------------------------------------------------------------------------
# Descriptions
# -----------------------------------------------------------------------------

# This script subsets the dataset from UCI (https://archive.ics.uci.edu/ml/datasets/Bank+Marketing) by dropping categorical variables and leaving only numerical ones.

# -----------------------------------------------------------------------------
# Subsetting and saving
# -----------------------------------------------------------------------------

rm( list=ls() )

# banking dataset from UCI repo:
banking <- read.table("bank-additional-full.csv", sep=";", header = TRUE,
                      stringsAsFactors = FALSE)
str(banking)

# keeping only numerical vars 
x <- as.matrix(banking[c("age", "duration", "euribor3m", "nr.employed", "previous", "emp.var.rate", "cons.price.idx", "cons.conf.idx")])
y <- ifelse(banking$y == "no", 0, 1)

# removing headers and row names
write.table(cbind(y,x), file="bank.csv", sep=",", 
            col.names = FALSE, row.names = FALSE)
