######################################################################################################################

#setting up the column classes for the next csv.
goodsclasses <- c("numeric", "character", "character","numeric", "character")

#make an csv file using the data inside the sql EB-build-goods, did some editing to the text file so that it can be read as csv.
goods <- read.csv(file="C:/Users/Amir/Desktop/MMU/3rd Year/2nd Sem/Data Mining/Assignment/Part2/goods.txt", header = FALSE, col.names = c("item_id","flavour","food","price","type_food"), colClasses = goodsclasses)

str(goods)

#merge the column flavour and food under one column.
goods$item = paste(goods$flavour, goods$food, sep=" ")
goods

#remove " ' " in column item
goods$item = gsub( "'", "", as.character(goods$item))
goods

#remove other unecessary columns
goods <- goods[ -c(2:5)]
goods
#goods.csv is ready

###############################################################################################################
#setting up column classes
dfclasses <- c("numeric", "numeric", "numeric")

#read file 5000i which contains 5000 transactions
df <- read.csv(file = 'C:/Users/Amir/Desktop/MMU/3rd Year/2nd Sem/Data Mining/Assignment/Part2/5000i.csv', header = FALSE, colClasses = dfclasses)
df

#set column name for dataset
colnames(df) <- c("transaction_id","item_count", "item_id")
#5000i.csv is ready

############################################################################################################################

#take steps to merge both dataframe based on item_id
?merge
extended_bakery <- merge(df, goods, by="item_id")
extended_bakery

#move column so that transaction_id is first
extended_bakery <- extended_bakery[,c(2,3,1,4)]
extended_bakery

#change dataset to transaction (group up the items for a single receipt)
transaction_bakery <- as(split(extended_bakery[, "item"], extended_bakery[, "transaction_id"]), "transactions")
inspect(transaction_bakery)

#############################################################################################################################
#start association rules

#search for association rules
install.packages("arules")

library(arules)
?apriori
association_rules <- apriori(transaction_bakery, parameter=list(support=0.005, confidence=0.5))
inspect(association_rules)

#association rules are sorted by lift
sorted_rules <- sort(association_rules, by = "lift")
inspect(sorted_rules)


#find redundant rules
subset.matrix <- is.subset(sorted_rules,sorted_rules)
subset.matrix[lower.tri(subset.matrix,diag=T)] <- NA
redundant <- colSums(subset.matrix,na.rm = T) >= 1
which(redundant)

#remove redundant rules
cleaned_rules <- sorted_rules[!redundant]
inspect(cleaned_rules)

