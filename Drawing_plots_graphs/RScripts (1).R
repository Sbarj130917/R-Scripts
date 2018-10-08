load('/Users/chuqinli/Documents/KDD_2018Fall/05PatternMining/titanic.raw.rdata')
str(titanic.raw)

install.packages("arules")
library(arules)

rules<-apriori(titanic.raw)
inspect(rules)

#We then set rhs=c("Survived=No", "Survived=Yes") in appearance to make sure that only "Survived=No" and "Survived=Yes" will appear in the rhs of rules.
# rules with rhs containing "Survived" only
rules2<-apriori(titanic.raw,parameter=list(minlen=2,supp=0.005,conf=0.8),appearance=list(rhs=c("Survived=No","Survived=Yes"),default="lhs"))
inspect(rules2)

rules2.sort<-sort(rules2,by="lift")
inspect(rules2.sort)


#If a rule is a super rule of another rule, the former rule is considered redundant.  
#Generally speaking, when a rule (such as rule 2) is a super rule of another rule (such as rule 1) and the former has the same or a lower lift, 
#the former rule (rule 2) is considered to be redundant. Below we prune redundant rules. 

#remove redundant rule
subset.matrix<-is.subset(rules2.sort,rules2.sort)
subset.matrix
subset.matrix[lower.tri(subset.matrix,diag=T)]<-0
subset.matrix
redudant<-colSums(subset.matrix)>=1
redudant

rules2.sort.pruned<-rules2.sort[!redudant]
inspect(rules2.sort.pruned)

install.packages("arulesViz")
library(arulesViz)

plot(rules2.sort.pruned)
plot(rules2.sort.pruned,method="graph",control=list(type="items"))
plot(rules2.sort.pruned,method="paracoord",control=list(reorder=TRUE))
