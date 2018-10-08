install.packages("tm")
library(tm)
setwd("https://raw.githubusercontent.com/ShailyBarjatya/R-Scripts/master/Text_WordAssociation_Pattern-Mining/")
text_corpus<-Corpus(DirSource("HealthNews"))
writeLines(as.character(text_corpus[[1]]))
text_corpus <- tm_map(text_corpus, stripWhitespace)
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))
text_corpus <- tm_map(text_corpus, removePunctuation)
dtm <- DocumentTermMatrix(text_corpus)
dtm2 <- removeSparseTerms(dtm, sparse=0.95)
dtm3 <-DocumentTermMatrix(text_corpus, control=list(wordLengths=c(4, 20), bounds = list(global = c(5,200))))
findFreqTerms(dtm3, lowfreq = 200)
freq <- sort(colSums(as.matrix(dtm3)), decreasing=TRUE)
head(freq,10)
tail(freq,10)

m3<-as.matrix(dtm3)
df3<-as.data.frame(m3)
labels<-c(rep("dentistry",60),rep("depression",40),rep("diabetes",100))
labels<-as.factor(labels)
df3$label<-labels


# Calculate the entropy of a vector of counts or proportions
# Inputs: Vector of numbers
# Output: Entropy
entropy <- function(p) {
  # Assumes: p is a numeric vector
  if (sum(p) == 0) {
    return(0) 
  }
  p <- p/sum(p) # Normalize so it sums to 1
  p <- p[p > 0] # Discard zero entries (because 0 log 0 = 0)
  H = -sum(p*log(p,base=2))
  return(H)
}

entropy(c(60,40,100))


# Count how many documents in each class do or don’t contain a
# word
# Inputs: data matrix of word counts with class labels (BoW),
# word to check (word)
# Outputs: table of counts
word.class.indicator.counts <- function(dataframe,word) {
  # What are the classes?
  classes <- levels(dataframe[,"label"])
  # Prepare a matrix to store the counts, 1 row per class, 2 cols
  # (for present/absent)
  counts <- matrix(0,nrow=length(classes),ncol=2)
  # Name the rows to match the classes
  rownames(counts) = classes
  for (i in 1:length(classes)) {
    # Get a Boolean vector showing which rows belong to the class
    instance.rows = (dataframe[,"label"]==classes[i])
    # sum of a boolean vector is the number of TRUEs
    n.class = sum(instance.rows) # Number of class instances
    present = sum(dataframe[instance.rows,word] > 0)
    # present = Number of instances of class containing the word
    counts[i,1] = present
    counts[i,2] = n.class - present
  }
  return(counts)
}

counts_food<-word.class.indicator.counts(df3,"food")

# Get the mutual information
# Inputs: array of indicator counts
# Calls: entropy()
# Outputs: mutual information
word.mutual.info <- function(counts) {
  # Assumes: counts is a numeric matrix
  # get the marginal entropy of the classes (rows) C
  marginal.entropy = entropy(rowSums(counts))
  # Get the probability of each value of X
  probs <- colSums(counts)/sum(counts)
  # Calculate the entropy of each column
  column.entropies = apply(counts,2,entropy)
  conditional.entropy = sum(probs*column.entropies)
  mutual.information = marginal.entropy - conditional.entropy
  return(mutual.information)
}

word.mutual.info(counts_food)

# Calculate mutual information for each word
# Inputs: data frame of word counts with class labels
# Output: two-column matrix giving the mutual information of each word

# checking the word
  infos <- function(dataframe) {
  lexicon <- colnames(dataframe)
  # One of these columns will be class labels, that’s not a lexical item
  lexicon <- setdiff(lexicon,"label")
  vocab.size = length(lexicon)
  word.infos <- matrix(0,nrow=vocab.size,ncol=1)
  # Name the rows so we know what we’re talking about
  rownames(word.infos) = lexicon
  for (i in 1:vocab.size) {
    counts <- word.class.indicator.counts(dataframe,lexicon[i])
    word.infos[i,1] = word.mutual.info(counts)
  }
  return(word.infos)
  }
infos<-infos(df3)
head(infos,10)
row_numbers<-order(infos[,1],decreasing = TRUE)
head(infos[row_numbers,],10)

