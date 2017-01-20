# Frankfurt Workshop 2017

# Session 2

# Session 2 is adapted from Benjamin Schmidt's (2015) blog post 
# http://bookworm.benschmidt.org/posts/2015-10-25-Word-Embeddings.html and my
# doctoral research 'Jane Austen in Vector Space' presented at JADH (2016), 
# which can be found here: https://sarajkerr.com/talks-and-papers/

# Install the packages needed for this session
install.packages("magrittr")

library(wordVectors)
library(tsne)
library(Rtsne)
library(ggplot2)
library(ggrepel)
library(stringi)
library(magrittr)

# If a prepared text file has not already been created follow this step - it 
# takes in a folder of .txt files and outputs a single .txt file which combines
# the texts in one document removes punctuation and converts all words to lower
# case. 


prep_word2vec("Austen_Texts/Novel_Files", "Results/Novel_corpus.txt", 
              lowercase =  T)

# This can take some time depending on the size of the files

# train_word2vec takes several parameters - input prepared .txt file, an 
# output file, vectors are the number of dimensions the default is 100, and
# window is the number of words either side of the context word, by default
# the function uses the skip-gram method

ja <- train_word2vec("Results/Novel_corpus.txt", 
                          output = "Results/Novel.bin", threads = 3, 
                          vectors = 100, window = 15)

# This will take a little while to process
# A Vector Space Model has been created

# Exploring the model 
nearest_to(ja, ja[[c("marriage")]], 50) # 50 nearest words to marriage

marriage <- nearest_to(ja, ja[[c("marriage")]], 100)

# Create a plot of the 100 words nearest to marriage       
plot(filter_to_rownames(ja, names(marriage)))  
# This takes a little time to run


wealth <- ja %>% nearest_to(ja[[c("establishment","income", "fortune",
                "privilege","wealth", "property", "affluence")]], 300) %>% names
sample(wealth, 100)

plot(ja[[wealth[1:100], average = F]])

# using the code from session 1 explore the context of some of the words in the
# plot
