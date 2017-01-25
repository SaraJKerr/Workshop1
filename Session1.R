# Frankfurt Workshop 2017

# Session 1

# Session 1, subsections 1--4 are adapted from Matthew L. Jockers (2014) 
# Text Analysis with R for Students of Literature.
# Session 1, subsection 5 is adapted from blog post 
# https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/
# Session 2 is adapted from Benjamin Schmidt's (2015) blog post 
# http://bookworm.benschmidt.org/posts/2015-10-25-Word-Embeddings.html and my
# doctoral research 'Jane Austen in Vector Space' presented at JADH (2016), 
# which can be found here: https://sarajkerr.com/talks-and-papers/

# Set your working directory - go to Session - Set Working Directory - Choose 
# Directory select the Workshop folder which you have saved to your desktop.
# 1
# Load the first file
ja_ch <- scan("Austen_Texts/Chapter_Files/PP1_En.txt", what = "character", 
              sep = "\n")

# This can also be done using
mytext <- scan(file.choose(), what ="character", sep = "\n") 
# which allows you to search for the file you want

# A character file has been created with 80 elements.
# View the top 6 elements of the file

head(ja_ch)

# To access an individual element or a selection of elements:
ja_ch[1]
ja_ch[3:4]

# Remove the first two lines, the title and the chapter number
ja_ch1 <- ja_ch[3:80]
# Check the first 6 lines 
head(ja_ch1)

# Load the second file, remove the first two lines and save as ja_ch1d
ja_ch_d <- scan("Austen_Texts/Chapter_Files/PP1_De.txt", what = "character", 
                sep = "\n")
ja_ch1d <- ja_ch_d[3:80]

# Remove line breaks from each file (if you wish to start over in the same 
# session rerun these two lines of code to recreate the unprocessed text)
text1 <- paste(ja_ch1, collapse = " ")
text2 <- paste(ja_ch1d, collapse = " ")
               
# The chapters are now a single string of characters

# Processing
# Change text to lower case
text1 <- tolower(text1) 
# Check in the environment pane - the capital letters have now been removed

# select words only
text1 <- strsplit(text1, "\\W") # The second argument is a regular expression

# If we look in the environment pane text1 is now a 'List of 1'
text1 <- unlist(text1) # simplify to a vector
text1
# The whole of the chapter is now displayed - but where there was punctuation we
# have "". Now we need to remove these blank spaces.
text1 <- text1[which(text1 != "")]
# This subsets the vector by removing elements which are not spaces
text1

length(text1) # number of tokens


text2 <- tolower(text2)
text2 <- strsplit(text2, "\\W")
text2 <- unlist(text2)
text2 <- text2[which(text2 != "")]
text2

# 2.
# You can search for a word by its index number - R starts indexing at 1 - or by
# using the which() function.

text1[42]
text2[723]

which(text1 == "neighbourhood")
which(text2 == "nachbarschaft")

# number of times a word appears in the text
length(text1[which(text1 == "neighbourhood")])

# number of types in the text
length(unique(text2))

# create a table of frequencies
text1_freqs <- table(text1)

# sort by frequency
sorted_text1 <- sort(text1_freqs, decreasing = TRUE)

# View the top 10 most frequent words in the text
sorted_text1[1:10]

# View as a plot
plot(sorted_text1[1:10])
# If all 10 words don't appear click on the Zoom option in the plot pane.

# To calculate the relative frequencies
rel_freqs_text1 <- 100 * (sorted_text1 / sum(sorted_text1))

# Plot top 10 relative frequencies
plot(rel_freqs_text1[1:10], type = "b", main= "Relative frequencies in PP Ch 1",
        xlab = "Top Ten Words", ylab = "Percentage of Chapter")
# If all 10 words don't appear click on the Zoom option in the plot pane.

# 3
# So far we have focused on a single chapter - now we will explore a
# full novel, Mansfield Park

# Load the text into R
text_MP <- scan("Austen_Texts/Novel_Files/Austen_1814_MP.txt", 
                what = "character", sep = "\n")

# View the first 6 lines of the file
head(text_MP)

# Using regular expressions to identify chapter index positions
chapters <- grep("^CHAPTER \\d", text_MP)

text_MP[chapters]

# Add an end point to the novel and add to chapters
text_MP <- c(text_MP, "END") # the length of text_MP is increased by 1
finish <- length(text_MP)
chapters <- c(chapters, finish) 

# Calculating the word frequencies by chapter using a for loop

ch_raw_freqs <- list() # Creating empty lists to be filled by the loop
ch_rel_freqs <- list()

for(i in 1:length(chapters)) {
        if(i != length(chapters)){
                ch_title <- text_MP[chapters[i]]
                start <- chapters[i] + 1
                end <- chapters[i + 1] - 1
                ch_lines <- text_MP[start:end]
                ch_text <- tolower(paste(ch_lines, collapse = " "))
                ch_text <- strsplit(ch_text, "\\W")
                ch_text <- unlist(ch_text)
                ch_text <- ch_text[which(ch_text != "")]
                ch_freqs <- table(ch_text)
                ch_raw_freqs[[ch_title]] <- ch_freqs
                rel_ch_freqs <- 100 * (ch_freqs / sum(ch_freqs))
                ch_rel_freqs[[ch_title]] <-rel_ch_freqs
        }
}

# To find out the frequency of a word across each chapter
marriage <- lapply(ch_rel_freqs, "[", "marriage")

head(marriage) # shows the relative frequency per chapter

marriage_com <- do.call(rbind, marriage) # Combines the results by row 
# This creates a matrix

head(marriage_com) 

# Compare instances of 'he' and 'she' in the novel
he <- lapply(ch_rel_freqs, "[", "he")
he_com <- do.call(rbind, he)
she <- lapply(ch_rel_freqs, "[", "she")
she_com <- do.call(rbind, she)

# Extract the relative frequencies and combine in columns
he_rf <- he_com[, 1] 
she_rf <- she_com[, 1] 
# This extracts all rows of the 1st column - to extract data from a matrix
# you use nameofmatrix[rows, columns]
# Combine the frequencies into two columns
rf_he_she <- cbind(he_rf, she_rf)

head(rf_he_she)

# Create a plot comparing the frequencies side by side
barplot(rf_he_she, beside = TRUE, col = "red")

# This can be made into a function for ease of use
# The arguments for the function are the chapter relative frequencies and 
# your two chosen words - the words need to be in speech marks
rel_freq_comp <- function(ch_rel_freqs, word1, word2) {
        worda <- lapply(ch_rel_freqs, "[", word1)
        worda_com <- do.call(rbind, worda)
        wordb <- lapply(ch_rel_freqs, "[", word2)
        wordb_com <- do.call(rbind, wordb)
        worda_rf <- worda_com[, 1] 
        wordb_rf <- wordb_com[, 1] 
        rf_a_b <- cbind(worda_rf, wordb_rf)
        # This changes the column names to your chosen words
        colnames(rf_a_b) <- c(word1, word2) 
        barplot(rf_a_b, beside = TRUE, col = "red", 
                main = "Relative Frequencies By Chapter")
}

# To use the function to compare the names of the main characters
rel_freq_comp(ch_rel_freqs, "fanny", "edmund")

# 4
# Create a function to read contexts of a file

# Create a path to the files
input.dir <- "Austen_Texts/Novel_Files" 
# Read the name of all .txt files
files <- dir(input.dir, "\\.txt") 

# Create a function to list the files
show_files <- function(files){
        for(i in 1:length(files)){
                cat(i, files[i], "\n", sep = " ")
        }
}

# To use the function:
show_files(files) # the files are listed and numbered

# Create a function to create a list of words from each file, you will recognise
# the code from the previous sections

make_word_list <- function(files, input.dir) {
       # create an empty list for the results
        word_list <- list()
        # read in the files and process them
        for(i in 1:length(files)) {
             text <- scan(paste(input.dir, files[i], sep = "/"), 
                          what = "character", sep = "\n")   
             text <- paste(text, collapse = " ")
             text_lower <- tolower(text)
             text_words <- strsplit(text_lower, "\\W")
             text_words <- unlist(text_words)
             text_words <- text_words[which(text_words != "")]
             word_list[[files[i]]] <- text_words
        }
        return(word_list)
}

# If you scroll down to the bottom of the environment pane you will see the
# 3 functions we have created
# To use the function:
my_corpus <- make_word_list(files, input.dir) 

# Creating a Key Word in Context (KWIC) function - this uses the output from
# the make_word_list and includes the show_files function 

kwic <- function(my_corpus) {
        show_files(names(my_corpus))
        # identify the chosen file
        file_id <- as.numeric(
          readline("Which file would you like to examine? Enter a number: \n"))
        # identifythe number of words each side of the keyword
        context <- as.numeric(
                readline("How much context do you want? Enter a number: \n"))
        # identify the focus word
        keyword <- tolower((readline("Enter a keyword: \n")))
        # create the KWIC readout
        hits <- which(my_corpus[[file_id]] == keyword)
        if(length(hits) > 0) {
                result <- NULL
                for(j in 1:length(hits)) {
                        start <- hits[j] - context
                        if(start < 1) {
                                start <- 1
                        }
                        end <- hits[j] + context
                        cat("\n--------------------", j, "----------------\n")
                        cat(my_corpus[[file_id]][start:(hits[j] -1)], sep = " ")
                        cat("[", my_corpus[[file_id]][hits[j]], "] ", sep = " ")
                        cat(my_corpus[[file_id]][(hits[j] +1): end], sep = " ")
                        myrow <- cbind(hits[j],
                                paste(my_corpus[[file_id]][start: (hits[j] -1)],
                                      collapse = " "),
                                paste(my_corpus[[file_id]][hits[j]],
                                      collapse = " "),
                                paste(my_corpus[[file_id]][(hits[j] +1): end],
                                      collapse = " "))
                        result <- rbind(result, myrow)
                }
                colnames(result) <- c("position", "left", "keyword", "right")
                return(result)
        } else {
                cat("YOUR KEYWORD WAS NOT FOUND\n")
        }
}

# To use the function:
results <- kwic(my_corpus)
# select the file, context and keyword when prompted
# For example - choose text 1, 7 for context and work as the keyword


# If you wish to save the results as a .csv file
write.csv(results, "Results/yourkeyword_text.csv")
# e.g. if the chosen word was word in Pride and Prejudice
write.csv(results, "Results/work_PP.csv")

# 5
# Start by clearing your workspace - this can be done by clicking on the 
# broom in the environment pane or choosing Session - Clear Workspace

# Tell the computer where the texts are located

dname <- file.path("Austen_Texts/Novel_Files") # Tells R the path to the files
dname
dir(dname) # Lists the files in the directory/folder

# 
# Once a package has been installed it needs to be loaded into the library 
# before it can be used 
library(tm) # tm is a text mining package
library(ggplot2) # ggplot2 is a graph plotting package
library(wordcloud) # lets you create wordclouds

# This instruction reads in a collection of texts from a directory, the result 
# is a V or volatile Corpus - this corpus is temporary to the R session
# Load the corpus
docs <- Corpus(DirSource(dname)) 

# Preprocess the text
docs <- tm_map(docs, removePunctuation)   # Remove punctuation   
docs <- tm_map(docs, removeNumbers)      # Remove numbers    
docs <- tm_map(docs, tolower)   # Convert to lowercase   
# docs <- tm_map(docs, removeWords, stopwords("english")) # To remove stopwords
docs <- tm_map(docs, stripWhitespace)   # Strip whitespace   
docs <- tm_map(docs, PlainTextDocument) 
## This is the end of the preprocessing stage.   

# Create a Document Term Matrix
dtm <- DocumentTermMatrix(docs)   

# To view or export the DTM
corpus_dtm <- as.matrix(dtm)
rownames(corpus_dtm) <- c("PP", "MP")

# It is also possible to create a Term Document Matrix
tdm <- TermDocumentMatrix(docs)
corpus_tdm <- as.matrix(tdm)
colnames(corpus_tdm) <- c("PP", "MP")

# Write files to a folder
write.csv(corpus_dtm, "Results/dtm.csv")
write.csv(corpus_tdm, "Results/tdm.csv")

# Explore your data - this gives the raw frequency for each word in the corpus      
freq <- colSums(as.matrix(dtm))   
length(freq)   # the number of unique words in the corpus
head(freq) # view the top 6 elements

# Create a frequency graph of the words which appear more than 500 times  
  
word_freq <- data.frame(word=names(freq), freq=freq)   
p <- ggplot(subset(word_freq, freq>500), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p  

# Create a wordcloud
  
set.seed(142)   # set.seed is used to ensure replicability
wordcloud(names(freq), freq, max.words=100)   

set.seed(142)   
wordcloud(names(freq), freq, min.freq=100, scale=c(5, .1), 
          colors=brewer.pal(8, "Set1"))  # This section determines the colours

# You could compare the results by removing the stopwords
