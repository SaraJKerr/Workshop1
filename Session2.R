# Frankfurt Workshop 2017

# Session 2

# Session 2 is adapted from Benjamin Schmidt's (2015) blog post 
# http://bookworm.benschmidt.org/posts/2015-10-25-Word-Embeddings.html and my
# doctoral research 'Jane Austen in Vector Space' presented at JADH (2016), 
# which can be found here: https://sarajkerr.com/talks-and-papers/

# 1 - Create, or read in, a Vector Space Model
 
library(wordVectors)
library(tsne)
library(Rtsne)
library(ggplot2)
library(ggrepel)
library(stringi)


# If a prepared text file has not already been created follow this step - it 
# takes in a folder of .txt files and outputs a single .txt file which combines
# the texts in one document removes punctuation and converts all words to lower
# case. 


prep_word2vec("Austen_Texts/Novel_Files", 
              "Austen_Texts/Novel_Files/Novel_corpus.txt", lowercase =  T)

# This can take some time depending on the size of the files

# train_word2vec takes several parameters - input prepared .txt file, an 
# output file, vectors are the number of dimensions the default is 100, and
# window is the number of words either side of the context word, by default
# the function uses the skip-gram method

ja <- train_word2vec("Austen_Texts/Novel_Files/Novel_corpus.txt", 
                          output = "Results/Novel.bin", threads = 3, 
                          vectors = 100, window = 15)

# This will take a little while to process
# A Vector Space Model has been created

# If a model has previously been created and saved it can be loaded using
ja <- read.vectors("Results/Novel.bin")

# 2 Prepare texts for kwic analysis
# Using the Novel_corpus.txt file created above, and the code we wrote in 
# Session 1.4, create a word list and load the kwic function

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


# To use the function:
my_corpus <- make_word_list(files, input.dir) 

# The Novel_corpus.txt file will be read in as 2 items as it has already been
# processed - this doesn't have an impact on using kwic() later.

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

# To use the function, this time there will be a choice of three files, 
# Pride and Prejudice, Mansfield Park, and a text combining the two:
results <- kwic(my_corpus)

# 3. Exploring the Vector Space Model
# Exploring the model 
# The nearest_to() function uses cosine similarity
nearest_to(ja, ja[["marriage"]], 50) # 50 nearest words to marriage

marriage <- nearest_to(ja, ja[["marriage"]], 100)
# This creates a vector of distances for the 100 words nearest to marriage

# Create a plot of the 100 words nearest to marriage       
plot(ja[[names(marriage), average = F]])  
# This takes a little time to run

# You can also search for words nearest to a semantic field
wealth <- nearest_to(ja, ja[[c("establishment","income", "fortune",
                "privilege","wealth", "property", "affluence")]], 300)

wealth <- names(wealth)

sample(wealth, 100)
# This allows a random sample of 100 words to be taken from the vector


# An alternative is to use the 'magrittr' package which acts as a pipeline 
# enabling short cuts
# It works in a similar manner to the vector for marriage but extracts the names
install.packages("magrittr")
library(magrittr)
wealth1 <- ja %>% nearest_to(ja[[c("establishment","income", "fortune",
                "privilege","wealth", "property", "affluence")]], 300) %>% names
sample(wealth1, 100) 


# Plot the chosen segment
plot(ja[[wealth[1:100], average = F]])

# using the kwic() function explore the context of some of the words in the
# plot
results1 <- kwic(my_corpus)

# If you wish to save your results amend this piece of code:
# If you wish to save the results as a .csv file
write.csv(results, "Results/yourkeyword_text.csv")

# 4 Using the w2v_analysis Function

# To speed up the exploration of a vector space model I created a function 
# using the wordVectors package but plotting with Rtsne (which is quicker than
# tsne) in ggplot2 which allows more modifications

# The function takes 5 arguments:
# vsm - a vector space model 
# words - a character vector of focus words
# seed - an integer
# path - the path to the folder you want files saved to 
# ref_name - the reference name for the exported files 

# The function will create a vector which is the average of the words input and 
# will output a wordlist of the 500 nearest words, a csv of the words and their
# positions, and a plot of the 2D reduction of the vector space
# model using the Barnes-Hut implementation of t-SNE. The points for each word
# are marked in red so the labels can be moved by ggrepel for ease of reading.
# set.seed is used to ensure replicability

w2v_analysis <- function(vsm, words, seed, path, ref_name) {
        # Set the seed
        if (!missing(seed))
                set.seed(seed)
        
        # Identify the nearest 10 words to the average vector of search terms
        ten <- nearest_to(vsm, vsm[[words]])
        
        # Identify the nearest 500 words to the average vector of search terms and 
        # save as a .txt file
        main <- nearest_to(vsm, vsm[[words]], 500)
        wordlist <- names(main)
        filepath <- paste0(path, ref_name)
        write(wordlist, paste0(filepath, ".txt"))
        
        
        # Create a subset vector space model
        new_model <- vsm[[wordlist, average = F]]
        
        # Run Rtsne to reduce new VSM to 2D (Barnes-Hut)
        reduction <- Rtsne(as.matrix(new_model), dims = 2, initial_dims = 50, 
                           perplexity = 30, theta = 0.5, check_duplicates = F,
                           pca = F, max_iter = 1000, verbose = F, 
                           is_distance = F, Y_init = NULL)
        
        # Extract Y (positions for plot) as a dataframe and add row names
        df <- as.data.frame(reduction$Y)
        rows <- rownames(new_model)
        rownames(df) <- rows
        
        # Save dataframe as .csv file
        write.csv(df, paste0(filepath, ".csv"))
        
        # Create t-SNE plot and save as jpeg
        ggplot(df) +
                geom_point(aes(x = V1, y = V2), color = "red") +
                geom_text_repel(aes(x = V1, y = V2, label = rownames(df))) +
                xlab("Dimension 1") +
                ylab("Dimension 2 ") +
                # geom_text(fontface = 2, alpha = .8) +
                theme_bw(base_size = 12) + 
                theme(legend.position = "none") +
                ggtitle(paste0("2D reduction of VSM ", ref_name," using t_SNE"))
        
        ggsave(paste0(ref_name, ".jpeg"), path = path, width = 24, 
               height = 18, dpi = 100)
        
        new_list <- list("Ten nearest" = ten, "Status" = "Analysis Complete") 
        return(new_list)
        
}

# To use the function a single word or a vector of connected words can be used
status <- c("status", "rank", "position", "superior", "aristocracy", "gentry")

# The function is called as below:
# w2v_analysis(VectorSpaceModel, Word, Number, "Path/", "Reference_Name")
# E.g.
w2v_analysis(ja, status, 42, "Results/", "Status")

# This will take a short time to run. The console will return the 10 nearest
# words and include the status "Analysis Complete".
# If you look in the Results folder there will be 3 new files a .txt file
# containing the word list of the 500 nearest words, a csv containing the
# words and their position in the plot, and a .jpeg file containing the plot

