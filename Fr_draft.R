# Set your working directory - go to Session - Set Working Directory - Choose 
# Directory select the Workshop folder which you have saved to your desktop.

# Load the first file
ja_ch <- scan("Austen_Texts/Chapter_Files/PP1_En.txt", what = "character", 
              sep = "\n")

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
plot(rel_freqs_text1[1:10], type = "b", xlab = "Top Ten Words", 
     ylab = "Percentage of Chapter")

# So far we have focused on a single chapter - now we will explore a
# full novel, Mansfield Park

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

