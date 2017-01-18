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

