# Set your working directory - go to Session - Set Working Directory - Choose 
# Directory select the Fr_Workshop_17 which you have saved to your desktop.

# Load the first file
ja_ch <- scan("Austen_Texts/Chapter_Files/PP1_En.txt", what = "character", 
              sep = "\n")

# The a character file has been created with 80 elements.
# View the top 6 elements of the file

head(ja_ch)

# Remove the first two lines, the title and the chapter number, leaving just the 
# text
ja_ch1 <- ja_ch[3:80]
# Now check the first 6 lines of text1
head(ja_ch1)

# Load the second file, remove the first two lines and save as text2
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

text2 <- tolower(text2)

# select words only
text1 <- strsplit(text1, " ", fixed = T)
# If we look in the environment pane text1 is now a 'List of 1'
# simplify to a vector
text1 <- unlist(text1)
text1
# The whole of the chapter is now displayed - but where there was punctuation we
# have "". Now we need to remove these blank spaces.
text1 <- text1[which(text1 != "")]



text2 <- strsplit(text2, "\\W")
text2 <- unlist(text2)
text2 <- text2[which(text2 != "")]


# We can look in the environment pane to see how many words there are in each
# version of the chapter or use:
length(text1)
length(text2)

# Each word