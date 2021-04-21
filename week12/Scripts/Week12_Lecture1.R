####Working with strings ###
####Shanelle Wikramanayake###
#### 2021-03-08####

### Load libraries ###
library(here)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(janeaustenr)

  

#Paste words together
paste("High temp", "Low pH", sep = "-") #can create a column together
#paste 0 does't add a space


shapes <- c("Square", "Circle", "Triangle") #create repeat phrases with a vector
paste("My favorite shape is a", shapes)

#analyses with strings
shapes
str_length(shapes) #length of string

seq_data<-c("ATCCCGTC")
str_sub(seq_data, start = 2, end = 4) #string subset to extract specific characters

str_sub(seq_data, start = 3, end = 3) <- "A" #This adds an A in the third position
seq_data


str_dup(seq_data, times = c(2, 3)) #duplicate twice and thrice

badtreatments<-c("High", " High", "High ", "Low", "Low") 
badtreatments
str_trim(badtreatments) #removes extra blanks on either. use side = "left/right" to remove from a particular side

str_pad(badtreatments, 5, side = 'right', pad = "1") #adds spaces to make sure everything is the same number of characters, but you can specific a letter or number with pad = "x"

#locale sensitive: default is English

x<-"I love R!"
str_to_upper(x) #makes everything caps

str_to_lower(x) #makes everything simple

str_to_title(x) #caps first letter of every word



#pattern matching 
data<-c("AAA", "TATA", "CTAG", "GCTT")
str_view(data, pattern = "A") #find first instance of A in all our strings in our vector

str_detect(data, pattern = "A") #gives you true/false if your charcter pattern in your strings

str_locate(data, pattern = "AT") #tells if you have character and where it starts and ends



#Regular expressions: use cheat sheet


#metacharacters won't be used by R for R operations by using two //

vals<-c("a.b", "b.c","c.d")

str_replace(vals, "\\.", " ") #searches for fullstop and replaces . with a space


#_all_ looks for all instances 

str_replace_all(vals, "\\.", " ")


#sequences

val2<-c("test 123", "test 456", "test")
str_subset(val2, "\\d") #match a digit character: only subset the string with digit characters in it 


#character class are a set of chars in a []

str_count(val2, "[aeiou]") #number of lower case vowels in our string 
str_count(val2, "[0-9]") #count digits 

#quantfiers also have meaning liek metacharacters
#ex find phone numbers
strings<-c("550-153-7578",
           "banana",
           "435.114.7586",
           "home: 672-442-6739")

#extract the numbers and clean vector
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"  #find a phone number that doesn't start with a 1 and has . or - 
#^ {2} means the next two digits. so 1st digit is between 2-9 and next {2} can be 0-9
str_detect(strings, phone)

#think pair share

test<-str_subset(strings, phone)
test

test_clean <- test %>% 
  str_replace_all("\\.", "-") %>% 
  str_replace_all(pattern = "home: ", "")
  
  

test_clean


#tidy text
head(austen_books()) #don't use view. Computer will die
tail(austen_books())


original_books <- austen_books() %>% # get all of Jane Austen's books
  group_by(book) %>%
  mutate(line = row_number(), # find every line
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", # cumulative sum, and use str_detect that has the regular expression "chapter [\\divxlc] is any digit or roman numeral,  "^" is starting with 
                                                 ignore_case = TRUE)))) %>% #ignores upper and lower case 
  ungroup() # ungroup it so we have a dataframe again

head(original_books)


#individuals words are tokens. Unnesting makes each row one word

tidy_books <- original_books %>%
  unnest_tokens(output = word, input = text) #one word per row that's all lower case
head(tidy_books) #Only head and tail. Don't kill computer with view


head(get_stopwords()) #stopwords are the meh banal words

cleaned_books <- tidy_books %>%
  anti_join(get_stopwords()) #gets rid of all the stopwords 

head(cleaned_books)


cleaned_books %>%
  count(word, sort = TRUE) #mr is the most and mrs is is the second most ????
#group by book sorts by book or group by book chapter for chapter in book


#sentiment analysis
#get sentiment is all positive and negative words
sent_word_counts <- tidy_books %>%
  inner_join(get_sentiments()) %>% #inner join to keep positive and negative words
  count(word, sentiment, sort = TRUE) #counts them
sent_word_counts

#plot it 
sent_word_counts %>%
  filter(n > 150) %>% #take only if there are over 150 instances of it otheriwse too much. you will kill computer
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% #create column for if word is negative then number is minus
  mutate(word = reorder(word, n)) %>% #sort it so its largest to smallest
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")


#word cloud
words<-cleaned_books %>%
  count(word) %>% #count all the words
  arrange(desc(n))%>% #sott them in descending order
  slice(1:100) #take only the top 100, others don't matter
wordcloud2(words, shape = 'triangle', size=0.3) #make the word cloud

