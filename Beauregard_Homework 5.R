# Before we do anything, we need to load the following libraries. They will help us
# organize the speeches so that we can process them into usable data.

library(tidyverse)
library(tidytext)
library(textstem)
library(topicmodels)

# Then we load our collection of 1000 presidential speeches.

speeches = read_csv("presidential_speeches_sample.csv")

# Next, we want to generate "tokens" by dividing the speeches into individual words.

tokens = unnest_tokens(speeches, word, content)

# Now, we will create a new list of "stop words." In addition to the usual stop words -- frequently used
# words that will gum up our later analysis -- we will add the following frequent, unhelpful words so
# they don't pollute our data going forward.

custom_stop_words = bind_rows(
  stop_words,
  tibble(word=c("america", "american", "americans", "laughter", "congress", "president", "applause", "people",
                "country", "time", "government", "world", "nation", "national", "law", "bill", "prime", "minister", "talk",
                "talked")
  ))

# Now, using 'anti_join' we remove our stop words from the tokens list. 

tokens = anti_join(tokens, custom_stop_words, by="word")

# Next, we'll "lemmatize" the words, meaning that we'll reduce each word into its "lemma." For example,
# the words "running", "ran", and "runs" would all be converted to "run". 

tokens = mutate(tokens, lemma=lemmatize_words(word))

# Next, we'll create a table listing each word in each document. Then, we'll
# convert this list into a matrix.

wcounts = group_by(tokens, document, lemma) %>% summarize(count=n())
word_mtx = cast_dtm(wcounts, document, lemma, count)

# Now, we create our model.

model = LDA(word_mtx, 10, control=list(seed=42))

# Now, we create a beta, which tells us how frequently a word is associated
# with a certain topic.

beta = tidy(model, matrix="beta")

# This beta can then be used to generate "topics", which come with a list
# of the top ten words most closely associated with them. By looking at the
# words associated with each topic, we can form an idea of the subject
# of each topic. 

ggplot(top_10, aes(y=reorder_within(term, beta, topic), x=beta)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()

# The gamma matrix, on the other hand, checks how related each document is
# to certain topics.

gamma = tidy(model, matrix="gamma")

# By creating a list of documents that have high gammas with 
# certain topics, we can continue to get a sense of what each topic is about.

filter(gamma, topic==9) %>% arrange(-gamma)

# The results of this filter show that topic 9 is strongly associated with
# soldiers and veterans, as well as memorials to the war dead.

# Now, in order to see how topic prevalance changes year to year, we first
# need to grab the years from the titles of each document. We can do so
# by using the following regular expression, and by creating a new "year"
# variable in the 'speeches' dataset.

speeches[,c("year")] = str_match(speeches$document, "([[:digit:]]{4})")[2]

# Then, we convert this new year variable from being a "character" using
# 'as.numeric".

speeches = mutate(speeches, year=as.numeric(year))

# Then, we create a new dataset which has the gamma for each document
# as well as the year in which the speech was delivered.

# This will create a dataset that contains just the speeches and which 
# year they were given by removing the 'document' column.

speeches_year = subset(speeches, select = -c(content))

# This will join the 'speeches_year' dataset with the 'gamma' dataset.

gamma_year = left_join(gamma, speeches_year, by="document")

# Now, we use the following functions to create an average for each gamma
# in each year. This will show the prevalence of each issue in each 
# year.

iraq = filter(gamma_year, topic==5)
iraq = iraq %>%
  group_by(year) %>%
  summarize(gamma=mean(gamma))
  
energy = filter(gamma_year, topic==7)
energy = energy %>%
  group_by(year) %>%
  summarize(gamma=mean(gamma))

# Finally, we can plot a graph showing frequency of each topic in every
# year. 

ggplot(data = iraq, aes(x=year, y=gamma)) +
  geom_line()

ggplot(data = energy, aes(x=year, y=gamma)) +
  geom_line()
