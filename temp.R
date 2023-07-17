library(text)
library(tm)

# Example vector of university names
university_names <- c("Stanford University", "Harvard University", "Massachusetts Institute of Technology", "California Institute of Technology")

# Example vector where names appear slightly changed
input_vector <- c("Stanford Universty", "Harvad University", "MIT", "Caltech")

# Preprocess the university names
university_corpus <- Corpus(VectorSource(university_names))
preprocessed_universities <- tm_map(university_corpus, content_transformer(tolower))
preprocessed_universities <- tm_map(preprocessed_universities, removePunctuation)
preprocessed_universities <- tm_map(preprocessed_universities, removeNumbers)
preprocessed_universities <- tm_map(preprocessed_universities, removeWords, stopwords("en"))
preprocessed_universities <- tm_map(preprocessed_universities, stripWhitespace)

# Create a document-term matrix for the university names
dtm <- DocumentTermMatrix(preprocessed_universities)

# Preprocess the input vector
input_corpus <- Corpus(VectorSource(input_vector))
preprocessed_input <- tm_map(input_corpus, content_transformer(tolower))
preprocessed_input <- tm_map(preprocessed_input, removePunctuation)
preprocessed_input <- tm_map(preprocessed_input, removeNumbers)
preprocessed_input <- tm_map(preprocessed_input, removeWords, stopwords("en"))
preprocessed_input <- tm_map(preprocessed_input, stripWhitespace)

# Create a document-term matrix for the input vector
input_dtm <- DocumentTermMatrix(preprocessed_input, control = list(dictionary = Terms(dtm)))

# Calculate TF-IDF scores for the university names and input vector
university_tfidf <- weightTfIdf(dtm)
input_tfidf <- weightTfIdf(input_dtm)

# Compute cosine similarity between the input vector and university names
similarity_scores <- cosine(input_tfidf, university_tfidf)

# Find the highest similarity score for each input and its corresponding university name
matched_indices <- apply(similarity_scores, 1, which.max)
matched_universities <- university_names[matched_indices]

# Print the matched university names
print(matched_universities)