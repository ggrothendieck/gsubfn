
# linguistic applications by Stefan Th. Gries

# create word frequency list from the gsubfn COPYING file

fn1 <- system.file("COPYING", package = "gsubfn")
Lines1 <- tolower(scan(fn1, what = "char", sep = "\n"))
print(tail(sort(table(unlist(strapply(Lines1, "\\w+"))))))

# frequency list of words from an SGML-annotated text file

fn2 <- system.file("sample.txt", package = "gsubfn")
Lines2 <- scan(fn2, what = "char", sep = "\n")
tagged.corpus.sentences <- grep("^<s n=", Lines2, value = TRUE)
# just to see what it looks like
print(tagged.corpus.sentences[c(3, 8)]) 
words <- unlist(strapply(tagged.corpus.sentences, ">([^<]*)", backref = -1))
words <- gsub(" $", "", words)
print(tail(words, 25))

# frequency list of words AND tags from same file

word.tag.pairs <- unlist(strapply(tagged.corpus.sentences, "<[^<]*")) 
cleaned.word.tag.pairs <- grep("<w ", word.tag.pairs, value = TRUE)
print(tail(sort(table(cleaned.word.tag.pairs))))
print(tail(cleaned.word.tag.pairs))

