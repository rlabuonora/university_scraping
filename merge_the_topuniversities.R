# Merge data from both sources

# topuniversities
programs_final <- readRDS('./data/programs_final.rds')


# THE
# Scores has no names
scores <- readRDS('./data/scores.rds')
rank <- readRDS('./data/ranks.rds')

programs_final %>% count(university)


rank
