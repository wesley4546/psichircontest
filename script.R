
# Package installing/loading ----------------------------------------------

# Below are the packages used and a brief description of their purpose

# `dplyr` - Data wrangling with ease!
# `tidyr`- Functions to help in the pursuit of "tidy data"
# `ggplot2` - The bee's knees of data visualization
# `readr` - A better way to read in data
# `modeest` - A solution to missing mode functionality within R

if(!requireNamespace("dplyr")) install.packages("dplyr")
if(!requireNamespace("tidyr")) install.packages("tidyr")
if(!requireNamespace("ggplot2")) install.packages("ggplot2")
if(!requireNamespace("readr")) install.packages("readr")
if(!requireNamespace("modeest")) install.packages("modeest")


# Now that we've installed all the latest tools we can load them into our environment

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(modeest)

# Level 1 -----------------------------------------------------------------

# Create the total superficial judgment summary score
# (i.e., sum judge_pic, judge_outfit, judge_job, judge_born, judge_live,
# judge_car, judge_uni, judge_diet, judge_drink, judge_teeth, judge_posts,
# judge_grammar, judge_sparetime)

# `read_csv()` does a better job at "guessing" default data types
singleUS <- read_csv("singleUS-singleUS.csv")

# Since I'm working within each row of our data `rowwise()` is a super easy tool
# that allows us to do that easily
summary_scores <-
  singleUS %>%
  rowwise() %>%
  mutate(sfj_score = sum(c_across(judge_pic:judge_sparetime))) %>%
  ungroup() # `ungroup()` to remove `rowwise()` meta-grouping


# Level 2 -----------------------------------------------------------------

# Examine the total superficial judgment summary score with a histogram.

# This is where the lovely ggplot2 comes in
ggplot(summary_scores, aes(x = sfj_score)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Histogram of Superficial Judgment Summary Scores (SFJ Score)",
    subtitle = "Total yes/no judgment responses by each candidate",
    x = "SFJ Score",
    y = "Number of Responses",
    caption = "Data: OSF PsyChiR Contest"
  ) +
  scale_x_continuous(breaks = 0:13) # label each bar with respective score


# Level 3 -----------------------------------------------------------------

# Compute mean/median/mode/standard deviation for the total superficial judgment summary score.

summary_scores %>%
  summarize(
    mean = mean(sfj_score),
    std_dev = sd(sfj_score),
    med = median(sfj_score),
    most = modeest::mfv(sfj_score) # modeest's mode function
  )


# Level 4 -----------------------------------------------------------------

# Compute means and standard deviation for superficial judgment summary
# by "onenight" group & do a t-test.

summary_scores %>%
  group_by(onenight) %>% # group by different `onenight` scores
  summarize(
    mean = mean(sfj_score),
    std_dev = sd(sfj_score),
    med = median(sfj_score),
    most = modeest::mfv(sfj_score)
  )


# Now in order to do a t-test, we have to get our data in a specific format that
# matches the `t.test()` function

# We can see that from the code above, we have some NA values. Thanks to the
# helpful `drop_na()` function we can deal with them easily.

t_test <- summary_scores %>%
  group_by(onenight) %>%
  select(ID, sfj_score, onenight) %>%
  drop_na()

# Now it's as simple as running the test

t.test(t_test$sfj_score ~ t_test$onenight)
