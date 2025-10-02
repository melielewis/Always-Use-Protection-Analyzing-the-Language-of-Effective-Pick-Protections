if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, spacyr, tidymodels, 
               textrecipes, finetune, httr, rvest)

load("~/bball.RData")
set.seed(9302025)
protected_assets = protected_assets %>%
  mutate(y = factor(protectionApplied))
df_split = initial_split(protected_assets, strata = y)
df_train = training(df_split)
df_test = testing(df_split)
set.seed(2025)
df_folds = vfold_cv(df_train, strata = y, v = 10)

rec = recipe(y ~ desc2, data = df_train) %>%
  step_tokenize(desc2, custom_token = spacy_ent) %>%
  step_stopwords(desc2, custom_stopword_source = c(date,"(",")"
                                                   , "pick", "picks", "in", "protected",
                                                   "round", "TEAM", "first", "second", "of", "to",
                                                   "draft", "and", "pick(s", "from", "prior", "trade",
                                                   "with"
  )) %>%
  step_untokenize(desc2) %>%
  step_tokenize(desc2, engine = "spacyr") %>%
  step_pos_filter(desc2, keep_tags = c("ADJ", "ADP", "ADV", "NOUN", "NUM", "PROPN","VERB")) %>%
  step_ngram(desc2, num_tokens = 3, min_num_tokens =1) %>%
  step_tfidf(desc2)

### model specs
lasso_spec = logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

words = workflow() %>%
  add_recipe(rec) %>%
  add_model(lasso_spec) 

tune_rs = tune_bayes(words,
                     resamples = df_folds,
                     iter = 20,
                     metrics = metric_set(roc_auc, accuracy, sens, spec),
                     control = control_bayes(save_pred = TRUE, verbose_iter = TRUE)
) 

final = finalize_workflow(words,
                          tune_rs %>% select_best())

test_fit = last_fit(final, df_split, metrics = metric_set(roc_auc, accuracy, sens, spec))
test_fit %>% collect_metrics()
last_fit = extract_workflow(test_fit)
tidy(last_fit) %>%
  slice_max(abs(estimate), n = 20) %>%
  mutate(term = str_remove_all(term, "tfidf_desc2"),
         term = fct_reorder(term, abs(estimate))) %>%
  ggplot(aes(x = abs(estimate), y = term, fill = estimate > 0)) +
  geom_col() + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_fill_discrete(labels = c("Less effective", "More effective"))

save.image("~/bball.RData")