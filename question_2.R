if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, quanteda, spacyr, tidymodels, 
               textrecipes, udpipe, finetune, httr, rvest)

load("~/bball.RData")

cf_perf = cf_perf %>%
  mutate(games = as.integer(games),
         mp = as.numeric(mp),
         PER = as.numeric(PER),
         ws_48 = as.numeric(ws_48),
         bpm = as.numeric(bpm),
         vorp = as.numeric(vorp),
         player_type = ifelse(playerType == "Relinquished", "Substitute", "Protected"))

cf_text = left_join(cf_data %>% mutate(idPlayer = as.character(idPlayer)) %>%
                      select(tradeId, idPlayer, pick, round, desc2, cf_ProtectionId), 
                    cf_perf %>% select(idPlayer, games, PER, ws_48, bpm, vorp))
cf_text = left_join(cf_text, 
                    cf_perf %>%
                      select(idPlayer, round, pick, games, PER, ws_48, bpm, vorp) %>%
                      rename(cf_ProtectionId = idPlayer,
                             cf_round = round,
                             cf_pick = pick,
                             cf_games = games,
                             cf_per = PER,
                             cf_ws48 = ws_48,
                             cf_bpm = bpm,
                             cf_vorp = vorp))
cf_text = cf_text %>%
  mutate(pick_dif = pick - cf_pick,
         ws_dif = ws_48 - cf_ws48,
         vorp_dif = vorp - cf_vorp)
cf_sum = cbind(cf_text %>% summarize(pick = round(mean(pick),2), 
                                   games = round(mean(games, na.rm = T),2), 
                                   per = round(mean(PER, na.rm = T),2), 
                                   bpm = round(mean(bpm, na.rm = T),2), 
                                   ws_48 = round(mean(ws_48, na.rm = T),2), 
                                   vorp = round(mean(vorp, na.rm = T),2)) %>%
  gather() %>%
  rename(Substitute = value),
  cf_text %>% summarize(pick = round(mean(cf_pick),2), 
                        games = round(mean(cf_games, na.rm = T),2), 
                        per = round(mean(cf_per, na.rm = T),2), 
                        bpm = round(mean(cf_bpm, na.rm = T),2), 
                        ws_48 = round(mean(cf_ws48, na.rm = T),2), 
                        vorp = round(mean(cf_vorp, na.rm = T),2)) %>%
    gather() %>%
    rename(Protected = value) %>%
    select(-key)) %>%
  mutate(Difference = round(Substitute - Protected,2),
         `T-Value` = NA,
         `P-Value` = NA)
cf_sum$`T-Value`[1] = round(t.test(cf_text$pick, cf_text$cf_pick, paired = TRUE)$statistic,2)
cf_sum$`T-Value`[2] = round(t.test(cf_text$games, cf_text$cf_games, paired = TRUE)$statistic,2)
cf_sum$`T-Value`[3] = round(t.test(cf_text$PER, cf_text$cf_per, paired = TRUE)$statistic,2)
cf_sum$`T-Value`[4] = round(t.test(cf_text$bpm, cf_text$cf_bpm, paired = TRUE)$statistic,2)
cf_sum$`T-Value`[5] = round(t.test(cf_text$ws_48, cf_text$cf_ws48, paired = TRUE)$statistic,2)
cf_sum$`T-Value`[6] = round(t.test(cf_text$vorp, cf_text$cf_vorp, paired = TRUE)$statistic,2)
cf_sum$`P-Value`[1] = round(t.test(cf_text$pick, cf_text$cf_pick, paired = TRUE)$p.value,3)
cf_sum$`P-Value`[2] = round(t.test(cf_text$games, cf_text$cf_games, paired = TRUE)$p.value,3)
cf_sum$`P-Value`[3] = round(t.test(cf_text$PER, cf_text$cf_per, paired = TRUE)$p.value,3)
cf_sum$`P-Value`[4] = round(t.test(cf_text$bpm, cf_text$cf_bpm, paired = TRUE)$p.value,3)
cf_sum$`P-Value`[5] = round(t.test(cf_text$ws_48, cf_text$cf_ws48, paired = TRUE)$p.value,3)
cf_sum$`P-Value`[6] = round(t.test(cf_text$vorp, cf_text$cf_vorp, paired = TRUE)$p.value,3)

set.seed(123)
cf_split = initial_split(cf_text %>% filter(!is.na(vorp_dif)),
                         strata = vorp_dif)
cf_train = training(cf_split)
cf_test = testing(cf_split)
set.seed(234)
cf_folds = vfold_cv(cf_train, strata = vorp_dif, v = 10)
cf_rec =  recipe(vorp_dif ~ desc2, data = cf_train) %>%
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
  step_tfidf(desc2)  %>%
  step_normalize(all_predictors())

svm_spec = svm_linear(cost = tune(), margin = tune()) %>%
  set_mode("regression") %>%
  set_engine("LiblineaR")

cf_words = workflow() %>%
  add_recipe(cf_rec) %>%
  add_model(svm_spec)
tune_cf = tune_bayes(cf_words,
                     resamples = cf_folds,
                     iter = 20,
                     metrics = metric_set(rmse,rsq, mae, mape),
                     control = control_bayes(save_pred = TRUE, 
                                             verbose_iter = TRUE))
final_cf = finalize_workflow(cf_words,
                             tune_cf %>% select_best())                     
cf_fit = last_fit(final_cf, cf_split, 
                  metrics = metric_set(rmse,rsq, mae, mape))
cf_fit %>% collect_metrics()                     
cf_last_fit = extract_workflow(cf_fit)
# cf_last_fit %>%
#   tidy() %>%
#   filter(term != "Bias") %>%
#   mutate(
#     sign = case_when(estimate > 0 ~ "Favors substitute pick ",
#                      TRUE ~ "Favors protected pick"),
#     estimate = abs(estimate),
#     term = str_remove_all(term, "tfidf_desc2_")
#   ) %>%
#   group_by(sign) %>%
#   top_n(20, estimate) %>%
#   ungroup() %>%
#   ggplot(aes(x = estimate,
#              y = fct_reorder(term, estimate),
#              fill = sign)) +
#   geom_col(show.legend = FALSE) +
#   scale_x_continuous(expand = c(0, 0)) +
#   facet_wrap(~sign, scales = "free")

tidy(cf_last_fit) %>%
  filter(term != "Bias") %>%
  slice_max(abs(estimate), n = 20) %>%
  mutate(
    term = str_remove_all(term, "tfidf_desc2_"),
    term = fct_reorder(term, abs(estimate))
  ) %>%
  ggplot(aes(x = abs(estimate), y = term, fill = estimate > 0)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_discrete(labels = c("Favors substitute pick", "Favors protected pick")) +
  labs(x = "Estimate from linear SVM (absolute value)", y = NULL, 
       fill = "Substitute - Protected VORP?")
