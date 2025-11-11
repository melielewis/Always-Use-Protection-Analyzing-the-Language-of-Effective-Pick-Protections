if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, spacyr, tidymodels, tidytext,
               textrecipes, finetune, showtext, patchwork)

load("~/bball.RData")
font_add(family = "Cambria", regular = "cambria.ttf")
theme_set(theme_minimal(base_family = "Cambria"))
###### create data.frame of examples #####
examples = data.frame("desc2" = c("first round pick protected top 14 in 2024, top 12 in 2025, top 10 in 2026, else 2026 second round pick, 2027 second round pick",
                                  "first round pick protected top 12 in 2025, top 10 in 2026, else 2026 second round pick, 2027 second round pick",
"2026 first round pick (least favorable of TEAM, TEAM (protected top 4), TEAM picks)",
"2026 draft pick (first round protected top 4, else second round)",
"2026 first round pick (least favorable of TEAM, TEAM, TEAM)",
"RELINQUISHING_TEAM option to swap 2026 first round picks picks",
"RELINQUISHING_TEAM option to swap 2026 first round picks (less favorable of ACQUIRING_TEAM, TEAM picks)",
"first round pick protected top 16 in 2021-22, top 18 in 2023-24, top 13 in 2025, top 11 in 2026, top 9 in 2027, else 2027 second round pick",                                
                                  "first round pick protected top 13 in 2025, top 11 in 2026, top 9 in 2027, else 2027 second round pick"),
                      "actual" = c("TRUTH = Y, Derik Queen (2025) - trade happened", 
                                   "TRUTH = N Derik Queen (2025) - trade did not happen", 
                                   "TRUTH - UNK (2026)",
                                   "TRUTH - UNK (2026 rockets pick)",
                                   "TRUTH - UNK (fake)",
                                   "TRUTH - UNK (2026 wizards/suns)",
                                   "TRUTH - UNK (2026 magic/suns)",
                                   "TRUTH = Y Joan Beringer (2025) - trade happened",
                                   "TRUTH = N Joan Beringer (2025) - trade happened")) %>% 
  bind_cols(predict(last_fit, ., type = "prob")) %>%
  bind_cols(
    predict(
      last_fit,
      .,
      type = "class"
    )) %>%
  bind_cols(predict(
    cf_last_fit,
    .
  ))

## helper function for prepping examples
get_dfm = function(text){
  return(rec %>% prep() %>% bake(new_data = data.frame("desc2" = text)))
}
get_dfm2 = function(text){
  return(cf_rec %>% prep() %>% bake(new_data = data.frame("desc2" = text)))
}

######### Figure 1 #########
collect_predictions(test_fit) %>%
  conf_mat(truth = y, estimate = .pred_class) %>%
  autoplot(type = "heatmap") +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(vjust = 2.5,
                                   color = "black"),
        axis.text.y = element_text(hjust = 2.5,
                                   color = "black"))

######### Figure 2 #########
tidy(last_fit) %>%
  filter(term != ("(Intercept)")) %>%
  mutate(term = str_remove_all(term, "tfidf_desc2_"),
         term = fct_reorder(term, abs(estimate)),
         Sign = ifelse(estimate < 0, "Reduced efficacy", "Improved efficacy")) %>%
  group_by(Sign) %>%
  top_n(10, abs(estimate)) %>%
  ungroup %>%
  ggplot(aes(x = estimate,
             y = fct_reorder(term, estimate),
             fill = Sign)) +
  geom_col(show.legend = FALSE) +
  scale_fill_discrete(palette = c("#088158FF", "#BA2F2AFF")) + 
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal() + 
  theme(text = element_text(family = "Cambria"),
        plot.title.position = "plot",
        plot.title = element_text(
          size = 13,
          face = "bold", 
        ),
        axis.text = element_text(size=7),) +
  
  labs(
    y = NULL,
    x = "Importance from 10-fold elastic net logistic regression"
  #  title = "Figure 2: Most Important Terms for Predicting if the Protection was Exercised"
  )

######### Figure 3 #########
tmp = rbind(
  rbind(inner_join(get_dfm(examples$desc2[1]) %>%
                     tidyr::pivot_longer(everything(), names_to = "term", values_to = "frequency") %>%
                     filter(frequency != 0) %>%
                     mutate(term = str_remove_all(term, "tfidf_desc2_")),
                   tidy(last_fit) %>%
                     mutate(term = str_remove_all(term, "tfidf_desc2_"),
                            term = fct_reorder(term, abs(estimate)))) %>%
          mutate(value = frequency*estimate,
                 prob_term = 1/(1+exp(-(value))),
                 example = paste0("1. ",examples$desc2[1])),
        data.frame(term = "intercept",
                   frequency = NA,
                   estimate = NA,
                   penalty = 1e-05,
                   value = -0.007448833,
                   prob_term = 1/(1+exp(-(-0.007448833))),
                   example = paste0("1. ",examples$desc2[1]))
  ),
  rbind(inner_join(get_dfm(examples$desc2[2]) %>%
                     tidyr::pivot_longer(everything(), names_to = "term", values_to = "frequency") %>%
                     filter(frequency != 0) %>%
                     mutate(term = str_remove_all(term, "tfidf_desc2_")),
                   tidy(last_fit) %>%
                     mutate(term = str_remove_all(term, "tfidf_desc2_"),
                            term = fct_reorder(term, abs(estimate)))) %>%
          mutate(value = frequency*estimate,
                 prob_term = 1/(1+exp(-(value))),
                 example = paste0("2. ", examples$desc2[2])),
        data.frame(term = "intercept",
                   frequency = NA,
                   estimate = NA,
                   penalty = 1e-05,
                   value = -0.007448833,
                   prob_term = 1/(1+exp(-(-0.007448833))),
                   example = paste0("2. ",examples$desc2[2])))) 

tmp = tmp %>%
  mutate(term = factor(term, levels = c("intercept", "top", "14", "top_14",
                                        "14_top", "top_14_top", "14_top_12",
                                        "12", "top_12", "12_top", "top_12_top",
                                        "12_top_10", "10", "top_10", "10_else",
                                        "top_10_else", "else"))) %>%
  arrange(term) %>%
  group_by(example) %>%
  mutate(cum_val = cumsum(value),
         feature_sum = sum(value)) %>%
  ungroup() %>%
  mutate(cum_prob = 1/(1+exp(-(cum_val))),
         probability = 1/(1+exp(-(feature_sum + -0.007448833))))

p2 = tmp %>% mutate(sign = ifelse(cum_prob < .5, "Neg", "Pos")) %>%
  ggplot(aes(y = cum_prob,
             x = fct_rev(term),
             fill = sign,
             color = sign)) +
  geom_line(aes(group = example),  show.legend = FALSE) +
  geom_hline(yintercept = .5, aes(linewidth = 0.5, alpha = 0.8)) +
  geom_point(
    shape = 21,
    size = 2,
    stroke = 1,
    show.legend = FALSE
  ) +
  coord_flip() + 
  facet_wrap(~example, scales = "fixed", nrow = 1, labeller = label_wrap_gen(50)) + 
  scale_fill_discrete(palette = c("#BA2F2AFF", "#088158FF")) + 
  scale_color_discrete(palette = c("#BA2F2AFF", "#088158FF")) +
  theme_minimal() + 
  theme(text = element_text(family = "Cambria"),
        plot.title.position = "plot",
        plot.title = element_text(
          size = 13,
          face = "bold", 
        ),
        axis.text = element_text(size=7),
        strip.text = element_blank(),
        axis.title = element_text(size = 7)
  ) +
  labs(
    x = NULL,
    y = "Cumulative Probability")
p1= tmp %>% mutate(sign = ifelse(value < 0, "Neg", "Pos")) %>% 
  filter(term != "intercept") %>%
  ggplot(aes(x = value,
             y = fct_reorder(term, value),
             fill = sign)) +
  geom_col(show.legend = FALSE) +
  scale_fill_discrete(palette = c("#BA2F2AFF", "#088158FF")) + 
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal() + 
  theme(text = element_text(family = "Cambria"),
        plot.title.position = "plot",
        plot.title = element_text(
          size = 13,
          face = "bold", 
        ),
        axis.text = element_text(size=7),
        axis.title = element_text(size = 7),
        strip.text = element_blank()
        
  ) +
  facet_wrap(~example, scales = "fixed", nrow = 1, labeller = label_wrap_gen(50))+
  labs(
    y = NULL,
    x = "Estimated Contribution by Term Coefficient * TFIDF"
   # title = "Figure 3: Side-by-Side Comparison of a Real Transaction (1) VS Fake Re-Trade (2)"
   )
p1/p2

######### Figure 4 #########
cf_last_fit %>%
  tidy() %>%
  filter(term != "Bias") %>%
  mutate(
    sign = case_when(estimate > 0 ~ "Favors substitute",
                     TRUE ~ "Favors protected"),
    term = str_remove_all(term, "tfidf_desc2_")
  ) %>%
  group_by(sign) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(x = estimate,
             y = fct_reorder(term, estimate),
             fill = sign)) +
  geom_col() +
  scale_fill_discrete(palette = c("#BA2F2AFF", "#088158FF")) + 
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal() + 
  theme(text = element_text(family = "Cambria"),
        plot.title.position = "plot",
        plot.title = element_text(
          size = 13,
          face = "bold", 
        ),
        axis.text = element_text(size=7),
        legend.position = "top",
        legend.title = element_blank()
  ) +
  labs(
    y = NULL,
    x = "Importance from 10-fold linear SVM"
   # ,title = "Figure 4: Most Important Terms for Predicting VORP Difference"
    )

######### Figure 5 #########
tmp = rbind(
  rbind(inner_join(get_dfm2(examples$desc2[3]) %>%
                     tidyr::pivot_longer(everything(), names_to = "term", values_to = "frequency") %>%
                     filter(frequency != 0) %>%
                     mutate(term = str_remove_all(term, "tfidf_desc2_")),
                   tidy(cf_last_fit) %>%
                     mutate(term = str_remove_all(term, "tfidf_desc2_"),
                            term = fct_reorder(term, abs(estimate)))) %>%
          mutate(value = frequency*estimate,
                 example = paste0("1. ",examples$desc2[3])),
        data.frame(term = "intercept",
                   frequency = NA,
                   estimate = NA,
                   value = -0.08283224,
                   example = paste0("1. ",examples$desc2[3]))
    ),
  rbind(inner_join(get_dfm2(examples$desc2[4]) %>%
                     tidyr::pivot_longer(everything(), names_to = "term", values_to = "frequency") %>%
                     filter(frequency != 0) %>%
                     mutate(term = str_remove_all(term, "tfidf_desc2_")),
                   tidy(cf_last_fit) %>%
                     mutate(term = str_remove_all(term, "tfidf_desc2_"),
                            term = fct_reorder(term, abs(estimate)))) %>%
          mutate(value = frequency*estimate,
                 example = paste0("2. ", examples$desc2[4])),
        data.frame(term = "intercept",
                   frequency = NA,
                   estimate = NA,
                   value = -0.08283224,
                   example = paste0("2. ",examples$desc2[4]))),
  rbind(inner_join(get_dfm2(examples$desc2[5]) %>%
                     tidyr::pivot_longer(everything(), names_to = "term", values_to = "frequency") %>%
                     filter(frequency != 0) %>%
                     mutate(term = str_remove_all(term, "tfidf_desc2_")),
                   tidy(cf_last_fit) %>%
                     mutate(term = str_remove_all(term, "tfidf_desc2_"),
                            term = fct_reorder(term, abs(estimate)))) %>%
          mutate(value = frequency*estimate,
                 example = paste0("3. ",examples$desc2[5])),
        data.frame(term = "intercept",
                   frequency = NA,
                   estimate = NA,
                   value = -0.08283224,
                   example = paste0("3. ",examples$desc2[5]))
  )
  ) 
  tmp = tmp %>% 
    mutate(term = ifelse(!(term %in% c("intercept","least", "favorable",
                                       "least_favorable", "top", "favorable_top",
                                       "least_favorable_top","4", "top_4",  "excluded terms")), 
                         "excluded terms", term),
           term= factor(term, levels = c("intercept","least", "favorable",
                                         "least_favorable", "top", "favorable_top",
                                         "least_favorable_top","4", "top_4", "excluded terms"))
    )

tmp =  tmp %>%
    group_by(term, example) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
    arrange(term) %>%
  group_by(example) %>%
  mutate(cum_val = cumsum(value),
         feature_sum = sum(value)) %>%
  ungroup()  
p2 = tmp %>% mutate(sign = ifelse(cum_val < 0, "Neg", "Pos")) %>%
    ggplot(aes(y = cum_val,
               x = fct_rev(term),
               fill = sign,
               color = sign)) +
    geom_line(aes(group = example),  show.legend = FALSE) +
    geom_hline(yintercept = 0, aes(linewidth = 0.5, alpha = 0.8)) +
    geom_point(
      shape = 21,
      size = 2,
      stroke = 1,
      show.legend = FALSE
    ) +
    coord_flip() + 
    facet_wrap(~example, scales = "fixed", nrow = 1, labeller = label_wrap_gen(50)) + 
    scale_fill_discrete(palette = c("#BA2F2AFF", "#088158FF")) + 
    scale_color_discrete(palette = c("#BA2F2AFF", "#088158FF")) +
    theme_minimal() + 
    theme(text = element_text(family = "Cambria"),
          plot.title.position = "plot",
          plot.title = element_text(
            size = 13,
            face = "bold", 
          ),
          axis.text = element_text(size=7),
          strip.text = element_blank(),
          axis.title = element_text(size = 7)
    ) +
    labs(
      x = NULL,
      y = "Cumulative Prediction")
  p1= tmp %>% mutate(sign = ifelse(value < 0, "Neg", "Pos")) %>% 
    filter(term != "intercept") %>%
    ggplot(aes(x = value,
               y = fct_reorder(term, value),
               fill = sign)) +
    geom_col(show.legend = FALSE) +
    scale_fill_discrete(palette = c("#BA2F2AFF", "#088158FF")) + 
    scale_x_continuous(expand = c(0, 0)) +
    theme_minimal() + 
    theme(text = element_text(family = "Cambria"),
          plot.title.position = "plot",
          plot.title = element_text(
            size = 13,
            face = "bold", 
          ),
          axis.text = element_text(size=7),
          axis.title = element_text(size = 7),
          strip.text = element_blank()
          
    ) +
    facet_wrap(~example, scales = "fixed", nrow = 1, labeller = label_wrap_gen(50))+
    labs(
      y = NULL,
      x = "Estimated Contribution by Term Coefficient * TFIDF"
    #  ,title = "Figure 5: Side-by-Side Comparison of Two 2026 Draft Pick Transactions"
      )
  p1/p2
