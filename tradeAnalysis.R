library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(spacyr)
library(lattice)
library(tidytext)
library(reticulate)
library(htmltools)
if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse, quanteda, spacyr, tidymodels, 
               textrecipes, udpipe, finetune, httr, rvest)

load("~/bball.RData")

counterfactuals = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Z-iRJuP4fk0pHZL93Vt18vnvd9X8kBzlLX52BZDE2r0/edit?gid=382933428#gid=382933428")
cf = counterfactuals %>%
  filter(!is.na(tradeId) & draftYear > 2004) %>%
  mutate(Protected = ifelse(is.na(Protected),0,Protected),
         Preference = ifelse(Preference == TRUE, 1, 0),
       #  protectionApplied = ifelse(protectionApplied == TRUE, 1, 0),
       #  preferenceApplied = ifelse(preferenceApplied == TRUE, 1, 0),
         n_prot_alts = ifelse(is.na(cf_ProtectionId), 0,
                         1+str_count(cf_ProtectionId, ",")),
         n_pref_alts = ifelse(is.na(cf_PreferenceId), 0,
                              1+str_count(cf_PreferenceId,",")),
        n_alts = n_prot_alts + n_pref_alts,
       conditional = ifelse(Protected+Preference == 0, 0, 1),
       y = ifelse(protectionApplied == TRUE, 1,0 )
         ) %>%
  group_by(assetId) %>%
  mutate(n = n(),
         n_desc = n_distinct(desc)) %>%
  ungroup()

# unique at the pick level
cf_sum = left_join(cf %>%
  group_by(assetId,draftTeam, originTeam, idPlayer, brefUrl,
           proSportsName, draftYear, round, pick) %>%
  summarize(n_trades = n(),
         n_desc = n_distinct(desc),
         first = min(Date),
         Protected = max(Protected),
         protectionApplied = max(y)) %>%
  ungroup(),
  draftFinal %>% select(Date, idPlayer, yearSeasonFirst, yearSeasonLast, total_gm, avg_ws, avg_bpm, avg_vorp) %>%
    mutate(Date = as.Date(Date))) %>%
  mutate(date_diff = as.numeric(Date - as.Date(first)))

protected_assets = cf %>% 
  filter(Protected == 1) %>%
  distinct(assetId, desc, .keep_all = T) %>%
  mutate(doc_id = row_number())

teams = unique(cf$draftTeam)
names = rbind(cf %>%
  select(proSportsName), 
  draftFinal %>% filter(idPlayer %in% cf$idPlayer) %>%
    select(Acquired) %>% rename(proSportsName = Acquired),
  draftFinal %>% filter(idPlayer %in% cf$idPlayer) %>%
    select(), 
  proSportsName = "Dee Brown (a)") %>%
  distinct() %>%
  arrange(desc(nchar(proSportsName))) %>%
  mutate(proSportsName = gsub("(", "\\(",proSportsName,fixed = TRUE),
         proSportsName = gsub(")", "\\)", proSportsName,fixed = TRUE))
names = unique(names$proSportsName)
protected_assets = protected_assets %>%
  rowwise() %>%
  mutate(desc2 = gsub(Acquired, "ACQUIRING_TEAM", desc),
         desc2 = gsub(Relinquished, "RELINQUISHING_TEAM", desc2)) %>% 
  ungroup() %>%
  mutate(desc2 = gsub(paste0(teams, collapse="|"), "TEAM",desc2), #mask team
         desc2 = gsub(paste0(names, collapse="|"), "PLAYER", desc2), #mask player
         desc2 = gsub("#", "", desc2, fixed = TRUE), #remove #s
         desc2 = gsub("\\(not exercised.*?\\)", "", desc2)) %>% ##here down remove the outcome from the text
  rowwise() %>%
  mutate(
         desc2 = gsub(paste0("\\(",draftYear," ",pick,"-PLAYER\\)"), "", desc2),
         desc2 = gsub(paste0("\\(",pick,"-PLAYER\\)"),"", desc2)
         ) %>%
  ungroup()
#remove all team and player names first
df_corp = corpus(protected_assets,
                 docid_field = "doc_id",
                 text_field = "desc2")

spacy_initialize()
df_spacy = spacy_parse(df_corp, lemma = FALSE, tag = TRUE, entity = TRUE, 
                       nounphrase = TRUE, dependency = TRUE, pos = TRUE)  
ent = entity_extract(df_spacy, type = "all")
spacy_finalize()

spacy_ent = function(x){
  tokens = spacy_parse(x) %>%
    entity_consolidate()
  token_list = split(tokens$token, tokens$doc_id)
  names(token_list) = gsub("text", "", names(token_list))
  res = unname(token_list[as.character(seq_along(x))])
  empty = length(res) == 0
  res[empty] = lapply(seq_len(sum(empty)), function(x) character(0))
  res
}
spacy_np = function(x){
  tokens = spacy_parse(x, nounphrase = TRUE) %>%
    nounphrase_consolidate()
  token_list = split(tokens$token, tokens$doc_id)
  names(token_list) = gsub("text", "", names(token_list))
  res = unname(token_list[as.character(seq_along(x))])
  empty = length(res) == 0
  res[empty] = lapply(seq_len(sum(empty)), function(x) character(0))
  res
}
spacy_dep = function(x){
  tokens = spacy_parse(x, pos = TRUE)
  token_list = split(tokens$pos, tokens$doc_id)
  names(token_list) = gsub("text", "", names(token_list))
  res = unname(token_list[as.character(seq_along(x))])
  empty = length(res) == 0
  res[empty] = lapply(seq_len(sum(empty)), function(x) character(0))
  res
}
years = 2003:2027
date = ent %>% filter(entity_type == "DATE" & grepl(paste0(years, collapse="|"), entity))
date = unique(date$entity)

cf_data =  protected_assets %>% filter(protectionApplied & !is.na(cf_ProtectionId)) %>%
  separate_longer_delim(cf_ProtectionBref, ",") %>%
  select(tradeId, draftYear, round, pick, idPlayer,brefUrl, desc2, cf_ProtectionBref)
cf_data = left_join(cf_data,
                    draftFinal %>% select(draftYear, pick, round, idPlayer,brefUrl) %>%
                      mutate(idPlayer = as.character(idPlayer)) %>%
                      rename(cf_year = draftYear, cf_pick = pick,
                             cf_round = round, cf_ProtectionId = idPlayer,
                             cf_ProtectionBref = brefUrl)) %>%
  group_by(tradeId, desc2) %>%
  mutate(n_subs = n_distinct(idPlayer),
         min_sub = min(pick),
         n_prot = n_distinct(cf_ProtectionId),
         min_prot = min(cf_pick)) %>%
  ungroup() %>%
  filter(cf_pick == min_prot)

cf_perf = rbind(
  cf_data %>%
    select(tradeId, draftYear, round, pick, idPlayer, desc2, brefUrl) %>%
    mutate(playerType = "Relinquished") %>% distinct(tradeId, idPlayer, .keep_all = TRUE),
  cf_data %>%
    select(tradeId, cf_year, cf_round, cf_pick, cf_ProtectionId, desc2,cf_ProtectionBref) %>%
    rename(draftYear = cf_year,
           round = cf_round,
           pick = cf_pick,
           idPlayer = cf_ProtectionId,
           brefUrl = cf_ProtectionBref) %>%
    mutate(playerType = "Retained")  %>% distinct(tradeId, idPlayer, .keep_all = TRUE)) %>%
  arrange(desc(tradeId)) %>%
  distinct(idPlayer, .keep_all = TRUE)
cf_perf = left_join(cf_perf, 
                    df_dict_nba_players %>%
                      select(idPlayer, yearSeasonFirst) %>%
                      mutate(idPlayer = as.character(idPlayer))) %>%
  mutate(games = NA,
         mp = NA,
         PER = NA,
         ws_48 = NA,
         bpm = NA,
         vorp = NA)
base_bref = "https://www.basketball-reference.com"
for(i in 1:nrow(cf_perf)){
  if(!is.na(cf_perf$yearSeasonFirst[i])){
    url = paste0(base_bref, cf_perf$brefUrl[i])
    tmp = content(GET(url))
    if(length(tmp %>% html_elements("#advanced")) > 0){
      adv = tmp %>%
        html_elements("#advanced") %>%
        html_table(header = TRUE) %>%
        as.data.frame %>%
        select(G, MP, PER, WS.48, BPM, VORP)
      adv = adv[1,]
      cf_perf$games[i] = adv$G
      cf_perf$mp[i] = adv$MP
      cf_perf$PER[i] = adv$PER
      cf_perf$ws_48[i] = adv$WS.48
      cf_perf$bpm[i] = adv$BPM
      cf_perf$vorp[i] = adv$VORP
    }
    Sys.sleep(2)
  }
}
rm(list = c("adv", "df", "tmp"))
cf_perf = cf_perf %>%
  mutate(games = as.integer(games),
         mp = as.numeric(mp),
         PER = as.numeric(PER),
         ws_48 = as.numeric(ws_48),
         bpm = as.numeric(bpm))

save.image("~/bball.RData")



