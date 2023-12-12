# Comparacao da distribuicao de renda domiciliar em faixas de SM para pessoas com 16 anos ou mais da 
# PNADC 2021 e PNADC 2022 5a entrevistas

# 1. carrega pacotes ----
library(PNADcIBGE)
library(tidyverse)
library(survey)
library(srvyr)
library(scales)
library(kableExtra)


# 2. opcoes do pacote survey ----
options(survey.lonely.psu = "adjust")


# 3. baixa dados da PNADC 2021 - 5a entrevista ----
pnadc_2021.5 <- get_pnadc(year = 2021, interview = 5, vars = c("Estrato","UPA","V1032","V2009","VD5010"), 
                          defyear = 2021, design = FALSE)


# 4. calcula distribuicao de renda domiciliar por faixas de SM para pessoas com 16 anos ou mais da PNADC 2021 5a entrevista ---- 
renda_dom_2021 <- 
  pnadc_2021.5 %>% 
  mutate(renda = VD5010 * CO1) %>% # calcula renda domiciliar deflacionada  
  mutate(faixa_renda = case_when(  # categoriza renda domiciliar em faixas de SM (SM em 2021 = R$1100)
    renda < (1100 * 1) ~ 1, 
    renda >= (1100 * 1) & renda < (1100 * 2) ~ 2,
    renda >= (1100 * 2) & renda < (1100 * 5) ~ 3,
    renda >= (1100 * 5) ~ 4),
    faixa_renda = factor(faixa_renda, levels = c(1,2,3,4), labels = c("Ate 1 SM", "De 1 ate 2 SM", "De 2 ate 5 SM", "Acima de 5 SM"))) %>%
  as_survey(ids = UPA, strata = Estrato, weights = V1032) %>% # define desenho amostral 
  subset(V2009 >= 16) %>%  # filtra pessoas com 16 anos ou mais
  # subset(UF == "São Paulo") %>%  # filtra UF (tirar do comentário se quiser obter estimativas para uma UF em específico)
  group_by(faixa_renda) %>% 
  summarise(proportion = survey_mean(na.rm = T)) %>% 
  filter(!is.na(faixa_renda))


# 5. baixa dados da PNADC 2022 - 5a entrevista ----
pnadc_2022.5 <- get_pnadc(year = 2022, interview = 5, vars = c("Estrato","UPA","V1032","V2009","VD5010"), 
                          defyear = 2022, design = FALSE)


# 6. calcula distribuicao de renda domiciliar por faixas de SM para pessoas com 16 anos ou mais da PNADC 2022 5a entrevista ---- 
renda_dom_2022 <- 
  pnadc_2022.5 %>% 
  mutate(renda = VD5010 * CO1) %>% # calcula renda domiciliar deflacionada  
  mutate(faixa_renda = case_when(  # categoriza renda domiciliar em faixas de SM (SM em 2022 = R$1212)
    renda < (1212 * 1) ~ 1, 
    renda >= (1212 * 1) & renda < (1212 * 2) ~ 2,
    renda >= (1212 * 2) & renda < (1212 * 5) ~ 3,
    renda >= (1212 * 5) ~ 4),
    faixa_renda = factor(faixa_renda, levels = c(1,2,3,4), labels = c("Ate 1 SM", "De 1 ate 2 SM", "De 2 ate 5 SM", "Acima de 5 SM"))) %>%
  as_survey(ids = UPA, strata = Estrato, weights = V1032) %>% # define desenho amostral 
  subset(V2009 >= 16) %>%  # filtra pessoas com 16 anos ou mais
  # subset(UF == "São Paulo") %>%  # filtra UF (tirar do comentário se quiser obter estimativas para uma UF em específico)
  group_by(faixa_renda) %>% 
  summarise(proportion = survey_mean(na.rm = T)) %>% 
  filter(!is.na(faixa_renda))


# 7. compara distribuicao de renda domiciliar de SM para pessoas com 16 anos ou mais de 2021 e 2002 ----
renda_dom_2021 %>% 
  select(-proportion_se) %>% 
  mutate(proportion = label_percent()(proportion)) %>% 
  rename("2021" = "proportion") %>% 
  left_join(renda_dom_2022 %>% 
              select(-proportion_se) %>% 
              mutate(proportion = label_percent()(proportion)), 
            by = "faixa_renda") %>% 
  rename("2022" = "proportion", 
         "Faixa de renda domiciliar" = "faixa_renda") %>% 
  kbl() %>%
  kable_classic(full_width = F) %>% 
  footnote(general = c("Fonte: IBGE - PNAD Continua (5ª entrevista), 2021 e 2022",
           "Salário Minimo 2021: R$1100",
           "Salário Minimo 2022: R$1212"))