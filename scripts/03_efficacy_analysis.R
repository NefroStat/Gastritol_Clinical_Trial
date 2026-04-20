# ==============================================================================
# Проєкт: Клінічна оцінка лікарського засобу Гастритол
# Скрипт: 03_efficacy_analysis.R
# Призначення: Аналіз ефективності лікування (динаміка Day 1 -> Day 21 та розрахунок Дельти)
# ==============================================================================

library(tidyverse)
library(writexl)

# ---- 1. Зчитування даних ----
clean_data <- read_csv2("data/processed/clean_gastritol_data.csv")

# ---- 2. Підготовка списку базових змінних ----
# Перелік показників без суфіксів _d1 чи _d21
base_vars <- c(
  "nausea", "vomiting", "sickness", "fullness", "cramps", 
  "satiety", "reflux", "appetite_loss", "discomfort", "epigastric_pain",
  "qol_gen", "qol_phys", "qol_psych", "qol_active"
)

results_list <- list()

# ---- 3. Цикл для розрахунку динаміки та Дельти ----
for (var in base_vars) {
  
  # Формуємо назви колонок для 1 та 21 дня
  var_d1 <- paste0(var, "_d1")
  var_d21 <- paste0(var, "_d21")
  
  # Розрахунок Дельти (День 1 - День 21). 
  # Позитивне значення = покращення (симптом зменшився)
  clean_data[[paste0("delta_", var)]] <- clean_data[[var_d1]] - clean_data[[var_d21]]
  
  # --- Дані Контрольної групи ---
  ctrl_d1 <- clean_data %>% filter(group == "Control") %>% pull(var_d1)
  ctrl_d21 <- clean_data %>% filter(group == "Control") %>% pull(var_d21)
  ctrl_delta <- clean_data %>% filter(group == "Control") %>% pull(paste0("delta_", var))
  
  # --- Дані Основної групи ---
  treat_d1 <- clean_data %>% filter(group == "Treatment") %>% pull(var_d1)
  treat_d21 <- clean_data %>% filter(group == "Treatment") %>% pull(var_d21)
  treat_delta <- clean_data %>% filter(group == "Treatment") %>% pull(paste0("delta_", var))
  
  # Функція для форматування Медіани (Q1; Q3)
  format_med <- function(x) {
    sprintf("%.1f (%.1f; %.1f)", median(x, na.rm=T), quantile(x, 0.25, na.rm=T), quantile(x, 0.75, na.rm=T))
  }
  
  # --- Статистичні тести ---
  # 1. Внутрішньогрупова динаміка (Вілкоксон для ПОВ'ЯЗАНИХ вибірок)
  # exact = FALSE використовується для уникнення помилок через однакові значення (ties)
  p_ctrl_intra <- wilcox.test(ctrl_d1, ctrl_d21, paired = TRUE, exact = FALSE)$p.value
  p_treat_intra <- wilcox.test(treat_d1, treat_d21, paired = TRUE, exact = FALSE)$p.value
  
  # 2. Міжгрупова різниця ефективності за ДЕЛЬТОЮ (Манн-Уїтні для НЕЗАЛЕЖНИХ вибірок)
  p_inter_delta <- wilcox.test(ctrl_delta, treat_delta, exact = FALSE)$p.value
  
  # --- Формування рядка результатів ---
  results_list[[var]] <- tibble(
    `Показник` = var,
    
    # Блок Контролю
    `Контроль (День 1)` = format_med(ctrl_d1),
    `Контроль (День 21)` = format_med(ctrl_d21),
    `p-знач. (динаміка Контроль)` = round(p_ctrl_intra, 4),
    `Дельта (Контроль)` = format_med(ctrl_delta),
    
    # Блок Основної групи (Гастритол)
    `Основна (День 1)` = format_med(treat_d1),
    `Основна (День 21)` = format_med(treat_d21),
    `p-знач. (динаміка Основна)` = round(p_treat_intra, 4),
    `Дельта (Основна)` = format_med(treat_delta),
    
    # Головний результат дослідження
    `p-знач. (Міжгрупова Дельта)` = round(p_inter_delta, 4)
  )
}

# Об'єднуємо все в одну таблицю
efficacy_table <- bind_rows(results_list)

# Перекодовуємо нульові p-значення у формат <0.0001
efficacy_table <- efficacy_table %>%
  mutate(across(starts_with("p-знач"), ~ ifelse(.x == 0, "<0.0001", as.character(.x))))

# ---- 4. Збереження результатів в Excel ----
output_excel <- "results/tables/Table_2_Efficacy_Analysis.xlsx"
write_xlsx(efficacy_table, output_excel)

message("Аналіз ефективності завершено! Файл збережено у форматі Excel: ", output_excel)
# ==============================================================================