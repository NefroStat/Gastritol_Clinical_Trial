# ==============================================================================
# Проєкт: Клінічна оцінка лікарського засобу Гастритол
# Скрипт: 02_baseline_custom_stats.R
# Призначення: Аналіз базових характеристик з перевіркою на нормальність (Шапіро) 
#              та розрахунком точних 95% ДІ для відсоткових показників (стать).
# ==============================================================================

library(tidyverse)
library(writexl)

# ---- 1. Зчитування даних ----
clean_data <- read_csv2("data/processed/clean_gastritol_data.csv")

# Створюємо директорію для таблиць, якщо її немає
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

# ---- 2. Підготовка списку змінних ----
# Числові зміні, які ми будемо тестувати
num_vars <- c(
  "age", "nausea_d1", "vomiting_d1", "sickness_d1", "fullness_d1", 
  "cramps_d1", "satiety_d1", "reflux_d1", "appetite_loss_d1", 
  "discomfort_d1", "epigastric_pain_d1", 
  "qol_gen_d1", "qol_phys_d1", "qol_psych_d1", "qol_active_d1"
)

# Порожній список для збору результатів
results_list <- list()

# ---- 3. Цикл для автоматичного аналізу числових змінних ----
for (var in num_vars) {
  
  # Витягуємо дані по групах (видаляємо можливі пропуски)
  ctrl_data <- clean_data %>% filter(group == "Control") %>% pull(var) %>% na.omit()
  treat_data <- clean_data %>% filter(group == "Treatment") %>% pull(var) %>% na.omit()
  
  # 3.1. Тест Шапіро-Уїлка на нормальність
  # Якщо всі значення однакові (напр. всі 0), Шапіро видасть помилку, тому використовуємо tryCatch
  shapiro_ctrl <- tryCatch(shapiro.test(ctrl_data)$p.value, error = function(e) 0)
  shapiro_treat <- tryCatch(shapiro.test(treat_data)$p.value, error = function(e) 0)
  
  # Розподіл нормальний ТІЛЬКИ якщо в обох групах p >= 0.05
  is_normal <- (shapiro_ctrl >= 0.05) & (shapiro_treat >= 0.05)
  
  # 3.2. Розрахунки залежно від розподілу
  if (is_normal) {
    # ПАРАМЕТРИКА
    stat_ctrl <- sprintf("%.2f ± %.2f", mean(ctrl_data), sd(ctrl_data))
    stat_treat <- sprintf("%.2f ± %.2f", mean(treat_data), sd(treat_data))
    
    # t-тест Стьюдента
    p_val <- t.test(ctrl_data, treat_data)$p.value
    test_used <- "t-test"
    dist_type <- "Нормальний"
    
  } else {
    # НЕПАРАМЕТРИКА
    stat_ctrl <- sprintf("%.1f (%.1f; %.1f)", median(ctrl_data), quantile(ctrl_data, 0.25), quantile(ctrl_data, 0.75))
    stat_treat <- sprintf("%.1f (%.1f; %.1f)", median(treat_data), quantile(treat_data, 0.25), quantile(treat_data, 0.75))
    
    # Критерій Манна-Уїтні (в R він називається wilcox.test для незалежних вибірок)
    p_val <- wilcox.test(ctrl_data, treat_data, exact = FALSE)$p.value
    test_used <- "Mann-Whitney U"
    dist_type <- "Ненормальний"
  }
  
  # Записуємо результат у рядок таблиці
  results_list[[var]] <- tibble(
    `Показник (День 1)` = var,
    `Контрольна група (n=245)` = stat_ctrl,
    `Основна група (n=288)` = stat_treat,
    `p-значення` = round(p_val, 4),
    `Тип розподілу` = dist_type,
    `Стат. тест` = test_used
  )
}

# Об'єднуємо всі результати по числах у єдину таблицю
table_numeric <- bind_rows(results_list)

# ---- 4. Додавання статі (Категоріальна змінна + Точний 95% ДІ) ----
gender_table <- clean_data %>%
  group_by(group, gender) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(group) %>%
  mutate(total = sum(n)) %>%
  # Використовуємо rowwise для розрахунку ДІ для кожного рядка окремо
  rowwise() %>%
  mutate(
    pct = (n / total) * 100,
    # Точний біноміальний метод (Клоппер-Пірсон / Фішер)
    ci_lower = binom.test(n, total)$conf.int[1] * 100,
    ci_upper = binom.test(n, total)$conf.int[2] * 100,
    # Формуємо красивий рядок: "Кількість (Відсоток% [Нижня_межа%; Верхня_межа%])"
    stat = sprintf("%d (%.1f%% [%.1f%%; %.1f%%])", n, pct, ci_lower, ci_upper)
  ) %>%
  ungroup() %>%
  select(group, gender, stat) %>%
  pivot_wider(names_from = group, values_from = stat)

# Рахуємо Точний критерій Фішера для статі
fisher_p <- fisher.test(table(clean_data$gender, clean_data$group))$p.value

# Формуємо рядки для статі
gender_rows <- tibble(
  `Показник (День 1)` = c("Стать: Жінки", "Стать: Чоловіки"),
  `Контрольна група (n=245)` = c(gender_table$Control[gender_table$gender == "Female"], gender_table$Control[gender_table$gender == "Male"]),
  `Основна група (n=288)` = c(gender_table$Treatment[gender_table$gender == "Female"], gender_table$Treatment[gender_table$gender == "Male"]),
  `p-значення` = c(round(fisher_p, 4), NA), # p-значення ставимо тільки в перший рядок
  `Тип розподілу` = c("Категоріальний", NA),
  `Стат. тест` = c("Fisher's Exact", NA)
)

# ---- 5. Зведення фінальної таблиці та експорт ----
final_table <- bind_rows(gender_rows, table_numeric)

# Перекодовуємо дуже малі p-значення, щоб виглядало професійно
final_table <- final_table %>%
  mutate(`p-значення` = ifelse(`p-значення` == "0" | `p-значення` == "0.0000", "<0.0001", as.character(`p-значення`)))

# Зберігаємо у повноцінний файл Excel
output_excel <- "results/tables/Table_1_Baseline_Stats.xlsx"
write_xlsx(final_table, output_excel)

message("Аналіз завершено! Файл збережено у форматі Excel: ", output_excel)
# ==============================================================================