# ==============================================================================
# Проєкт: Клінічна оцінка лікарського засобу Гастритол при функціональній диспепсії
# Скрипт: 01_data_preparation.R
# Призначення: Імпорт вихідних даних, очищення, трансформація типів змінних
# ==============================================================================

# ---- 1. Налаштування робочого середовища ----
# Очищення пам'яті перед початком роботи (запобігає конфліктам змінних)
rm(list = ls())

# Завантаження необхідних пакетів
library(readxl)    # Для коректного зчитування файлів .xlsx (без проблем з роздільниками)
library(tidyverse) # Набір пакетів для маніпуляції даними (dplyr, tidyr, stringr тощо)

# ---- 2. Імпорт даних ----
# Вказуємо шлях до файлу з вихідними даними
# Шлях вказано відносно головної папки проєкту (Gastritol_Clinical_Trial)
file_path <- "data/raw/Результати дослідження Гастритол краплі (1).xlsx"

# Зчитуємо дані, пропускаючи перший технічний рядок об'єднаних клітинок (skip = 1)
raw_data <- read_excel(file_path, skip = 1)

# ---- 3. Стандартизація назв змінних ----
# Заміна довгих кириличних назв на короткі англійські для уникнення помилок кодування
# d1 = Базовий рівень (1-й день), d21 = Кінець спостереження (21-й день)
new_colnames <- c(
  "gender", "age", "group", 
  
  # Профіль симптомів (Day 1)
  "nausea_d1", "vomiting_d1", "sickness_d1", "fullness_d1", "cramps_d1", 
  "satiety_d1", "reflux_d1", "appetite_loss_d1", "discomfort_d1", "epigastric_pain_d1",
  
  # Профіль симптомів (Day 21)
  "nausea_d21", "vomiting_d21", "sickness_d21", "fullness_d21", "cramps_d21", 
  "satiety_d21", "reflux_d21", "appetite_loss_d21", "discomfort_d21", "epigastric_pain_d21",
  
  "doc_eval_d21", # Глобальна оцінка ефективності лікарем
  
  # Оцінка якості життя (Day 1)
  "qol_gen_d1", "qol_phys_d1", "qol_psych_d1", "qol_active_d1", 
  
  # Оцінка якості життя (Day 21)
  "qol_gen_d21", "qol_phys_d21", "qol_psych_d21", "qol_active_d21", 
  
  "tolerability_d21", "compliance", "adverse_events", "ae_relation"
)

# Застосування нових назв колонок до датасету
colnames(raw_data) <- new_colnames

# ---- 4. Очищення та трансформація даних ----
clean_data <- raw_data %>%
  # Видалення порожніх рядків (якщо вони були випадково створені в Excel)
  filter(!is.na(gender)) %>%
  
  # 4.1. Форматування груп дослідження
  mutate(
    group = case_when(
      str_detect(group, "(?i)основна") ~ "Treatment",
      str_detect(group, "(?i)контрольна") ~ "Control",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # 4.2. Конвертація текстових оцінок (шкала Лікерта) у числовий формат
  # Функція parse_number автоматично витягує цифрове значення з тексту (напр., "4 бали" -> 4)
  mutate(across(c(doc_eval_d21, qol_gen_d1:qol_active_d1, 
                  qol_gen_d21:tolerability_d21), 
                ~ parse_number(as.character(.)))) %>%
  
  # 4.3. Приведення балів вираженості симптомів та віку до числового формату (numeric)
  mutate(across(nausea_d1:epigastric_pain_d21, as.numeric)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(compliance = parse_number(as.character(compliance))) %>%
  
  # 4.4. Факторизація категоріальних змінних
  # Необхідна умова для коректного виконання статистичних тестів та побудови графіків
  mutate(
    gender = factor(gender, levels = c("чоловік", "жінка"), labels = c("Male", "Female")),
    group = factor(group, levels = c("Control", "Treatment")),
    compliance = factor(compliance, levels = c(0, 1), labels = c("Non-compliant", "Compliant")),
    adverse_events = factor(adverse_events, levels = c("Ні", "Так"), labels = c("No", "Yes"))
  )

# ---- 5. Контроль якості та експорт ----
# Виведення структури очищеного масиву даних у консоль для перевірки
glimpse(clean_data)

# Збереження підготовленого набору даних у папку processed
# Використовуємо write_excel_csv2 замість write_excel_csv. 
# Це створює файл, де роздільником колонок є крапка з комою (;). 
# Такий формат автоматично і коректно відкривається в Excel з українською локалізацією.
output_path <- "data/processed/clean_gastritol_data.csv"
write_excel_csv2(clean_data, output_path)

message("Очищення даних завершено успішно. Файл адаптовано для українського Excel і збережено: ", output_path)
# ==============================================================================