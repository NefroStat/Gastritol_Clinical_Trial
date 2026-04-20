# ==============================================================================
# Проєкт: Клінічна оцінка лікарського засобу Гастритол
# Скрипт: 04_patchwork_style_viz.R
# Призначення: Графік для 6 ключових симптомів (Стиль Patchwork / Custom Nature)
# ==============================================================================

# install.packages("patchwork") # Розкоментуйте, якщо пакет не встановлено

library(tidyverse)
library(ggplot2)
library(patchwork) # Для об'єднання окремих графіків

# Створюємо папку
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

# ---- 1. Зчитування даних ----
clean_data <- read_csv2("data/processed/clean_gastritol_data.csv")

# ---- 2. Підготовка даних (Дельти для 6 симптомів) ----
plot_data <- clean_data %>%
  mutate(
    delta_epigastric_pain = epigastric_pain_d1 - epigastric_pain_d21,
    delta_fullness        = fullness_d1 - fullness_d21,
    delta_satiety         = satiety_d1 - satiety_d21,
    delta_nausea          = nausea_d1 - nausea_d21,
    delta_cramps          = cramps_d1 - cramps_d21,
    delta_reflux          = reflux_d1 - reflux_d21
  ) %>%
  mutate(group = factor(group, levels = c("Control", "Treatment"), 
                        labels = c("Контроль", "Гастритол")))

# Словник змінних та їхніх назв (для заголовків)
symptom_vars <- c(
  "delta_epigastric_pain" = "Біль в епігастрії",
  "delta_fullness"        = "Переповнення, здуття", 
  "delta_satiety"         = "Раннє насичення",
  "delta_nausea"          = "Нудота", 
  "delta_cramps"          = "Спастичний біль", 
  "delta_reflux"          = "Печія, відрижка"
)

# ---- 3. Побудова графіків у циклі (Ваш стиль) ----
plot_list <- list()

for (var in names(symptom_vars)) {
  sym_title <- symptom_vars[var]
  
  # Відкидаємо пропущені значення для конкретної змінної
  df_plot <- plot_data[!is.na(plot_data[[var]]) & !is.na(plot_data$group), ]
  
  # Рахуємо p-значення (Манн-Уїтні)
  p_val <- wilcox.test(df_plot[[var]] ~ df_plot$group, exact = FALSE)$p.value
  
  # Форматуємо p-значення згідно з вимогою (< 0.001)
  p_label <- ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", format(round(p_val, 3), nsmall = 3)))
  
  # Будуємо графік
  p <- ggplot(df_plot, aes(x = group, y = .data[[var]], fill = group)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6, color = "black", linewidth = 0.5) +
    # Чорні крапки з джитером
    geom_jitter(width = 0.2, size = 1.5, alpha = 0.5, color = "black", shape = 16) +
    # Ваша кольорова палітра
    scale_fill_manual(values = c("#2E9FDF", "#E7B800")) + 
    # Класична тема
    theme_classic(base_size = 12) +
    # Додаємо назву і p-значення, видаляємо осі X і Y
    labs(title = sym_title, subtitle = p_label, x = NULL, y = "Дельта (бали)") +
    # Ваші налаштування стилю
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#d73027", face = "italic"),
      axis.text.x = element_text(color = "black", size = 12, face = "bold"),
      axis.text.y = element_text(color = "black", size = 10),
      axis.title.y = element_text(color = "gray30", size = 10),
      axis.line = element_line(color = "black")
    )
  
  # Зберігаємо графік у список
  plot_list[[var]] <- p
}

# ---- 4. Об'єднання та збереження ----
# Використовуємо patchwork для склеювання 6 графіків у сітку (3 колонки)
final_plot <- wrap_plots(plot_list, ncol = 3)

# Зберігаємо результат
output_file <- "results/figures/Figure_1_Custom_Style.png"
ggsave(output_file, plot = final_plot, width = 12, height = 8, dpi = 300, bg = "white")

message("Графік в оновленому стилі збережено: ", output_file)
# ==============================================================================