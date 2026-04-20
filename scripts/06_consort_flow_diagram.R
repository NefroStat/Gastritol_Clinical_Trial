# ==============================================================================
# Проєкт: Клінічна оцінка лікарського засобу Гастритол
# Скрипт: 06_consort_flow_diagram.R
# Призначення: Побудова блок-схеми дизайну дослідження (CONSORT Flow Diagram)
# ==============================================================================

library(ggplot2)

# Створюємо директорію
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

# Налаштування кольорів та розмірів
box_color <- "black"
box_fill <- "white"
text_size <- 4.5

p_consort <- ggplot() +
  theme_void() + # Порожній фон
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  
  # --- БЛОК 1: РАНДОМІЗАЦІЯ (Верхній) ---
  annotate("rect", xmin = 25, xmax = 75, ymin = 85, ymax = 95, 
           color = box_color, fill = box_fill, linewidth = 0.8) +
  annotate("text", x = 50, y = 90, 
           label = "Рандомізовано у дослідження\n(n = 533)", 
           size = 5, fontface = "bold") +
  
  # Стрілка вниз від Блоку 1 до розгалуження
  annotate("segment", x = 50, xend = 50, y = 85, yend = 75, 
           arrow = arrow(length = unit(0.3, "cm")), linewidth = 0.8) +
  
  # Горизонтальна лінія розгалуження
  annotate("segment", x = 25, xend = 75, y = 75, yend = 75, linewidth = 0.8) +
  
  # Стрілки вниз до груп
  annotate("segment", x = 25, xend = 25, y = 75, yend = 65, 
           arrow = arrow(length = unit(0.3, "cm")), linewidth = 0.8) +
  annotate("segment", x = 75, xend = 75, y = 75, yend = 65, 
           arrow = arrow(length = unit(0.3, "cm")), linewidth = 0.8) +
  
  # --- БЛОК 2: ОСНОВНА ГРУПА (Лівий) ---
  annotate("rect", xmin = 5, xmax = 45, ymin = 50, ymax = 65, 
           color = box_color, fill = "#FFF8DC", linewidth = 0.8) +
  annotate("text", x = 25, y = 57.5, 
           label = "Розподілено в Основну групу\n(Комплексна терапія + Гастритол)\n(n = 288)", 
           size = text_size, fontface = "bold") +
  
  # --- БЛОК 3: КОНТРОЛЬНА ГРУПА (Правий) ---
  annotate("rect", xmin = 55, xmax = 95, ymin = 50, ymax = 65, 
           color = box_color, fill = "#F0F8FF", linewidth = 0.8) +
  annotate("text", x = 75, y = 57.5, 
           label = "Розподілено в Контрольну групу\n(Стандартна комплексна терапія)\n(n = 245)", 
           size = text_size, fontface = "bold") +
  
  # Стрілки вниз до аналізу
  annotate("segment", x = 25, xend = 25, y = 50, yend = 35, 
           arrow = arrow(length = unit(0.3, "cm")), linewidth = 0.8) +
  annotate("segment", x = 75, xend = 75, y = 50, yend = 35, 
           arrow = arrow(length = unit(0.3, "cm")), linewidth = 0.8) +
  
  # --- БЛОК 4: АНАЛІЗ ОСНОВНА (Лівий нижній) ---
  annotate("rect", xmin = 5, xmax = 45, ymin = 20, ymax = 35, 
           color = box_color, fill = box_fill, linewidth = 0.8) +
  annotate("text", x = 25, y = 27.5, 
           label = "Завершили лікування (21 день)\nта включені в аналіз\n(n = 288)", 
           size = text_size, fontface = "bold") +
  
  # --- БЛОК 5: АНАЛІЗ КОНТРОЛЬ (Правий нижній) ---
  annotate("rect", xmin = 55, xmax = 95, ymin = 20, ymax = 35, 
           color = box_color, fill = box_fill, linewidth = 0.8) +
  annotate("text", x = 75, y = 27.5, 
           label = "Завершили лікування (21 день)\nта включені в аналіз\n(n = 245)", 
           size = text_size, fontface = "bold")

# Зберігаємо малюнок
ggsave("results/figures/Figure_1_CONSORT_Flow_Diagram.png", plot = p_consort, 
       width = 10, height = 7, dpi = 300, bg = "white")

message("Блок-схему успішно згенеровано та збережено як Figure_1_CONSORT_Flow_Diagram.png")
# ==============================================================================