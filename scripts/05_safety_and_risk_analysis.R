# ==============================================================================
# Проєкт: Клінічна оцінка лікарського засобу Гастритол
# Скрипт: 05_final_analysis_and_viz.R
# Призначення: 1) Розрахунок Таблиць 3 і 4 (з точними 95% ДІ).
#              2) Генерація 3 ОКРЕМИХ графіків (БЕЗ ДІ на картинках, з p-значеннями).
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(writexl)

# Встановлення класичної журнальної теми
theme_set(theme_classic(base_size = 14, base_family = "sans"))

# Створюємо потрібні папки
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

# ---- 0. Зчитування даних ----
clean_data <- read_csv2("data/processed/clean_gastritol_data.csv")

# Допоміжна функція для таблиць: рахує відсоток і точний 95% ДІ (Клоппер-Пірсон)
format_pct_ci <- function(k, n) {
  pct <- (k / n) * 100
  ci <- binom.test(k, n)$conf.int * 100
  sprintf("%d (%.1f%%; 95%% ДІ [%.1f-%.1f])", k, pct, ci[1], ci[2])
}

# Допоміжна функція для медіани
format_med <- function(x) {
  sprintf("%.1f (%.1f; %.1f)", median(x, na.rm=T), quantile(x, 0.25, na.rm=T), quantile(x, 0.75, na.rm=T))
}

# ==============================================================================
# ---- БЛОК 1: ТАБЛИЦЯ 4 ТА ГРАФІК УСПІХУ (FIGURE 2) ----
# ==============================================================================

risk_data <- clean_data %>%
  filter(!is.na(doc_eval_d21)) %>%
  mutate(Success = ifelse(doc_eval_d21 >= 4, 1, 0)) %>%
  mutate(group = factor(group, levels = c("Control", "Treatment"), 
                        labels = c("Контрольна терапія", "Гастритол")))

# --- 1.1 РОЗРАХУНОК ТАБЛИЦІ 4 (З 95% ДІ) ---
a <- sum(risk_data$group == "Гастритол" & risk_data$Success == 1)
b <- sum(risk_data$group == "Гастритол" & risk_data$Success == 0)
c <- sum(risk_data$group == "Контрольна терапія" & risk_data$Success == 1)
d <- sum(risk_data$group == "Контрольна терапія" & risk_data$Success == 0)

n_treat <- a + b
n_ctrl <- c + d

ci_treat_succ <- binom.test(a, n_treat)$conf.int * 100
ci_ctrl_succ <- binom.test(c, n_ctrl)$conf.int * 100

RD <- (a/n_treat) - (c/n_ctrl)
RD_se <- sqrt(((a/n_treat)*(1-a/n_treat)/n_treat) + ((c/n_ctrl)*(1-c/n_ctrl)/n_ctrl))
RD_ci_lower <- RD - 1.96 * RD_se
RD_ci_upper <- RD + 1.96 * RD_se

RR <- (a/n_treat) / (c/n_ctrl)
RR_se <- sqrt((1/a) - (1/n_treat) + (1/c) - (1/n_ctrl))
RR_ci_lower <- exp(log(RR) - 1.96 * RR_se)
RR_ci_upper <- exp(log(RR) + 1.96 * RR_se)

OR <- (a * d) / (b * c)
OR_se <- sqrt((1/a) + (1/b) + (1/c) + (1/d))
OR_ci_lower <- exp(log(OR) - 1.96 * OR_se)
OR_ci_upper <- exp(log(OR) + 1.96 * OR_se)

NNT <- 1 / RD

table_risks <- tibble(
  `Метрика ефективності` = c(
    "Частота клінічного успіху (Гастритол)", 
    "Частота клінічного успіху (Контроль)",
    "Різниця ризиків (RD)",
    "Відносний ризик (RR)",
    "Відношення шансів (OR)",
    "Number Needed to Treat (NNT)"
  ),
  `Значення` = c(
    sprintf("%d/%d (%.1f%%)", a, n_treat, (a/n_treat)*100),
    sprintf("%d/%d (%.1f%%)", c, n_ctrl, (c/n_ctrl)*100),
    sprintf("%.3f", RD),
    sprintf("%.2f", RR),
    sprintf("%.2f", OR),
    sprintf("%.1f", NNT)
  ),
  `95% Довірчий інтервал (CI)` = c(
    sprintf("[%.1f%%; %.1f%%]", ci_treat_succ[1], ci_treat_succ[2]), 
    sprintf("[%.1f%%; %.1f%%]", ci_ctrl_succ[1], ci_ctrl_succ[2]), 
    sprintf("[%.3f; %.3f]", RD_ci_lower, RD_ci_upper),
    sprintf("[%.2f; %.2f]", RR_ci_lower, RR_ci_upper),
    sprintf("[%.2f; %.2f]", OR_ci_lower, OR_ci_upper),
    "-"
  )
)
write_xlsx(table_risks, "results/tables/Table_4_Risk_Metrics.xlsx")

# --- 1.2 ГРАФІК 1: ЧАСТОТА УСПІХУ (FIGURE 2) ---
success_summary <- risk_data %>%
  group_by(group) %>%
  summarise(Total = n(), Successes = sum(Success), Rate = (Successes / Total) * 100)

p_success_val <- fisher.test(table(risk_data$group, risk_data$Success))$p.value
p_success_label <- ifelse(p_success_val < 0.001, "p < 0.001", sprintf("p = %.3f", p_success_val))

p_success <- ggplot(success_summary, aes(x = group, y = Rate, fill = group)) +
  geom_col(width = 0.5, color = "black", linewidth = 0.7, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%", Rate)), vjust = -0.8, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#2E9FDF", "#E7B800")) +
  scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 20)) +
  
  # Анотації RD та p-значення
  annotate("segment", x = 1, xend = 2, y = 94, yend = 94, color = "#d73027", linewidth = 0.8) +
  annotate("segment", x = 1, xend = 1, y = 90, yend = 94, color = "#d73027", linewidth = 0.8) +
  annotate("segment", x = 2, xend = 2, y = 90, yend = 94, color = "#d73027", linewidth = 0.8) +
  annotate("text", x = 1.5, y = 101, label = sprintf("RD = +%.1f%%", RD*100), 
           color = "#d73027", fontface = "bold", size = 4.5) +
  annotate("text", x = 1.5, y = 108, label = p_success_label, 
           color = "#d73027", fontface = "bold.italic", size = 4.5) +
  
  labs(y = "Частота клінічного успіху (%)", x = NULL, title = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_text(face = "bold", size = 13, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(face = "bold", margin = margin(r=10)))

ggsave("results/figures/Figure_2_Clinical_Success.png", plot = p_success, width = 7, height = 6, dpi = 300, bg = "white")


# ==============================================================================
# ---- БЛОК 2: FOREST PLOT ДЛЯ МЕТРИК ЙМОВІРНОСТІ (FIGURE 3) ----
# ==============================================================================

forest_df <- data.frame(
  Metric = factor(c("Відносний ризик (RR)", "Відношення шансів (OR)"), 
                  levels = c("Відношення шансів (OR)", "Відносний ризик (RR)")),
  Estimate = c(RR, OR),
  Lower = c(RR_ci_lower, OR_ci_lower),
  Upper = c(RR_ci_upper, OR_ci_upper),
  P_val = c("p < 0.001", "p < 0.001") 
)

p_forest <- ggplot(forest_df, aes(y = Metric, x = Estimate)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.15, linewidth = 1.2, color = "#4682B4") +
  geom_point(size = 5, shape = 15, color = "#104E8B") +
  
  geom_text(aes(label = sprintf("%.2f [%.2f; %.2f]", Estimate, Lower, Upper)), 
            vjust = -1.5, size = 4.5, fontface = "bold") +
  geom_text(aes(label = P_val), 
            vjust = 2.5, size = 4.5, fontface = "bold.italic", color = "#d73027") +
  
  scale_x_log10(breaks = c(1, 1.5, 2, 3, 5, 8), limits = c(0.8, 8)) +
  labs(x = "Значення (Логарифмічна шкала)", y = NULL, title = NULL) +
  theme(axis.text.y = element_text(face = "bold", size = 13, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.x = element_text(face = "bold", margin = margin(t=10))) +
  
  annotate("text", x = 0.95, y = 0.6, label = "← На користь Контролю", color = "gray50", fontface = "italic", hjust = 1, size = 4) +
  annotate("text", x = 1.05, y = 0.6, label = "На користь Гастритолу →", color = "gray50", fontface = "italic", hjust = 0, size = 4) +
  coord_cartesian(ylim = c(0.8, 2.3), clip = "off")

ggsave("results/figures/Figure_3_Forest_Plot.png", plot = p_forest, width = 8, height = 4.5, dpi = 300, bg = "white")


# ==============================================================================
# ---- БЛОК 3: ТАБЛИЦЯ 3 ТА ГРАФІК БЕЗПЕКИ/КОМПЛАЄНСУ (FIGURE 4) ----
# ==============================================================================

ae_treat_n <- sum(clean_data$group == "Treatment" & clean_data$adverse_events == "Yes")
ae_ctrl_n <- sum(clean_data$group == "Control" & clean_data$adverse_events == "Yes")
comp_treat_n <- sum(clean_data$group == "Treatment" & clean_data$compliance == "Compliant")
comp_ctrl_n <- sum(clean_data$group == "Control" & clean_data$compliance == "Compliant")

total_treat <- sum(clean_data$group == "Treatment")
total_ctrl <- sum(clean_data$group == "Control")

ctrl_tol <- clean_data %>% filter(group == "Control") %>% pull(tolerability_d21)
treat_tol <- clean_data %>% filter(group == "Treatment") %>% pull(tolerability_d21)
p_tol <- wilcox.test(ctrl_tol, treat_tol, exact = FALSE)$p.value

ctrl_doc <- clean_data %>% filter(group == "Control") %>% pull(doc_eval_d21)
treat_doc <- clean_data %>% filter(group == "Treatment") %>% pull(doc_eval_d21)
p_doc <- wilcox.test(ctrl_doc, treat_doc, exact = FALSE)$p.value

p_ae <- fisher.test(table(clean_data$group, clean_data$adverse_events))$p.value
p_comp <- fisher.test(table(clean_data$group, clean_data$compliance))$p.value

# --- 3.1 РОЗРАХУНОК ТАБЛИЦІ 3 (З 95% ДІ) ---
table_safety <- tibble(
  `Показник (День 21)` = c(
    "Глобальна оцінка лікарем (бали), Me (Q1; Q3)",
    "Суб'єктивна переносимість (бали), Me (Q1; Q3)",
    "Наявність побічних явищ, n (%; 95% ДІ)",
    "Висока прихильність до лікування, n (%; 95% ДІ)"
  ),
  `Контроль (n=245)` = c(
    format_med(ctrl_doc),
    format_med(ctrl_tol),
    format_pct_ci(ae_ctrl_n, total_ctrl),
    format_pct_ci(comp_ctrl_n, total_ctrl)
  ),
  `Гастритол (n=288)` = c(
    format_med(treat_doc),
    format_med(treat_tol),
    format_pct_ci(ae_treat_n, total_treat),
    format_pct_ci(comp_treat_n, total_treat)
  ),
  `p-значення` = c(
    ifelse(p_doc < 0.0001, "<0.0001", sprintf("%.4f", p_doc)),
    ifelse(p_tol < 0.0001, "<0.0001", sprintf("%.4f", p_tol)),
    sprintf("%.4f", p_ae),
    ifelse(p_comp < 0.0001, "<0.0001", sprintf("%.4f", p_comp))
  )
)
write_xlsx(table_safety, "results/tables/Table_3_Safety_and_Compliance.xlsx")

# --- 3.2 ГРАФІК 3: БЕЗПЕКА ТА КОМПЛАЄНС (FIGURE 4) ---
safety_df <- data.frame(
  Metric = rep(c("Наявність побічних явищ", "Прихильність (Комплаєнс)"), each = 2),
  Group = rep(c("Контрольна терапія", "Гастритол"), 2),
  Percentage = c(
    (ae_ctrl_n/total_ctrl)*100, 
    (ae_treat_n/total_treat)*100, 
    (comp_ctrl_n/total_ctrl)*100, 
    (comp_treat_n/total_treat)*100
  )
)

safety_df$Group <- factor(safety_df$Group, levels = c("Контрольна терапія", "Гастритол"))
safety_df$Metric <- factor(safety_df$Metric, levels = c("Наявність побічних явищ", "Прихильність (Комплаєнс)"))

lbl_ae <- ifelse(p_ae < 0.001, "p < 0.001", sprintf("p = %.3f", p_ae))
lbl_comp <- ifelse(p_comp < 0.001, "p < 0.001", sprintf("p = %.3f", p_comp))

p_safety <- ggplot(safety_df, aes(x = Metric, y = Percentage, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black", alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_dodge(width = 0.7), vjust = -0.8, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("#2E9FDF", "#E7B800")) +
  scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 20)) +
  
  annotate("text", x = 1, y = 12, label = lbl_ae, fontface = "italic", size = 4.5, color = "gray40") +
  annotate("text", x = 2, y = 108, label = lbl_comp, fontface = "bold.italic", size = 4.5, color = "#d73027") +
  
  labs(y = "Частка пацієнтів (%)", x = NULL, fill = "", title = NULL) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(face = "bold", size = 13, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(face = "bold", margin = margin(r=10)))

ggsave("results/figures/Figure_4_Safety_Compliance.png", plot = p_safety, width = 8, height = 6, dpi = 300, bg = "white")

message("Успіх! Згенеровано 2 таблиці (з точними 95% ДІ) та 3 рисунки (тільки візуал).")
# ==============================================================================