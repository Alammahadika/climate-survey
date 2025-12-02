# Climate Change in the Indonesian Mind 
This repository is created as part of the selection process for the **Researcher Assosiate** position at [Communication for Change (C4C)](https://communicationforchange.id)
Analyzing gender differences in climate risk perception using the C4C–Yale National Survey on “Climate Change in the Indonesian Mind”

Repository link to dataset source:  
https://labnarasi.id/topik/perubahan-iklim-dalam-pikiran-masyarakat-indonesia-2/

---

## Overview

This repository contains the analysis and visualization of Indonesian public perceptions regarding **when global warming will begin to harm people**.  
The analysis focuses specifically on **gender-disaggregated patterns**, using:

- **Q14** — “When will global warming begin to harm people in Indonesia?”
- **S27** — Gender variable  
- **N = 2,990** respondents from the C4C–Yale national climate survey.

The goal is to explore how men and women differ in their temporal estimations of climate risk, and why “No Answer” emerges as a significant category.

---

## Key Findings
![visualisasi hasil](/visual-data.png)

### 1. High “No Answer” Responses (31%–35%)
A striking proportion of respondents selected **“No Answer”**.  
This trend supports Weber’s argument that climate risks are often perceived as *abstract* and *temporally distant*, making it difficult for people to judge when harm will occur.

### 2. “Already Harming” Responses (~33%)
A sizeable share of respondents believe that global warming is already harming people.  
This aligns with theories of **experiential risk perception**, where individuals base judgments on direct exposure to extreme heat, floods, or seasonal disruptions.

### 3. Age Pattern: 16-Year-Olds as a Major “No Answer” Group
National studies show that climate literacy among Indonesian youth remains low.  
Martha & Bersal (2025) find that **49.7% of young Indonesians have low climate literacy**.

This appears in the C4C dataset:  
Respondents aged **16** form the *second-largest age group* selecting “No Answer,” reinforcing the link between literacy gaps and uncertainty.

### 4. Gender Differences in Non-Responses
Women show slightly higher proportions of “No Answer.”  
This pattern reflects **gendered information gaps**, where women often express lower confidence—not lower concern—when answering scientific or technical questions.

In contrast, men are more likely to offer definitive estimates under uncertainty, consistent with literature on **male overconfidence in risk assessments** (Finucane, 2000).

---

## Code for Visualization (R)

```r
# ============================
# 1. DATA PREPARATION
# ============================

library(tidyverse)

# Select only gender (S27) and global warming timing (Q14)
df <- Perubahan_iklim_Narrative_mapping_Survey_Raw_data %>%
  select(S27, Q14) %>%
  mutate(
    gender = recode(S27,
                    `1` = "Male",
                    `2` = "Female"),
    q14_label = recode(Q14,
                       `1` = "Already harming",
                       `2` = "In 10 years",
                       `3` = "In 25 years",
                       `4` = "In 50 years",
                       `5` = "In 100 years",
                       `6` = "Never",
                       `7` = "No answer",
                       .default = NA_character_)
  ) %>%
  drop_na(gender, q14_label)

# Count percentages within each gender
plot_df <- df %>%
  count(gender, q14_label) %>%
  group_by(gender) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

# Calculate total % per category to define ordering
order_df <- plot_df %>%
  group_by(q14_label) %>%
  summarise(total_pct = sum(pct)) %>%
  arrange(desc(total_pct))

# Apply reversed ordering for visualization
plot_df$q14_label <- factor(
  plot_df$q14_label,
  levels = rev(order_df$q14_label)
)


# ============================
# 2. VISUALIZATION
# ============================

ggplot(plot_df, aes(x = pct, y = q14_label, fill = gender)) +
  
  geom_bar(
    data = subset(plot_df, gender == "Male"),
    stat = "identity",
    width = 0.5
  ) +
  geom_bar(
    data = subset(plot_df, gender == "Female"),
    aes(x = -pct),
    stat = "identity",
    width = 0.5
  ) +
  
  geom_text(
    data = subset(plot_df, gender == "Male"),
    aes(label = paste0(round(pct, 1), "%")),
    hjust = -0.5,
    size = 3.5,
    color = "black",
    fontface = "bold"
  ) +
  geom_text(
    data = subset(plot_df, gender == "Female"),
    aes(x = -pct, label = paste0(round(pct, 1), "%")),
    hjust = 1.5,
    size = 3.5,
    color = "black",
    fontface = "bold"
  ) +
  
  scale_fill_manual(values = c("Female" = "darkblue", "Male" = "darkred")) +
  labs(
    title = "When Will Global Warming Start Harming People in Indonesia?",
    subtitle = "Gender-Disaggregated Perception (Reversed Order by Total %)",
    x = NULL,
    y = NULL,
    fill = "Gender",
    caption = "C4C, Yale & Kantar Indonesia (2025)"
  ) +
  
  scale_x_continuous(
    labels = NULL,
    breaks = NULL,
    limits = max(plot_df$pct) * c(-1.3, 1.3)
  ) +
  
  theme_bw() +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

 
```
