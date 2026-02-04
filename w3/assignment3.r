---
title: "assignment3_results_Jin"
output: html_document
date: "2026-02-01"
---

```{r}
library(quanteda)
library(ggrepel)
library(textclean)
library(tidyverse)
library(glmnet)
library(pROC)
library(sentimentr)
library(stm)

library(wordcloud)           # Load the library
library(igraph)

library(tidyverse)
library(sentimentr)
```

```{r}
# Set seed and open the data
set.seed(070104)
final_bundle <- readRDS("Assignment3_Final_Bundle.RDS")

# 1. Train ngram to predict salary 
q1_model <- final_bundle$q1$model
q1_acc   <- final_bundle$q1$accuracy
q1_plot  <- final_bundle$q1$coef_plot_data

# 2. Structural Topic Model (Twenty-Topic Model) Labels
stm_model   <- final_bundle$q2$model
topic_names <- final_bundle$q2$labels
stm_summary <- final_bundle$q2$proportions

# 3. Word cloud
top_thoughts <- final_bundle$q3$top_docs

# 4. Estimated topic proportions for each document
q4_acc  <- final_bundle$q4$accuracy
q4_plot <- final_bundle$q4$effect_data

# 5. Benchmark results
comparison_df <- final_bundle$q5$comparison_results

# 6.Filtering 
conf_matrix <- final_bundle$q6$confusion_matrix
q6_accuracy <- final_bundle$q6$accuracy_value
```


```{r}

# ==========================================
# 4000 randomly selected rows from the set to create a multinomial classifier to predict five categories from the text
# ==========================================
res <- readRDS("Assignment3_Final_Bundle.RDS")

# --- Q1: N-gram LASSO 可视化 ---

ggplot(res$q1$coef_plot_data, aes(x = score, y = freq, label = ngram, color = score)) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_label_repel(max.overlaps = 30, force = 6) +  
  scale_y_continuous(trans = "log2",
                     breaks = c(.01, .05, .1, .2, .5, 1, 2, 5)) +
  theme_bw() +
  labs(x = "Coefficient in Model", 
       y = "Uses per Review",
       title = "Q1: Lasso Coefficients for Salary Prediction") +
  theme(legend.position = "right")

print(res$q3$top_docs)

ggplot(res$q4$effect_data, aes(x = reorder(topic, Estimate_scaled), y = Estimate_scaled)) +
  geom_point(size = 3, color = "#377EB8") +
  geom_errorbar(aes(ymin = se_l, ymax = se_u), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Q4: Topic Influence on Salary", 
       y = "Effect Size (per 1 Million units)", x = "Industry Topics")

ggplot(res$q5$comparison_results, aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Model)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_text(aes(label = sprintf("%.2f", Accuracy)), hjust = -0.5) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Q5: Performance Comparison", y = "Kendall's Tau Accuracy")

conf_df <- as.data.frame(res$q6$confusion_matrix)
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal() +
  labs(title = paste("Q6: Multi-class Accuracy:", res$q6$accuracy_value))
```


