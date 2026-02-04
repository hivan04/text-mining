# 4. Estimated topic proportions for each document
q4_acc  <- final_bundle$q4$accuracy
q4_plot <- final_bundle$q4$effect_data

q4_acc


ggplot(res$q4$effect_data, aes(x = reorder(topic, Estimate_scaled), y = Estimate_scaled)) +
  geom_point(size = 3, color = "#377EB8") +
  geom_errorbar(aes(ymin = se_l, ymax = se_u), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Q4: Topic Influence on Salary", 
       y = "Effect Size (per 1 Million units)", x = "Industry Topics")


# 5. Benchmark results
comparison_df <- final_bundle$q5$comparison_results
comparison_df

ggplot(res$q5$comparison_results, aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Model)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  geom_text(aes(label = sprintf("%.2f", Accuracy)), hjust = -0.5) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Q5: Performance Comparison", y = "Kendall's Tau Accuracy")


# 6.Filtering 
conf_matrix <- final_bundle$q6$confusion_matrix
q6_accuracy <- final_bundle$q6$accuracy_value

conf_matrix

conf_df <- as.data.frame(res$q6$confusion_matrix)
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal() +
  labs(title = paste("Q6: Multi-class Accuracy:", res$q6$accuracy_value))