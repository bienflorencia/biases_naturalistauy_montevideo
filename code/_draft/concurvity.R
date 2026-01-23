library(mgcv)
library(dplyr)
library(tidyr)
library(ggplot2)
#### run "SamplingParticipation.R" first 
## must have 'fit_count5' and 'mvd_hex' your Global Environment
## substitute model name to test other models
vars <- c("ndmi", "hou_tot", "dist", "pap", "edu_adu")

# Drop geometry and enforce numeric
df_vars <- mvd_hex %>%
  st_drop_geometry() %>%
  mutate(across(all_of(vars), as.numeric))

# 1. Concurvity
conc <- concurvity(fit_count5, full = FALSE)
conc_est <- as.data.frame(conc$estimate)

conc_df <- conc_est %>%
  mutate(term1 = rownames(.)) %>%
  pivot_longer(cols = -term1,
               names_to = "term2",
               values_to = "concurvity")

# 2. Correlation
cor_mat <- cor(df_vars[, vars], use = "pairwise.complete.obs")

cor_df <- as.data.frame(cor_mat) %>%
  mutate(term1 = rownames(.)) %>%
  pivot_longer(cols = -term1,
               names_to = "term2",
               values_to = "correlation")

conc_df$term1<- gsub("^s\\((.*?)\\)$", "\\1", (conc_df$term1))
conc_df$term2<- gsub("^s\\((.*?)\\)$", "\\1", (conc_df$term2))

# 3. Merge concurvity + correlation
plot_df <- left_join(conc_df, cor_df, by = c("term1", "term2"))
plot_df
conc_df
cor_df

# 4. Plot
ggplot(plot_df, aes(x = term1, y = term2, fill = concurvity)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  scale_fill_gradient(low = "white", high = "red",
                      limits = c(0, 1),
                      name = "Concurvity (0–1)") +
  coord_equal() +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Concurvity vs Correlation Matrix",
       subtitle = "Heatmap = concurvity; Numbers = correlations",
       x = "Term", y = "Term")
