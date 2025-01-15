# Some custom plotting functions inspired by vis_dat() and vis_miss() from {visdat}: https://cran.r-project.org/web/packages/visdat/readme/README.html
# The visdat plots had ggplot theming that didn't work with thematic, so I made my own

plot_missing_values <- function(df){
  df %>% 
    rowid_to_column("Observations") %>% 
    mutate(across(-Observations, ~ if_else(is.na(.x), TRUE, FALSE))) %>% 
    pivot_longer(cols = -Observations,
                 names_to = "Variables",
                 names_transform = ~ factor(.x, levels = names(df)),
                 values_to = "Missing") %>% 
    mutate(.by = Variables, Variables = paste0(Variables, " (", scales::percent(mean(Missing)), ")")) %>% 
    ggplot(mapping = aes(x = Variables, y = Observations)) +
    geom_tile(mapping = aes(fill = Missing, color = Missing)) +
    scale_y_reverse() +
    scale_x_discrete(position = "top") +
    scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "gray"), aesthetics = c("fill", "color")) +
    coord_cartesian(expand = FALSE) +
    theme(axis.text.x.top = element_text(angle = 45, vjust = 0, hjust = 0))
}

plot_data_types <- function(df){
  df %>% 
    rowid_to_column("Observations") %>% 
    mutate(across(-Observations, ~ paste0(class(.x), collapse = (", ")))) %>% 
    pivot_longer(cols = -Observations,
                 names_to = "Variables",
                 names_transform = ~ factor(.x, levels = names(df)),
                 values_to = "Type") %>% 
    ggplot(mapping = aes(x = Variables, y = Observations)) +
    geom_tile(mapping = aes(fill = Type, color = Type)) +
    scale_y_reverse() +
    scale_x_discrete(position = "top") +
    coord_cartesian(expand = FALSE) +
    theme(axis.text.x.top = element_text(angle = 45, vjust = 0, hjust = 0))
}




