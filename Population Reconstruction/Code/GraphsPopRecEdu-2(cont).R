################################################################################
#Graph 2018
popedu_graph2018 <- popedu_graph %>%
  filter(year == 2018) %>%
  mutate(pop_pm = if_else(sex == "Female", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary")))) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = reversed_blues, name = "Education") +  # Use the reversed palette
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.background = element_blank(),  # remove the background
        panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))



################################################################################
#Graph 2013
popedu_graph2013 <- popedu_graph %>%
  filter(year == 2013) %>%
  mutate(pop_pm = if_else(sex == "Female", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary")))) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = reversed_blues, name = "Education") +  # Use the reversed palette
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.background = element_blank(),  # remove the background
        panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))


################################################################################
#Graph 2008
popedu_graph2008 <- popedu_graph %>%
  filter(year == 2008) %>%
  mutate(pop_pm = if_else(sex == "Female", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary")))) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = reversed_blues, name = "Education") +  # Use the reversed palette
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.background = element_blank(),  # remove the background
        panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))

################################################################################
#Graph 2003
popedu_graph2003 <- popedu_graph %>%
  filter(year == 2003) %>%
  mutate(pop_pm = if_else(sex == "Female", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary")))) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = reversed_blues, name = "Education") +  # Use the reversed palette
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.background = element_blank(),  # remove the background
        panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))


################################################################################
#Graph 1998
popedu_graph1998 <- popedu_graph %>%
  filter(year == 1998) %>%
  mutate(pop_pm = if_else(sex == "Female", -pop_pm, pop_pm),
         education = factor(education, levels = rev(c("No Education", "Primary", "Secondary", "Post Secondary")))) %>%
  ggplot(mapping = aes(x = pop_pm, y = age, fill = education)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "white") +
  scale_x_continuous(labels = abs, expand = c(0, 0)) +
  scale_fill_manual(values = reversed_blues, name = "Education") +  # Use the reversed palette
  labs(x = "Colombian Population 2018 (millions)", y = "Age") +
  geom_blank(mapping = aes(x = pop_max * 1.1)) +
  theme(panel.background = element_blank(),  # remove the background
        panel.spacing.x = unit(0, "pt"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "transparent"),
        strip.text.x = element_text(margin = margin( b = 0, t = 0)))




################################################################################
#Exporting graphs to output
print(popedu_graph1998)
ggsave("Output/popedu_graph1998.png", device = "png")
dev.off()
print(popedu_graph2003)
ggsave("Output/popedu_graph2003.png", device = "png")
dev.off()
print(popedu_graph2008)
ggsave("Output/popedu_graph2008.png", device = "png")
dev.off()
print(popedu_graph2013)
ggsave("Output/popedu_graph2013.png", device = "png")
dev.off()
print(popedu_graph2018)
ggsave("Output/popedu_graph2018.png", device = "png")
dev.off()


