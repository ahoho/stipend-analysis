library(tidyverse)
library(ggdark)

stipends <- 
  read_csv("http://www.phdstipends.com/csv") %>%
  janitor::clean_names() %>%
  mutate(across(
    ends_with("_pay"),
    ~(
      str_remove_all(., pattern=",|\\$") %>%
        as.numeric()
    )  
  ))

schools <- read_csv("school_data.csv")

big10 <-
  stipends %>%
  filter(university %in% schools$university) %>%
  mutate(
     # 3-month data too noisy to consider
     pay_12_or_9 = coalesce(x12_m_gross_pay,x9_m_gross_pay)
  ) %>%
  
  # remove outliers
  filter(pay_12_or_9 > 9000 & pay_12_or_9 < 75000) %>%
  
  # create a 9-month & 12-month equivalent rate
  mutate(
    # counts only academic year
    pay_9_equivalent = coalesce(x9_m_gross_pay, x12_m_gross_pay * (3 / 4)),
    
    # assumes money made during the summer is not supplemented
    pay_12_no_summer = coalesce(x12_m_gross_pay, x9_m_gross_pay),
    
    # includes summer funding (which may include outside internships)
    pay_12_plus_summer = coalesce(
      x12_m_gross_pay,
      x9_m_gross_pay + coalesce(x3_m_gross_pay, 0)
    ),
  )

# summarize the big10 data
big10_counts <- group_by(big10, university) %>% summarize(responses=n())

avg_fn <- median
col_diff_fn <- `-`

calc_ratios <- function(df, fn=`/`){
  df %>%
    mutate(
      col_9_mo = col * (3 / 4),
      pay_9_to_col = fn(pay_9_equivalent, col_9_mo),
      pay_12_no_summer_to_col = fn(pay_12_no_summer, col),
      pay_12_plus_summer_to_col = fn(pay_12_plus_summer, col),
      min_stipend_to_col = fn(min_stipend, col)
    )
}

big10_summary <-
  big10 %>%
  group_by(university) %>%
  summarize(across(where(is.numeric), avg_fn, na.rm=TRUE)) %>%
  left_join(big10_counts) %>%
  left_join(schools) %>%
  calc_ratios(col_diff_fn) %>%
  select(
    university, uni, is_unionized, responses,
    starts_with("col"), starts_with("pay"), min_stipend, min_stipend_to_col, -pay_12_or_9
  )

write_csv(big10_summary, "big10_summary.csv")
write_lines(c(
  "Pay data from www.phdstipends.com/csv",
  "COL data from https://livingwage.mit.edu/metros/14020",
  "12 or 9-month stipend amounts below 9000 and above 75000 were removed as noise.",
  "Unless otherwise noted, all absolute pay values are medians.",
  "",
  "Columns:",
  "    university: University name",
  "    is_unionized: Is campus unionized (includes Northwestern, which is ongoing)",
  "    responses: Individual survey responses in data.",
  "    col: Required annual cost of living, per MIT data",
  "    col_9_mo: col*0.75",
  "    pay_9_equivalent: 9-month pay if reported, otherwise reported 12-month pay multiplied by 3/4",
  "    pay_12_no_summer: 12-month pay if reported, otherwise flat 9-month pay (i.e., assumption is no summer pay unless indicated)",
  "    pay_12_plus_summer: 12-month pay if reported, otherwise 9-month pay plus any reported 3-month pay (may include outside internships; data are noisy)",
  "    min_stipend: minimum stipend reported for the school",
  "    <payment_column>_to_col: ratio of pay to COL over the specified period"
), "big10_summary_readme.txt")

union_summary <-
  big10 %>%
  left_join(schools) %>%
  calc_ratios(col_diff_fn) %>%
  group_by(is_unionized) %>%
  summarize(across(where(is.numeric), avg_fn, na.rm=TRUE)) %>%
  left_join(union_counts) %>%
  select(
    is_unionized, responses, starts_with("col"), starts_with("pay"), 
    min_stipend, min_stipend_to_col, -pay_12_or_9
  )

# Plotting
pay_var <- "pay_12_plus_summer_to_col"
pay_period <- c(
  pay_9_to_col="Median, academic year",
  pay_12_no_summer_to_col="Median, full-year w/o summer",
  pay_12_plus_summer_to_col="Median, full-year",
  min_stipend_to_col="Based on minimum stipend"
)

big10_summary %>%
  mutate(
    uni_n = paste0(uni, " (", responses, ")"),
    union_labels = if_else(is_unionized, "Unionzied", "Not Unionized"),
    union_labels = factor(
      if_else(uni == "UMD", "UMD", union_labels),
      levels=c("Unionzied", "Not Unionized", "UMD"),
      ordered=TRUE
    )
  ) %>%
  arrange(is_unionized, .data[[pay_var]]) %>%
  mutate(
    order = 1:n(),
  ) %>%
  ggplot(aes(
        x = reorder(uni, order),
        y = !!ensym(pay_var),
  )) +
  geom_bar(aes(fill=union_labels), stat="identity") +
  scale_fill_manual(
    values=c("#E21833", "gray", "#2a2a2a"),
  ) +
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(
    x = "",
    y = "",
    title = paste0("Cost-of-Living Deficit\n(", pay_period[pay_var], ")"),
    fill = ""
  ) +
  #dark_theme_classic() +
  theme_classic() + 
  theme(
    text = element_text(size=24),
    axis.text.x = element_text(angle=45, hjust=1)
  )
  ggsave(paste0(pay_var,".png"), width=12, height=10, dpi=300)
  


  big10_summary %>%
    mutate(
      uni_n = paste0(uni, " (", responses, ")"),
      union_labels = if_else(is_unionized, "Unionzied", "Not Unionized"),
      union_labels = factor(
        if_else(uni == "UMD", "UMD", union_labels),
        levels=c("Unionzied", "Not Unionized", "UMD"),
        ordered=TRUE
      )
    ) %>%
    arrange(is_unionized, .data[[pay_var]]) %>%
    mutate(
      order = 1:n(),
    ) %>%
    ggplot(aes(
      x = reorder(uni, order),
      y = !!ensym(pay_var),
    )) +
    geom_bar(aes(fill=union_labels), stat="identity") +
    scale_fill_manual(
      values=c("#E21833", "gray", "#2a2a2a"),
    ) +
    scale_y_continuous(labels=scales::dollar_format()) +
    labs(
      x = "",
      y = "",
      title = paste0("Cost-of-Living Deficit\n(", pay_period[pay_var], ")"),
      fill = ""
    ) +
    #dark_theme_classic() +
    theme_classic() + 
    theme(
      text = element_text(size=24),
      axis.text.x = element_text(angle=45, hjust=1)
    )
  ggsave(paste0(pay_var,".png"), width=12, height=10, dpi=300)
  

  
  big10_summary %>%
    mutate(
      uni_n = paste0(uni, " (", responses, ")"),
    ) %>%
    arrange(.data[[pay_var]]) %>%
    mutate(
      order = 1:n(),
    ) %>%
    ggplot(aes(
      x = reorder(uni, order),
      y = !!ensym(pay_var),
    )) +
    geom_bar(stat="identity") +
    scale_y_continuous(labels=scales::dollar_format()) +
    labs(
      x = "",
      y = "",
      title = paste0("Cost-of-Living Deficit\n(", pay_period[pay_var], ")"),
      fill = ""
    ) +
    #dark_theme_classic() +
    theme_classic() + 
    theme(
      text = element_text(size=24),
      axis.text.x = element_text(angle=45, hjust=1)
    )
  ggsave(paste0(pay_var,"-without-union.pdf"), width=12, height=10, dpi=300)  
  
  
union_summary %>%
    mutate(
      union_labels = factor(
        if_else(is_unionized, "Unionzied", "Not Unionized"),
        levels=c("Unionzied", "Not Unionized"),
        ordered = TRUE
      )
    ) %>%
    ggplot(aes(
      x = reorder(union_labels, is_unionized),
      y = !!ensym(pay_var),
    )) +
    geom_bar(aes(fill=union_labels), stat="identity") +
    scale_fill_manual(
      values=c("#ffd200", "gray"),
    ) +
    scale_y_continuous(labels=scales::dollar_format()) +
    labs(
      x = "",
      y = paste("Cost-of-Living Deficit", pay_period[pay_var], "\n"),
      fill = ""
    ) +
    theme_classic() +
    theme(
      text = element_text(size=16)#,
      #axis.text.x = element_text(angle=45, hjust=1)
    )
    ggsave("median_col_deficit_by_union.png", width=6, height=10, dpi=600)
