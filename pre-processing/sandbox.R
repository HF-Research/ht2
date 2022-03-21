library(magrittr)
library(data.table)
library(ggplot2)
library(flextable)
library(officer)
edu <-
  fread(file = "input_ui_text/edu_description.csv", encoding = "UTF-8")
tmp <- HTData::export_diag %>% copy()
x <-
  tmp[aggr_level == "edu" &
        event == "n.incidence" & ht.code == "d1"]
x[, "Uddannelse" := factor(
  aggr_level_val,
  levels = tolower(edu$edu_name_en),
  labels =
    edu$edu_name_dk
)]
x$aggr_level_val %>% unique()
x$Uddannelse %>% unique()
facet_labels  <-
  c("Kvinder", "Mænd")
x[, sex_plot := factor(sex, levels = c("female", "male"), labels = facet_labels)]

# PLOT RATES --------------------------------------------------------------------

cols <- c(RColorBrewer::brewer.pal(n = 4, name = "Dark2"), 'grey')
z <- ggplot() +
  geom_line(
    data = x,
    mapping = aes(
      x = year,
      y = rate,
      group = Uddannelse,
      color = Uddannelse
    ),
    size = 1
  ) +
  facet_wrap(vars(sex_plot)) +
  ggthemes::theme_pander() +
  coord_cartesian(ylim = c(0, max(x$rate))) +
  scale_color_manual(values = cols) +
  ylab("Age standardized rate \nper 100.000") +
  theme(legend.position = c(0.86, .2)) +
  scale_x_continuous(breaks = 2006:2018) +
  ggtitle("Rate of first-time diagnoses by eduction level \n(all heart diseases, ages 35-79)")
z
ggsave(filename = "documentation/edu_time.jpg",
       width = 14,
       height = 7.5)


# TABLE -------------------------------------------------------------------
tmp <- x[, .(rate, Uddannelse, sex, year)]
tmp[, rate := round(rate)]
tab <-
  dcast(data = tmp,
        formula = Uddannelse + year ~ sex,
        value.var = "rate")[order(year, Uddannelse)]
tab[, year := factor(year)]
setcolorder(tab, "year")
tab_flex <- flextable(tab) %>%
  merge_v(j = "year") %>%
  valign(j = "year", valign = "top") %>%
  width(j = "Uddannelse", width = 3) %>%
  colformat_num(
    j = c("male", "female"),
    decimal.mark = ",",
    big.mark = "."
  )
tab_flex
out_work <- read_docx() %>%
  body_add_par(value = "Age-standardized rates (per 100.000) of first-time heart disease diagnoses") %>%
  body_add_par(value = "") %>%
  body_add_flextable(tab_flex)

print(out_work, "documentation/edu_tables_word.docx")

write.csv2(x = tab, file = "documentation/table_edu.csv")



# PLOT RR -----------------------------------------------------------------

tmp <-
  dcast(x, sex + sex_plot + year ~ aggr_level_val, value.var = "rate") %>%
  melt(id.vars = c("sex", "sex_plot", "year", "basic"))
tmp[, r_ratio := value / basic]
setnames(tmp, "variable", "Uddannelse")
cols <- c(RColorBrewer::brewer.pal(n = 3, name = "Dark2"), 'grey')
z <- ggplot() + geom_line(
  tmp,
  mapping = aes(
    x = year,
    y = r_ratio,
    group = Uddannelse,
    color = Uddannelse
  ),
  size = 1
) + facet_wrap(vars(sex)) +
  ggthemes::theme_pander() +
  coord_cartesian(ylim = c(0, max(tmp$r_ratio))) +
  scale_color_manual(values = cols) +
  ylab("Age standardized rate ratio") +
  theme(legend.position = c(0.86, .2)) +
  scale_x_continuous(breaks = 2006:2018) +
  ggtitle(
    "Rate ratio compared to Grundskole of first-time diagnoses by eduction level \n(all heart diseases, ages 35-79)"
  )
z
ggsave(filename = "documentation/edu_rr_time.jpg",
       width = 14,
       height = 7.5)
