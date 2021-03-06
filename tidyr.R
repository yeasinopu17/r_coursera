library(tidyr)
table1
table2
table3
table4a

##gather is older version of pivot_longer
pivot_longer(table4a, cols = c("1999","2000"), names_to = "year", values_to = "cases") # 1999 and 2000 column year column name nibe and 1999 oo 2000 er value cases name column a jabe
gather(table4a, "1999","2000", key = "year", value = "cases")# same result as pivot longer

pivot_wider(table2, names_from = "type", values_from = "count") # type column er value column a transfer hobe and value asbe count column theke

table3
separate(table3, rate, into = c("rate","population"), sep = "/", convert = T) # rate column separate hobe, pattern = "/"; convert T means datatype coverted to right type instead of chr


# unite fun separate fun er opposite
table5
unite(table5, new, century, year, sep = "")# new name a akta column create hobe century and year ke combine kore


cars2 <- cars
cars2
cars2[5, 2] <- NA # here 5 num row er 2 num col NA
cars2
#Fill in missing values with previous or next value
fill(cars2, dist,.direction = "up") # dist col er NA ke fill korbe , porer col er value deye

#Complete a data frame with missing combinations of data
df <- tibble(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6
)
df
df2 <- df %>% complete(group, nesting(item_id, item_name))
df2
