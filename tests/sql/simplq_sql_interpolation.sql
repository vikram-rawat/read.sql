select
  *
from
  {main_table}
where
  {column1} >= ?mincol1
  and
  {column2} >= ?mincol2
  and
  {column3} in ({species_value})
  and
  `Petal.Width` in ({width_value})
