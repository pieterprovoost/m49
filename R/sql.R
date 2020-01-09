#' Generate PostgreSQL statements.
#'
#' @usage generate_sql())
#' @export
generate_sql <- function() {
  sql_regions <- m49_regions %>%
    mutate(parent = ifelse(!is.na(parent), parent, "null")) %>%
    mutate(
      statement = paste0("insert into regions (id, name, parent) values (", code, ", '", name, "', ", parent, ");")
    ) %>%
    pull(statement)
  sql_countries <- m49_countries %>%
    mutate(
      parent = ifelse(!is.na(parent), parent, "null"),
      name = gsub("'", "''", name)
    ) %>%
    mutate(
      statement = paste0("insert into countries (id, name, parent, iso_alpha3_code, ldc, lldc, sids, developed, developing) values (", code, ", '", name, "', ", parent, ", '", iso_alpha3_code, "', ", ldc, ", ", lldc, ", ", sids, ", ", developed, ", ", developing, ");")
    ) %>%
    pull(statement)
  sql_tables <- c(
    "drop table if exists countries;",
    "drop table if exists regions;",
    "create table regions (id int primary key, name text, parent int references regions(id));",
    "create table countries (id int primary key, name text, parent int references regions(id), iso_alpha3_code char(3), ldc boolean, lldc boolean, sids boolean, developed boolean, developing boolean);",
    "create index ix_regions_parent on regions(parent);",
    "create index ix_countries_parent on countries(parent);"
  )
  sql <- c(sql_tables, sql_regions, sql_countries)
  return(sql)
}
