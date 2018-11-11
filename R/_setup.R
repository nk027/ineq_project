# Setup -------------------------------------------------------------------

library(dplyr)
issue_1_dealt_with <- FALSE
if(!exists(c("country", "year"))) {
  stop("Please specify country and year.")
}


# Prepare Data ------------------------------------------------------------

# Connect to the PostgreSQL database
if(issue_1_dealt_with) {
  pg <- src_postgres(dbname = "dbname", host = "host",
                     user = "user", password = "password",
                     options = "options")
} else {
  stop("Please deal with Issue #1.")
}


# Download data
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 %in% country & pb010 %in% year) %>%
  select(pb020, pb030, pb040, pb150, py010g, py050, px010, px030) %>%
  collect(n = Inf)

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 %in% country & hb010 %in% year) %>%
  select(hb020, hb030, hy010, hx010) %>%
  collect(n = Inf)

silc.d <- tbl(pg, "dd") %>%
  filter(db020 %in% country & db010 %in% year) %>%
  select(db010, db020, db030, db040, db090, px010) %>%
  collect(n = Inf)

silc.r <- tbl(pg, "rr") %>% 
  filter(rb020 %in% country & rb010 %in% year) %>%
  select(rb010, rb020, rb030, rb050) %>%
  collect(n = Inf)

# Create unique IDs for merging
silc.p <- silc.p %>% mutate(id_h = paste0(pb020, px030))

silc.h <- silc.h %>% mutate(id_h = paste0(hb020, hb030))

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030))

# Merge the datasets
silc.pd <- left_join(silc.p, silc.d %>% select(id_h, db020, db090))

silc.hd <- left_join(silc.h, silc.d)

# Remove
rm(silc.p, silc.h, silc.d)


# Fin ---------------------------------------------------------------------

message("Prepared data for ", country, " in ", year, ".")
