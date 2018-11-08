issue_1_dealt_with <- FALSE
# Dependencies ------------------------------------------------------------

library(dplyr)

# Connect to the PostgreSQL database
if(issue_1_dealt_with) {
  pg <- src_postgres(...)
} else {
  stop("Please deal with Issue #1 ")
}


# Download some data
silc.d <- tbl(pg, "c11d") %>% filter(db020 == "AT") %>%
  select(db010, db020, db030, db040, db090) %>% collect()

silc.p <- tbl(pg, "c11p") %>% filter(pb020 == "AT") %>%
  select(pb020, pb030, px030, pb150, py010g, py050g, pl060, pl100,
         pl073, pl074, pl075, pl076, pb140, pe040) %>% collect(n = Inf)

# Join the data
silc.dp <- right_join(silc.d, silc.p,
                      by = c('db020' = 'pb020', 'db030' = 'px030'))

# Remove
rm(silc.d, silc.p)