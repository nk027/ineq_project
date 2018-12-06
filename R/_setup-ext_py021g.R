# ------------------------------------------------------------------------
#
# py021g R-Script
# Autor: Lasser
# Datum: 2018-11-08
# Comment: This script won't work in isolation. 
#   You may want to include it in the download step of `_setup.R`.
#   Make sure silc.p includes the required variables for merging.
#   The original can be found at:
#   github.com/lasserro/ineq_project/blob/master/R/_setup_including_py021g.R
#
# -------------------------------------------------------------------------

# Download c[YY]p tables from 2007 - 2013
c07p <- tbl(pg, "c07p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c08p <- tbl(pg, "c08p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c09p <- tbl(pg, "c09p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c10p <- tbl(pg, "c10p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c11p <- tbl(pg, "c11p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c12p <- tbl(pg, "c12p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c13p <- tbl(pg, "c13p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

cxxp <- bind_rows(c07p, c08p, c09p, c10p, c11p, c12p, c13p)
rm(c07p, c08p, c09p, c10p, c11p, c12p, c13p)

# Merge cxxp with silc.p to include the py021g variable for 2007-2013

silc.p <- left_join(silc.p, cxxp %>% select(py021g, pb010, pb030))
rm(cxxp)
