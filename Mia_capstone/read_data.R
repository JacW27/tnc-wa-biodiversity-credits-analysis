# Hoh files -> H1, H2, H3, ...
Hoh <- list.files(
    here("Data", "Hoh"),
  pattern = "\\.xlsx$",
  full.names = TRUE
)

h <- lapply(Hoh, read_excel)

list2env(
  setNames(h, paste0("H", seq_along(h))),
  envir = .GlobalEnv
)


# Clearwater files -> C1, C2, C3, ...
Clearwater <- list.files(
   here("Data", "Clearwater"),
  pattern = "\\.xlsx$",
  full.names = TRUE
)

c <- lapply(Clearwater, read_excel)

list2env(
  setNames(c, paste0("C", seq_along(c))),
  envir = .GlobalEnv
)


# Ellsworth files -> E1, E2, E3, ...
Ellsworth <- list.files(
    here("Data", "Ellsworth"),
  pattern = "\\.xlsx$",
  full.names = TRUE
)

e <- lapply(Ellsworth, read_excel)

list2env(
  setNames(e, paste0("E", seq_along(e))),
  envir = .GlobalEnv
)
