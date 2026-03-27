library(tidyverse, quietly = TRUE)

# Useful fun
classify_gear <- function(x) {

  # capture the NAs
  if (is.na(x)) return(NA_character_)

  # split on underscores and treat each piece as a separate code
  parts <- unlist(str_split(x, "_"))
  parts <- parts[str_detect(parts, "^[A-Za-z]+$")]

  # helper for standalone-ish match
  tok <- function(x) paste0("(?<![A-Z])(", x, ")(?![A-Z])")

  cats <- c(
    PelTrawl = any(str_detect(parts, regex(tok("OTM|PTM|PT"), ignore_case = TRUE))),
    DemTrawl = any(str_detect(parts, regex(tok("OTB|PTB|TBB|OTT|TB"), ignore_case = TRUE))),
    DemSeine = any(str_detect(parts, regex(tok("SSC|SPR|SDN"), ignore_case = TRUE))),
    PelSeine = any(str_detect(parts, regex(tok("PS"), ignore_case = TRUE))),
    LinesHooks = any(str_detect(parts, regex(tok("LLD|LLS|LH|LHP|LHM|LTL"), ignore_case = TRUE))),
    Gillnetters = any(str_detect(parts, regex(tok("GNS|GTR|GN|GND|GTN|GNC"), ignore_case = TRUE)))
  )

  # free-text rules look at full string
  cats["PelTrawl"]    <- cats["PelTrawl"]    | str_detect(x, regex("pelagic trawler", ignore_case = TRUE))
  cats["Gillnetters"] <- cats["Gillnetters"] | str_detect(x, regex("gillnetter", ignore_case = TRUE))
  cats["DemTrawl"]    <- cats["DemTrawl"]    | (str_detect(x, regex("trawler", ignore_case = TRUE)) &
                                                 str_detect(x, regex("demersal|cod", ignore_case = TRUE)))

  present <- names(cats)[cats]

  if (length(present) == 0) {
    "Others"
  } else if (length(present) == 1) {
    present
  } else {
    paste0("Multiple (", paste(sort(present), collapse = "+"), ")")
  }
}

# Expand area
expandIcesArea <- function(x) {
  x <- str_replace_all(x, " ", "")
  x <- str_replace(x, "\\.$", "")   # drop dot

  # Pure 27 region or fully specified already
  if (x == "27") return(x)
  if (str_detect(x, "^27\\.[0-9]+(\\.[A-Za-z0-9]+)*$")) return(x)

  # 27.4b, 27.7bcjk
  if (str_detect(x, "^27\\.[0-9]+[A-Za-z]+$")) {
    num     <- str_match(x, "^27\\.([0-9]+)")[, 2]
    letters <- str_match(x, "^27\\.[0-9]+([A-Za-z]+)$")[, 2]
    letters_vec <- strsplit(letters, "")[[1]]
    letters_vec <- letters_vec[letters_vec != ""]
    return(sprintf("27.%s.%s", num, letters_vec))
  }

  # 7bcjk, 7fgh etc
  if (str_detect(x, "^[0-9]+[A-Za-z]+$")) {
    num     <- str_match(x, "^([0-9]+)")[, 2]
    letters <- str_match(x, "^[0-9]+([A-Za-z]+)$")[, 2]
    letters_vec <- strsplit(letters, "")[[1]]
    letters_vec <- letters_vec[letters_vec != ""]
    return(sprintf("27.%s.%s", num, letters_vec))
  }

  # 27.1, 27.12 etc keep as is will be expanded
  if (str_detect(x, "^27\\.\\d+$")) return(x)

  # Fallback: keep unchanged
  x
}

# Parse
parse_one_code <- function(code) {
  orig <- code

  code <- str_squish(code)

  if (is.na(code) || code == "" || code == "NA") {
    return(tibble(
      original       = orig,
      AreaCode_system = NA_character_,
      AreaCode        = NA_character_
    ))
  }

  # Split on both "~" and "," (mixed strings)
  parts <- str_split(code, "[~,]")[[1]] %>%
    str_squish()

  parts <- parts[parts != ""]

  map_dfr(parts, function(p) {
    if (is.na(p) || p == "") {
      return(tibble(
        original        = orig,
        AreaCode_system = NA_character_,
        AreaCode        = NA_character_
      ))
    }

   #  explicit GSA + number
    if (str_detect(str_to_upper(p), "^GSA\\s*\\d+$")) {
      num <- as.integer(str_extract(p, "\\d+"))
      return(tibble(
        original        = orig,
        AreaCode_system = "GSA",
        AreaCode        = paste0("GSA ", num)
      ))
    }

    # Plain digits
    if (str_detect(p, "^\\d{1,2}$")) {
      # If original string has a 27.* and no 'GSA'  interpret as 27.<n> (like 27.12,14)
      if (str_detect(orig, "27\\.") &&
          !str_detect(orig, regex("\\bGSA\\b", ignore_case = TRUE))) {
        new_code <- paste0("27.", p)
        return(tibble(
          original        = orig,
          AreaCode_system = "ICES",
          AreaCode        = new_code
        ))
      } else {
        # Otherwise treat as GSA number
        num <- as.integer(p)
        return(tibble(
          original        = orig,
          AreaCode_system = "GSA",
          AreaCode        = paste0("GSA ", num)
        ))
      }
    }

    p_clean <- str_replace_all(p, " ", "")
    p_clean <- str_replace(p_clean, "\\.$", "")

    if (str_detect(p_clean, "^27(\\.|$)") ||
        str_detect(p_clean, "^[0-9]+[A-Za-z]+$")) {
      expanded <- expandIcesArea(p_clean)
      # expanded may be length > 1
      return(tibble(
        original        = orig,
        AreaCode_system = "ICES",
        AreaCode        = expanded
      ))
    }

    # Unknown stuff
    tibble(
      original        = orig,
      AreaCode_system = "Unknown",
      AreaCode        = p
    )
  })
}

# Look up table
ices_divisions <- tribble(
  ~subarea,   ~division,
  # 27.1
  "27.1",     "27.1.a",
  "27.1",     "27.1.b",
  # 27.2
  "27.2",     "27.2.a",
  "27.2",     "27.2.b",
  # 27.3 (Baltic + Kattegat)
  "27.3.a",   "27.3.a.20",
  "27.3.a",   "27.3.a.21",
  "27.3.b",   "27.3.b.23",
  "27.3.c",   "27.3.c.22",
  # 27.3.d (Baltic Proper) incl. 28.1/28.2
  "27.3.d",   "27.3.d.24",
  "27.3.d",   "27.3.d.25",
  "27.3.d",   "27.3.d.26",
  "27.3.d",   "27.3.d.27",
  "27.3.d",   "27.3.d.28",
  "27.3.d",   "27.3.d.28.1",
  "27.3.d",   "27.3.d.28.2",
  "27.3.d",   "27.3.d.29",
  "27.3.d",   "27.3.d.30",
  "27.3.d",   "27.3.d.31",
  "27.3.d",   "27.3.d.32",
  # 27.4 (North Sea)
  "27.4",     "27.4.a",
  "27.4",     "27.4.b",
  "27.4",     "27.4.c",
  # 27.5 (Iceland & Faroes)
  "27.5",     "27.5.a",
  "27.5",     "27.5.b",
  # 27.6 (W of Scotland/Rockall)
  "27.6",     "27.6.a",
  "27.6",     "27.6.b",
  "27.6.b",   "27.6.b.1",
  # 27.7 (Celtic/Biscay)
  "27.7",     "27.7.a",
  "27.7",     "27.7.b",
  "27.7",     "27.7.c",
  "27.7",     "27.7.d",
  "27.7",     "27.7.e",
  "27.7",     "27.7.f",
  "27.7",     "27.7.g",
  "27.7",     "27.7.h",
  "27.7",     "27.7.j",
  "27.7",     "27.7.k",
  # 27.8 (Bay of Biscay)
  "27.8",     "27.8.a",
  "27.8",     "27.8.b",
  "27.8",     "27.8.c",
  "27.8",     "27.8.d",
  "27.8",     "27.8.e",
  # 27.9
  "27.9",     "27.9.a",
  "27.9",     "27.9.b.1",
  "27.9",     "27.9.b.2",
  "27.9",     "27.9.b",
  # 27.10 (Azores)
  "27.10",    "27.10.a",
  "27.10.a",  "27.10.a.1",
  "27.10.a",  "27.10.a.2",
  "27.10",    "27.10.b",
  # 27.12 (Svalbard/Barents fringe)
  "27.12",    "27.12.a",
  "27.12",    "27.12.b",
  "27.12",    "27.12.c",
  # 27.14 (Greenland)
  "27.14",    "27.14.a",
  "27.14",    "27.14.b"
)
