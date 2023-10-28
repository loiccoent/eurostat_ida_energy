

prepare_industry_GVA <- function(
    nama_10_a64,
    first_year,
    last_year,
    country_list) {
    nama_10_a64 %>%
        filter(
            geo %in% country_list,
            # from first year
            time >= first_year,
            # to last year
            time <= last_year,
            # take industry sub sectors
            nace_r2 %in% GVA_IND_SECTORS,
            # Gross Value Added in Chain linked volumes (2015), million euro
            na_item == "B1G",
            unit == "CLV15_MEUR"
        ) %>%
        select(c("geo", "time", "nace_r2", "values")) %>%
        # reshape to wide
        pivot_wider(
            names_from = nace_r2,
            values_from = values
        ) %>%
        # aggregate
        mutate(
            # Paper
            "C17-C18" = rowSums(select(., c("C17", "C18")), na.rm = TRUE),
            # Chem and petchem
            "C20-C21" = rowSums(select(., c("C20", "C21")), na.rm = TRUE),
            # Non-metallic minerals
            "C22-C23" = rowSums(select(., c("C22", "C23")), na.rm = TRUE),
            # Machinery
            "C25-C28" = rowSums(select(., c("C25", "C26", "C27", "C28")), na.rm = TRUE),
            # Transport equipment
            "C29-C30" = rowSums(select(., c("C29", "C30")), na.rm = TRUE)
        ) %>%
        # keep only relevant columns
        select(-c(C17, C18, C20, C21, C22, C23, C25, C26, C27, C28, C29, C30)) %>%
        # Rename to explicit names
        rename(
            "Construction" = "F",
            "Mining and quarrying" = "B",
            # "Food, beverages and tobacco" = "C10-C12",
            "Food, bev. and tobacco" = "C10-C12",
            "Textile and leather" = "C13-C15",
            "Wood and wood products" = "C16",
            "Paper, pulp and printing" = "C17-C18",
            # "Coke and refined petroleum products" = "C19",
            "Coke and ref. pet. products" = "C19",
            # "Chemical and petrochemical" = "C20-C21",
            "Chemical and petrochem." = "C20-C21",
            "Non-metallic minerals" = "C22-C23",
            "Basic metals" = "C24",
            "Machinery" = "C25-C28",
            "Transport equipment" = "C29-C30",
            "Other manufacturing" = "C31_C32"
        ) %>%
        # Reshape to long
        pivot_longer(
            cols = -c(geo, time),
            names_to = "sector",
            values_to = "GVA"
        )
}