library(dplyr)

apply_gva_corrections <- function(df) {
	df <- df %>%
		# correction
		mutate(GVA = case_when(
			(geo == "AT" & sector == "Coke and ref. pet. products" & time == 2013) ~ GVA * 10,
			(geo == "AT" & sector == "Coke and ref. pet. products" & time == 2014) ~ GVA * 100,
			TRUE ~ GVA
		))

	message("AT 2013, 2014 Coke and ref. pet. products GVA manual correction")

	df
}

apply_vkm_corrections <- function(df) {
	df <- df %>%
		mutate(VKM = case_when(
			(geo == "BG") &
				(time == 2010) ~ 1301900000,
			TRUE ~ VKM
		))

	print("BG 2010 road traffic manual correction")

	df
}

# Skipped because only data for 2011
transport_skipped_countries <- c("CY", "EL", "LU", "PT")