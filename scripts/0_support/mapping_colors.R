library(RColorBrewer)

IndustryGVAColorsIndex <- c(
    "Energy consumption" = "blue4",
    "Gross Value Added" = "red4",
    "Energy intensity" = "green4"
)

IndustryGVAPrimaryColorsIndex <- c(
    "Primary energy consumption" = "blue4",
    "Gross Value Added" = "red4",
    "Final energy intensity" = "green4",
    "Primary to final energy transformation" = "yellow4"
)

EconomyEmploymentColorsIndex <- c(
    "Energy consumption" = "blue4",
    "Employment" = "red4",
    "Energy consumption per employee" = "green4"
)

IndustryGVAColorsEffect <- c(
    "Activity" = "red4",
    "Intensity" = "green4",
    "Structure" = "purple4"
)

IndustryGVAPrimaryColorsEffect <- c(
    "Activity" = "red4",
    "Intensity" = "green4",
    "Structure" = "purple4",
    "Transformation" = "yellow4"
)

EconomyEmploymentColorsEffect <- c(
    "Activity" = "red4",
    "Intensity" = "green4",
    "Structure" = "purple4"
)

AllSectorsColors <- c(
    # "Agriculture, forestry and fishing" = brewer.pal(5, "Set3")[1],
    "Agricult., forest. and fish." = brewer.pal(7, "Set3")[3],
    "Manufacturing" = brewer.pal(7, "Set3")[2],
    "Construction" = brewer.pal(7, "Set3")[1],
    "Other industries" = brewer.pal(7, "Set3")[4],
    # "Commercial and public services" = brewer.pal(5, "Set3")[5]
    "Comm. and pub. services" = brewer.pal(7, "Set3")[5],
    "Residential" = brewer.pal(7, "Set3")[7],
    "Transport" = brewer.pal(7, "Set3")[6]
)

ManufacturingSectorsColors <- c(
    "Construction" = brewer.pal(12, "Set3")[1],
    "Mining and quarrying" = brewer.pal(12, "Set3")[2],
    # "Food, beverages and tobacco" = brewer.pal(12, "Set3")[3],
    "Food, bev. and tobacco" = brewer.pal(12, "Set3")[3],
    "Textile and leather" = brewer.pal(12, "Set3")[4],
    "Wood and wood products" = brewer.pal(12, "Set3")[5],
    "Paper, pulp and printing" = brewer.pal(12, "Set3")[6],
    # "Coke and refined petroleum products" = brewer.pal(12, "Set3")[7],
    "Coke and ref. pet. products" = brewer.pal(12, "Set3")[7],
    # "Chemical and petrochemical" = brewer.pal(12, "Set3")[8],
    "Chemical and petrochem." = brewer.pal(12, "Set3")[8],
    "Non-metallic minerals" = brewer.pal(12, "Set3")[9],
    "Basic metals" = brewer.pal(12, "Set3")[10],
    "Machinery" = brewer.pal(12, "Set3")[11],
    "Transport equipment" = brewer.pal(12, "Set3")[12],
    "Other manufacturing" = "grey"
)

EconomySectorsColors <- c(
    # "Agriculture, forestry and fishing" = brewer.pal(5, "Set3")[1],
    "Agricult., forest. and fish." = brewer.pal(5, "Set3")[3],
    "Manufacturing" = brewer.pal(5, "Set3")[2],
    "Construction" = brewer.pal(5, "Set3")[1],
    "Other industries" = brewer.pal(5, "Set3")[4],
    # "Commercial and public services" = brewer.pal(5, "Set3")[5]
    "Comm. and pub. services" = brewer.pal(5, "Set3")[5]
)

FinalProductsColors <- c(
    "Coal" = brewer.pal(10, "Set3")[10],
    "Oil" = brewer.pal(10, "Set3")[6],
    "Gas" = brewer.pal(10, "Set3")[8],
    "Biofuels and renewable wastes" = brewer.pal(10, "Set3")[7],
    "Non-renewable wastes" = brewer.pal(10, "Set3")[9],
    #    "Nuclear" = brewer.pal(10, "Set3")[2],
    #    "Hydro" = brewer.pal(10, "Set3")[5],
    "Wind, solar, geothermal, etc." = brewer.pal(10, "Set3")[1],
    "Heat" = brewer.pal(10, "Set3")[4],
    "Electricity" = brewer.pal(10, "Set3")[3]
)

PrimaryProductsColors <- c(
    "Coal" = brewer.pal(10, "Set3")[10],
    "Oil" = brewer.pal(10, "Set3")[6],
    "Gas" = brewer.pal(10, "Set3")[8],
    "Biofuels and renewable wastes" = brewer.pal(10, "Set3")[7],
    "Non-renewable wastes" = brewer.pal(10, "Set3")[9],
    "Nuclear" = brewer.pal(10, "Set3")[2],
    "Hydro" = brewer.pal(10, "Set3")[5],
    "Wind, solar, geothermal, etc." = brewer.pal(10, "Set3")[1],
    "Heat" = brewer.pal(10, "Set3")[4],
    "Electricity" = brewer.pal(10, "Set3")[3]
)