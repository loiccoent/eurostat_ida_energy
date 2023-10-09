library(RColorBrewer)

ColorsIndex <- c(
    "Energy consumption" = "blue4",
    "Gross Value Added" = "red4",
    "Energy intensity" = "green4"
  )
  
ColorsEffect <- c(
"Activity" = "red4",
"Intensity" = "green4",
"Structure" = "purple4"
)

ColorsSector <- c(
"Construction" = brewer.pal(12, "Set3")[1],
"Mining and quarrying" = brewer.pal(12, "Set3")[2],
#"Food, beverages and tobacco" = brewer.pal(12, "Set3")[3],
"Food, bev. and tobacco" = brewer.pal(12, "Set3")[3],
"Textile and leather" = brewer.pal(12, "Set3")[4],
"Wood and wood products" = brewer.pal(12, "Set3")[5],
"Paper, pulp and printing" = brewer.pal(12, "Set3")[6],
#"Coke and refined petroleum products" = brewer.pal(12, "Set3")[7],
"Coke and ref. pet. products" = brewer.pal(12, "Set3")[7],
#"Chemical and petrochemical" = brewer.pal(12, "Set3")[8],
"Chemical and petrochem." = brewer.pal(12, "Set3")[8],
"Non-metallic minerals" = brewer.pal(12, "Set3")[9],
"Basic metals" = brewer.pal(12, "Set3")[10],
"Machinery" = brewer.pal(12, "Set3")[11],
"Transport equipment" = brewer.pal(12, "Set3")[12],
"Other manufacturing" = "grey"
)

ColorsProduct <- c(
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