# list of end uses sectors, used for the industry subset the energy balance (nrg_bal_c)
NRG_IND_SECTORS <- c(
  "NRG_CM_E",
  "NRG_OIL_NG_E",
  "NRG_PR_E",
  "NRG_CO_E",
  "NRG_BF_E",
  "FC_IND_CON_E",
  "FC_IND_CPC_E",
  "FC_IND_FBT_E",
  "FC_IND_IS_E",
  "FC_IND_MAC_E",
  "FC_IND_MQ_E",
  "FC_IND_NFM_E",
  "FC_IND_NMM_E",
  "FC_IND_NSP_E",
  "FC_IND_PPP_E",
  "FC_IND_TE_E",
  "FC_IND_TL_E",
  "FC_IND_WP_E",
  "NRG_PF_E",
  "NRG_BKBPB_E",
  "NRG_CL_E",
  "NRG_GTL_E",
  "NRG_CPP_E",
  "NRG_NSP_E"
)

# list of end uses sectors, used for the industry subset of the economic data (nama_10_a64)
GVA_IND_SECTORS <- c(
  "F",
  "B",
  "C10-C12",
  "C13-C15",
  "C16",
  "C17",
  "C18",
  "C19",
  "C20",
  "C21",
  "C22",
  "C23",
  "C24",
  "C25",
  "C26",
  "C27",
  "C28",
  "C29",
  "C30",
  "C31_C32"
)

# list of end uses sectors, as they will be named in the LMDI results
IDA_IND_SECTOR <- c(
  "Construction",
  "Mining and quarrying",
  # "Food, beverages and tobacco",
  "Food, bev. and tobacco",
  "Textile and leather",
  "Wood and wood products",
  "Paper, pulp and printing",
  # "Coke and refined petroleum products",
  "Coke and ref. pet. products",
  # "Chemical and petrochemical",
  "Chemical and petrochem.",
  "Non-metallic minerals",
  "Basic metals",
  "Machinery",
  "Transport equipment",
  "Other manufacturing"
)

# Inputs to electricity and heat generation
NRG_EHG_INPUT <-
  c(
    "TI_EHG_E",
    "TI_EHG_MAPE_E",
    "TI_EHG_MAPCHP_E",
    "TI_EHG_MAPH_E",
    "TI_EHG_APE_E",
    "TI_EHG_APCHP_E",
    "TI_EHG_APH_E",
    "TI_EHG_EDHP",
    "TI_EHG_EB",
    "TI_EHG_EPS",
    "TI_EHG_DHEP"
  )

# Output of electricity and heat
NRG_EHG_OUTPUT <-
  c(
    "TO_EHG",
    "TO_EHG_MAPE",
    "TO_EHG_MAPCHP",
    "TO_EHG_MAPH",
    "TO_EHG_APE",
    "TO_EHG_APCHP",
    "TO_EHG_APH",
    "TO_EHG_EDHP",
    "TO_EHG_EB",
    "TO_EHG_PH",
    "TO_EHG_OTH"
  )