#list of end uses sectors, used for the industry subset the energy balance (nrg_bal_c)

#Manufacturing
NRG_MAN <- c(
    "FC_IND_FBT_E",
    "FC_IND_TL_E",
    "FC_IND_WP_E",
    "FC_IND_PPP_E",
    "NRG_PR_E",
    "FC_IND_CPC_E",
    "FC_IND_NMM_E",
    "FC_IND_IS_E",
    "NRG_CO_E",
    "NRG_BF_E",
    "FC_IND_NFM_E",
    "FC_IND_MAC_E",
    "FC_IND_TE_E",
    "NRG_PF_E",
    "NRG_BKBPB_E",
    "NRG_CL_E",
    "NRG_GTL_E",
    "NRG_CPP_E",
    "NRG_NSP_E",
    "FC_IND_NSP_E"
  )

#Agriculture, forestry and fishing
NRG_AGRI <- c("FC_OTH_AF_E",
                "FC_OTH_FISH_E")

#Other industries
NRG_OTH <- c(
    "FC_IND_MQ_E",
    "NRG_CM_E",
    "NRG_OIL_NG_E",
#"TI_EHG_E", # removed because double counting with electricity
    "NRG_EHG_E",
    "NRG_GW_E",
    "NRG_LNG_E",
    "NRG_BIOG_E",
    "NRG_NI_E"
  )

NRG_TRA <- c(
    "FC_TRA_RAIL_E",
    "FC_TRA_ROAD_E",
    "FC_TRA_DNAVI_E"
  )

#list of end uses sectors, used in the subset
NRG_FULL_SECTORS <- c(
    NRG_AGRI,
    NRG_MAN,
    NRG_OTH,
    NRG_TRA,
    "FC_IND_CON_E",
    "FC_OTH_CP_E",
    "FC_OTH_HH_E"
  )

#list of supply items, used in the context
NRG_SUPPLY <- c(
    "PPRD",
    "IMP",
    "EXP",
    "STK_CHG"
  )

#list of end uses sectors, as they will be named in the LMDI results
IDA_FULL_SECTORS <- c(
#"Agriculture, forestry and fishing",
    "Agricult., forest. and fish.",
    "Manufacturing",
    "Construction",
    "Other industries",
#"Commercial and public services"
    "Comm. and pub. services",
    "Residential",
    "Transport"
  )
