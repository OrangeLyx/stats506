data <- read.csv("./nonprofit-survey-spring-2021-puf.csv")
prefixes <- c("ProgChanges_", "OpsChanges_", "Assistance_", "ExtAffairs_", "NTEE_",
              "FRchanges_","Funding","Finances_","Reserves_","FinanceChanges_",
              "ProgDem_","PeopleServed_","MeetDemand","GeoAreas_","ProgLocations_" ,"RuralUrban_","LocChanges")
pattern <- paste0("^(", paste(prefixes, collapse="|"), ").*")
vars <- grep(pattern, names(data), value = TRUE)
new_data <- data[, vars]
new_data[new_data== -99 |new_data == 99 | new_data == 98] <- NA
library(dplyr)

ana_data <- new_data %>%
    rename(
        cate_arts = NTEE_1,
        cate_edu  = NTEE_2,
        cate_envir=NTEE_3,
        cate_animl=NTEE_4,
        cate_heal=NTEE_5,
        cate_mentl=NTEE_6,
        cate_volut=NTEE_7,
        cate_medic=NTEE_8,
        cate_crime=NTEE_9,
        cate_employ=NTEE_10,
        cate_food=NTEE_11,
        cate_housg=NTEE_12,
        cate_public=NTEE_13,
        cate_recret=NTEE_14,
        cate_youth=NTEE_15,
        cate_human=NTEE_16,
        cate_foreign=NTEE_17,
        cate_right=NTEE_18,
        cate_commuty=NTEE_19,
        cate_donate=NTEE_20,
        cate_tech=NTEE_21,
        cate_science=NTEE_22,
        cate_society=NTEE_23,
        cate_relign=NTEE_24,
        cate_member=NTEE_25,
    )%>% dplyr::select(-NTEE_26, - ProgDem_21)


summary_data <- data.frame(
    variable = names(ana_data),
    non_na_count = sapply(ana_data, function(x) sum(!is.na(x))),
    total_rows = nrow(ana_data)
)

summary_data$non_na_pct <- summary_data$non_na_count / summary_data$total_rows * 100

summary_data$quality <- cut(
    summary_data$non_na_pct,
    breaks = c(-Inf, 10, 40, 70, Inf),
    labels = c("bad data", "doubtful data", "basic data", "good data"),
    right = FALSE
)



delete_cols <- summary_data$variable[summary_data$non_na_pct < 2]


ana_data <- ana_data %>% dplyr::select(-any_of(delete_cols))

 
