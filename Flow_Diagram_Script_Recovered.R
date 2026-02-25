# ==================================
# Read and Prepare Data
# ==================================
# ----- Read packages -----

rm(list = ls()) # Clean environment
library(tidyverse)
library(readxl)
library(data.table)
library(forcats)
library(lubridate)
library(scales)
library(tools)
library(here)


# ----- Demographic data -----

# Read demographic information
demos = read_xlsx("TwinDetails_18.08.2025.xlsx") 

# Ensure consistent formatting
demos = demos %>%
  rename(STUDY_ID = STUDY_NO)


# ----- Non-responders data -----

# Load data
non_responders = read_xlsx("Data_2025/ContactedTwins_LEAP.xlsx")

# Ensure consistent formatting
non_responders = non_responders %>%
  rename(STUDY_ID = STUDY_NO)

# Remove Family No variable (to avoid duplicates when merging dataframes)
non_responders = non_responders %>%
  select(-Family_No)


# ----- LEAP twins data -----

# Load data
leap_participants = read_xlsx("Data_2025/All LEAP parrticipants as of 24Sept25.xlsx")

# Ensure consistent formatting
leap_participants = leap_participants %>%
  rename(STUDY_ID = STUDY_NO)

# Create dataframe of singletons
singletons = leap_participants %>% 
  filter(Status == "Singleton - co-twin without clinic visit or postal LEAPclusion")


# ----- LEAPclusion data -----

# Load data
leapclusion = read_xlsx("Data_2025/All LEAP parrticipants as of 24Sept25.xlsx",
                        sheet = "LEAPclusion")

# Ensure consistent formatting
leapclusion = leapclusion %>%
  rename(STUDY_ID = STUDY_NO)


# ----- LEAP not contacted data ----- 

# load data
leap_not_contacted = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx", 
                               sheet = "Not contacted")

# Ensure consistent formatting
leap_not_contacted = leap_not_contacted %>%
  rename(STUDY_ID = STUDY_NO)


# ----- LEAP refused data ----- 

# load data
leap_inactive_all = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx", 
                              sheet = "Unbooked_inactive")

# Ensure consistent formatting
leap_inactive_all = leap_inactive_all %>%
  rename(STUDY_ID = STUDY_NO)

# Create data for those inactive before LEAP
leap_inactive = leap_inactive_all %>% 
  filter(is.na(Notes))

# Create data for those that refused
leap_refused = leap_inactive_all %>% 
  filter(!is.na(Notes))


# ----- LEAP eligibility data -----

# Load LEAP eligibility data
leap_eligibility_all = read_xlsx("Data_2025/EntireRegistryNov2025.xlsx")

# Ensure consistent formatting
leap_eligibility_all = leap_eligibility_all %>%
  rename(STUDY_ID = STUDY_NO) %>% 
  mutate(fam_id = substr(STUDY_ID, 1, nchar(STUDY_ID) - 1)) 

# Identify eligible individuals that are marked as ineligible in error
eligible_error_ids = leap_participants %>% 
  filter(!STUDY_ID %in% subset(leap_eligibility_all, LEAP != "No")$STUDY_ID) %>% 
  pull(STUDY_ID)

# Correct errors in eligiblity data
leap_eligibility_all = leap_eligibility_all %>% 
  mutate(LEAP = ifelse(STUDY_ID %in% eligible_error_ids, "LEAP - Error Corrected", LEAP))

# Create dataframe for only individuals that were eligible
leap_eligibility = leap_eligibility_all %>%
  filter(LEAP != 'No')


# ==================================
# Attrition Reason
# ==================================
# ----- Create working data with all individuals and relevant data -----

# Create dataset for all individuals within TwinsUK cohort
# Include some individuals not included within demo file, but are present in HES APC
working_data = leap_eligibility_all %>% 
  left_join(demos, by = "STUDY_ID") %>% 
  select(STUDY_ID, Type_Desc) %>% 
  distinct(STUDY_ID, .keep_all = T)

# Create variable to identify only twins/triplets
working_data = working_data %>%
  mutate(not_twin_or_triplet = ifelse(Type_Desc %in% c("Non-Twin", "Parent", "Sibling"), T, F))

# Create family ID variable
working_data = working_data %>% 
  mutate(family_id = STUDY_ID %/% 10)

# Create variables to outline which lists (attrition grouping) each individual is included within
working_data = working_data %>% 
  mutate(non_responder = ifelse(STUDY_ID %in% non_responders$STUDY_ID, T, F),
         leap_recruited = ifelse(STUDY_ID %in% leap_participants$STUDY_ID, T, F),
         leap_leapclusion = ifelse(STUDY_ID %in% leapclusion$STUDY_ID, T, F),
         leap_eligible = ifelse(STUDY_ID %in% leap_eligibility$STUDY_ID & !STUDY_ID %in% leap_inactive$STUDY_ID, T, F),
         singleton = ifelse(STUDY_ID %in% singletons$STUDY_ID, T, F),
         not_contacted = ifelse(STUDY_ID %in% leap_not_contacted$STUDY_ID, T, F),
         refused = ifelse(STUDY_ID %in% leap_refused$STUDY_ID, T, F))

# Create variable to outline attrition group
working_data = working_data %>%
  mutate(attrition_group = case_when(
    leap_eligible == F ~ "Total not LEAP eligible",
    not_contacted == T  ~ "Total not contacted (but eligible)",
    non_responder == T ~ "Total non-responders",
    refused == T  ~ "Total excluded due to refusing",
    singleton == T  ~ "Total singletons",
    leap_recruited == T ~ "Total recruited",
    leap_leapclusion == T ~ "Total leapclusion",
    T ~ "Not in any category above"))


# ==================================
# Flow Diagram
# ==================================
# ----- All individuals -----

# Total twins/triplets in cohort
working_data %>% 
  summarise(n_distinct(STUDY_ID))

# >>>>>>>>>> Total not LEAP eligible
working_data %>%
  filter(leap_eligible == F) %>%
  summarise(n_distinct(STUDY_ID))

# >>>>>>>>>> Total not LEAP eligible due to inactivity
working_data %>%
  filter(leap_eligible == F) %>%
  filter(STUDY_ID %in% leap_inactive$STUDY_ID) %>%
  summarise(n_distinct(STUDY_ID))

# Total with linkage and LEAP eligible
working_data %>%
  filter(leap_eligible == T) %>% 
  summarise(n_distinct(STUDY_ID))

# >>>>>>>>>> Total not contacted (but eligible)
working_data %>%
  filter(leap_eligible == T) %>% 
  filter(not_contacted == T) %>% 
  summarise(n_distinct(STUDY_ID))

# Total with linkage, LEAP eligible, and contacted 
working_data %>%
  filter(leap_eligible == T) %>% 
  filter(not_contacted == F) %>% 
  summarise(n_distinct(STUDY_ID))

# >>>>>>>>>> Total non responders 
working_data %>% 
  filter(leap_eligible == T) %>% 
  filter(non_responder == T) %>% 
  summarise(n_distinct(STUDY_ID))

# Total LEAP eligible, contacted, and responded
working_data %>%
  filter(leap_eligible == T) %>% 
  filter(not_contacted == F) %>% 
  filter(non_responder == F) %>% 
  summarise(n_distinct(STUDY_ID))

# >>>>>>>>>> Total excluded due to refusing participation
working_data %>%
  filter(leap_eligible == T) %>% 
  filter(not_contacted == F) %>% 
  filter(non_responder == F) %>% 
  filter(refused == T) %>% 
  summarise(n_distinct(STUDY_ID))

# >>>>>>>>>> Total excluded as twin refused participation
# Not possible to ascertain from admin lists

# Total leapclusion 
working_data %>%
  filter(leap_eligible == T) %>% 
  filter(not_contacted == F) %>% 
  filter(non_responder == F) %>% 
  filter(refused == F) %>% 
  filter(leap_leapclusion == T) %>% 
  summarise(n_distinct(STUDY_ID))

# Total recruited - method 1
working_data %>%
  filter(leap_eligible == T) %>% 
  filter(not_contacted == F) %>% 
  filter(non_responder == F) %>% 
  filter(refused == F) %>% 
  summarise(n_distinct(STUDY_ID))

# Total recruited with linkage - method 2
working_data %>% 
  filter(leap_eligible == T) %>% 
  filter(non_responder == F) %>% 
  filter(leap_recruited == T) %>% 
  summarise(n_distinct(STUDY_ID))


# ==================================
# Unaccounted Individuals 
# ==================================
# ----- Calculate variable overlap -----

unaccounted = setdiff(
  # Total recruited with linkage - method 1
  working_data %>%
    filter(leap_eligible == T) %>% 
    filter(not_contacted == F) %>% 
    filter(non_responder == F) %>% 
    filter(refused == F) %>% 
    filter(leap_leapclusion == F),
  
  # Total recruited with linkage - method 2
  working_data %>% 
    filter(leap_eligible == T) %>% 
    filter(non_responder == F) %>% 
    filter(leap_recruited == T) %>% 
    filter(leap_leapclusion == F)
) 

unaccounted_ids = unaccounted %>% 
  pull(STUDY_ID)

lists = list(
  
  # Not contacted
  Not_contacted = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                            sheet = "Not contacted") %>%
    rename(STUDY_ID = STUDY_NO),
  
  # Unbooked_Postal
  Unbooked_Postal = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                              sheet = "Unbooked_Postal") %>%
    rename(STUDY_ID = STUDY_NO),
  
  # Booked
  Booked = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx", 
                     sheet = "Booked") %>%
    rename(STUDY_ID = STUDY_NO),
  
  # Booked_NoPBMCs
  Booked_NoPBMCs = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx", 
                             sheet = "Booked_NoPBMCs") %>%
    rename(STUDY_ID = STUDY_NO),
  
  # Returned_LEAPclusion
  Returned_LEAPclusion = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                   sheet = "Returned_Leapclusion") %>%
    rename(STUDY_ID = STUDY_NO),
  
  # Returned_LEAPclusion_contacted_yes
  Returned_LEAPclusion_contacted_yes = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                 sheet = "Returned_Leapclusion") %>%
    filter(Contacted == 'yes') %>% 
    rename(STUDY_ID = STUDY_NO),
  
  # Returned_LEAPclusion_contacted_no
  Returned_LEAPclusion_contacted_no = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                sheet = "Returned_Leapclusion") %>%
    filter(Contacted == 'no') %>% 
    rename(STUDY_ID = STUDY_NO),
  
  # Returned_LEAPclusion_booked_yes
  Returned_LEAPclusion_booked_yes = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                              sheet = "Returned_Leapclusion") %>%
    filter(Booked == 'yes') %>% 
    rename(STUDY_ID = STUDY_NO),
  
  # Returned_LEAPclusion_booked_no
  Returned_LEAPclusion_booked_no = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                             sheet = "Returned_Leapclusion") %>%
    filter(Booked == 'no') %>% 
    rename(STUDY_ID = STUDY_NO),
  
  # NoEmail_Phoned
  NoEmail_Phoned = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx", 
                             sheet = "NoEmail_Phoned") %>%
    rename(STUDY_ID = STUDY_NO)
  
)

# Compute % overlap
calculate_overlap = function(x, id_col = "STUDY_ID"){
  round(mean(unaccounted[[id_col]] %in% x[[id_col]]) * 100, 1)
}

sapply(lists, calculate_overlap)


# ==================================
# Logic Table - September 
# ==================================
# ----- Read data -----

# Read leapclusion
Returned_LEAPclusion = read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                 sheet = "Returned_Leapclusion")

contacted = bind_rows(
  
  read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
            sheet = "Contacted once") %>% select(STUDY_NO),
  
  read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
            sheet = "Contacted twice") %>% select(STUDY_NO),
  
  read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
            sheet = "Contacted 3+") %>% select(STUDY_NO)
)


# ----- Create logic table -----


unaccounted_binary = unaccounted %>% 
  select(STUDY_ID) %>% 
  mutate(
    
    # Non-responders 
    contacted = ifelse(STUDY_ID %in% contacted$STUDY_NO, T, ""),  
    
    
    # Not contacted
    Not_contacted = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                   sheet = "Not contacted")$STUDY_NO, T, ""),
    
    # Unbooked_Postal
    Unbooked_Postal = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                     sheet = "Unbooked_Postal")$STUDY_NO, T, ""),
    
    # Unbooked_inactive
    Unbooked_inactive = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                       sheet = "Unbooked_inactive")$STUDY_NO, T, ""),
    
    # Booked
    Unbooked_Postal = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                     sheet = "Booked")$STUDY_NO, T, ""),
    
    # Booked_NoPBMCs
    Booked_NoPBMCs = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                    sheet = "Booked_NoPBMCs")$STUDY_NO, T, ""),
    
    # Returned_Leapclusion
    Returned_Leapclusion = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                          sheet = "Returned_Leapclusion")$STUDY_NO, T, ""),
    
    # Returned_LEAPclusion_contacted_yes
    Returned_LEAPclusion_contacted_yes = ifelse(STUDY_ID %in% subset(Returned_LEAPclusion, Contacted == 'yes')$STUDY_NO, T, ""),
    
    # Returned_LEAPclusion_booked_yes
    Returned_LEAPclusion_booked_yes = ifelse(STUDY_ID %in% subset(Returned_LEAPclusion, Booked == 'yes')$STUDY_NO, T, ""),
    
    # NoEmail_Phoned
    NoEmail_Phoned = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                    sheet = "NoEmail_Phoned")$STUDY_NO, T, ""),
    
    # Leap Clinic Visits
    Leap_Clinic_Visits = ifelse(STUDY_ID %in% leap_participants$STUDY_ID, T, ""),
    
    # LEAPclusion
    LEAPclusion = ifelse(STUDY_ID %in% leapclusion$STUDY_ID, T, ""),
    
    Leap_recruited = ifelse(STUDY_ID %in% leap_participants$STUDY_ID, T, ""),
    
    # Leap_eligible
    Leap_Eligible = ifelse(STUDY_ID %in% leap_eligibility$STUDY_ID, T, ""))

unaccounted_logic_table = unaccounted_binary %>% 
  select(-STUDY_ID) %>% 
  count(across(everything())) %>% 
  arrange(desc(n)) %>% 
  select(n, everything())

# Save logic table 
write.csv(unaccounted_logic_table, "Outputs/LEAP_Recruitment_Unaccounted_Individuals_Logic_Table.csv",
          row.names = F)

# Save unaccounted cohort 
write.csv(unaccounted, "Outputs/LEAP_Recruitment_Unaccounted_Individuals.csv",
          row.names = F)


# ==================================
# Logic Table - December 
# ==================================

# Read data
twinclusion = read_xlsx("Data_2026/LEAP_Recruitment_Unaccounted_Individuals_Leapclusion.xlsx",
                        sheet = "Twinc.Invited")

twinclusion_invite = twinclusion %>% 
  filter(`Twinclusion Inivte?` == 'Yes')

twinclusion_booked = twinclusion %>% 
  filter(`Twinclusion Booked?` == 'Yes')

twinclusion_cancelled = twinclusion %>% 
  filter(`Twinclusion Booked?` == 'Yes')

twinclusion_any_samples = twinclusion %>% 
  filter(`Any Samples Returned?` == 'Yes')

twinclusion_blood_samples = twinclusion %>% 
  filter(`Blood sample returned?` == 'Yes')

# ----- Sept-Oct recruitment data -----

# Read data
leapseptoct = read_xlsx("Data_2026/Leap Unaccounted Sept Oct 2025 Visit.xlsx",
                        sheet = "LeapSeptOct")


# ----- Oct- recruitment data -----

# Read data
leapcompleted = read_xlsx("Data_2026/Leap Unaccounted Sept Oct 2025 Visit.xlsx",
                          sheet = "LeapCompleted")

# Ensure consistent formatting
leapcompleted = leapcompleted %>%
  rename(STUDY_ID = Study_No)


# ----- Cancelled data -----

# Read data
leapcancelled = read_xlsx("Data_2026/Leap Unaccounted Sept Oct 2025 Visit.xlsx",
                          sheet = "LeapCancelled")

# Ensure consistent formatting
leapcancelled = leapcancelled %>%
  rename(STUDY_ID = STUDY_NO)


# ----- Sept-Oct static recruitment data -----

# Read data
sepocstatic = read_xlsx("Data_2026/Leap Unaccounted Sept Oct 2025 Visit.xlsx",
                        sheet = "SepOcStatic")

# Ensure consistent formatting
sepocstatic = sepocstatic %>%
  rename(STUDY_ID = Study_No)


# ----- Create logic table -----

unaccounted_binary_new = unaccounted %>% 
  select(STUDY_ID) %>% 
  mutate(
    
    # Non-responders 
    contacted = ifelse(STUDY_ID %in% contacted$STUDY_NO, T, ""),  
    
    
    # Not contacted
    Not_contacted = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                   sheet = "Not contacted")$STUDY_NO, T, ""),
    
    # Unbooked_Postal
    Unbooked_Postal = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                     sheet = "Unbooked_Postal")$STUDY_NO, T, ""),
    
    # Unbooked_inactive
    Unbooked_inactive = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                       sheet = "Unbooked_inactive")$STUDY_NO, T, ""),
    
    # Booked
    Unbooked_Postal = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                     sheet = "Booked")$STUDY_NO, T, ""),
    
    # Booked_NoPBMCs
    Booked_NoPBMCs = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                    sheet = "Booked_NoPBMCs")$STUDY_NO, T, ""),
    
    # Returned_Leapclusion
    Returned_Leapclusion = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                          sheet = "Returned_Leapclusion")$STUDY_NO, T, ""),
    
    # Returned_LEAPclusion_contacted_yes
    Returned_LEAPclusion_contacted_yes = ifelse(STUDY_ID %in% subset(Returned_LEAPclusion, Contacted == 'yes')$STUDY_NO, T, ""),
    
    # Returned_LEAPclusion_booked_yes
    Returned_LEAPclusion_booked_yes = ifelse(STUDY_ID %in% subset(Returned_LEAPclusion, Booked == 'yes')$STUDY_NO, T, ""),
    
    # NoEmail_Phoned
    NoEmail_Phoned = ifelse(STUDY_ID %in% read_xlsx("Data_2025/LEAPBreakdown_September2025 1.xlsx",
                                                    sheet = "NoEmail_Phoned")$STUDY_NO, T, ""),
    
    # Leap Clinic Visits
    Leap_Clinic_Visits = ifelse(STUDY_ID %in% leap_participants$STUDY_ID, T, ""),
    
    # twinclusion_invite
    twinclusion_invite = ifelse(STUDY_ID %in% twinclusion_invite$STUDY_ID, T, ""),
    
    # twinclusion_booked
    twinclusion_booked = ifelse(STUDY_ID %in% twinclusion_booked$STUDY_ID, T, ""),
    
    # twinclusion_cancelled
    twinclusion_cancelled = ifelse(STUDY_ID %in% twinclusion_cancelled$STUDY_ID, T, ""),
    
    # twinclusion_any_samples
    twinclusion_any_samples = ifelse(STUDY_ID %in% twinclusion_any_samples$STUDY_ID, T, ""),
    
    # twinclusion_blood_samples
    twinclusion_blood_samples = ifelse(STUDY_ID %in% twinclusion_blood_samples$STUDY_ID, T, ""),
    
    # LeapSeptOct
    LeapSeptOct = ifelse(STUDY_ID %in% leapseptoct$STUDY_ID, T, ""),
    
    # LeapCompleted
    LeapCompleted = ifelse(STUDY_ID %in% leapcompleted$STUDY_ID, T, ""),
    
    # LeapCancelled
    LeapCancelled = ifelse(STUDY_ID %in% leapcancelled$STUDY_ID, T, ""),
    
    # SepOcStatic
    SepOcStatic = ifelse(STUDY_ID %in% sepocstatic$STUDY_ID, T, ""),
    
    # LEAPclusion
    LEAPclusion = ifelse(STUDY_ID %in% leapclusion$STUDY_ID, T, ""),
    
    Leap_recruited = ifelse(STUDY_ID %in% leap_participants$STUDY_ID, T, ""),
    
    # Leap_eligible
    Leap_Eligible = ifelse(STUDY_ID %in% leap_eligibility$STUDY_ID, T, ""))

unaccounted_logic_table_new = unaccounted_binary_new %>% 
  select(-STUDY_ID) %>% 
  count(across(everything())) %>% 
  arrange(desc(n)) %>% 
  select(n, everything())

# Save logic table 
write.csv(unaccounted_logic_table_new, "Outputs/LEAP_Recruitment_Unaccounted_Individuals_Logic_Table_Updated.csv",
          row.names = F)

# Save unaccounted cohort 
write.csv(unaccounted, "Outputs/LEAP_Recruitment_Unaccounted_Individuals_Updated.csv",
          row.names = F)


# ==================================
# Checking New Lists - 2026
# ==================================
# ----- 2026 LEAP Eligibility -----  

# Read updated eligibility data 
leap_eligble_2026 = read_xlsx("Data_2026/FULL Leap Status List.xlsx", sheet = "ALL LEAP") %>%
  rename(STUDY_ID = `STUDY NUMBER`)

# Check for differences in LEAP Status counts
left_join(
  leap_eligibility %>% count(LEAP) %>% rename(n_2025 = n, LEAP_Status = LEAP),
  leap_eligble_2026 %>% count(LEAP_Status) %>% rename(n_2026 = n),
  by = "LEAP_Status")


# ----- 2026 LEAP Booking Data -----

# Read updated booking data 
leap_booked_2026 = read_xlsx("Data_2026/FULL Leap Status List.xlsx", sheet = "LEAP Bookings") %>% 
  select(-StudyID) %>% 
  rename(STUDY_ID = STUDY_NO) 

# Create list of booking completed individuals 
leap_booked_completed_2026 = leap_booked_2026 %>% 
  filter(VisitStatusID == "Completed")

# Create list of booking cancelled individuals 
leap_booked_cancelled_2026 = leap_booked_2026 %>% 
  filter(VisitStatusID == "Cancelled")


# ----- 2026 LEAP Contacting Data -----

# Read updated contacting data 
leap_invites_2026 = read_xlsx("Data_2026/FULL Leap Status List.xlsx", sheet = "LEAP Invites") %>% 
  rename(STUDY_ID = `STUDY NUMBER`)

# Create list of non-responding individuals 
leap_non_responder_2026 = leap_invites_2026 %>% 
  filter(!is.na(`Email sent`) & `Email sent` != "No email") %>% # Filter for those that have been invited
  filter(!STUDY_ID %in% leap_booked_2026$STUDY_ID) # Filter for those that haven't been booked

# Create list of not contacted individuals 
leap_not_contacted_2026 = leap_invites_2026 %>% 
  filter(is.na(`Email sent`) | `Email sent` == "No email") 


# ----- 2026 LEAPclusion Data -----

# Read updated LEAPclusion data 
leapclusion_2026 = read_xlsx("Data_2026/Leapclusion Final Recruitment 2025.xlsx", sheet = "TwinclusionALL") %>% 
  rename(STUDY_ID = Study_No) 

# Create list of non-responsive LEAPclusion individuals 
leapclusion_non_responder_2026 = leapclusion_2026 %>% 
  filter(!is.na(`Contacted?`) & `Booked?` == 'No') 

# Create list of not contacted LEAPclusion individuals 
leapclusion_not_contacted_2026 = leapclusion_2026 %>% 
  filter(is.na(`Contacted?`))

# Create list of booked LEAPclusion individuals 
leapclusion_booked_2026 = leapclusion_2026 %>% 
  filter(`Booked?` == "Yes")

# Read lab LEAPclusion data 
leapclusion_lab_2026 = read_xlsx("Data_2026/LEAP Twinclusion Samples_as of 09Dec25.xlsx")


# Create list of individuals in molly list and not lab list


# ----- Attrition Exploration -----

# Create variable to outline attrition group
working_data_2026 = leap_eligibility_all %>% 
  mutate(attrition_group = case_when(
    
    # LEAP Final Recruitment
    STUDY_ID %in% leap_booked_completed_2026$STUDY_ID ~ "Recruited - LEAP booked and completed",
    
    # LEAPclusion Final Recruitment
    STUDY_ID %in% leapclusion_booked_2026$STUDY_ID ~ "Recruited - LEAPclusion booked",
    
    # LEAP attrition
    STUDY_ID %in% leap_booked_cancelled_2026$STUDY_ID ~ "Attritted - LEAP booked and cancelled",
    !STUDY_ID %in% leap_eligble_2026$STUDY_ID ~ "Attritted - Not LEAP eligible",
    STUDY_ID %in% leap_not_contacted_2026$STUDY_ID ~ "Attritted - Not contacted",
    STUDY_ID %in% leap_non_responder_2026$STUDY_ID ~ "Attritted - Non-responders/refused",
    
    # LEAPclusion attrition
    STUDY_ID %in% leapclusion_not_contacted_2026$STUDY_ID ~ "Attritted - LEAPclusion Not contacted",
    STUDY_ID %in% leapclusion_non_responder_2026$STUDY_ID ~ "Attritted - LEAPclusion Non-responders/refused",
    
    # Status attrition
    IsDeceased == T ~ "Attritted - Deceased",
    NotParticipating == T ~ "Attritted - Not Participating",
    
    # Other
    T ~ "Not in any category above"
  ))

# Count attrition and recruitment totals 
working_data_2026 %>% 
  count(attrition_group)

# Create working data with all groups as binary variables
working_data_2026_groups = working_data_2026 %>% 
  mutate(Deceased = ifelse(IsDeceased == T, T, ""),  
         Not_Participating = ifelse(NotParticipating == T, T, "")) %>% 
  select(STUDY_ID, Deceased, Not_Participating) %>% 
  mutate(
    
    # LEAP Final Recruitment
    Visit_Completed = ifelse(STUDY_ID %in% leap_booked_completed_2026$STUDY_ID, T, ""),
    
    # LEAPclusion Final Recruitment
    LEAPclusion_Booked = ifelse(STUDY_ID %in% leapclusion_booked_2026$STUDY_ID, T, ""),  
    
    # LEAP attrition
    Visit_Cancelled = ifelse(STUDY_ID %in% leap_booked_cancelled_2026$STUDY_ID, T, ""),  
    Not_Eligible = ifelse(!STUDY_ID %in% leap_eligble_2026$STUDY_ID, T, ""),  
    Not_Contacted = ifelse(STUDY_ID %in% leap_not_contacted_2026$STUDY_ID, T, ""),  
    Non_Responder_Refused = ifelse(STUDY_ID %in% leap_non_responder_2026$STUDY_ID, T, ""),  
    
    # LEAPclusion attrition
    LEAPclusion_Not_Contacted = ifelse(STUDY_ID %in% leapclusion_not_contacted_2026$STUDY_ID, T, ""),  
    LEAPclusion_Non_Responder_Refused = ifelse(STUDY_ID %in% leapclusion_non_responder_2026$STUDY_ID, T, ""),  

    # Unaccounted cohort
    #Unaccounted = ifelse(STUDY_ID %in% unnacounted$STUDY_ID, T, "")
    )

# Create logic table
working_data_2026_groups_logic = working_data_2026_groups %>% 
  select(-STUDY_ID) %>% 
  count(across(everything())) %>% 
  arrange(desc(n)) %>% 
  select(n, everything())

# Create variable for total groups flagged as TRUE
working_data_2026_groups_logic = working_data_2026_groups_logic %>% 
  mutate(true_total = rowSums(across(everything()) == "TRUE")) 


# ----- Initially Unaccounted Individuals ----- 

# Read unaccounted cohort
unnacounted = read.csv("Outputs/LEAP_Recruitment_Unaccounted_Individuals.csv")

# Count attrition and recruitment totals within unaccounted cohort
working_data_2026 %>% 
  filter(STUDY_ID %in% unnacounted$STUDY_ID) %>% 
  count(attrition_group)

# Examine not contacted data for unaccounted cohort
leap_invites_2026 %>% 
  mutate(not_contacted = ifelse(is.na(`Email sent`) | `Email sent` == "No email", 1, 0)) %>% 
  filter(STUDY_ID %in% unnacounted$STUDY_ID) %>% 
  filter(STUDY_ID %in% working_data_2026$STUDY_ID[working_data_2026$attrition_group == "Not contacted"]) %>% 
  View()


# ----- LEAP Cancellations after Visit Completion ----- 

# Create dataframe to examine individuals who cancelled visits after completion
leap_booked_2026 %>%
  group_by(STUDY_ID) %>% 
  filter(n() > 1) %>%
  group_by(STUDY_ID, VisitStatusID) %>%
  summarise(first_date = min(VisitDate), .groups = "drop") %>%
  pivot_wider(names_from = VisitStatusID, values_from = first_date) %>%
  mutate(completed_earlier = as.integer(Completed < Cancelled),
         cancelled_earlier = as.integer(Cancelled < Completed)) %>% 
  filter(completed_earlier == 1)

