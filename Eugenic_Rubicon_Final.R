## Eugenic Rubicon

## Name: Caroline Casey
## Date: Summer 2019

# Initial settings --------------------------------------------------------

library(tidyverse)
library(reshape2)
library(lubridate)
library(CGPfunctions)
options(stringsAsFactors = FALSE)

# Load Data ---------------------------------------------------------------

filename <- "data/California.csv"
data <- read.csv(filename)

# Data Wrangling ----------------------------------------------------------

## Altering initial variables to shorten column names and changing responses to yes/no from checked/unchecked
df <- data %>% 
  mutate(Alcoholic = ifelse(Diagnosis..check.all.that.apply....choice.Alcoholic. == "Checked", 1, 0),
         Blind = ifelse(Diagnosis..check.all.that.apply....choice.Blind. == "Checked", 1, 0),
         Catatonic = ifelse(Diagnosis..check.all.that.apply....choice.Catatonic. == "Checked", 1, 0),
         Dementia = ifelse(Diagnosis..check.all.that.apply....choice.Dementia.Praecox. == "Checked", 1, 0),
         Drugs = ifelse(Diagnosis..check.all.that.apply....choice.Drug.Addict. == "Checked", 1, 0),
         Epileptic = ifelse(Diagnosis..check.all.that.apply....choice.Epileptic. == "Checked", 1, 0), 
         Feebleminded = ifelse(Diagnosis..check.all.that.apply....choice.Feebleminded...Mental.Deficiency. == "Checked", 1, 0),
         Paralysis = ifelse(Diagnosis..check.all.that.apply....choice.General.Paralysis. == "Checked", 1, 0),
         Hebephrenic = ifelse(Diagnosis..check.all.that.apply....choice.Hebephrenic. == "Checked", 1, 0),
         Insane = ifelse(Diagnosis..check.all.that.apply....choice.Insane. == "Checked", 1, 0),
         Lunatic = ifelse(Diagnosis..check.all.that.apply....choice.Lunatic. == "Checked", 1, 0),
         ManicDepressive = ifelse(Diagnosis..check.all.that.apply....choice.Manic.Depressive. == "Checked", 1, 0),
         Mute = ifelse(Diagnosis..check.all.that.apply....choice.Mute.or.other.speech.pathology. == "Checked", 1, 0),
         Narcotic = ifelse(Diagnosis..check.all.that.apply....choice.Narcotic. == "Checked", 1, 0),
         Nervous = ifelse(Diagnosis..check.all.that.apply....choice.Nervous...anxious. == "Checked", 1, 0),
         None = ifelse(Diagnosis..check.all.that.apply....choice.None.Stated. == "Checked", 1, 0),
         Other = ifelse(Diagnosis..check.all.that.apply....choice.Other. == "Checked", 1, 0),
         OtherSTD = ifelse(Diagnosis..check.all.that.apply....choice.Other.sexually.transmitted.disease. == "Checked", 1, 0), 
         Paranoia = ifelse(Diagnosis..check.all.that.apply....choice.Paranoia.or.paranoid.condition. == "Checked", 1, 0), 
         PhysicallyNegative = ifelse(Diagnosis..check.all.that.apply....choice.Physically.negative. == "Checked", 1, 0), 
         PhysicallyNormal = ifelse(Diagnosis..check.all.that.apply....choice.Physically.normal. == "Checked", 1, 0),
         Psychosis = ifelse(Diagnosis..check.all.that.apply....choice.Psychosis. == "Checked", 1, 0),
         Schizophrenic = ifelse(Diagnosis..check.all.that.apply....choice.Schizophrenic. == "Checked", 1, 0), 
         SexualPsychopath = ifelse(Diagnosis..check.all.that.apply....choice.Sexual.psychopath. == "Checked", 1, 0),
         Syphilitic = ifelse(Diagnosis..check.all.that.apply....choice.Syphilitic. == "Checked", 1, 0),
         Suicidal = ifelse(Diagnosis..check.all.that.apply....choice.Suicidal. == "Checked", 1, 0),
         Voluntary = ifelse(Diagnosis..check.all.that.apply....choice.Voluntary. == "Checked", 1, 0))

## Gathering data from long form to short form for institution and who consented
## Shortening who consented responses and changing to factor variable
## Changing dates to year-month-date form
df <- df %>% 
  gather(key = Institution, value = Superintendent, 4:15, na.rm = TRUE) %>% 
  gather(key = WhoConsented, value = Consent_YN, 33:43, na.rm = TRUE) %>% 
  filter(Superintendent != "") %>% 
  filter(Consent_YN != "Unchecked") %>% 
  mutate(RequestDate = ymd(Date.sterilization.request.made.by.superintendent)) %>% 
  mutate(RequestYear = year(RequestDate)) %>% 
  mutate(ApproveDate = ymd(Enter.Date.Approved)) %>% 
  mutate(ApproveYear = year(ApproveDate)) %>% 
  mutate(WhoConsented = factor(WhoConsented,
                               levels = c("Who.consented....choice.Mother.", "Who.consented....choice.Father.",
                                          "Who.consented....choice.Brother.", "Who.consented....choice.Sister.",
                                          "Who.consented....choice.Spouse.", "Who.consented....choice.Extended.family.member.",
                                          "Who.consented....choice.Medical.Staff.", "Who.consented....choice.Parole.Officer.",
                                          "Who.consented....choice.Other.", "Who.consented....choice.Not.stated."),
                               labels = c("Mother", "Father", "Brother", "Sister", "Spouse", "Extended_family_member",
                                          "Medical_staff", "Parole_officer", "Other", "Not_stated"))) 

df_1 <- df %>%
  gather(key = Reason, value = y_n, 35:61) %>% 
  filter(y_n != 0) %>% 
  select(Record.ID, Name.of.institution, Acting.Assistant.Superintendent.,
         Consent.given.., Institution, Superintendent, WhoConsented,
         RequestDate, RequestYear, ApproveDate, ApproveYear,
         Reason)

df_1 <- df_1 %>% 
  mutate(Reason = ifelse(Reason == "PhysicallyNormal", "Physically Normal", Reason),
         Reason = ifelse(Reason == "SexualPsychopath", "Sexual Psychopath", Reason),
         Reason = ifelse(Reason == "ManicDepressive", "Manic Depressive", Reason),
         Reason = ifelse(Reason == "PhysicallyNegative", "Physically Negative", Reason),
         Reason = ifelse(Reason == "OtherSTD", "Other STD", Reason))

# Pct_Reason_By_Year Visualization ----------------------------------------

## Filtered from 1919 to 1955
YearSummaryShare <- df_1 %>% 
  group_by(ApproveYear, Reason) %>% 
  summarize(Diagnosis = n()) %>% 
  ungroup() %>% 
  filter(!is.na(ApproveYear)) %>% 
  filter(ApproveYear >= 1919 & ApproveYear <= 1952) %>% 
  group_by(ApproveYear) %>% 
  mutate(DiagnosisPerYear = sum(Diagnosis)) %>% 
  mutate(DiagnosisShare = round(100 * (Diagnosis / DiagnosisPerYear), 2)) %>% 
  ungroup()

(YearSummaryShare$Reason)

ggplot(YearSummaryShare) +
  geom_bar(aes(x = ApproveYear, 
               y = DiagnosisShare, 
               fill = Reason),
           color = "white",
           stat = "identity") +
  scale_color_brewer(type='qual', palette = 2) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Percent", 
       title = "Doccumented Reason for Sterilization by Year", ## Change text in "" to change title
       subtitle = "Sterilizations in California from 1919 to 1952")

ggsave("figures/Pct_Reason_By_Year.png", scale = 0.8, width = 14.5, height = 7.5)
ggsave("figures/Pct_Reason_By_Year.pdf", scale = 0.8, width = 14.5, height = 7.5)

# Number_Reason_By_Year Visualization -------------------------------------

## Filtered from 1919 to 1955
YearSummaryNumber <- df_1 %>% 
  group_by(ApproveYear, Reason) %>% 
  summarize(Diagnosis = n()) %>% 
  ungroup() %>% 
  filter(!is.na(ApproveYear)) %>% 
  filter(ApproveYear >= 1919 & ApproveYear <= 1952)

ggplot(YearSummaryNumber) +
  geom_bar(aes(x = ApproveYear, 
               y = Diagnosis, 
               fill = Reason),
           color = "white",
           stat = "identity") +
  scale_color_brewer(type='qual', palette = 2) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Number",
       title = "Doccumented Reason for Sterilization by Year and Number of Sterilizations",
       ## Change text in "" to change title
       subtitle = "Sterilizations in California data from 1919 to 1952")

ggsave("figures/Number_Reason_By_Year.png", scale = 0.8, width = 15, height = 7.5)
ggsave("figures/Number_Reason_By_Year.pdf", scale = 0.8, width = 15, height = 7.5)

# 1935 Analysis -----------------------------------------------------------

## This is background for my 1930s visualizations

df_1935 <- df_1 %>% 
  select(ApproveYear, Reason, WhoConsented, Name.of.institution, Superintendent, Record.ID) %>% 
  filter(ApproveYear == 1935)

df_1934 <- df_1 %>% 
  select(ApproveYear, Reason, WhoConsented, Name.of.institution, Superintendent, Record.ID) %>% 
  filter(ApproveYear == 1934)

table(df_1934$Name.of.institution)
table(df_1934$Superintendent)
table(df_1935$Superintendent)
table(df_1934$Reason[df_1934$Superintendent == "G. M. Webster (1926-1946)"])
table(df_1935$Reason[df_1935$Superintendent == "G. M. Webster (1926-1946)"])
table(df_1935$Reason)
table(df_1934$Reason)
table(df_1934$Reason[df_1934$Superintendent == "F. O. Butler (1922-1949)"])
table(df_1935$Reason[df_1935$Superintendent == "F. O. Butler (1922-1949)"])


table(df_1$ApproveYear)

ReasonSummary <- df_1 %>% 
  group_by(Reason) %>% 
  summarize(Count = n()) %>% 
  ungroup()

## Diagnoses that were not very common in the 1930s
uncommon_diagnosis <- c("Narcotic", "Lunatic", "Blind", "Drugs", "Schizophrenic",
                        "Voluntary", "Nervous", "Paralysis", "Mute", "Physically Negative",
                        "Suicidal", "Insane", "None", "Sexual Psychopath", "Physically Normal",
                        "Alcoholic")

## Diagnoses that were very common in the 1930s
mostcommon_diagnosis <- c("Feebleminded", "Dementia", "Hebephrenic")

# Most_Common_1930s Visualization -----------------------------------------

df_line <- df_1 %>% 
  filter(ApproveYear >= 1930 & ApproveYear <= 1940) %>% 
  filter(Reason %in% mostcommon_diagnosis) %>% 
  group_by(ApproveYear, Reason) %>% 
  summarise(ReasonSummary = n()) %>% 
  ungroup() %>% 
  mutate(ApproveYear = factor(ApproveYear, ordered = TRUE)) %>% 
  filter(ApproveYear %in% c(1933, 1934, 1935, 1936, 1937))

newggslopegraph(df_line, ApproveYear, ReasonSummary, Reason,
                LineColor = c("Feebleminded" = "#5ab4ac",
                              "Dementia" = "#d8b365",
                              "Hebephrenic" = "#c6609d"),
                Title = "Feebleminded, Dementia, and Hebephrenic Diagnoses 1933-1937",
                SubTitle = "Eugenic Rubicon Project",
                Caption = "")

ggsave("figures/Most_Common_1930s.png", scale = 0.8, width = 10, height = 7.5)
ggsave("figures/Most_Common_1930s.pdf", scale = 0.8, width = 10, height = 7.5)

# Most_Common_and_Others_1930s_Gray Visualization -------------------------

df_line2 <- df_1 %>% 
  filter(ApproveYear >= 1930 & ApproveYear <= 1940) %>% 
  mutate(Reason = ifelse(Reason %in% uncommon_diagnosis, "Other*", Reason)) %>% 
  group_by(ApproveYear, Reason) %>% 
  summarise(ReasonSummary = n()) %>% 
  ungroup() %>% 
  mutate(ApproveYear = factor(ApproveYear, ordered = TRUE)) %>% 
  filter(ApproveYear %in% c(1933, 1934, 1935, 1936, 1937))

newggslopegraph(df_line2, ApproveYear, ReasonSummary, Reason,
                WiderLabels = TRUE,
                LineColor = c("Feebleminded" = "#5ab4ac",
                              "Dementia" = "#d8b365",
                              "Hebephrenic" = "#c6609d",
                              "Other*" = "gray",
                              "Other" = "gray",
                              "Paranoia" = "gray",
                              "Epileptic" = "gray",
                              "Psychosis" = "gray",
                              "Manic Depressive" = "gray",
                              "Catatonic" = "gray",
                              "Syphilitic" = "gray",
                              "Other STD" = "gray"),
                Title = "Diagnoses 1933 to 1937",
                SubTitle = "Eugenic Rubicon Project",
                Caption = "")

ggsave("figures/Most_Common_and_Others_1930s_Gray.png", scale = 0.8, width = 10, height = 7.5)
ggsave("figures/Most_Common_and_Others_1930s_Gray.pdf", scale = 0.8, width = 10, height = 7.5)

# Most_Common_and_Others_1930s_Color Visualization ------------------------

df_line3 <- df_1 %>% 
  filter(ApproveYear >= 1930 & ApproveYear <= 1940) %>% 
  mutate(Reason = ifelse(Reason %in% uncommon_diagnosis, "Other*", Reason)) %>% 
  group_by(ApproveYear, Reason) %>% 
  summarise(ReasonSummary = n()) %>% 
  ungroup() %>% 
  mutate(ApproveYear = factor(ApproveYear, ordered = TRUE)) %>% 
  filter(ApproveYear %in% c(1933, 1934, 1935, 1936, 1937))

newggslopegraph(df_line3, ApproveYear, ReasonSummary, Reason,
                WiderLabels = TRUE, 
                Title = "Diagnosis Frequency 1933-1937",
                SubTitle = "Eugenic Rubicon Project",
                Caption = "")

ggsave("figures/Most_Common_and_Others_1930s_Color.png", scale = 0.8, width = 10, height = 7.5)
ggsave("figures/Most_Common_and_Others_1930s_Color.pdf", scale = 0.8, width = 10, height = 7.5)

# 1934-1935 Diagnoses -----------------------------------------------------

df_line4 <- df_1 %>% 
  filter(ApproveYear >= 1930 & ApproveYear <= 1940) %>% 
  mutate(Reason = ifelse(Reason %in% uncommon_diagnosis, "Other*", Reason)) %>% 
  group_by(ApproveYear, Reason) %>% 
  summarise(ReasonSummary = n()) %>% 
  ungroup() %>% 
  mutate(ApproveYear = factor(ApproveYear, ordered = TRUE)) %>% 
  filter(ApproveYear %in% c(1934, 1935))

newggslopegraph(df_line4, ApproveYear, ReasonSummary, Reason,
                Title = "Diagnoses 1934 to 1935",
                SubTitle = "Eugenic Rubicon Project",
                Caption = "",
                LineColor = c("Feebleminded" = "#5ab4ac",
                              "Dementia" = "#d8b365",
                              "Hebephrenic" = "#c6609d",
                              "Other*" = "gray",
                              "Other" = "gray",
                              "Paranoia" = "gray",
                              "Epileptic" = "gray",
                              "Psychosis" = "gray",
                              "Manic Depressive" = "gray",
                              "Catatonic" = "gray",
                              "Syphilitic" = "gray",
                              "Dementia" = "gray",
                              "Other STD" = "gray"))

ggsave("figures/Diagnosis_1934_1935.png", scale = 0.8, width = 10, height = 7.5)
ggsave("figures/Diagnosis_1934_1935.pdf", scale = 0.8, width = 10, height = 7.5)



