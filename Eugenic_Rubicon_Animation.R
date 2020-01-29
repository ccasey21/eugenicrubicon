## Eugenic Rubicon

## Name: Caroline Casey
## Date: July 16, 2019

# Initial settings --------------------------------------------------------

library(tidyverse)
library(reshape2)
library(lubridate)
library(CGPfunctions)
library(plotly)
library(ggvis)
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

# Diagnosis Share by Year -------------------------------------------------

## Filtered to 1955 because there were only a couple of years past that and it distorted the graph
YearSummary <- df_1 %>% 
  group_by(ApproveYear, Reason) %>% 
  summarize(Diagnosis = n()) %>% 
  ungroup() %>% 
  filter(!is.na(ApproveYear)) %>% 
  filter(ApproveYear >= 1919 & ApproveYear <= 1955) %>% 
  group_by(ApproveYear) %>% 
  mutate(DiagnosisPerYear = sum(Diagnosis)) %>% 
  mutate(DiagnosisShare = round(100 * (Diagnosis / DiagnosisPerYear), 2)) %>% 
  ungroup()

ggplot(YearSummary) +
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
       title = "Doccumented Reason for Sterilization by Year",
       subtitle = "Sterilizations in California...")

ggsave("figures/Pct_Reason_By_Year.png", scale = 0.8, width = 10, height = 7.5)
ggsave("figures/Most_Common_1930s.pdf", scale = 0.8, width = 10, height = 7.5)

# Diagnosis Number by Year ------------------------------------------------

YearSummary2 <- df_1 %>% 
  group_by(ApproveYear, Reason) %>% 
  summarize(Diagnosis = n()) %>% 
  ungroup() %>% 
  filter(!is.na(ApproveYear)) %>% 
  filter(ApproveYear >= 1919 & ApproveYear <= 1955)

g <- ggplot(YearSummary2) +
  geom_bar(aes(x = ApproveYear, 
               y = Diagnosis, 
               fill = Reason),
           color = "white",
           stat = "identity") +
  scale_color_brewer(type='qual', palette = 2) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Number",
       title = "Doccumented Reason for Sterilization by Year and Number of Sterilizations",
       subtitle = "Sterilizations in California data from...")

ggplotly(g)

ggsave("figures/Number_Reason_By_Year.png", scale = 0.8, width = 10, height = 7.5)
ggsave("figures/Most_Common_1930s.pdf", scale = 0.8, width = 10, height = 7.5)

# ggvis -------------------------------------------------------------------

YearSummary2 <- df_1 %>% 
  group_by(ApproveYear, Reason) %>% 
  summarize(Diagnosis = n()) %>% 
  ungroup() %>% 
  filter(!is.na(ApproveYear)) %>% 
  filter(ApproveYear >= 1919 & ApproveYear <= 1955)

g <- ggplot(YearSummary2) +
  geom_bar(aes(x = ApproveYear, 
               y = Diagnosis, 
               fill = Reason),
           color = "white",
           stat = "identity") +
  scale_color_brewer(type='qual', palette = 2) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Number",
       title = "Doccumented Reason for Sterilization by Year and Number of Sterilizations",
       subtitle = "Sterilizations in California data from...")

Reasons <- unique(YearSummary2$Reason)

select <- input_select(choices = Reason,
                       label = "Select:",
                       selected = "Feebleminded",
                       map = as.name)

g2 <- YearSummary2 %>% 
  ggvis(y = select,
        x = ~factor(ApproveYear),
        size := 500) %>% 
  layer_points() %>% 
  scale_numeric("y", reverse = TRUE, domain = c(1, 16)) %>% 
  add_axis("y", title = "US News Ranking", values = seq(1, 16, by = 1)) %>% 
  add_axis("x", title = "Year")

ggplotly(g2)

ggsave("figures/Number_Reason_By_Year.png", scale = 0.8, width = 10, height = 7.5)
ggsave("figures/Most_Common_1930s.pdf", scale = 0.8, width = 10, height = 7.5)
