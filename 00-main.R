## libraries
library(vitae)
library(rcrossref)
library(anytime)
library(googlesheets4)
library(tidyverse)
library(extrafont)
loadfonts()

## load data on Google
dta <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1FM6LKW7jj8tl14Al08UiJAYP3Asr7Yw1IVUmoxGOxd4")
dta_lang <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1FM6LKW7jj8tl14Al08UiJAYP3Asr7Yw1IVUmoxGOxd4", 
  sheet = 2)
dta_lang_2 <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1FM6LKW7jj8tl14Al08UiJAYP3Asr7Yw1IVUmoxGOxd4", 
  sheet = 3)

## orcid-ID
orcid_id <- "0000-0002-4242-9062"

## orcid-authentication
rorcid::orcid_auth()

## education
orcid_edu <- 
  do.call("rbind",
          rorcid::orcid_educations(orcid_id)$`0000-0002-4242-9062`$`affiliation-group`$summaries
)

orcid_edu <- orcid_edu %>%
  detailed_entries(
    what = `education-summary.role-title`,
    when = glue::glue("{`education-summary.start-date.year.value`} - {`education-summary.end-date.year.value`}"),
    with = `education-summary.organization.name`,
    where = `education-summary.organization.address.city`
  )

# additional courses
add_edu <- dta %>%
  filter(id == "EDU") %>%
  detailed_entries(
    what = what,
    when = when,
    with = with,
    where = where
  )

## professional experience
pro <- dta %>%
  filter(id == "WORK") %>%
  detailed_entries(
    what = what,
    when = when,
    with = with,
    where = where
  )


## projects
projects <- dta %>%
  filter(id == "PROJECT") %>%
  detailed_entries(
    with = what,
    when = when,
    what = with,
    where = where,
    why = why
  )

## teaching
teach <- dta %>%
  filter(id == "TEACH") %>%
  brief_entries(
    what = what,
    when = when,
    with = with
  )

## presentations
presentation <- dta %>%
  filter(id == "PRESENTATION") %>%
  brief_entries(
    what = what,
    when = when,
    with = with
  )

## memberships
member <- dta %>%
  filter(id == "MEMBER") %>%
  brief_entries(
    what = what,
    when = when,
    with = with
  )

## International stays
int_stays <- dta %>%
  filter(id == "INT") %>%
  brief_entries(
    what = what,
    when = when,
    with = with
  )

## professional skills
dta_lang <- dta_lang %>%
  as_tibble() %>%
  unnest(cols = skill)

stat_fig <-
  ggplot() +
  geom_bar(data = tibble(x = dta_lang$name, y = 5),
           aes(x = x, y = y), fill = "grey",
           stat = "identity", width = 0.6) +
  geom_bar(data = dta_lang,
           aes(x = name, y = skill, fill = id),
           stat = "identity", width = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("#DC3522", "#DC3522", "#DC3522")) +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 15, family = "Roboto Medium"),
        axis.text.x = element_blank(),
        legend.position = "none")


## publications
# orcid_pub <- rorcid::works(orcid_id) %>%
#   as_tibble() %>%
#   janitor::clean_names() %>%
#   dplyr::mutate(created_date_value = anytime::anydate(created_date_value/1000))
# 
# bibliography_entries()

## knit
rmarkdown::render(input = "cv_template/cv_template.Rmd")

