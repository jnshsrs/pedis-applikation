library(ggthemes)
library(tidyverse)
library(shiny)
library(broom)
library(RSQLite)
library(shinyjs)
library(lubridate)
library(shinyBS)
library(shinydashboard)
library(digest)

appCSS <-  ".mandatory_star { color: red; size: large; } .btn.disabled { pointer-events: auto;}"

# CSS configurations
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Shiny app with 3 fields that the user can submit data for
ui <- fluidPage(

  # java settings
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),

    # application title
    titlePanel("Das Lernende Gesundheitssystem in der Region Osnabrück-Emsland - Teilprojekt \"Wunde\""),
    titlePanel("PROTOTYP"),
    navbarPage("Menü",
               navbarMenu("Dokumentation",
                          tabPanel("Neuer Patient",
                                   # Sidebar with a slider input for number of bins
                                   fluidPage(
                                     div(
                                       id = "form",
                                       br(),
                                       fluidRow(
                                         column(width = 3, textInput("first_name", label = "Vorname:", value = "", placeholder = "Vorname")),
                                         column(width = 3, textInput("name", label = labelMandatory("Nachname:"), value = "", placeholder = "Nachname"))
                                       ),
                                       fluidRow(
                                         column(width = 3,
                                                dateInput(
                                                  inputId = "birth_date",
                                                  label = labelMandatory("Geburtsdatum"),
                                                  startview = "decade",
                                                  language = "de",
                                                  width = '40%',
                                                  value = Sys.Date()
                                                )
                                         ),
                                         column(width = 3,
                                                radioButtons(
                                                  inputId = "gender",
                                                  label = "Geschlecht",
                                                  choices = c("männlich", "weiblich"),
                                                  inline = TRUE,
                                                  selected = NULL
                                                )
                                         )
                                       ),
                                       hr(),
                                       h3("PEDIS"),
                                       div(
                                         selectInput(inputId = "perfusion",
                                                     label = labelMandatory("Durchblutung (Perfusion)"),
                                                     choices = c(
                                                       "Stufe 1: Keine nachweisbare pAVK" = 1,
                                                       "Stufe 2: aVK, aber keine Extremitäten Ischämie" = 2,
                                                       "Stufe 3: Kritische Extremitätenischämie" = 3)
                                         ),
                                         sliderInput("extent",
                                                     label = labelMandatory("Ulcusausmaß in cm² (Extend)"),
                                                     min = 1,
                                                     max = 60,
                                                     value = 10),
                                         selectInput(inputId = "depth",
                                                     label = labelMandatory("Ulcustiefe (Depth)"),
                                                     choices = c(
                                                       "Stufe 1: Komplettes Ulcus bis zur Dermis" = 1,
                                                       "Stufe 2: Infiltration bis Subcutis, Faszie, Muskulatur, Sehnen" = 2,
                                                       "Stufe 3: Infiltration bis in Knochen und Gelenke" = 3) ),
                                         selectInput(inputId = "infection",
                                                     label = labelMandatory("Infektion (Infection)"),
                                                     choices = c(
                                                       "Stufe 1: Keine Entzündungszeichen" = 1,
                                                       "Stufe 2: Infektion von Haut oder subcutanem Gewebe. Mindestens zwei der folgenden Symptome" = 2,
                                                       "Stufe 3: Erythem > 2cm und eines der folgenden Symptome oder Infektion über die Subcutis" = 3,
                                                       "Stufe 4: Zwei oder mehr der folgenden Zeichen einer systematischen Inflammation" = 4)),
                                         conditionalPanel(
                                           condition = "input.infection == 2 || input.infection == 3",
                                           checkboxGroupInput(inputId = "infection_stage_2_3",
                                                              label = "Symptome Stufe 2 und 3:",
                                                              choices = c(
                                                                "Örtliche Schwellung oder Induration",
                                                                "Periulceröses Erythem 0,5 - 2 cm",
                                                                "Empfindlichkeit von Schmerzen",
                                                                "Überwärmung",
                                                                "Eitriges Sekret"
                                                              ))
                                         ),
                                         conditionalPanel(
                                           condition = "input.infection == 4",
                                           checkboxGroupInput(inputId = "infection_stage_4",
                                                              label = "Symptome Stufe 4:",
                                                              choices = c(
                                                                "Temperatur < 36°C oder > 38°C",
                                                                "Herzfrequenz > 90 bpm",
                                                                "Atemfrequenz > 20/ min",
                                                                "PaCO2 < 32mmHg",
                                                                "Über 10% unreife Granulozyten"
                                                              ))
                                         ),
                                         selectInput(inputId = "sensation",
                                                     label = labelMandatory("Sensibilität (Sensation)"),
                                                     choices = c(
                                                       "Stufe 1: Keine nachweisbare Neuropathie " = 1,
                                                       "Stufe 2: Monofilament an 2 von 3 Auflagepunkten nicht spürbar oder fehlendes Vibrationsempfinden an der Großzehe oder Biothesiometerschwelle über 25V" = 2)),
                                         hr(),
                                         fluidRow(
                                           column(width = 3,
                                                  textInput(inputId = "crp",
                                                            label = "CRP-Wert bei der Aufnahme")
                                           ),
                                           column(width = 3,
                                                  selectizeInput(inputId = "dialyse",
                                                                 label = "Dialyse",
                                                                 choices = c(
                                                                   "Dialysepflichtig",
                                                                   "Nicht dialysepflichtig"
                                                                 ),
                                                                 options = list(
                                                                   placeholder = 'Bitte auswählen',
                                                                   onInitialize = I('function() { this.setValue(""); }'),
                                                                   reset = I('function()  { this.setValue(""); }')
                                                                 )
                                                  )
                                           )
                                         ),

                                         hr(),
                                         fluidRow(
                                           column(width = 3, selectInput(inputId = "lokalisation", label = "Lokalisation", choices = c("Fuss", "Vorfuss", "Rückfuss"))),
                                           column(width = 3, textInput(inputId = "initial_therapy", label = labelMandatory("Initialtherapie"), placeholder = "Beschreiben Sie die Initialtherapie"))
                                         ),

                                         dateInput(inputId = "admission_date", label = "Erhebungsdatum", startview = "month", language = "de", width = '40%', value = Sys.Date())
                                       ),
                                       wellPanel(helpText("Eingabefelder mit rotem Sternchen müssen ausgefüllt werden, um den Patienten anzulegen")),
                                       bsButton(inputId = "submit", label  = "Patienten anlegen", style = "success"),
                                       actionButton(inputId = "reset_input", label = "Reset"),
                                       hr(),
                                       br()
                                     )
                                   )
                          ),

                          # Patientendaten ändern
                          tabPanel(title = "Patientendaten ändern",
                                   sidebarLayout(
                                     sidebarPanel(
                                       div(
                                         id = "sidebar_patientensuche",
                                         h1("Suche"),
                                         hr(),
                                         checkboxInput(inputId = "search_by_birthdate", label = "Nach Geburtsdatum suchen"),
                                         conditionalPanel(
                                           condition = "input.search_by_birthdate",
                                           sliderInput("date_range",
                                                       "Eingrenzen des Geburtsdatum:",
                                                       min = as.Date("1920-01-01"),
                                                       max = Sys.Date(),
                                                       value = c(as.Date("2010-01-01"), Sys.Date()),
                                                       step = lubridate::years(1)
                                           )
                                         ),
                                         fluidRow(
                                           column(width = 6, textInput("patientensuche_first_name", label = "Vorname:", value = "")),
                                           column(width = 6, textInput("patientensuche_name", label = "Nachname:", value = ""))
                                         ),
                                         uiOutput("patients_dropdown"),
                                         fluidRow(
                                           column(width = 6, actionButton(inputId = "sidebar_patientensuche_reset", label = "Suche zurücksetzten")),
                                           column(width = 6, actionButton(inputId = "patient_selected_submit", label = "Patienten auswählen"))
                                         )
                                       )
                                     ),
                                     mainPanel(
                                       tabsetPanel(
                                         tabPanel(title = "Endpunkt festlegen",
                                                  column(width = 10,
                                                         conditionalPanel("input.patient_selected_submit > 0",
                                                                          wellPanel(
                                                                            fluidRow(
                                                                              valueBoxOutput("valuebox_patient_id_output"),
                                                                              valueBoxOutput("valuebox_patient_name_output"),
                                                                              valueBoxOutput("valuebox_patient_geburtstag_output")
                                                                            )
                                                                          ),
                                                                          wellPanel(
                                                                            fluidRow(
                                                                              valueBoxOutput("pedis_wert_dashboard"),
                                                                              valueBoxOutput("pedis_quantile_dashboard")
                                                                            )
                                                                          ),
                                                                          div(
                                                                            id = "endpoint_ui",
                                                                            wellPanel(
                                                                              h3("Endpunkt festlegen"),
                                                                              fluidRow(
                                                                                valueBoxOutput("valuebox_patient_admission_output"),
                                                                                valueBoxOutput("valuebox_patient_endpoint_output"),
                                                                                valueBoxOutput("valuebox_date_today_output")
                                                                              ),
                                                                              hr(),
                                                                              fluidRow(
                                                                                column(width = 6,
                                                                                       selectizeInput(
                                                                                         'outcome_endpoint_amputation',
                                                                                         'Amputation zum Endpunkt',
                                                                                         choices = c(
                                                                                           "Amputation",
                                                                                           "Keine Amputation"
                                                                                         ),
                                                                                         options = list(
                                                                                           placeholder = 'Bitte den Endpunkt auswählen',
                                                                                           onInitialize = I('function() { this.setValue(""); }')
                                                                                         )
                                                                                       )
                                                                                )
                                                                              ),
                                                                              div(
                                                                                id = "free_text_amputation_endpoint",
                                                                                fluidRow(
                                                                                  column(width = 6,
                                                                                         conditionalPanel(condition = "input.outcome_endpoint_amputation == 'Amputation'",
                                                                                                          selectizeInput(inputId = "amputation_height",
                                                                                                                         label = "Amputationshöhe",
                                                                                                                         choices = c(
                                                                                                                           "Zehen",
                                                                                                                           "MFK",
                                                                                                                           "Lisfranc-Amputation",
                                                                                                                           "Chopart Amputation",
                                                                                                                           "Unterschenkel-Amputation",
                                                                                                                           "Oberschenkel-Amputation",
                                                                                                                           "Andere"
                                                                                                                         ),
                                                                                                                         options = list(
                                                                                                                           placeholder = 'Bitte den Endpunkt auswählen',
                                                                                                                           onInitialize = I('function() { this.setValue(""); }'),
                                                                                                                           reset = I('function()  { this.setValue(""); }')
                                                                                                                         )
                                                                                                          )
                                                                                         ),
                                                                                         conditionalPanel(condition  = "input.amputation_height == 'Andere' && input.outcome_endpoint_amputation == 'Amputation'",
                                                                                                          textInput(inputId = "amputation_height_nonstandard",
                                                                                                                    label = "Andere Amputationshöhe",
                                                                                                                    placeholder = ""
                                                                                                          )
                                                                                         ),
                                                                                         conditionalPanel(condition = "input.outcome_endpoint_amputation == 'Keine Amputation'",
                                                                                                          textInput(inputId = "behandlung_no_amputation",
                                                                                                                    label = "Bitte beschreiben Sie die Behandlung",
                                                                                                                    placeholder = "Behandlung")
                                                                                         )

                                                                                  )
                                                                                ),
                                                                                hr(),
                                                                                fluidRow(
                                                                                  column(width = 6, textAreaInput(inputId = "endpoint_freetext",
                                                                                                                  label = "Bemerkungen zum Endpunkt",
                                                                                                                  placeholder = "Bitte tragen Sie weitere Bemerkungen zu diesem Fall ein"
                                                                                  ))
                                                                                )
                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 6, actionButton(inputId = "action_save_endpoint", label = "Endpunkt speichern"))
                                                                              )
                                                                            )
                                                                          ),
                                                                          wellPanel(
                                                                            fluidRow(
                                                                              h4("Nur zu Demonstationszwecken. Daten werden nicht gespeichert!"),
                                                                              fileInput("wundfotografie", "Hier könne Wundfotos gespeichert werden.")
                                                                            )
                                                                          )
                                                         )
                                                  )
                                         ),
                                         # Patientendaten ändern
                                         tabPanel(title = "Patientendaten ändern",
                                                  tabPanel("Titel darf geändert werden",
                                                           fluidPage(
                                                             div(
                                                               id = "form",
                                                               br(),
                                                               fluidRow(
                                                                 valueBoxOutput("valuebox_change_patient_output"),
                                                                 valueBoxOutput("valuebox_patient_geburtstag_change_output")
                                                               ),
                                                               hr(),
                                                               fluidRow(
                                                                 h3("Bitte nehmen Sie Änderungen vor:"),
                                                                 uiOutput("change_perfusion"),
                                                                 uiOutput("change_extent"),
                                                                 uiOutput("change_depth"),
                                                                 uiOutput("change_infection"),
                                                                 uiOutput("change_sensation"),
                                                                 uiOutput("change_initial_therapy")
                                                               ),
                                                               hr(),
                                                               bsButton(inputId = "submit_changes", label  = "Änderungen speichern", style = "success"),
                                                               hr(),
                                                               br()
                                                             )
                                                           )
                                                  )
                                         )
                                       )
                                     )
                                   )
                          )
               ),
               tabPanel("Übersicht Patienten",
                        fluidPage(
                          titlePanel("Übersicht über die Patientendatenbank"),
                          hr(),
                          fluidRow(
                            column(width = 4, offset = 2, plotOutput(outputId = "output_plot_hist_pedis")),
                            column(width = 4, offset = 0, plotOutput(outputId = "output_plot_bar_gender"))
                          ),
                          hr(),
                          fluidRow(
                            column(width = 4, offset = 2, plotOutput(outputId = "output_plot_time_pedis")),
                            column(width = 4, offset = 0, plotOutput(outputId = "output_plot_patient_year"))
                          ),
                          hr(),
                          column(width = 4,
                                 tableOutput("cross_table")
                          ),
                          dataTableOutput("all_patients")

                        )
               ),
               tabPanel("Applikation beenden",
                        fluidPage(
                          titlePanel("Applikation beenden"),
                          hr(),
                          fluidRow(
                            p("Beendet die App. Alle bisherigen eingegebenen Daten sind gespeichert."),
                            tags$button(
                              id = 'close',
                              type = "button",
                              class = "btn action-button",
                              onclick = "setTimeout(function(){window.close();},500);",  # close browser
                              "Applikation beenden"
                            )
                          )
                        )
               )
    )
)
