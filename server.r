
# Define the fields we want to save from the form
fields <- c(
  "perfusion",
  "extent",
  "depth",
  "infection",
  "sensation",
  "initial_therapy",
  "first_name",
  "name",
  "admission_date",
  "birth_date",
  "gender",
  "lokalisation",
  "dialyse",
  "crp"
)

# Define the fields we want to save from the form
fields_change <- c(
  "perfusion_change",
  "extent_change",
  "depth_change",
  "infection_change",
  "sensation_change",
  "initial_therapy_change"
)

# Define SQLite Database Path and table
sqlitePath <- "./data.sqlite"
table <- "tbl_pedis"

# r-function  from https://shiny.rstudio.com/articles/persistent-data-storage.html#sqlite
save_data <- function(data) {
  print(data)
  # patient_id <- data$id
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields

  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table,
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )

  # return id of last inserted row
  dbGetQuery(db, query)
  id <- dbGetQuery(db, "SELECT last_insert_rowid();")
  dbDisconnect(db)
  return(c(id = id))
}

save_data_changes <- function(data, patient_id) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  for(col_name in names(data)){
    query <- sprintf(
      "UPDATE tbl_pedis SET %s = '%s' WHERE ID = %s;", col_name, data[col_name], patient_id
    )
    print(query)
    dbSendQuery(db, query)
  }
  dbDisconnect(db)
}

save_nebendiagnosen <- function(nebendiagnosen, patient_id) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  for(i in nebendiagnosen){
    query <- sprintf( "INSERT INTO tbl_nebendiagnosen (patient_id, nebendiagnose) VALUES ('%s', '%s')", patient_id, i)
    print(query)
    # Submit the update query and disconnect
    dbGetQuery(db, query)
  }
  dbDisconnect(db)
}

save_endpoint <- function(data) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO tbl_endpoint (%s) VALUES ('%s')",
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

# display all patients in the database
get_all_patients <- function(data) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Join and fetch all data from database
  query <- sprintf("select * from %s left join tbl_endpoint on %s.id = tbl_endpoint.patient_id;", table, table)
  # Submit the update query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return(data)
}

loadData <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  # Disconnect
  dbDisconnect(db)
  return(data)
}

# get patient from database which the user selected
select_patient_from_db <- function(id) {
  db <- dbConnect(SQLite(), sqlitePath)
  query <- sprintf("select * from %s where id = %s", table, id)
  # Submit the update query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  return(data)
}

# berechne quantil
pedis_quantile <- function(data, pedis_score) {
  # returns
  data <- data %>%
    mutate(pedis = perfusion + extent + depth + infection + sensation) %>%
    mutate(percent = percent_rank(as.numeric(pedis))) %>%
    mutate(percent = round(percent * 100, 1)) %>%
    filter(pedis == pedis_score) %>%
    mutate()
  # data$percent
  paste0(data$percent, " %")
}

calculate_pedis <- function(data) {
  pedis <- data %>%
    mutate(pedis = sum(perfusion, extent, infection, depth, sensation)) %>%
    select(pedis) %>%
    as.numeric()
  return(pedis)
}

# Patientensuche mit Datum
search_patient_from_db <- function(date, name, first_name, date_range) {
  db <- dbConnect(SQLite(), sqlitePath)
  date_start <- date_range[1]
  date_stop <- date_range[2]
  name <- paste0("%", name, "%")
  first_name <- paste0("%", first_name, "%")
  if(date) {
    query <- sprintf("select * from %s where (name like '%s' AND first_name like '%s') AND birth_date BETWEEN '%s' AND '%s';", table, name, first_name, date_start, date_stop)
  } else {
    query <- sprintf("select * from %s where (name like '%s' AND first_name like '%s');", table, name, first_name)
  }
  # Submit the update query and disconnect
  data <- dbGetQuery(db, query) %>% tbl_df
  dbDisconnect(db)
  return(data)
}

# Shiny Server Config
server <- function(input, output, session) {

  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x)
      as.character(input[[x]]))
    data
  })

  form_data_change <- reactive({
    data <- sapply(fields_change, function(x)
      as.character(input[[x]]))
    names(data) <-
      sub(pattern = "_change",
          replacement = "",
          x = names(data))
    print(names(data))
    data
  })

  form_endpoint <- reactive({
    input_fields <-
      c(
        "outcome_endpoint_amputation",
        "amputation_height",
        "amputation_height_nonstandard",
        "endpoint_freetext",
        "behandlung_no_amputation"
      )
    data <-
      sapply(input_fields, function(x)
        as.character(input[[x]]))
    data <- data[which(data != "")]
    data
  })

  # create histogram of pedis values
  plot_hist_pedis <- reactive({
    data <- loadData()
    p <- data %>%
      mutate(pedis = (perfusion + depth + extent + infection + sensation)) %>%
      ggplot(aes(x = pedis)) +
      geom_histogram(binwidth = 2.5, col = "white") +
      scale_x_continuous("PEDIS- Wert (Klassenbreite: 2,5 Punkte)", seq(0, 70, 5)) +
      scale_y_continuous("Anzahl der Patienten in einer Klasse (absolute Anzahl)") +
      theme_minimal() +
      theme(axis.title.y = element_text(size = 15),
            text = element_text(size = 18)) +
      ggtitle("Verteilung der Pedis-Werte")
    return(p)
  })
  output$output_plot_hist_pedis <- renderPlot({
    plot_hist_pedis()
  })

  # create barplot of absolute values of gernder
  plot_bar_gender <- reactive({
    data <- loadData()
    data %>%
      mutate(gender = if_else(is.na(gender), "Keine Angabe", gender)) %>%
      group_by(gender) %>%
      tally() %>%
      ggplot(aes(x = factor(gender), y = n)) +
      geom_bar(stat = "identity") +
      scale_x_discrete("Patienten nach Geschlecht") +
      scale_y_continuous("Anzahl der Patienten (absolute Anzahl)") +
      theme_minimal() +
      theme(text = element_text(size = 18)) +
      ggtitle("Verteilung der Geschlechter")
  })

  output$output_plot_bar_gender <- renderPlot({
    plot_bar_gender()
  })

  # create timeseries of data entries
  plot_time_pedis <- reactive({
    data <- loadData()
    p <- data %>%
      mutate(pedis = perfusion + depth + extent + infection + sensation) %>%
      mutate(timestamp = as.character(timestamp)) %>%
      mutate(timestamp = ymd_hms(timestamp)) %>%
      ggplot(aes(x = timestamp, y = pedis)) +
      geom_line() +
      geom_point() +
      scale_x_datetime("Datum") +
      scale_y_continuous("PEDIS-Wert bei Aufnahme", limits = c(0, 70)) +
      theme_minimal() +
      theme(text = element_text(size = 18)) +
      ggtitle("Die PEDIS-Werte im Zeitverlauf")
    return(p)
  })
  output$output_plot_time_pedis <- renderPlot({
    plot_time_pedis()
  })

  # create patients per yeaer
  plot_patient_year <- reactive({
    data <- loadData()
    p <- data %>%
      mutate(birth_date = ymd(birth_date)) %>%
      mutate(year = year(admission_date)) %>%
      mutate(year = as.character(admission_date)) %>%
      group_by(year) %>%
      tally() %>%
      ggplot(aes(x = year, y = n)) +
      geom_bar(stat = "identity") +
      scale_x_discrete("Datum") +
      scale_y_continuous("Anzahl der Patienten (absolute Anzahl)") +
      theme_minimal() +
      theme(text = element_text(size = 18)) +
      ggtitle("Anzahl der aufgenommenen Patienten pro Tag")

    return(p)
  })
  output$output_plot_patient_year <-
    renderPlot({
      plot_patient_year()
    })


  mandatory_fields <- c("initial_therapy", "name")

  observe({
    # check if all mandatory fields have a value
    mandatory_filled <- vapply(mandatory_fields,
                               function(x) {
                                 !is.null(input[[x]]) && input[[x]] != ""
                               },
                               logical(1))

    mandatory_filled <- all(mandatory_filled)
    if (mandatory_filled) {
      style <-  "success"
      label <- "Patient anlegen"
      disable_input <- FALSE
      message_tooltip <- NULL
    } else {
      message_tooltip <-
        "Bitte alle Felder, die mit einem roten Stern markiert sind, ausfüllen"
      style <-  "danger"
      label <- "Patient anlegen"
      disable_input <- FALSE
    }

    addTooltip(session,
               id = "submit",
               title = message_tooltip,
               placement = "right")

    updateButton(session,
                 inputId = "submit",
                 label = label,
                 style = style)

    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatory_filled)

  })

  observeEvent(input$reset_input, {
    shinyjs::reset("form")
    shinyjs::reset("patent_overview")
  })

  observeEvent(input$sidebar_patientensuche_reset, {
    shinyjs::reset("sidebar_patientensuche")
  })

  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    # save data (pedis values) to the main table (pedis) and retreive patient id (all in the function named save_data which retreives the last_insert_rowid)
    patient_id <- save_data(formData())
    # insert nebendiagnosen
    nebendiagnosen <- c(input$infection_stage_2_3, input$infection_stage_4)
    save_nebendiagnosen(nebendiagnosen = nebendiagnosen, patient_id = patient_id)
    # select patient and calcualte pedis value
    patient_data <- select_patient_from_db(id = patient_id)
    pedis_wert <- patient_data %>%
      mutate(pedis = perfusion + extent + depth + infection + sensation) %>%
      select(pedis) %>%
      as.numeric()
    # load all patient data
    data <- loadData()
    # calculate percent rank
    perc_rank <- data %>%
      mutate(pedis = perfusion + extent + depth + infection + sensation) %>%
      mutate(rank = 100 * round(percent_rank(pedis), 3)) %>%
      filter(pedis == pedis_wert) %>%
      select(rank) %>%
      unlist() %>%
      as.numeric() %>%
      unique()
    # At the end use shiny java script to reset the entry form and display an info text to the user
    shinyjs::reset("form")
    shinyjs::info(
      text = paste0(
        "Patientendaten erfolgreich gespeichert.\nDer PEDIS Wert beträgt: ",
        pedis_wert,
        ".\n",
        perc_rank,
        "% aller Patienten haben einen geringeren PEDIS-Wert."
      )
    )
  })

  # search for patients in database
  # return patients in a data table
  searching <- reactive({
    patient_names <-
      search_patient_from_db(
        date = input$search_by_birthdate,
        first_name = input$patientensuche_first_name,
        name = input$patientensuche_name,
        date_range = input$date_range
      )
    return(patient_names)
  })

  # function to concat patient names for the dropdown menu
  patient_names <- reactive({
    patient_data <- searching()
    if (nrow(patient_data) != 0) {
      data <- patient_data %>%
        dplyr::select(ID, first_name, name, birth_date) %>%
        unite(names, first_name, name, sep = " ") %>%
        mutate(birth_date = ymd(birth_date)) %>%
        mutate(birth_year = lubridate::year(birth_date)) %>%
        mutate(birth_year = as.character(birth_year)) %>%
        mutate(birth_year = paste0("Jahrgang: ", birth_year)) %>%
        unite(patient,
              ID,
              names,
              birth_year,
              sep = " - ",
              remove = FALSE) %>%
        # select(ID, patient)
        mutate()
      patients  <- data$ID
      names(patients) <- data$patient
      return(patients)
    } else {
      return("")
    }
  })

  # rendering the dropdown menu of patients found by the patient search
  output$patients_dropdown <- renderUI({
    selectInput(
      "patients_selected",
      label = "Gefundene Patienten",
      choices = patient_names(),
      selected = character(0)
    )
  })

  # läd patientendaten aus der datenbank, sobald der Patient ausgewählt wurde (actionButton - patient_selected_submit)
  patient_data <- eventReactive(input$patient_selected_submit, {
    data <- select_patient_from_db(input$patients_selected)
    return(data)
  })

  # creates a user interface which displays the patient data in the app
  output$patient_output <- renderUI({
  })

  # rendering valueboxes to display patient characteristics

  # render valuebox pedis quantiles
  patient_pedis_quantile <- reactive({
    data <- patient_data()
    pedis <- calculate_pedis(data)
    sqldata <- loadData() # returns data frame
    pedis <- pedis_quantile(data = sqldata, pedis_score = pedis)
    return(pedis)
  })

  output$pedis_quantile_dashboard <- renderValueBox(
    valueBox(
      value = patient_pedis_quantile(),
      subtitle = "aller Patienten haben einen geringeren PEDIS Wert",
      icon = icon("percent")
    )
  )

  # render valuebox pedis score
  patient_pedis_score <- reactive({
    data <- patient_data()
    x <- calculate_pedis(data)
    return(x)
  })

  output$pedis_wert_dashboard <- renderValueBox(
    valueBox(
      value = patient_pedis_score(),
      subtitle = "PEDIS-Wert bei Aufnahme",
      icon = icon("user-md")
    )
  )

  # render valuebox name
  valuebox_patient_name <- reactive({
    data <- patient_data()
    data <- data %>%
      mutate(gender = if_else(gender == "weiblich", "Frau", "Herr")) %>%
      mutate(gender = if_else(is.na(gender), "", gender))
    data <- paste(data$gender, data$first_name, data$name)
    data
  })

  output$valuebox_patient_name_output <- renderValueBox(valueBox(
    value = valuebox_patient_name(),
    subtitle = "Name",
    icon = icon("user-o")
  ))

  # render valuebox geburtsdatum
  valuebox_patient_geburtstag <- reactive({
    data <- patient_data()
    jahr <- year(data$birth_date)
    monat <- month(data$birth_date, label = F)
    tag <- day(data$birth_date)
    paste0(tag, ".", monat, ".", jahr)
  })

  output$valuebox_patient_geburtstag_output <- renderValueBox(
    valueBox(
      value = valuebox_patient_geburtstag(),
      subtitle = "Geburtsdatum",
      icon = icon("birthday-cake"),
      color = "red"
    )
  )

  # render valuebox geburtsdatum patientchange
  valuebox_patient_geburtstag_change <- reactive({
    data <- patient_data()
    jahr <- year(data$birth_date)
    monat <- month(data$birth_date, label = F)
    tag <- day(data$birth_date)
    paste0(tag, ".", monat, ".", jahr)
  })

  output$valuebox_patient_geburtstag_change_output <-
    renderValueBox(valueBox(value = valuebox_patient_geburtstag(),
                            subtitle = "Geburtsdatum"))

  reactive({
    data <- patient_data()
    print(data)
  })

  # render valuebox patiten to change
  valuebox_change_patient <- reactive({
    data <- patient_data()
    data %>%
      mutate(name = paste(first_name, name)) %>%
      pull(name)
  })

  output$valuebox_change_patient_output <- renderValueBox(valueBox(value = valuebox_change_patient(),
                                                                   subtitle = "Name"))

  # render valuebox admission date
  valuebox_patient_admission <- reactive({
    data <- patient_data()
    data$admission_date <- ymd(data$admission_date)
    jahr <- year(data$admission_date)
    monat <- month(data$admission_date, label = F)
    tag <- day(data$admission_date)
    paste0(tag, ".", monat, ".", jahr)
  })


  output$valuebox_patient_admission_output <- renderValueBox(
    valueBox(
      value = valuebox_patient_admission(),
      subtitle = "Aufnahmedatum",
      icon = icon("arrow-right", "fa-1x"),
      color = "red"
    )
  )
  # render valuebox days till endpoint
  valuebox_patient_endpoint <- reactive({
    # data <- patient_data()
    data <- select_patient_from_db(input$patients_selected)
    admission_date <- ymd(data$admission_date)
    endpoint_date <- admission_date + months(6)
    today_date <- today()
    time_left_to_endpoint <- endpoint_date - today_date
    return(paste0(time_left_to_endpoint, " Tage"))
  })

  output$valuebox_patient_endpoint_output <- renderValueBox(
    valueBox(
      value = valuebox_patient_endpoint(),
      subtitle = "bis der Endpunkt erreicht ist",
      icon = icon("calendar")
    )
  )

  # render date today
  output$valuebox_date_today_output <- renderValueBox(valueBox(
    value = paste0(day(today()), ".", month(today()), ".", year(today())),
    subtitle = "Heutiges Datum",
    icon = icon("calendar")
  ))

  # render valuebox patient id
  valuebox_patient_id <- reactive({
    data <- patient_data()
    as.character(data$ID)
  })

  output$valuebox_patient_id_output <- renderValueBox(valueBox(
    value = valuebox_patient_id(),
    subtitle = "Patienten-ID",
    icon = icon("hashtag")
  ))

  # render valuebox patient ...


  # all things that happen when pressing the save endpoint button (observer)
  observeEvent(input$action_save_endpoint, {
    # update of the dropdown menu to ensure that the initial value of dropdown menu is empty and has a placeholder value
    updateSelectizeInput(
      session,
      "outcome_endpoint_amputation",
      label = 'Amputation zum Endpunkt',
      choices = c("Amputation",
                  "Keine Amputation"),
      options = list(
        placeholder = 'Bitte den Endpunkt auswählen',
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
    # After submitting updated patient data, reset the text input field
    reset("free_text_amputation_endpoint")
    # )
    data <- patient_data()
    id <- as.character(data$ID)
    data <- form_endpoint()
    data <- c("patient_id" = id, data)
    save_endpoint(data)
    shinyjs::info(text = paste0("Endpunkt des Patienten erfolgreich gespeichert!"))
  })

  patient_data_to_change <- reactive({
    data <- patient_data()
    data
  })

  ##############
  ### Change ###
  ##############

  # Change Perfusion
  output$change_perfusion <- renderUI({
    data <- patient_data()
    print(data)
    init_value <- data$perfusion
    selectInput(
      inputId = "perfusion_change",
      label = "Perfusion",
      choices = c(
        "Stufe 1: Keine nachweisbare pAVK" = 1,
        "Stufe 2: aVK, aber keine Extremitäten Ischämie" = 2,
        "Stufe 3: Kritische Extremitätenischämie" = 3
      ),
      selected = init_value
    )
  })

  # Change Extent
  output$change_extent <- renderUI({
    data <- patient_data()
    inital_value <- data$extent
    sliderInput(
      "extent_change",
      label = "Ulcusausmaß in cm² (Extend)",
      min = 1,
      max = 60,
      value = inital_value
    )
  })

  # Change Depth
  output$change_depth <- renderUI({
    data <- patient_data()
    init_value <- data$depth
    selectInput(
      inputId = "depth_change",
      label = ("Ulcustiefe (Depth)"),
      choices = c(
        "Stufe 1: Komplettes Ulcus bis zur Dermis" = 1,
        "Stufe 2: Infiltration bis Subcutis, Faszie, Muskulatur, Sehnen" = 2,
        "Stufe 3: Infiltration bis in Knochen und Gelenke" = 3
      ),
      selected = init_value
    )
  })

  # Change Infection
  output$change_infection <- renderUI({
    data <- patient_data()
    init_value <- data$infection
    div(
      selectInput(
        inputId = "infection_change",
        label = ("Infektionsstatus"),
        choices = c(
          "Stufe 1: Keine Entzündungszeichen" = 1,
          "Stufe 2: Infektion von Haut oder subcutanem Gewebe. Mindestens zwei der folgenden Symptome" = 2,
          "Stufe 3: Erythem > 2cm und eines der folgenden Symptome oder Infektion über die Subcutis" = 3,
          "Stufe 4: Zwei oder mehr der folgenden Zeichen einer systematischen Inflammation" = 4
        ),
        selected = init_value

      ),

      conditionalPanel(
        condition = "input.infection_change == 2 || input.infection_change == 3",
        checkboxGroupInput(
          inputId = "infection_stage_2_3_change",
          label = "Symptome Stufe 2 und 3:",
          choices = c(
            "Örtliche Schwellung oder Induration",
            "Periulceröses Erythem 0,5 - 2 cm",
            "Empfindlichkeit von Schmerzen",
            "Überwärmung",
            "Eitriges Sekret"
          )
        )
      ),

      conditionalPanel(
        condition = "input.infection_change == 4",
        checkboxGroupInput(
          inputId = "infection_stage_4_change",
          label = "Symptome Stufe 4:",
          choices = c(
            "Temperatur < 36°C oder > 38°C",
            "Herzfrequenz > 90 bpm",
            "Atemfrequenz > 20/ min",
            "PaCO2 < 32mmHg",
            "Über 10% unreife Granulozyten"
          ),
          selected = NULL
        )
      )
    )
  })

  # Change Sensation
  output$change_sensation <- renderUI({
    data <- patient_data()
    init_value <- data$sensation
    selectInput(
      inputId = "sensation_change",
      label = labelMandatory("Sensibilität (Sensation)"),
      choices = c(
        "Stufe 1: Keine nachweisbare Neuropathie " = 1,
        "Stufe 2: Monofilament an 2 von 3 Auflagepunkten nicht spürbar oder fehlendes Vibrationsempfinden an der Großzehe oder Biothesiometerschwelle über 25V" = 2
      ),
      selected = init_value
    )

  })

  # Change Initialtherapie
  output$change_initial_therapy <- renderUI({
    data <- patient_data()
    init_value <- paste0(data$initial_therapy)
    print(init_value)
    wellPanel(
      textInput(
        label = "Sie können die Therapie hier anpassen:",
        value = init_value,
        inputId = "initial_therapy_change"
      )
    )
  })

  # Save changes
  observeEvent(input$submit_changes, {
    data <- patient_data()
    pat_id <- data$ID

    # save data (pedis values) to the main table (pedis) and retreive patient id (all in the function named save_data which retreives the last_insert_rowid)
    result_save_changes <- save_data_changes(form_data_change(), patient_id = pat_id)

    shinyjs::info(text = paste0("Patientendaten gespeichert"))
  })

  observe({
    if (input$close > 0)
      stopApp() # stop shiny
  })

}

