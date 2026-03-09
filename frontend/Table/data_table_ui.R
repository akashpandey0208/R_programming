data_table_ui <- function(id){

  ns <- NS(id)

  tagList(

    fluidRow(

      column(
        width = 6,

        fluidRow(
          column(
            12,
            div(
              class = "dm-panel-box",
              uiOutput(ns("group_by_ui"))
            )
          )
        ),

        fluidRow(

          column(
            4,
            div(
              class = "dm-panel-box",

              h4("Variables"),

              div(
                class = "variables-box",
                uiOutput(ns("variables_list"))
              )

            )
          ),

          column(
            8,
            div(
              class = "dm-panel-box",

              h4("Selected Variables"),

              div(
                id = ns("drop_area"),
                class = "drop-container",
                uiOutput(ns("selected_vars_ui"))
              )

            )
          )

        )

      ),

      column(
        width = 6,

        div(
          class = "dm-panel-box",

          h4("Operation Output"),

          uiOutput(ns("operation_tables"))

        )

      )

    ),

    fluidRow(

      column(
        width = 12,

        div(
          class = "dm-panel-box",

          h4("Table Title"),

          textInput(
            ns("table_title"),
            NULL,
            placeholder = "Table Title"
          ),

          fluidRow(

            column(
              6,
              downloadButton(
                ns("download_table"),
                "Download Table"
              )
            ),

            column(
              6,
              radioButtons(
                ns("download_type"),
                "Download Type",
                choices = c("RTF","CSV","HTML"),
                inline = TRUE
              )
            )

          )

        )

      )

    ),

    fluidRow(

      column(
        width = 12,

        div(
          class = "dm-panel-box",

          h4("Download R Script(s)"),
          p("Reproduce Table"),

          downloadButton(
            ns("download_code"),
            "Code"
          )

        )

      )

    )

  )

}