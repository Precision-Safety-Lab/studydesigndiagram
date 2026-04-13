library(shiny)
library(ggplot2)
library(jsonlite)
library(svglite)
library(base64enc)

source("make-diagram.R")

license <- '<p xmlns:dct="http://purl.org/dc/terms/" xmlns:cc="http://creativecommons.org/ns#" class="license-text">This work is licensed under <a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0">CC BY-NC-SA 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" /><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" /><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" /><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1"/></a></p>'

anchor1 <- c(
  "Cohort entry date",
  "ED"
)

anchor2 <- c(
  "Please choose",
  "Follow-up",
  "Eligibility",
  "Exclusion",
  "Covariates",
  "Washout",
  "Exposure"
)

make_row_inputs <- function(x, atype = 1, s = 0, e = 0) {
  select_default <- if (atype >= 1 && atype <= length(anchor2)) anchor2[[atype]] else anchor2[[1]]

  tagList(
    tags$div(
      id = paste0("row", x),
      fluidRow(
        column(
          1,
          tags$div(
            title = "Order of display (1 = Bottom)",
            numericInput(paste0("line", x), "Order", value = x, min = 1, max = 12)
          )
        ),
        column(
          2,
          tags$div(
            title = "Label to be displayed on the first line of the box",
            selectInput(
              paste0("lbl1_", x),
              "Second-order anchor",
              choices = anchor2,
              selected = select_default
            )
          )
        ),
        column(
          1,
          tags$div(
            title = "Label to be displayed on the box (line two)",
            textInput(paste0("lbl2_", x), "(line 2)", value = "")
          )
        ),
        column(
          1,
          tags$div(
            title = "Start of interval in days (also placement on the x-axis)",
            numericInput(paste0("start", x), "Start", value = s)
          )
        ),
        column(
          1,
          tags$div(
            title = "End of interval in days (also placement on the x-axis)",
            numericInput(paste0("end", x), "End", value = e)
          )
        ),
        column(
          2,
          tags$div(
            title = "Optional label for interval. When left blank start/end is displayed.",
            textInput(paste0("start_lbl", x), "Optional label (start)", value = "")
          )
        ),
        column(
          1,
          tags$div(
            title = "Optional label for interval. When left blank start/end is displayed.",
            textInput(paste0("end_lbl", x), "(end)", value = "")
          )
        ),
        column(
          1,
          tags$div(
            title = "Colors are chosen automatically based on numbers",
            numericInput(paste0("boxcolor", x), "Color", value = x, min = 1, max = 8)
          )
        ),
        column(
          2,
          selectInput(
            paste0("outbox", x),
            "Label outside box?",
            choices = c("No" = 0, "Yes, left" = 1, "Yes, right" = 2),
            selected = 0
          )
        )
      )
    )
  )
}

make_index_inputs <- function(x, selected = NULL, idx = 0) {
  select_default <- if (is.null(selected) || !selected %in% anchor1) anchor1[[1]] else selected

  tagList(
    tags$div(
      id = paste0("index", x),
      fluidRow(
        column(
          4,
          tags$div(
            title = "First order anchor (label to be displayed above vertical line)",
            selectInput(
              paste0("indexlbl1_", x),
              "First-order anchors",
              choices = anchor1,
              selected = select_default
            )
          )
        ),
        column(
          4,
          tags$div(
            title = "Label for vertical line (line two)",
            textInput(paste0("indexlbl2_", x), "Optional label", value = "")
          )
        ),
        column(
          4,
          tags$div(
            title = "Day for placement of vertical line (typically event date or cohort entry)",
            numericInput(paste0("index", x), "Index date", value = idx)
          )
        )
      )
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$script(HTML('
      (function () {
        function saveProjectToStorage(text) {
          localStorage.setItem("longdiag_state", text);
        }

        function loadSavedProjectOnConnect() {
          var saved = localStorage.getItem("longdiag_state");
          if (saved) {
            Shiny.setInputValue("restore_state", saved, {priority: "event"});
          }
        }

        document.addEventListener("shiny:connected", loadSavedProjectOnConnect);

        Shiny.addCustomMessageHandler("save_state", function (state) {
          saveProjectToStorage(JSON.stringify(state));
        });

        document.addEventListener("change", function (e) {
          var el = e.target;
          if (!el) return;
          if (el.id !== "load_project") return;
          if (!el.files || !el.files.length) return;

          var file = el.files[0];
          var reader = new FileReader();
          reader.onload = function (ev) {
            var text = ev.target.result;
            saveProjectToStorage(text);
            Shiny.setInputValue("restore_state", text, {priority: "event"});
          };
          reader.readAsText(file);
        });

        window.saveProject = function () {
          var saved = localStorage.getItem("longdiag_state");
          if (!saved) {
            alert("Nothing to save yet.");
            return;
          }
          var blob = new Blob([saved], {type: "application/json"});
          var url = URL.createObjectURL(blob);
          var a = document.createElement("a");
          a.href = url;
          a.download = "longdiag_project.json";
          a.click();
          URL.revokeObjectURL(url);
        };
      })();
    '))
  ),

  titlePanel("Diagrams for longitudinal study designs"),

  mainPanel(
    width = 12,
    fluidRow(
      br(),
      column(
        7,
        tabsetPanel(
          tabPanel(
            "First-order anchors",
            br(),
            p("(Hover over input boxes for help.)", style = "color:grey"),
            p(
              actionButton("vlines", "Add anchor"),
              actionButton("remline", "Remove anchor"),
              actionButton("clearv", "Remove all")
            ),
            tags$div(id = "index_container", make_index_inputs(1))
          ),
          tabPanel(
            "Second-order anchors",
            br(),
            p("(Hover over input boxes for help.)", style = "color:grey"),
            p(
              actionButton("add", "Add anchor"),
              actionButton("rem", "Remove anchor"),
              actionButton("clearrows", "Remove all")
            ),
            tags$div(id = "row_container", make_row_inputs(1))
          ),
          tabPanel(
            "Output",
            br(),
            p("Adjust text size if necessary.", style = "color:grey"),
            fluidRow(
              column(2, selectInput("aratio", "Aspect ratio", choices = c("4:3" = 1, "16:9" = 2), selected = 1)),
              column(2, numericInput("textsize", "Text size", value = 14, min = 4, max = 32)),
              column(2, numericInput("xpad", "Padding (x-axis)", value = 50, min = 0, max = 300)),
              column(2, br(), uiOutput("download_link")),
              column(2, br(), uiOutput("download_link_svg"))
            ),
            fluidRow(
              column(2, br(), tags$button("Save project", onclick = "saveProject()", class = "btn btn-default")),
              column(2, br(), fileInput("load_project", "Load project", accept = ".json"))
            )
          )
        )
      ),
      column(
        5,
        br(),
        br(),
        plotOutput("longdiag")
      )
    )
  ),

  tags$div(
    "This application is inspired by the work of ",
    a("Schneeweiss et al.", href = "http://annals.org/article.aspx?doi=10.7326/M18-3079"),
    ", please consider citing the original article when publishing a diagram generated by this application.", HTML("<br/>"),
     "This application is an extension of an application developed by Lars Christian Lund, available on ", a("Gitlab", href="https://source.coderefinery.org/lcl/repeat-diagrams"),
    HTML(license)
  )
)

server <- function(input, output, session) {
  rownums <- reactiveVal(1)
  vnums <- reactiveVal(1)
  is_restoring <- reactiveVal(FALSE)

  collect_state <- reactive({
    req(input$textsize, input$xpad, input$aratio)

    list(
      rownums = rownums(),
      vnums = vnums(),
      textsize = input$textsize,
      xpad = input$xpad,
      aratio = input$aratio,
      rows = lapply(seq_len(rownums()), function(x) {
        list(
          line = input[[paste0("line", x)]],
          lbl1 = input[[paste0("lbl1_", x)]],
          lbl2 = input[[paste0("lbl2_", x)]],
          start = input[[paste0("start", x)]],
          end = input[[paste0("end", x)]],
          start_lbl = input[[paste0("start_lbl", x)]],
          end_lbl = input[[paste0("end_lbl", x)]],
          boxcolor = input[[paste0("boxcolor", x)]],
          outbox = input[[paste0("outbox", x)]]
        )
      }),
      indices = lapply(seq_len(vnums()), function(x) {
        list(
          indexlbl1 = input[[paste0("indexlbl1_", x)]],
          indexlbl2 = input[[paste0("indexlbl2_", x)]],
          index = input[[paste0("index", x)]]
        )
      })
    )
  }) |> debounce(1000)

  observeEvent(collect_state(), {
    if (is_restoring()) return()
    session$sendCustomMessage("save_state", collect_state())
  }, ignoreInit = TRUE)

  restore_state <- function(state) {
    if (!is.list(state)) return()
    if (isTRUE(is_restoring())) return()

    is_restoring(TRUE)
    on.exit(is_restoring(FALSE), add = TRUE)

    n_rows <- length(state$rows %||% list())
    n_idx <- length(state$indices %||% list())

    updateNumericInput(session, "textsize", value = as.numeric(state$textsize %||% 14))
    updateNumericInput(session, "xpad", value = as.numeric(state$xpad %||% 50))
    updateSelectInput(session, "aratio", selected = as.character(state$aratio %||% 1))

    removeUI(selector = "#row_container > div", multiple = TRUE, immediate = TRUE)
    removeUI(selector = "#index_container > div", multiple = TRUE, immediate = TRUE)

    row_count <- max(1, n_rows)
    idx_count <- max(1, n_idx)

    for (i in seq_len(row_count)) {
      insertUI(selector = "#row_container", where = "beforeEnd", ui = make_row_inputs(i), immediate = TRUE)
    }
    for (i in seq_len(idx_count)) {
      insertUI(selector = "#index_container", where = "beforeEnd", ui = make_index_inputs(i), immediate = TRUE)
    }

    rownums(row_count)
    vnums(idx_count)

    session$onFlushed(function() {
      if (n_rows > 0) {
        for (x in seq_len(n_rows)) {
          r <- state$rows[[x]]
          updateNumericInput(session, paste0("line", x), value = as.numeric(r$line))
          updateSelectInput(session, paste0("lbl1_", x), selected = as.character(r$lbl1))
          updateTextInput(session, paste0("lbl2_", x), value = as.character(r$lbl2))
          updateNumericInput(session, paste0("start", x), value = as.numeric(r$start))
          updateNumericInput(session, paste0("end", x), value = as.numeric(r$end))
          updateTextInput(session, paste0("start_lbl", x), value = as.character(r$start_lbl))
          updateTextInput(session, paste0("end_lbl", x), value = as.character(r$end_lbl))
          updateNumericInput(session, paste0("boxcolor", x), value = as.numeric(r$boxcolor))
          updateSelectInput(session, paste0("outbox", x), selected = as.character(r$outbox))
        }
      }

      if (n_idx > 0) {
        for (x in seq_len(n_idx)) {
          idx <- state$indices[[x]]
          updateSelectInput(session, paste0("indexlbl1_", x), selected = as.character(idx$indexlbl1))
          updateTextInput(session, paste0("indexlbl2_", x), value = as.character(idx$indexlbl2))
          updateNumericInput(session, paste0("index", x), value = as.numeric(idx$index))
        }
      }
    }, once = TRUE)
  }

  observeEvent(input$restore_state, {
    req(input$restore_state)
    state <- fromJSON(input$restore_state, simplifyVector = FALSE)
    restore_state(state)
  }, ignoreInit = TRUE)

  observeEvent(input$add, {
    new_id <- rownums() + 1
    insertUI(selector = "#row_container", where = "beforeEnd", ui = make_row_inputs(new_id))
    rownums(new_id)
  })

  observeEvent(input$rem, {
    if (rownums() > 1) {
      removeUI(selector = paste0("#row", rownums()))
      rownums(rownums() - 1)
    }
  })

  observeEvent(input$clearrows, {
    if (rownums() > 1) {
      for (i in rownums():2) {
        removeUI(selector = paste0("#row", i))
      }
    }
    rownums(1)
  })

  observeEvent(input$vlines, {
    new_id <- vnums() + 1
    insertUI(selector = "#index_container", where = "beforeEnd", ui = make_index_inputs(new_id))
    vnums(new_id)
  })

  observeEvent(input$remline, {
    if (vnums() > 1) {
      removeUI(selector = paste0("#index", vnums()))
      vnums(vnums() - 1)
    }
  })

  observeEvent(input$clearv, {
    if (vnums() > 1) {
      for (i in vnums():2) {
        removeUI(selector = paste0("#index", i))
      }
    }
    vnums(1)
  })

  drows <- debounce(reactive(seq_len(rownums())), 500)
  irows <- debounce(reactive(seq_len(vnums())), 500)

  ggdf <- reactive({
    idx <- drows()
    data.frame(
      line = vapply(idx, function(x) input[[paste0("line", x)]], numeric(1)),
      lbl = vapply(idx, function(x) input[[paste0("lbl1_", x)]], character(1)),
      lbl2 = vapply(idx, function(x) input[[paste0("lbl2_", x)]], character(1)),
      outside_box = vapply(idx, function(x) input[[paste0("outbox", x)]], character(1)),
      start = vapply(idx, function(x) input[[paste0("start", x)]], numeric(1)),
      end = vapply(idx, function(x) input[[paste0("end", x)]], numeric(1)),
      start_lbl = vapply(idx, function(x) input[[paste0("start_lbl", x)]], character(1)),
      end_lbl = vapply(idx, function(x) input[[paste0("end_lbl", x)]], character(1)),
      color = vapply(idx, function(x) input[[paste0("boxcolor", x)]], numeric(1)),
      stringsAsFactors = FALSE
    )
  })

  indf <- reactive({
    idx <- irows()
    data.frame(
      vlines = vapply(idx, function(x) input[[paste0("index", x)]], numeric(1)),
      indexlabel = vapply(idx, function(x) input[[paste0("indexlbl1_", x)]], character(1)),
      indexlabel2 = vapply(idx, function(x) input[[paste0("indexlbl2_", x)]], character(1)),
      stringsAsFactors = FALSE
    )
  })

  plt <- reactive({
    gen_long_diag(
      ggdf(),
      indf(),
      txt_size = input$textsize - 2,
      addpad = input$xpad
    )
  })

  output$longdiag <- renderPlot({
    plt()
  }, width = 700, height = 450)

  output$download_link <- renderUI({
    p <- plt()
    tmp <- tempfile(fileext = ".pdf")

    ggsave(
      tmp,
      plot = p,
      width = if (identical(as.character(input$aratio), "1")) 16 else 16,
      height = if (identical(as.character(input$aratio), "1")) 12 else 9,
      units = "in",
      device = "pdf"
    )

    href <- paste0("data:application/pdf;base64,", base64encode(tmp))
    tags$a(href = href, download = "diagram.pdf", class = "btn btn-default", "Download .pdf")
  })

  output$download_link_svg <- renderUI({
    p <- plt()
    tmp <- tempfile(fileext = ".svg")

    ggsave(
      tmp,
      plot = p,
      width = if (identical(as.character(input$aratio), "1")) 16 else 16,
      height = if (identical(as.character(input$aratio), "1")) 12 else 9,
      units = "in",
      device = "svg"
    )

    href <- paste0("data:image/svg+xml;base64,", base64encode(tmp))
    tags$a(href = href, download = "diagram.svg", class = "btn btn-default", "Download .svg")
  })
}

shinyApp(ui = ui, server = server)
