library(shiny)
library(ggplot2)
library(jsonlite)
library(svglite)
library(base64enc)
library(ggfittext)

source("helpers.R")
source("make-diagram.R")

license <- '<p xmlns:dct="http://purl.org/dc/terms/" xmlns:cc="http://creativecommons.org/ns#" class="license-text">This work is licensed under <a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0">CC BY-NC-SA 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" /><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" /><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" /><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1"/></a></p>'

# ---- UI builders for dynamically added rows --------------------------------

# `pos` is the row's position in the list, which picks its default color (so
# the first row is always the first color, however many ids have been used).
make_row_inputs <- function(x, pos = 1, atype = 1, s = 0, e = 0) {
  select_default <- if (atype >= 1 && atype <= length(anchor2_choices)) anchor2_choices[[atype]] else anchor2_choices[[1]]

  tagList(
    tags$div(
      id = paste0("row", x),
      class = "second-anchor-row",
      `data-row-id` = x,

      # Line 1: anchor, line 2 text, color, outside-box option, delete
      tags$div(
        class = "second-anchor-top-row",

        tags$div(
          class = "drag-handle",
          draggable = "true",
          title = "Drag to reorder. Top row appears at the top of the plot.",
          tags$div("Drag", style = "font-size: 11px; color: #777;"),
          tags$div("\u2630", style = "font-size: 22px; line-height: 22px;")
        ),

        tags$div(
          class = "field-anchor",
          title = "Label to be displayed on the first line of the box",
          selectizeInput(
            paste0("lbl1_", x),
            "Second-order anchor",
            choices = anchor2_choices,
            selected = select_default,
            options = list(create = TRUE),
            width = "100%"
          )
        ),

        tags$div(
          class = "field-line2",
          title = "Label to be displayed on the box (line two)",
          textInput(paste0("lbl2_", x), "Line 2 (optional)", value = "", width = "100%")
        ),

        tags$div(
          class = "field-color",
          title = "Choose a box color. All available colors are dark enough for white text.",
          selectInput(
            paste0("boxcolor", x),
            "Color",
            choices = box_colors,
            selected = box_colors[((pos - 1) %% length(box_colors)) + 1],
            width = "100%"
          )
        ),

        tags$div(
          class = "field-outbox",
          selectInput(
            paste0("outbox", x),
            "Label outside box?",
            choices = c("No" = 0, "Yes, left" = 1, "Yes, right" = 2),
            selected = 0,
            width = "100%"
          )
        ),

        tags$div(
          class = "field-delete",
          actionButton(
            paste0("delrow_", x),
            label = "\u2715",
            class = "btn btn-default btn-sm",
            title = "Remove this anchor"
          )
        )
      ),

      # Line 2: interval placement and interval-label override
      tags$div(
        class = "second-anchor-bottom-row",

        tags$div(
          class = "field-day-small",
          title = "Start of interval in days (also placement on the x-axis)",
          numericInput(paste0("start", x), "Start", value = s, width = "100%")
        ),

        tags$div(
          class = "field-end-combined",
          tags$div(
            class = "end-combined-inner",

            tags$div(
              class = "end-number",
              title = "End of interval in days (also placement on the x-axis)",
              numericInput(paste0("end", x), "End", value = e, width = "100%")
            ),

            tags$div(
              class = "end-checkbox",
              title = "Check if the end of the interval is indefinite (open-ended)",
              checkboxInput(paste0("end_inf_", x), label = "Indefinite", value = FALSE)
            )
          )
        ),

        tags$div(
          class = "field-day-label",
          title = "Optional start label. When left blank, start is displayed.",
          textInput(paste0("start_lbl", x), "Optional label (start)", value = "", width = "100%")
        ),

        tags$div(
          class = "field-day-label",
          title = "Optional end label. When left blank, end is displayed.",
          textInput(paste0("end_lbl", x), "(end)", value = "", width = "100%")
        ),

        tags$div(
          class = "field-days-override",
          title = "Optional full override for the interval label. When filled, this replaces the default days [start; end] label.",
          textInput(paste0("days_lbl", x), "Overwrite days [;] label", value = "", width = "100%")
        )
      ),

      tags$hr(style = "margin-top: 8px; margin-bottom: 12px;")
    )
  )
}

make_index_inputs <- function(x, selected = NULL, idx = 0) {
  select_default <- if (is.null(selected) || !selected %in% anchor1_choices) anchor1_choices[[1]] else selected

  tagList(
    tags$div(
      id = paste0("index_row_", x),
      fluidRow(
        column(
          4,
          tags$div(
            title = "First order anchor (label to be displayed above vertical line)",
            selectizeInput(
              paste0("indexlbl1_", x),
              "First-order anchors",
              choices = anchor1_choices,
              selected = select_default,
              options = list(create = TRUE)
            )
          )
        ),
        column(
          3,
          tags$div(
            title = "Label for vertical line (line two)",
            textInput(paste0("indexlbl2_", x), "Optional sublabel", value = "")
          )
        ),
        column(
          2,
          tags$div(
            title = "Text shown before the index date on the last line of the label (e.g. Day, Week). Leave blank to show only the number.",
            textInput(paste0("indexdatelbl_", x), "Date label", value = "Day")
          )
        ),
        column(
          2,
          tags$div(
            title = "Day for placement of vertical line (typically event date or cohort entry)",
            numericInput(paste0("index", x), "Index date", value = idx)
          )
        ),
        column(
          1,
          tags$div(
            class = "index-delete-col",
            actionButton(
              paste0("delidx_", x),
              label = "\u2715",
              class = "btn btn-default btn-sm",
              title = "Remove this anchor"
            )
          )
        )
      )
    )
  )
}

# ---- UI ---------------------------------------------------------------------

ui <- fluidPage(

  tags$head(
    tags$style(HTML('/* The diagram is rendered at a fixed size (so it matches the PDF/SVG
   exports); on smaller screens, scale it down to fit its column and the
   window height instead of overflowing. It is never scaled up. */
#longdiag img {
  max-width: 100%;
  height: auto;
  max-height: calc(100vh - 140px);
  object-fit: contain;
  object-position: left top;
}

.aligned-row {
  display: flex;
  align-items: flex-end;
  gap: 10px;
}
.aligned-col {
  flex: 1;
  min-width: 0;
}

.second-anchor-row {
  width: 100%;
}

.second-anchor-row .form-group {
  margin-bottom: 8px;
}

.second-anchor-top-row,
.second-anchor-bottom-row {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
  align-items: flex-start;
  width: 100%;
}

.second-anchor-top-row {
  margin-bottom: 4px;
}

.drag-handle {
  flex: 0 0 48px;
  cursor: grab;
  user-select: none;
  text-align: center;
  padding-top: 22px;
  color: #555;
}

.drag-handle:active {
  cursor: grabbing;
}

.second-anchor-row.dragging {
  opacity: 0.45;
}

/* Top row */
.field-anchor {
  flex: 0 1 220px;
  min-width: 180px;
}

.field-line2 {
  flex: 1 1 280px;
  min-width: 220px;
}

.field-color {
  flex: 0 1 150px;
  min-width: 130px;
}

.field-outbox {
  flex: 0 1 165px;
  min-width: 145px;
}

.field-delete {
  flex: 0 0 40px;
  padding-top: 22px;
  text-align: center;
}

.field-delete .btn {
  padding: 4px 8px;
  line-height: 1;
}

.index-delete-col {
  padding-top: 24px;
  text-align: center;
}

/* Bottom row */
.field-day-small {
  flex: 0 0 82px;
  max-width: 82px;
}

.field-end-combined {
  flex: 0 0 165px;
  max-width: 165px;
}

.end-combined-inner {
  display: flex;
  align-items: flex-end;
  gap: 8px;
}

.end-number {
  flex: 0 0 82px;
  max-width: 82px;
}

.end-checkbox {
  flex: 0 0 auto;
  padding-bottom: 8px;
  white-space: nowrap;
}

.end-checkbox .checkbox {
  margin-top: 0;
  margin-bottom: 0;
}

.end-checkbox label {
  padding-left: 18px;
  font-weight: normal;
}

.field-day-label {
  flex: 0 1 145px;
  min-width: 125px;
}

.field-days-override {
  flex: 1 1 230px;
  min-width: 200px;
}

/* Keep numeric inputs compact */
.field-day-small input,
.end-number input {
  min-width: 0;
  padding-left: 6px;
  padding-right: 6px;
}

/* On narrow windows, allow sensible wrapping */
@media (max-width: 900px) {
  .field-anchor,
  .field-line2,
  .field-color,
  .field-outbox,
  .field-day-label,
  .field-days-override {
    flex-basis: 100%;
    max-width: none;
  }

  .field-day-small {
    flex: 0 0 90px;
    max-width: 90px;
  }

  .field-end-combined {
    flex: 0 0 180px;
    max-width: 180px;
  }
}')),
    tags$script(HTML('(function () {
  "use strict";

  // ---- Project state persistence (localStorage) ---------------------------
  //
  // Storage access is wrapped because it can throw (e.g. some private
  // browsing modes); the app must still start, and answer the server,
  // without it.

  var STATE_KEY = "longdiag_state";

  function readSavedState() {
    try { return localStorage.getItem(STATE_KEY) || ""; } catch (e) { return ""; }
  }

  function writeSavedState(text) {
    try { localStorage.setItem(STATE_KEY, text); } catch (e) {}
  }

  // Always answers, with "" when nothing is saved: the server keeps the plot
  // paused until it hears back, so the diagram is only drawn once at startup.
  Shiny.addCustomMessageHandler("request_cookie_state", function (_msg) {
    Shiny.setInputValue("restore_state_cookie", readSavedState(), { priority: "event" });
  });

  Shiny.addCustomMessageHandler("save_state", function (state) {
    writeSavedState(JSON.stringify(state));
  });

  Shiny.addCustomMessageHandler("clear_saved_state", function (_msg) {
    try { localStorage.removeItem(STATE_KEY); } catch (e) {}
  });

  Shiny.addCustomMessageHandler("restore_inputs_applied", function (_msg) {
    window.setTimeout(function () {
      Shiny.setInputValue("restore_inputs_applied", Date.now(), { priority: "event" });
    }, 150);
  });

  // The file is not written to storage here: the server saves the state
  // itself once it has restored it, so an invalid file can never be stored
  // and re-loaded on every visit.
  document.addEventListener("change", function (e) {
    var el = e.target;
    if (!el || el.id !== "load_project" || !el.files || !el.files.length) return;

    var file = el.files[0];
    var reader = new FileReader();
    reader.onload = function (ev) {
      Shiny.setInputValue("restore_state_upload", ev.target.result, { priority: "event" });
    };
    reader.readAsText(file);
  });

  // ---- Downloads ------------------------------------------------------------
  //
  // Files are built by the server only when a download button is clicked, and
  // handed over here as a message (this works the same under Shinylive).

  function downloadBlob(blob, filename) {
    var url = URL.createObjectURL(blob);
    var a = document.createElement("a");
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    window.setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
  }

  Shiny.addCustomMessageHandler("download_project", function (state) {
    downloadBlob(new Blob([JSON.stringify(state)], { type: "application/json" }), "longdiag_project.json");
  });

  Shiny.addCustomMessageHandler("download_file", function (msg) {
    var bytes = atob(msg.base64);
    var buf = new Uint8Array(bytes.length);
    for (var i = 0; i < bytes.length; i++) buf[i] = bytes.charCodeAt(i);
    downloadBlob(new Blob([buf], { type: msg.mime }), msg.filename);
  });

  // ---- Drag-to-reorder second-order anchor rows ----------------------------

  var lastSentOrder = null;

  function rowOrderArray() {
    return Array.prototype.slice
      .call(document.querySelectorAll("#row_container > .second-anchor-row"))
      .map(function (el) { return el.getAttribute("data-row-id"); })
      .filter(Boolean);
  }

  function pushRowOrder(force) {
    if (!window.Shiny) return;

    var order = rowOrderArray().join(",");
    if (!force && order === lastSentOrder) return;

    lastSentOrder = order;
    Shiny.setInputValue("row_order_str", order, { priority: "event" });
  }

  function getDragAfterElement(container, y) {
    var rows = Array.prototype.slice.call(
      container.querySelectorAll(".second-anchor-row:not(.dragging)")
    );

    var closest = { offset: Number.NEGATIVE_INFINITY, element: null };

    rows.forEach(function (row) {
      var box = row.getBoundingClientRect();
      var offset = y - box.top - box.height / 2;
      if (offset < 0 && offset > closest.offset) {
        closest = { offset: offset, element: row };
      }
    });

    return closest.element;
  }

  function initDraggableRows() {
    var container = document.getElementById("row_container");
    if (!container || container.dataset.dragSortReady === "true") return;

    container.dataset.dragSortReady = "true";
    var draggedRow = null;

    container.addEventListener("dragstart", function (e) {
      var handle = e.target.closest(".drag-handle");
      if (!handle) { e.preventDefault(); return; }

      draggedRow = handle.closest(".second-anchor-row");
      if (!draggedRow) { e.preventDefault(); return; }

      draggedRow.classList.add("dragging");
      if (e.dataTransfer) {
        e.dataTransfer.effectAllowed = "move";
        e.dataTransfer.setData("text/plain", draggedRow.id);
      }
    });

    container.addEventListener("dragover", function (e) {
      if (!draggedRow) return;
      e.preventDefault();

      var afterElement = getDragAfterElement(container, e.clientY);
      if (afterElement == null) {
        container.appendChild(draggedRow);
      } else {
        container.insertBefore(draggedRow, afterElement);
      }
    });

    container.addEventListener("drop", function (e) {
      e.preventDefault();
      if (draggedRow) draggedRow.classList.remove("dragging");
      draggedRow = null;
      window.setTimeout(function () { pushRowOrder(false); }, 0);
    });

    document.addEventListener("dragend", function () {
      if (draggedRow) draggedRow.classList.remove("dragging");
      draggedRow = null;
      window.setTimeout(function () { pushRowOrder(false); }, 0);
    });

    Shiny.addCustomMessageHandler("refresh_row_order", function (_msg) {
      window.setTimeout(function () { pushRowOrder(true); }, 0);
    });

    pushRowOrder(true);
  }

  function waitForRows() {
    if (document.getElementById("row_container") && window.Shiny) {
      initDraggableRows();
      pushRowOrder(true);
    } else {
      window.setTimeout(waitForRows, 100);
    }
  }

  document.addEventListener("DOMContentLoaded", waitForRows);
  window.addEventListener("load", waitForRows);

  document.addEventListener("shiny:connected", function () {
    waitForRows();
    window.setTimeout(function () { pushRowOrder(true); }, 100);
  });
})();'))
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
              actionButton("clearrows", "Remove all")
            ),
            tags$div(id = "row_container", make_row_inputs(1))
          ),
          tabPanel(
            "Output",
            br(),
            p("Adjust text size if necessary. End of x-axis should be specified if any windows are set to be indefinite.", style = "color:grey"),
            div(class = "aligned-row",
              div(class = "aligned-col",
                  selectInput("aratio", "Aspect ratio", choices = c("4:3" = 1, "16:9" = 2), selected = 1)
              ),
              div(class = "aligned-col",
                  numericInput("textsize", "Text size", value = 14, min = 4, max = 32)
              ),
              div(class = "aligned-col",
                  checkboxInput("hide_index_dates", "Remove first-order index dates", value = FALSE)
              )
            ),
            # X-axis settings
            div(class = "aligned-row",
              div(class = "aligned-col",
                  textInput("x_label", "X-axis label", value = "Time (days)")
              ),
              div(class = "aligned-col",
                  numericInput("xpad", "Padding (x-axis)", value = 50, min = 0, max = 300)
              ),
              div(class = "aligned-col",
                  numericInput("xlim_min", "Start of x-axis (optional)", value = NA)
              ),
              div(class = "aligned-col",
                  numericInput("xlim_max", "End of x-axis (optional)", value = NA)
              ),
              div(class = "aligned-col",
                  title = "Distance between x-axis ticks, e.g. 365 for yearly ticks. Ticks are placed at multiples of it (so day 0 is always one). Leave blank for automatic ticks.",
                  numericInput("x_tick_step", "Tick interval (optional)", value = NA, min = 0)
              ),
              div(class = "aligned-col",
                  checkboxInput("hide_x_labels", "Remove x-axis ticks", value = FALSE)
              )
            ),
            div(
              style = "display: flex; flex-wrap: wrap; gap: 8px; align-items: flex-start;",

              div(
                tags$label("\u00a0", style = "display: block;"),
                actionButton("save_project", "Download project file")
              ),

              div(
                style = "min-width: 180px;",
                fileInput("load_project", "Load project file", accept = ".json", width = "100%")
              ),

              div(
                tags$label("\u00a0", style = "display: block;"),
                actionButton("dl_pdf", "Print .pdf")
              ),

              div(
                tags$label("\u00a0", style = "display: block;"),
                actionButton("dl_svg", "Print .svg")
              )
            )
          )
        )
      ),
      column(
        5,
        br(),
        br(),
        # "auto" lets the container follow the (scaled) image's height, rather
        # than a fixed 400px the image would overflow onto the text below
        plotOutput("longdiag", height = "auto")
      )
    )
  ),

  tags$div(
    "This application is inspired by the work of ",
    a("Schneeweiss et al.,", href = "http://annals.org/article.aspx?doi=10.7326/M18-3079"),
    "please consider citing the original article when publishing a diagram generated by this application.", HTML("<br/>"),
    "This application is an extension of an application developed by Lars Christian Lund, available on ", a("Gitlab.", href = "https://source.coderefinery.org/lcl/repeat-diagrams"),
    HTML(license)
  )
)

# ---- Server -------------------------------------------------------------

server <- function(input, output, session) {

  is_restoring <- reactiveVal(FALSE)

  # Starts paused: the browser is asked for any saved state right after the
  # first flush, and the plot is only drawn once that has been answered (and
  # restored, if there was one) - not once with defaults and then again.
  plot_paused <- reactiveVal(TRUE)

  observeEvent(input$restore_inputs_applied, {
    is_restoring(FALSE)
    plot_paused(FALSE)
    session$sendCustomMessage("refresh_row_order", list())
  }, ignoreInit = TRUE)

  # Each dynamic row has an observer for its delete button. They're kept here
  # so they can be destroyed together with their row, instead of lingering
  # for the rest of the session.
  make_observer_registry <- function() {
    obs <- new.env(parent = emptyenv())
    drop <- function(id) {
      key <- as.character(id)
      if (exists(key, envir = obs, inherits = FALSE)) {
        obs[[key]]$destroy()
        rm(list = key, envir = obs)
      }
    }
    list(
      add      = function(id, o) assign(as.character(id), o, envir = obs),
      drop     = drop,
      drop_all = function() for (key in ls(obs)) drop(key)
    )
  }

  row_delete_obs   <- make_observer_registry()
  index_delete_obs <- make_observer_registry()

  # ---- Second-order anchor (interval) row management -----------------------
  #
  # `row_next_id` only ever increases: ids are never reused within a session,
  # so a delete-button observer registered for a given id can never collide
  # with a later row that happens to reuse the same id.
  # `row_order_ids` holds the ids of currently active rows, in visual/DOM order.

  row_next_id   <- reactiveVal(2)
  row_order_ids <- reactiveVal(1L)

  # Dynamically-inserted actionButtons can report their initial value (0)
  # to the server *after* this observer is registered, which `ignoreInit`
  # alone does not reliably suppress. Guarding explicitly on NULL/0 avoids
  # a delete firing spuriously the moment a row is created.
  register_row_delete <- function(id) {
    row_delete_obs$add(id, observe({
      val <- input[[paste0("delrow_", id)]]
      if (is.null(val) || val == 0) return()

      isolate({
        ids <- row_order_ids()
        if (length(ids) <= 1) return()  # always keep at least one row

        removeUI(selector = paste0("#row", id))
        row_delete_obs$drop(id)
        row_order_ids(ids[ids != id])
        session$sendCustomMessage("refresh_row_order", list())
      })
    }))
  }
  register_row_delete(1)

  parse_row_order <- function(ord_txt, valid_ids) {
    if (is.null(ord_txt) || !nzchar(ord_txt)) return(valid_ids)

    ord <- strsplit(ord_txt, ",", fixed = TRUE)[[1]]
    ord <- suppressWarnings(as.integer(ord))
    ord <- unique(ord[!is.na(ord) & ord %in% valid_ids])

    c(ord, setdiff(valid_ids, ord))
  }

  observeEvent(input$row_order_str, {
    ids <- row_order_ids()
    new_order <- parse_row_order(input$row_order_str, ids)

    if (!identical(as.integer(ids), as.integer(new_order))) {
      row_order_ids(new_order)
    }
  }, ignoreInit = FALSE)

  observeEvent(input$add, {
    new_id <- row_next_id()
    ids <- row_order_ids()

    insertUI(selector = "#row_container", where = "beforeEnd",
             ui = make_row_inputs(new_id, pos = length(ids) + 1))
    register_row_delete(new_id)

    row_next_id(new_id + 1)
    row_order_ids(c(ids, new_id))

    session$sendCustomMessage("refresh_row_order", list())
  })

  observeEvent(input$clearrows, {
    for (id in row_order_ids()) removeUI(selector = paste0("#row", id))
    row_delete_obs$drop_all()

    new_id <- row_next_id()
    insertUI(selector = "#row_container", where = "beforeEnd", ui = make_row_inputs(new_id))
    register_row_delete(new_id)

    row_next_id(new_id + 1)
    row_order_ids(new_id)

    session$sendCustomMessage("refresh_row_order", list())
  })

  # ---- First-order anchor (index date) management --------------------------

  index_next_id <- reactiveVal(2)
  index_ids     <- reactiveVal(1L)

  register_index_delete <- function(id) {
    index_delete_obs$add(id, observe({
      val <- input[[paste0("delidx_", id)]]
      if (is.null(val) || val == 0) return()

      isolate({
        ids <- index_ids()
        if (length(ids) <= 1) return()  # always keep at least one anchor

        removeUI(selector = paste0("#index_row_", id))
        index_delete_obs$drop(id)
        index_ids(ids[ids != id])
      })
    }))
  }
  register_index_delete(1)

  observeEvent(input$vlines, {
    new_id <- index_next_id()

    insertUI(selector = "#index_container", where = "beforeEnd", ui = make_index_inputs(new_id))
    register_index_delete(new_id)

    index_next_id(new_id + 1)
    index_ids(c(index_ids(), new_id))
  })

  observeEvent(input$clearv, {
    for (id in index_ids()) removeUI(selector = paste0("#index_row_", id))
    index_delete_obs$drop_all()

    new_id <- index_next_id()
    insertUI(selector = "#index_container", where = "beforeEnd", ui = make_index_inputs(new_id))
    register_index_delete(new_id)

    index_next_id(new_id + 1)
    index_ids(new_id)
  })

  # ---- Project state: collect / save / restore -----------------------------

  current_state <- reactive({
    req(input$textsize, input$xpad, input$aratio)

    row_ids <- row_order_ids()

    list(
      textsize = input$textsize,
      xpad = input$xpad,
      aratio = input$aratio,
      xlim_min = input$xlim_min,
      xlim_max = input$xlim_max,
      x_tick_step = input$x_tick_step,
      hide_x_labels = isTRUE(input$hide_x_labels),
      hide_index_dates = isTRUE(input$hide_index_dates),
      x_label = input$x_label,
      rows = lapply(seq_along(row_ids), function(pos) {
        x <- row_ids[[pos]]
        list(
          line = length(row_ids) - pos + 1,
          lbl1 = input[[paste0("lbl1_", x)]],
          lbl2 = input[[paste0("lbl2_", x)]],
          start = input[[paste0("start", x)]],
          end = input[[paste0("end", x)]],
          end_inf = input[[paste0("end_inf_", x)]],
          start_lbl = input[[paste0("start_lbl", x)]],
          end_lbl = input[[paste0("end_lbl", x)]],
          days_lbl = input[[paste0("days_lbl", x)]],
          boxcolor = input[[paste0("boxcolor", x)]],
          outbox = input[[paste0("outbox", x)]]
        )
      }),
      indices = lapply(index_ids(), function(x) {
        list(
          indexlbl1 = input[[paste0("indexlbl1_", x)]],
          indexlbl2 = input[[paste0("indexlbl2_", x)]],
          indexdatelbl = input[[paste0("indexdatelbl_", x)]],
          index = input[[paste0("index", x)]]
        )
      })
    )
  })

  # Browser storage is only written once edits settle
  collect_state <- debounce(current_state, 1000)

  session$onFlushed(function() {
    session$sendCustomMessage("request_cookie_state", list())
  }, once = TRUE)

  observeEvent(collect_state(), {
    if (is_restoring()) return()
    session$sendCustomMessage("save_state", collect_state())
  }, ignoreInit = TRUE)

  # The project file is built from the live (not debounced) state, so it
  # always includes the latest edit
  observeEvent(input$save_project, {
    session$sendCustomMessage("download_project", current_state())
  })

  # Parses and restores a project; never lets a bad file take the session
  # down. `from_storage` marks the state saved in the browser: if that's the
  # one that fails it's discarded, or it would fail again on every visit.
  restore_from_json <- function(txt, from_storage = FALSE) {
    tryCatch(
      restore_state(normalize_state(fromJSON(txt, simplifyVector = FALSE))),
      error = function(e) {
        is_restoring(FALSE)
        plot_paused(FALSE)
        if (from_storage) {
          session$sendCustomMessage("clear_saved_state", list())
          msg <- "Your previous session could not be restored, so the app has started fresh."
        } else {
          msg <- paste0("Could not load the project file: ", conditionMessage(e))
        }
        showNotification(msg, type = "error", duration = 10)
      }
    )
  }

  observeEvent(input$restore_state_upload, {
    req(input$restore_state_upload)
    restore_from_json(input$restore_state_upload)
  }, ignoreInit = TRUE)

  # Answered once at startup, with "" when nothing was saved
  observeEvent(input$restore_state_cookie, {
    if (!nzchar(input$restore_state_cookie)) {
      plot_paused(FALSE)
      return()
    }
    restore_from_json(input$restore_state_cookie, from_storage = TRUE)
  }, ignoreInit = FALSE)

  # `state` must already have been through normalize_state(), so every field
  # below is a single value of the right type.
  restore_state <- function(state) {
    if (isTRUE(is_restoring())) return()

    is_restoring(TRUE)
    plot_paused(TRUE)

    restore_success <- FALSE
    on.exit({
      if (!restore_success) {
        is_restoring(FALSE)
        plot_paused(FALSE)
      }
    }, add = TRUE)

    n_rows <- length(state$rows)
    n_idx  <- length(state$indices)

    updateNumericInput(session, "textsize", value = state$textsize)
    updateNumericInput(session, "xpad", value = state$xpad)
    updateNumericInput(session, "xlim_min", value = state$xlim_min)
    updateNumericInput(session, "xlim_max", value = state$xlim_max)
    updateNumericInput(session, "x_tick_step", value = state$x_tick_step)
    updateCheckboxInput(session, "hide_x_labels", value = state$hide_x_labels)
    updateCheckboxInput(session, "hide_index_dates", value = state$hide_index_dates)
    updateTextInput(session, "x_label", value = state$x_label)
    updateSelectInput(session, "aratio", selected = state$aratio)

    # Tear down all existing rows/indices, then rebuild with fresh ids so we
    # never reuse an id that might already have a delete-button observer.
    removeUI(selector = "#row_container > div", multiple = TRUE, immediate = TRUE)
    removeUI(selector = "#index_container > div", multiple = TRUE, immediate = TRUE)
    row_delete_obs$drop_all()
    index_delete_obs$drop_all()

    row_count <- max(1, n_rows)
    idx_count <- max(1, n_idx)

    new_row_ids <- seq(row_next_id(), length.out = row_count)
    row_next_id(max(new_row_ids) + 1)

    new_idx_ids <- seq(index_next_id(), length.out = idx_count)
    index_next_id(max(new_idx_ids) + 1)

    for (pos in seq_along(new_row_ids)) {
      x <- new_row_ids[pos]
      insertUI("#row_container", "beforeEnd", make_row_inputs(x, pos = pos), immediate = TRUE)
      register_row_delete(x)
    }
    for (x in new_idx_ids) {
      insertUI("#index_container", "beforeEnd", make_index_inputs(x), immediate = TRUE)
      register_index_delete(x)
    }

    row_order_ids(new_row_ids)
    index_ids(new_idx_ids)
    session$sendCustomMessage("refresh_row_order", list())

    session$onFlushed(function() {
      # Always hand control back (via restore_inputs_applied), even if
      # something here fails, so the plot can't stay paused for good.
      on.exit(session$sendCustomMessage("restore_inputs_applied", list()), add = TRUE)

      tryCatch({
        for (pos in seq_len(n_rows)) {
          x <- new_row_ids[pos]
          r <- state$rows[[pos]]

          lbl1_choices <- if (r$lbl1 %in% anchor2_choices) anchor2_choices else c(anchor2_choices, setNames(r$lbl1, r$lbl1))

          updateSelectizeInput(session, paste0("lbl1_", x),
            choices = lbl1_choices, selected = r$lbl1, options = list(create = TRUE))
          updateTextInput(session, paste0("lbl2_", x), value = r$lbl2)
          updateNumericInput(session, paste0("start", x), value = r$start)
          updateNumericInput(session, paste0("end", x), value = r$end)
          updateCheckboxInput(session, paste0("end_inf_", x), value = r$end_inf)
          updateTextInput(session, paste0("start_lbl", x), value = r$start_lbl)
          updateTextInput(session, paste0("end_lbl", x), value = r$end_lbl)
          updateTextInput(session, paste0("days_lbl", x), value = r$days_lbl)
          updateSelectInput(session, paste0("outbox", x), selected = r$outbox)
          updateSelectInput(session, paste0("boxcolor", x), choices = box_colors, selected = r$boxcolor)
        }

        for (pos in seq_len(n_idx)) {
          x <- new_idx_ids[pos]
          idx <- state$indices[[pos]]

          lbl1_choices <- if (idx$indexlbl1 %in% anchor1_choices) anchor1_choices else c(anchor1_choices, setNames(idx$indexlbl1, idx$indexlbl1))

          updateSelectizeInput(session, paste0("indexlbl1_", x),
            choices = lbl1_choices, selected = idx$indexlbl1, options = list(create = TRUE))
          updateTextInput(session, paste0("indexlbl2_", x), value = idx$indexlbl2)
          updateTextInput(session, paste0("indexdatelbl_", x), value = idx$indexdatelbl)
          updateNumericInput(session, paste0("index", x), value = idx$index)
        }
      }, error = function(e) {
        showNotification(paste0("Parts of the project could not be restored: ", conditionMessage(e)),
                         type = "warning", duration = 10)
      })
    }, once = TRUE)

    restore_success <- TRUE
  }

  # ---- Plot data --------------------------------------------------------
  #
  # Everything the diagram depends on, gathered in one place. This must never
  # throw: debounce() evaluates it inside an observer, where an error would
  # end the session. Incomplete input - a number box that's empty or holds a
  # lone "-" mid-typing (NA), or a just-added row whose inputs haven't arrived
  # yet (NULL) - is flagged via `valid` instead, and the last good diagram is
  # kept on screen until it's complete again.

  plot_inputs <- reactive({
    row_ids <- row_order_ids()
    idx_ids <- index_ids()

    num_in <- function(id) as_num1(input[[id]])
    chr_in <- function(id, default = "") as_chr1(input[[id]], default)

    rows <- data.frame(
      line = length(row_ids) - seq_along(row_ids) + 1,
      lbl = vapply(row_ids, function(x) chr_in(paste0("lbl1_", x)), character(1)),
      lbl2 = vapply(row_ids, function(x) chr_in(paste0("lbl2_", x)), character(1)),
      outside_box = vapply(row_ids, function(x) chr_in(paste0("outbox", x), "0"), character(1)),
      start = vapply(row_ids, function(x) num_in(paste0("start", x)), numeric(1)),
      end = vapply(row_ids, function(x) num_in(paste0("end", x)), numeric(1)),
      end_inf = vapply(row_ids, function(x) isTRUE(input[[paste0("end_inf_", x)]]), logical(1)),
      start_lbl = vapply(row_ids, function(x) chr_in(paste0("start_lbl", x)), character(1)),
      end_lbl = vapply(row_ids, function(x) chr_in(paste0("end_lbl", x)), character(1)),
      days_lbl = vapply(row_ids, function(x) chr_in(paste0("days_lbl", x)), character(1)),
      color = vapply(row_ids, function(x) chr_in(paste0("boxcolor", x), box_colors[[1]]), character(1)),
      stringsAsFactors = FALSE
    )

    indices <- data.frame(
      vlines = vapply(idx_ids, function(x) num_in(paste0("index", x)), numeric(1)),
      indexlabel = vapply(idx_ids, function(x) chr_in(paste0("indexlbl1_", x)), character(1)),
      indexlabel2 = vapply(idx_ids, function(x) chr_in(paste0("indexlbl2_", x)), character(1)),
      datelabel = vapply(idx_ids, function(x) chr_in(paste0("indexdatelbl_", x), "Day"), character(1)),
      stringsAsFactors = FALSE
    )

    txt_size <- as_num1(input$textsize) - 2
    addpad   <- as_num1(input$xpad)

    list(
      rows = rows,
      indices = indices,
      txt_size = txt_size,
      addpad = addpad,
      xlim_min = as_num1(input$xlim_min),
      xlim_max = as_num1(input$xlim_max),
      x_tick_step = as_num1(input$x_tick_step),
      hide_x_labels = isTRUE(input$hide_x_labels),
      hide_index_dates = isTRUE(input$hide_index_dates),
      x_label = as_chr1(input$x_label, "Time (days)"),
      valid = all(is.finite(rows$start)) &&
        all(is.finite(rows$end) | rows$end_inf) &&
        all(is.finite(indices$vlines)) &&
        is.finite(txt_size) && txt_size > 0 &&
        is.finite(addpad)
    )
  })

  build_plot <- function(args) {
    gen_long_diag(
      args$rows,
      args$indices,
      txt_size = args$txt_size,
      addpad = args$addpad,
      xlim_min = args$xlim_min,
      xlim_max = args$xlim_max,
      x_tick_step = args$x_tick_step,
      hide_x_labels = args$hide_x_labels,
      hide_index_dates = args$hide_index_dates,
      x_label = args$x_label
    )
  }

  # The on-screen preview waits for typing to settle before redrawing, so a
  # burst of edits costs one render rather than one per keystroke
  debounced_plot_inputs <- debounce(plot_inputs, 400)

  plt <- reactive({
    args <- debounced_plot_inputs()
    # Also skip a value the inputs have already moved on from (e.g. when the
    # plot is unpaused right after a restore): the debounce is about to
    # deliver the settled one, so drawing this would just be thrown away.
    req(args$valid, identical(args, isolate(plot_inputs())), cancelOutput = TRUE)
    build_plot(args)
  })

  # 4:3 vs 16:9 export/preview dimensions, shared by the plot preview and both
  # downloads.
  export_dims <- reactive({
    if (identical(as.character(input$aratio), "1")) c(w = 12, h = 9) else c(w = 16, h = 9)
  })

  # `width`/`height` below are in pixels, at 50 pixels per declared inch.
  # `res` must match that same 50 px/inch so the *effective* canvas size
  # (width_px / res inches) equals the PDF/SVG's actual inches - otherwise
  # point-sized text ends up disproportionately large in the browser (the
  # default res is 72, which made the effective canvas ~30% smaller than
  # intended, and text overflow the plot as a result).
  output$longdiag <- renderPlot({
    req(!isTRUE(plot_paused()), cancelOutput = TRUE)
    plt()
  }, width = function() export_dims()[["w"]] * 50, height = function() export_dims()[["h"]] * 50,
     res = 50)

  # PDF/SVG are only built when their button is clicked (building them on
  # every edit tripled the rendering work), from the live inputs so the file
  # always matches the latest edit.
  send_export <- function(device, mime) {
    if (isTRUE(plot_paused())) return()

    args <- plot_inputs()
    if (!isTRUE(args$valid)) {
      showNotification("Fill in all start, end and index dates (and the text size) before exporting.",
                       type = "warning")
      return()
    }

    dims <- export_dims()
    tmp <- tempfile(fileext = paste0(".", device))
    on.exit(unlink(tmp), add = TRUE)

    ok <- tryCatch({
      ggsave(tmp, plot = build_plot(args), width = dims[["w"]], height = dims[["h"]],
             units = "in", device = device)
      TRUE
    }, error = function(e) {
      showNotification(paste0("Export failed: ", conditionMessage(e)), type = "error")
      FALSE
    })
    if (!ok) return()

    session$sendCustomMessage("download_file", list(
      filename = paste0("diagram.", device),
      mime = mime,
      base64 = base64encode(tmp)
    ))
  }

  observeEvent(input$dl_pdf, send_export("pdf", "application/pdf"))
  observeEvent(input$dl_svg, send_export("svg", "image/svg+xml"))
}

shinyApp(ui = ui, server = server)
