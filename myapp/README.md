# Longitudinal study-design diagram builder

## Files

```
app.R                 UI + server (Shiny) — single file, CSS/JS inline as in the original
make-diagram.R        gen_long_diag(): builds the ggplot from two data frames
helpers.R             shared constants + pure helper functions (no shiny/ggplot2)
tests/test-helpers.R   smoke tests for helpers.R, run with: Rscript tests/test-helpers.R
```

Run the app as usual from the folder containing `app.R` (e.g. `shiny::runApp()` or
the "Run App" button in RStudio).

## What changed since the first pass

Two things were broken and are now fixed:

- **CSS/JS had moved to a `www/` folder**, referenced via `href="styles.css"` /
  `src="app.js"`. That's a real Shiny pattern, but it depends on the `www/`
  folder being placed exactly right next to `app.R`, and it wasn't loading —
  which is exactly why the second-order anchor rows and the Output tab
  rendered as an unstyled, vertically-stacked list (the flexbox layout lives
  entirely in that stylesheet). Reverted to inlining the CSS/JS directly in
  `app.R` via `tags$style(HTML(...))` / `tags$script(HTML(...))`, matching
  the original single-file app exactly — no separate folder to get wrong.

- **The overlap-highlight layer** was added to the plot conditionally
  (`if (nrow(overlap_df) > 0) geom_rect(...)`, relying on `ggplot + NULL`
  being a no-op when there's nothing to highlight). I can't run
  shiny/ggplot2 in my sandbox to confirm that behaves the way I expect on
  your installed version, so I replaced it with a `geom_rect()` that's
  always added — drawing a zero-row data frame is unambiguously a no-op in
  ggplot2, so this removes that risk entirely regardless of version.

If the plot still doesn't render after these, please paste the actual error
text from the R console (not just the browser) — "errors loading packages"
could be a real problem with one of `svglite`/`textshaping`/`ggfittext` in
your R environment, which I have no way to reproduce or diagnose without
seeing the message, and which the app's `library(...)` calls are unchanged
from your original file.

## Everything else from the first pass, unchanged

- `box_colors`, anchor choice lists, and legacy numeric-color handling now
  live once in `helpers.R` as `resolve_box_color()`, instead of being
  duplicated in `app.R` and `make-diagram.R`.
- Both "First-order anchors" and "Second-order anchors" tabs have a small
  "✕" delete button on each row (replacing the old "Remove anchor"
  remove-last button on both tabs).
- First-order anchors are drawn with width scaled to the plotted time range,
  and any second-order window whose drawn extent includes that day gets a
  highlighted overlap marker.
- Row/anchor bookkeeping uses an ever-increasing id counter + a vector of
  active ids, and ids are never reused within a session (including after
  loading a saved project).
- Fixed an id collision between the first-order anchor row's wrapper `<div>`
  and its "Index date" numeric input (both were `index{n}`).
- Fixed the PDF/SVG export ignoring the "Aspect ratio" (4:3 / 16:9) setting;
  the on-screen preview now matches it too.
- Removed a dead `observeEvent(input$restore_state, ...)` block nothing ever
  triggered, and a duplicated `x_range` calculation that silently overwrote
  itself with a different value partway through `gen_long_diag()`.
