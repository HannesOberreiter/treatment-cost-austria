# simple function to save images
fSaveImages <- function(currentplot, filename, w = 7.5, h = 4) {
    ggsave(paste0("output/figs/", filename, ".pdf"), currentplot, width = w, height = h)
    ggsave(paste0("output/figs/", filename, ".png"), currentplot, width = w, height = h, dpi = 320)
    invisible(currentplot)
}