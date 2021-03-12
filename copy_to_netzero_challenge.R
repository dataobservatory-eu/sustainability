

netzero_dir <- file.path(
  "C:/Users/Daniel Antal/OneDrive - Visegrad Investments/2021 Project", 
  "netzero" )



netzero_img_assets <- file.path( netzero_dir, "assets", "media", "img")
dir ( netzero_img_assets)

file.exists( file.path(netzero_dir, "content", "post", "2021-03-04_retroharmonize_intro", "Introduction_to_retroharmonize.md") )

copy_plots <- function ( x ) {
  file.copy ( from = file.path("plots", x), 
              to = file.path(netzero_img_assets, x), 
              overwrite = TRUE)
}


vapply ( dir("plots")[ grepl( "\\.png", dir("plots"))], copy_plots, logical(1))


file.copy ( from = "Introduction_to_retroharmonize.md", 
            to = file.path(netzero_dir, "content", "post", "2021-03-04_retroharmonize_intro", "Introduction_to_retroharmonize.md"), 
            overwrite = TRUE)

file.copy ( from = dir("plots"), 
            to )

yaml <- read.delim ( "yaml/2021-03-04.txt", header=FALSE)
md = read.delis("Introduction_to_retroharmonize.md", header = FALSE)
write.table(, file = "test.md",row.names=FALSE, na="", sep = " ",col.names=FALSE)
fileConn <-file("test.md")
writeLines(c( yaml$V1, md$V1), fileConn)
close(fileConn)
readline(fileConn )
