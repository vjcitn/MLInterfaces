hclustWidget = function(mat, featureName="feature", 
    title=paste0("hclustWidget for ", deparse(substitute(mat))),
     minfeats=2, auxdf=NULL) {
#
# software that defines interactive browser interface
# to approaches to clustering the rows of a matrix
#
#
# to use: library(MLInterfaces); run(hclustWidget(mat))
#
 shinyApp(ui = fluidPage(
  fluidRow( column(6, textOutput("title", container=h1)),
            column(2, actionButton("btnSend", "Stop widget"))),
  fluidRow(
   column(2,  numericInput("ngenes", label = paste0("N ", featureName, "s"), 
     minfeats, min = minfeats, max = ncol(mat))),
   column(2,  selectInput("distmeth", label = "Distance method:",
               choices = c("euclidean", "maximum", "manhattan",
               "binary"), selected = "euclidean")),

   column(2,  selectInput("fusemeth", label = "Agglomeration method:",
               choices = c("complete", "average", "ward.D2", "single",
                   "median", "centroid"), selected="complete")),
   column(2,  numericInput("numclus", label = "K:", 2, min = 1, max = nrow(mat)/2))
          ),
  fluidRow(column(7, plotOutput("tree"))),
  fluidRow(column(7, ggvis::ggvisOutput("pcp")))
 ), server= function(input, output, session) {
    output$title <- renderText(title)
    output$tree <- renderPlot({
dm = dist(mat[,seq_len(input$ngenes)], method=input$distmeth)
sink(tempfile())
cb <- fpc::clusterboot(dm, clustermethod=fpc::hclustCBI, method=input$fusemeth, k=input$numclus, showplots=FALSE, scaling=FALSE)
sink(NULL)
      dend = hclust( dm, method=input$fusemeth )
      par(mar=c(3,3,3,1))
      plot(dend, main=paste0("Boot. Jacc. at k=", input$numclus, ": ",
        paste(round(cb$bootmean,2), collapse=", ")), xlab=" ")
    })
    P1 <- reactive({
           all_values <- function(x) {
             if(is.null(x)) return(NULL)
             row <- pcdf[pcdf$rowid == x$rowid, ]
             paste0(names(row), ": ", format(row), collapse = "<br />")
           }

      pc = prcomp(mat[,seq_len(input$ngenes)])$x
      dm = dist(mat[,seq_len(input$ngenes)], method=input$distmeth)


      dend = hclust( dm, method=input$fusemeth )
      ct = cutree(dend, k=input$numclus)
      pcdf = data.frame(PC1=pc[,1], PC2=pc[,2], #tiss=pData(tiss)$Tissue,
         rowid=seq_len(nrow(pc)), assigned=factor(ct))
      if (!is.null(auxdf)) {
         if ((nrow(auxdf) == nrow(pcdf))) pcdf = cbind(pcdf, auxdf)
           else message("nrow(auxdf) != nrow(mat), ignoring auxdf")
         }
      pcdf %>% ggvis::ggvis(~PC1, ~PC2, key := ~rowid, fill = ~assigned) %>% ggvis::layer_points() %>%
               ggvis::add_tooltip(all_values, "hover") 
      }) 
      P1 %>% ggvis::bind_shiny("pcp")
      observe({
         if (input$btnSend > 0)
            isolate({
                stopApp(returnValue = 0)
                })
         })       
} )
}
