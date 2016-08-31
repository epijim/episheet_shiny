
shinyServer(function(input, output) {
  
  makeplot <- reactive({
    jb_plotfunction(
      est1.ll = input$est1.ll,
      est1.ul = input$est1.ul,
      xlabel = input$xlabel,
      citype = input$citype,
      labelsize = input$labelsize,
      referencewidth = input$referencewidth,
      functionwidth = input$functionwidth
    )
  })
  
  output$theplot <- renderPlot({
    makeplot()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste('pvalue-plot', sep = '.', switch(
        input$filetype, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      src <- normalizePath('pvalueplot.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'pvalueplot.Rmd', overwrite = TRUE)

      library(rmarkdown)
      out <- render('pvalueplot.Rmd', switch(
        input$filetype,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
})
