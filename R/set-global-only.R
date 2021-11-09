
# global options for R
options(
  formatR.indent = 2, width = 55, 
  digits = 2,scipen=999, tinytex.verbose = TRUE,
  knitr.kable.NA = '',
  # html widget for xaringan slide
  htmltools.dir.version = FALSE, 
  #htmltools.preserve.raw = T,
  #widgetframe_widgets_dir = 'widgets',
  echo=FALSE, warning=FALSE, message=FALSE,comment="")

# global options for knitr
knitr::opts_chunk$set(fig.align='center',echo = FALSE,
                      message = FALSE,comment="",
                      # html widget for docx,
                      screenshot.force = knitr::pandoc_to("docx"),
                      #widgetframe_widgets_dir = 'widgets',
                      fig.width=11, fig.height=5.5) # Places figures on their own pages

# global options for DT
options(DT.options = list(dom ="t" ,  # pure table with no search blank
                          columnDefs = list(
                            list(className = "dt-center", targets = "_all"), # align center
                            list(visible=FALSE,targets=0) # hide index column
                            )
                          )
        )

# global options for servr pkg

options(servr.interval = 0.5) # control time to refresh the preview
options(servr.daemon = TRUE) # unlock thread when infinite moon render

