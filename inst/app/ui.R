library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "taipan"
    ),
    dashboardSidebar(
      disable = TRUE
    ),
    dashboardBody(
      box(
        title = "Image",
        imageOutput("out_img",
                    click = clickOpts(id = "img_click"),
                    dblclick = dblclickOpts(id = "img_dblclick"),
                    brush = brushOpts(id = "img_brush")),
        width = 12,
        status = "primary",
        collapsible = TRUE
      ),
      uiOutput("ui_questions"),
      actionLink(
        "btn_prev",
        box(
          "Previous",
          width = 3,
          background = "green"
        )
      ),
      column(1),
      uiOutput("ui_saveSelection"),
      column(1),
      actionLink(
        "btn_next",
        box(
          "Next",
          width = 3,
          background = "green"
        )
      )
    )
  )
)