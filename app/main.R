box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags,
        uiOutput, fluidRow, column, h2, h5, br, hr, htmlOutput,
        verbatimTextOutput, textOutput, textInput, actionButton,
        reactiveValues, observeEvent, renderText, tagList, fluidPage,
        isolate,],
  shinyjs[useShinyjs, extendShinyjs, ],
  shinyalert[useShinyalert, shinyalert,],
  tidyverse[...],
  shinythemes[shinytheme,],
)
jsCode <- "
// send message on enter
jQuery(document).ready(function(){
  jQuery('#text_msg').keypress(function(evt){
    if (evt.keyCode == 13){
      // Enter, simulate clicking send
      jQuery('#app-send').click();
      jQuery('#text_msg').html('hihihi');
    }
  });
})

// auto scroll to bottom
var oldContent = null;
window.setInterval(function() {
  var elem = document.getElementById('app-chat_window');
  if (oldContent != elem.innerHTML){
    scrollToBottom();
  }
  oldContent = elem.innerHTML;
}, 300);

// Scroll to the bottom of the chat window.
function scrollToBottom(){
  var elem = document.getElementById('app-chat_window');
  elem.scrollTop = elem.scrollHeight;
}"

intro <- "
##      ## ######## ##        ######   #######  ##     ## ########
##  ##  ## ##       ##       ##    ## ##     ## ###   ### ##
##  ##  ## ##       ##       ##       ##     ## #### #### ##
##  ##  ## ######   ##       ##       ##     ## ## ### ## ######
##  ##  ## ##       ##       ##       ##     ## ##     ## ##
##  ##  ## ##       ##       ##    ## ##     ## ##     ## ##
 ###  ###  ######## ########  ######   #######  ##     ## ########
"

val <- reactiveValues(txt = NULL, users = c(), new_usr = NULL, usr_left = NULL)

if (file.exists("chat_txt.Rds")) {
  val$txt <- readRDS("chat_txt.Rds")
} else {
  val$txt <- intro
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    theme = shinytheme("superhero"),
    shinyjs::useShinyjs(),
    useShinyalert(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    extendShinyjs(text = jsCode, functions = c()),
    fluidRow(
      column(width = 6, h2("Anonymous chat")),
      column(width = 6, align = "right", br(), htmlOutput("logged_usr"))
    ),
    div(
      style = "height: 500px;",
      fluidRow(
        column(
          width = 9,
          verbatimTextOutput(ns("chat_window")),
          tags$head(tags$style("")),
          uiOutput(ns("notify"))
        ),
        column(width = 3, h5("Active Users"), hr(), textOutput("users"))
      )
    ),
    fluidRow(
      column(
        width = 9,
        textInput(
          "text_msg", "", value = "", width = "100%",
          placeholder = "Enter your message"
        ),
      ),
      column(
        width = 3,
        br(),
        actionButton("send", "Send", width = "100%")
      )
    )
  )
}

#' @export
server <- function(id) {

  moduleServer(id, function(input, output, session) {
    # renaming your user name ----
    observeEvent("", {
      username <- paste0("Username")
      shinyalert(
        inputId = "username",
        "Welcome to Anonymous Chat",
        html = TRUE,
        text = tagList(
          textInput("uname", "Please rename yourself", value = username),
        ),
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        showCancelButton = FALSE,
        showConfirmButton = TRUE
      )
    })
    #
    # to display new user joined ----
    observeEvent(input$username, {
      val$users <- c(val$users, input$uname)
      new_usr <- paste0("New user joined: ", input$uname)
      print(new_usr)
      print(val$users)
    })

    output$users <- renderText({
      paste(val$users, collapse = '\n')
    })
    # ----

    output$logged_usr <- renderText({
      paste("<b>", input$uname, "</b>")
    })

    # sending msg ----
    observeEvent(input$send, {
      # if the txt msg is empty
      if (input$text_msg == "") {
        shinyalert(
          "Oops!",
          "Can't send a blank message",
          type = "error",
          closeOnEsc = TRUE,
          timer = 3000,
          closeOnClickOutside = TRUE,
          showCancelButton = FALSE,
          showConfirmButton = TRUE
        )
      } else {
        if (object.size(val$txt) > 50000) {
          val$txt <- intro
        }
        new <-
          paste(Sys.time(), "#", input$uname, ":", input$text_msg)

        val$txt <- paste(val$txt, new, sep = '\n')

        updateTextInput(session, "text_msg", value = "")

        saveRDS(val$txt, "chat_txt.Rds")
      }
    })

    output$chat_window <- renderText({
      val$txt
    })
    # ----

    # update the active user list on exit ----
    session$onSessionEnded(function() {
      isolate({
        val$users <- val$users[val$users != input$uname]
        usr_left <- paste0("User: ", input$uname, " left the room")
        print(usr_left)
        print(val$users)
      })
    })
    # ----
  })
}
