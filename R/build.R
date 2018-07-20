#' buildTaipan
#'
#' @param questions a taipan Questions list of scene and selection questions
#' @param images a vector of image locations, can be local or URLs
#' @param appdir location to export the completed app
#' @param launch launch the app from the new directory after build is completed
#' @param overwrite replace the contents of the supplied location with the completed app
#' @param additionalHelp allow for contextual help on the survey as a modal dialog.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(taipan)
#'
#' questions <- taipanQuestions(
#'   scene = div(radioButtons("graphic", label = ("2D Graphic"),
#'                            choices = list("Live image", "2D Graphic")),
#'               radioButtons("bg", label = ("Background"),
#'                            choices = list("Crowd",
#'                                           "Court", "Logo wall", "Not applicable")),
#'               radioButtons("person", label = ("Detectable Person"),
#'                            choices = list("Yes", "No")),
#'               radioButtons("shotangle", label = ("Shot angle"),
#'                            choices = list("Level with players",
#'                                           "Birds eye",
#'                                           "Upward angle")),
#'               radioButtons("situation", label = ("Situation"),
#'                            choices = list("Court in play",
#'                                           "Court player close-up",
#'                                           "Court close-up not player",
#'                                           "Crowd",
#'                                           "Off court close up of player",
#'                                           "Transition"))),
#'   selection = div(radioButtons("detect", label = ("Detect Face"),
#'                                choices = list("Player" ,
#'                                               "Other staff on court", "Fan", "None")),
#'                   radioButtons("obscured", label = ("Face obscured"),
#'                                choices = list("Yes", "No")),
#'                   radioButtons("lighting", label = ("Lighting"),
#'                                choices = list("Direct sunlight", "Shaded", "Partially shaded")),
#'                   radioButtons("headangle", label = ("Head angle"),
#'                                choices = list("Front on", "Back of head",
#'                                               "Profile", "Other")),
#'                   radioButtons("glasses", label = ("Glasses"),
#'                                choices = list("Yes", "No")),
#'                   radioButtons("visorhat", label = ("Visor/hat"),
#'                                choices = list("Yes", "No")))
#' )
#'
#' additionalHelp <-  paste(h4("Program Specific Help"), br(),
#' p("This is some specific text, about something specific to your application"), br(),
#' br(),
#' "For more information on how to use this tool, contact either information-at-information.com or
#'                  John.Smith-at-information.com")
#'
#'
#' buildTaipan(
#'   questions = questions,
#'   "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/2016_CT6_R01_CGarcia_FRA_vs_BStrycova_CZE_WS145_clip.0015.png",
#'   file.path(tempdir(), "taipan")
#' )
#'
#' }
#'
#' @importFrom shiny runApp
#' @importFrom downloader download
#'
#' @export

buildTaipan <- function(questions, images, appdir, launch = TRUE, overwrite = FALSE, additionalHelp = NULL){
  # images <- tools::file_path_as_absolute(images)
  if(!inherits(questions, "taipanQuestions")){
    stop("Questions must be created using the taipanQuestions() function.")
  }

  if(overwrite){
    message(paste0("Are you sure you want to overwrite '", appdir, "'? All files in this folder will be deleted!\nYes: Delete ALL of these files!\nNo: Keep it the way it is!"))
    auth <- readline()
    if(toupper(auth)!="YES"){
      message("Aborted building of taipan app.")
      return(invisible(NULL))
    }
    unlink(appdir, recursive = TRUE)
  }
  if(dir.exists(appdir)){
    if(length(list.files(appdir))>0){
      stop(sprintf('Output appdir "%s" already exists, please provide a different location to save app',
                   appdir))
    }
  }
  else{
    dir.create(appdir)
  }
  appdir <- tools::file_path_as_absolute(appdir)

  if (is.null(additionalHelp)) {
    additionalHelp="There is no help specific for this project"
  }



  # WRITE APPDIR
  app_files <- list.files(file.path(system.file(package="taipan"), "app"))
  file.copy(file.path(system.file(package="taipan"), "app", app_files), appdir, recursive = TRUE)

  # SAVE QUESTIONS
  saveRDS(questions, file = file.path(appdir, "data", "questions.rds"))

  # SAVE HELP
  saveRDS(additionalHelp, file = file.path(appdir, "data", "additionalHelp.rds"))

  # CONSTRUCT IMAGE DIR
  if(dir.exists(images)){
    images <- list.files(images, full.names = TRUE)
  }
  img_success <- file.copy(images, file.path(appdir, "www", "app_images", basename(images)))
  if(any(!img_success)){
    download(images[!img_success], file.path(appdir, "www", "app_images", basename(images)), mode = "wb")
  }

  # LAUNCH APP
  if(launch){
    runApp(appdir)
  }
}

