#' variant_annoter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @importFrom shinyWidgets closeSweetAlert progressSweetAlert updateProgressBar ask_confirmation pickerInput sendSweetAlert
#' @importFrom shiny NS tagList 
mod_variant_annoter_ui <- function(id, modal  = NULL, reactiveValues = NULL, conn = con){
  ns <- NS(id)
  req(reactiveValues$my_variant_id)
  if (modal == FALSE){uiOutput(ns("ui"))}
}
    
#' variant_annoter Server Functions
#'
#' @noRd 
mod_variant_annoter_server <- function(id, conn = NULL, modal = FALSE, reactiveValues = NULL,reload =  reactiveValues(value = 0)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    print("entering mod_variant_annoter_server")
    reloadinside <- reactiveValues( value = 0 )
    colors <- paste0("color:",c('gray','green','blue','black','orange','red','purple'),";")
    colors <- paste0(colors,"font-family: Arial;")
    colors <- paste0(colors,"font-weight: bold;")
    output$nocommenttext <- renderText({"No comment for this variant yet"})
    observeEvent(c(reactiveValues$my_variant_id,reloadinside$value),{
      req(reactiveValues$my_variant_id)
      print("rendering variant annoter UI ")
      VKB <- unique(dbGetQuery(con, paste0("SELECT VKB from variant_impact WHERE variant_id = '",reactiveValues$my_variant_id,"'"))$VKB)
      comments_table <- DBI::dbReadTable(conn = con, name="variant_comments") %>% 
        dplyr::filter(variant_id == reactiveValues$my_variant_id) %>%
        mutate(delete = case_when(user == Sys.getenv("SHINYPROXY_USERNAME") ~ paste0('<button id="delete_com_',
                                                                                     com_id,
                                                                                      '" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;',ns("godeletecom"),'&quot;,  this.id, {priority: &quot;event&quot;})"><i class="fa fa-trash"></i>',
                                                                                      '</button>')))
      output$comments <- DT::renderDataTable(DT::datatable(comments_table,
                                                           rownames = FALSE, escape = FALSE,
                                                           options = list(
                                                              columnDefs = list(list(visible=FALSE, targets=c(0))))))
      output$ui <- renderUI({
        tagList(
          column(width = 4,
                 pickerInput(inputId = ns('annotate'),"Select a category to the attribute to this variant",
                             multiple = F, width = "100%", 
                             selected = VKB,
                             choices = c("PossibleArtifact","Benign","LikelyBenign","UncertainSignificance","LikelyPathogenic","Pathogenic","Unknown"),
                             choicesOpt = list(style = colors),
                 ),actionButton(ns("okVKB"), "Update VKB classification",width = "100%")),
          column(width = 8,textAreaInput(inputId = ns("annotatecom"),label = "Write a commentary about the variant",placeholder = "my commentary",width  = "100%",
                                         value = "my commentary"),actionButton(ns("okComment"), "Add comment on variant"),
                 br(),br(),
                 if(nrow(comments_table) >= 1) {DT::dataTableOutput(ns("comments"))} else {verbatimTextOutput(ns("nocommenttext"))},br()
          )
        )
      })
    })
    
    observeEvent(input$godeletecom,{
      req(reactiveValues$my_variant_id)
      com_id <- paste0(stringr::str_split(input$godeletecom, pattern = "_")[[1]][c(3)],collapse = "_")
      progressSweetAlert(session = session, id = "Just_to_restore_reactivity_[Tricky]",title = "",display_pct = TRUE, value = 9)
      closeSweetAlert(session = session)
      showModal(modalDialog(size = "l",
        title = "Are you sure ??", 
        "Following commentary will be removed from base :",br(),
        DT::renderDataTable(DT::datatable(dbGetQuery(con, paste0("SELECT variant_id, user, date, comment FROM variant_comments WHERE com_id = '",com_id,"';")) %>% 
                                            filter(variant_id == reactiveValues$my_variant_id),rownames = FALSE)),
        footer = tagList(
          modalButton("No"),
          actionButton(ns("deletecomconf"), "Yes")
        )))
    })
    observeEvent(input$deletecomconf, {
      req(input$godeletecom)
      req(input$deletecomconf)
      removeModal()
      com_id <- paste0(stringr::str_split(input$godeletecom, pattern = "_")[[1]][c(3)],collapse = "_")
      print(com_id)
      dbSendQuery(con, paste0("DELETE FROM variant_comments WHERE com_id = '",com_id,"';"))
      #dbSendQuery(con, paste0("DELETE FROM variant_comments WHERE com_id = '",com_id,"' AND variant_id = '",reactiveValues$my_variant_id,"';"))
      reloadinside$value <- reloadinside$value +1
      sendSweetAlert(session = session,title = "Database updated with sucess !",text = "comment correctly removed",type = "success")
    })
    
    if (modal == TRUE){
      observeEvent(reactiveValues$launchmodal,{
                  req(reactiveValues$my_variant_id)
                  print(reactiveValues$my_variant_id)
                  VKB <- unique(dbGetQuery(con, paste0("SELECT VKB from variant_impact WHERE variant_id = '",reactiveValues$my_variant_id,"'"))$VKB)
                  comments_table <- DBI::dbReadTable(conn = con, name="variant_comments") %>% 
                    dplyr::filter(variant_id == reactiveValues$my_variant_id) %>%
                    mutate(delete = case_when(user == Sys.getenv("SHINYPROXY_USERNAME") ~ paste0('<button id="delete_com_',
                                                                                                 com_id,
                                                                                                 '" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;',ns("godeletecom"),'&quot;,  this.id, {priority: &quot;event&quot;})"><i class="fa fa-trash"></i>',
                                                                                                 '</button>')))
                   output$comments <- DT::renderDataTable(DT::datatable(comments_table,
                                                                       rownames = FALSE, escape = FALSE,
                                                                       options = list(
                                                                         columnDefs = list(list(visible=FALSE, targets=c(0))))))
                   showModal(session = session,modalDialog(size = 'l',
                           title = paste0("Annotate ", reactiveValues$my_variant_id ," in your database"),
                           uiOutput(ns("ui"))))
                  })
    }

    observeEvent(input$okVKB,{
      if (modal == TRUE){ removeModal()}
      progressSweetAlert(session = session, id = "Just_to_restore_reactivity_[Tricky]",title = "",display_pct = TRUE, value = 9)
      closeSweetAlert(session = session)
      ask_confirmation(
        inputId = ns("annotationconfVKB"), type = "warning",
        title = "Are you sure ??", "Previous annotation state will be overriden",
        btn_labels = c("No", "Yes"),
        btn_colors = c("#FE642E", "#04B404"))
    })
    observeEvent(input$okComment,{
      if (modal == TRUE){removeModal()}
      print(input$okComment)
      progressSweetAlert(session = session, id = "Just_to_restore_reactivity_[Tricky]_Comments",title = "",display_pct = TRUE, value = 9)
      closeSweetAlert(session = session)
      ask_confirmation(inputId = ns("annotationconfComment"), type = "warning",
                       title = "Are you sure ??", "Previous annotation state will be overriden",
                       btn_labels = c("No", "Yes"),
                       btn_colors = c("#FE642E", "#04B404"))
    })
    
    observeEvent(input$annotationconfVKB,{
      req(input$annotate)
      req(input$annotationconfVKB)
      if(input$annotationconfVKB ==  TRUE){
        # Check that data object exists and is data frame.
        apn_sql <- paste0("UPDATE variant_impact SET VKB = '",input$annotate,"' WHERE variant_id = '",reactiveValues$my_variant_id,"';")
        dbSendQuery(con, apn_sql)
        sendSweetAlert(session = session,title = "Database updated with sucess !",text = paste0(reactiveValues$my_variant_id , " annotation correctly overwritten"),type = "success")
        reload$value <- reload$value + 1
        reloadinside$value <- reloadinside$value + 1 
      } else {
        sendSweetAlert(session = session, cancelOnDismiss = TRUE,title = "Variant annotation has been canceled",type = "info")
      }
    })
    
    observeEvent(input$annotationconfComment,{
      req(input$annotatecom)
      req(input$annotationconfComment)
      if(input$annotationconfComment ==  TRUE){
        # Check that data object exists and is data frame.
        new_comment <- data.frame(com_id = dbGetQuery(con, "SELECT COUNT(*) FROM variant_comments;")[1,1] + 1 ,
                                  variant_id = reactiveValues$my_variant_id,
                                  comment = HTML(gsub("\n","<br/>",input$annotatecom)),
                                  user = Sys.getenv("SHINYPROXY_USERNAME"), 
                                  date = format(Sys.Date()," %d/%m/%Y"))
        DBI::dbWriteTable(conn = con, name="variant_comments", new_comment, append = TRUE)
        sendSweetAlert(session = session,title = "Database updated with sucess !",text = paste0(reactiveValues$my_variant_id , " annotation correctly overwritten"),type = "success")
        #reload$value <- reload$value + 1
        reloadinside$value <- reloadinside$value + 1 
      } else {
        sendSweetAlert(session = session,cancelOnDismiss = TRUE, title = "Variant annotation has been canceled",type = "info")
      }
    })
  })
}

