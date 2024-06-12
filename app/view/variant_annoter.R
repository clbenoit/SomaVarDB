#app/view/variant_annoter.R

box::use(
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel, 
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, renderText, verbatimTextOutput,
        sliderInput, req, numericInput, selectInput, selectizeInput, observeEvent,
        actionButton,  textAreaInput, modalDialog, modalButton, 
        updateSelectizeInput, fluidPage, bindCache, reactive, observe, reactiveValues,
        bindEvent, isolate, showModal, removeModal, HTML],
  dplyr[filter, `%>%`, select, case_when, mutate, arrange],
  DBI[dbGetQuery, dbSendQuery, dbReadTable, dbWriteTable],
  shinyWidgets[progressSweetAlert, closeSweetAlert, pickerInput, ask_confirmation, sendSweetAlert],
  stringr[str_split],
  DT[renderDataTable, datatable, dataTableOutput]
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)
      tagList(
        uiOutput(ns("ui"))
      )
}

#' @export
server <- function(id, con, appData, modal = FALSE) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    reloadinside <- reactiveValues( value = 0 )
    colors <- paste0("color:",c('gray','green','blue','black','orange','red','purple'),";")
    colors <- paste0(colors,"font-family: Arial;")
    colors <- paste0(colors,"font-weight: bold;")
    output$nocommenttext <- renderText({"No comment for this variant yet"})
    
    observeEvent(c(appData$annoter_reactives$my_variant_id, reloadinside$value),{
      req(appData$annoter_reactives$my_variant_id)
      print("rendering variant annoter UI ")
      VKB <- unique(dbGetQuery(appData$con, paste0("SELECT VKB from variant_impact WHERE variant_id = '", appData$annoter_reactives$my_variant_id,"'"))$VKB)
      comments_table <- dbReadTable(conn = appData$con, name="variant_comments") %>% 
        filter(variant_id == appData$annoter_reactives$my_variant_id) %>%
        mutate(delete = case_when(user == Sys.getenv("SHINYPROXY_USERNAME") ~ paste0('<button id="delete_com_',
                                                                                     com_id,
                                                                                     '" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;',ns("godeletecom"),'&quot;,  this.id, {priority: &quot;event&quot;})"><i class="fa fa-trash"></i>',
                                                                                     '</button>')))
      output$comments <- renderDataTable(datatable(comments_table,
                                                           rownames = FALSE, escape = FALSE,
                                                           options = list(
                                                             columnDefs = list(list(visible=FALSE, targets=c(0))))))
      output$ui <- renderUI({
        tagList(
          column(width = 4,
                 pickerInput(inputId = ns('annotate'),"Select a category to the attribute to this variant",
                             multiple = F, width = "100%", 
                             selected = VKB,
                             choices = c("PossibleArtifact", "Benign", "LikelyBenign", "UncertainSignificance", "LikelyPathogenic", "Pathogenic", "Unknown"),
                             choicesOpt = list(style = colors),
                 ), 
                 actionButton(ns("okVKB"), "Update VKB classification", width = "100%")),
          column(width = 8, textAreaInput(inputId = ns("annotatecom"),label = "Write a commentary about the variant",placeholder = "my commentary",width  = "100%",
                                         value = "my commentary"),actionButton(ns("okComment"), "Add comment on variant"),
                 br(), br(),
                 if(nrow(comments_table) >= 1) {dataTableOutput(ns("comments"))} else {verbatimTextOutput(ns("nocommenttext"))},br()
          )
        )
      })
    })
    
    observeEvent(input$godeletecom,{
      req(appData$annoter_reactives$my_variant_id)
      com_id <- paste0(str_split(input$godeletecom, pattern = "_")[[1]][c(3)], collapse = "_")
      progressSweetAlert(session = session, id = "Just_to_restore_reactivity_[Tricky]", title = "", display_pct = TRUE, value = 9)
      closeSweetAlert(session = session)
      showModal(modalDialog(size = "l",
                            title = "Are you sure ??", 
                            "Following commentary will be removed from base :",br(),
                            renderDataTable(datatable(dbGetQuery(appData$con, paste0("SELECT variant_id, user, date, comment FROM variant_comments WHERE com_id = '",com_id,"';")) %>% 
                                                                filter(variant_id == appData$annoter_reactives$my_variant_id),rownames = FALSE)),
                            footer = tagList(
                              modalButton("No"),
                              actionButton(ns("deletecomconf"), "Yes")
                            )))
    })
    observeEvent(input$deletecomconf, {
      req(input$godeletecom)
      req(input$deletecomconf)
      removeModal()
      com_id <- paste0(str_split(input$godeletecom, pattern = "_")[[1]][c(3)],collapse = "_")
      dbSendQuery(appData$con, paste0("DELETE FROM variant_comments WHERE com_id = '", com_id, "';"))
      #dbSendQuery(con, paste0("DELETE FROM variant_comments WHERE com_id = '",com_id,"' AND variant_id = '",data$annoter_reactives$my_variant_id,"';"))
      reloadinside$value <- reloadinside$value +1
      sendSweetAlert(session = session,title = "Database updated with sucess !",text = "comment correctly removed",type = "success")
    })    
  
    if (modal == TRUE){
      observeEvent(appData$annoter_reactives$launchmodal, {
        req(appData$annoter_reactives$my_variant_id)
        VKB <- unique(dbGetQuery(appData$con, paste0("SELECT VKB from variant_impact WHERE variant_id = '",appData$annoter_reactives$my_variant_id,"'"))$VKB)
        comments_table <- dbReadTable(conn = appData$con, name="variant_comments") %>% 
          filter(variant_id == appData$annoter_reactives$my_variant_id) %>%
          mutate(delete = case_when(user == Sys.getenv("SHINYPROXY_USERNAME") ~ paste0('<button id="delete_com_',
                                                                                       com_id,
                                                                                       '" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;',ns("godeletecom"),'&quot;,  this.id, {priority: &quot;event&quot;})"><i class="fa fa-trash"></i>',
                                                                                       '</button>')))
        output$comments <- renderDataTable(datatable(comments_table,
                                                     rownames = FALSE, escape = FALSE,
                                                     options = list(
                                                     columnDefs = list(list(visible=FALSE, targets=c(0))))))
        
        showModal(session = session, modalDialog(size = 'l',
                                                title = paste0("Annotate ", appData$annoter_reactives$my_variant_id ," in your database"),
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
        apn_sql <- paste0("UPDATE variant_impact SET VKB = '", input$annotate,"' WHERE variant_id = '", appData$annoter_reactives$my_variant_id, "';")
        dbSendQuery(appData$con, apn_sql)
        sendSweetAlert(session = session,title = "Database updated with sucess !",text = paste0(appData$annoter_reactives$my_variant_id , " annotation correctly overwritten"),type = "success")
        appData$annoter_reactives$reload <- appData$annoter_reactives$reload + 1
        reloadinside$value <- reloadinside$value + 1 
      } else {
        sendSweetAlert(session = session, cancelOnDismiss = TRUE, title = "Variant annotation has been canceled", type = "info")
      }
    })
    
    observeEvent(input$annotationconfComment,{
      req(input$annotatecom)
      req(input$annotationconfComment)
      if(input$annotationconfComment ==  TRUE){
        # Check that data object exists and is data frame.
        new_comment <- data.frame(com_id = dbGetQuery(appData$con, "SELECT COUNT(*) FROM variant_comments;")[1,1] + 1 ,
                                  variant_id = appData$annoter_reactives$my_variant_id,
                                  comment = HTML(gsub("\n","<br/>",input$annotatecom)),
                                  user = Sys.getenv("SHINYPROXY_USERNAME"), 
                                  date = format(Sys.Date()," %d/%m/%Y"))
        dbWriteTable(conn = appData$con, name="variant_comments", new_comment, append = TRUE)
        sendSweetAlert(session = session,title = "Database updated with sucess !", text = paste0(appData$annoter_reactives$my_variant_id , " annotation correctly overwritten"),type = "success")
        #appData$annoter_reactives$reload <- appData$annoter_reactives$reload + 1
        reloadinside$value <- reloadinside$value + 1 
      } else {
        sendSweetAlert(session = session,cancelOnDismiss = TRUE, title = "Variant annotation has been canceled",type = "info")
      }
    })      
 
  })
}
