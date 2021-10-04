#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
require("shinyalert")
source("eod_class.R")
library(ggplot2)
library(gridExtra)
library(DT)


file_array<-c()
output_folder<-""
l_frames<-list()
eod_cluster<-NULL
loaded<-FALSE
normalized<-FALSE
current_eod_idx<-NULL
updated_files<-c()
superimposed_plot<-NULL
cluster_file<-NULL

normalize_all_plots<-function()
{
    print("__DISPLAY__")
    max_size<-eod_cluster$getSize()
    print(max_size) 
    list_chart <- list()
    
            for(i in 1:max_size)
            {
                tmp_eod<-eod_cluster$getEODS(i)
                tmp_eod$getPossibleBaseline()
                #tmp_eod$normalize()
                list_chart[[i]]<-tmp_eod$getMainPlot()
            }
           
      list_chart  
    
}

append_metadata_batch<-function(new_file, target_folder)
{
    eod_cluster$createMergedFile(new_file, target_folder)
}

manage_cluster<-function(file_array, output,session)
{

  cluster_file<<-file_array$name[1]
  print("TRUE_FILE")
  print(cluster_file)
  eod_cluster<<-EodCluster$new(cluster_files=file_array$datapath, true_filenames=file_array$name)
  output$summary<-renderText(paste0("1","/", eod_cluster$getSize(), " file(s)" ))
  
 
  i<-1
  tmp_choices<-list()
  lapply(eod_cluster$getSourceFiles(),
                       function(x)
                       {
                         print("x=")
                         print(x)
                         tmp_choices[[i]]<<-paste0(i, " ", x)
                         i<<-i+1
                       })
  print(tmp_choices)
  normalize_all_plots()
  updateSelectInput(session, "select_eod",
                   
                    choices = setNames(seq(1,length(eod_cluster$getSourceFiles())),
                                      tmp_choices)
                    
  )
  loaded<<-TRUE
}

invert_phase<-function( output,session)
{
  if(!is.null(eod_cluster)&&loaded)
  {
    print(current_eod_idx)
    current_eod<-eod_cluster$getAllEods()[[as.numeric(current_eod_idx)]]
    print(typeof(current_eod))
    current_eod$inversePhase()
    current_eod$getPossibleBaseline()
    current_chart<-current_eod$getMainPlot()
    output$plot_eods<-renderPlot(current_chart)
    output$summary_data<-renderTable(current_eod$getMetadata())
    modified_file<-eod_cluster$getUploadedFile(as.numeric(current_eod_idx))
    print("MOD")
    print(modified_file)
    if(! modified_file %in% updated_files)
    {
      updated_files<<-c(updated_files,modified_file)
    }
  }
}


save_update<-function( output_folder, output,session)
{
  print("try to save")
  if(!is.null(eod_cluster)&&loaded)
  {
    print("in save")
    print(updated_files)
    sapply(updated_files, 
           function(x)
           {
             eod_cluster$updateCluster(output_folder, x)
           }
          )
    updated_files<<-c()
  }
}
redraw_plot<-function(i, output,session)
{
  print("redraw")
  if(!is.null(eod_cluster)&&loaded)
  {
    print(i)
    current_eod_idx<<-i
    tmp_eod<-eod_cluster$getEODS(as.numeric(i))
    if(!normalized)
    {
      #print("go for normalization")
      #tmp_eod$getPossibleBaseline()
      normalized<<-TRUE
    }
    print("go for plot")
    current_chart<-tmp_eod$getMainPlot()
    output$plot_eods<-renderPlot(current_chart)
    output$summary_data<-renderTable(tmp_eod$getMetadata())
    print(tmp_eod$getNormalizedBaseLine())
    output$baseline_text<- renderText(paste0("baseline level (after normalisation)",as.character(tmp_eod$getNormalizedBaseLine())))
  }
}

remove_at_index<-function(i, output,session)
{
  if(!is.null(eod_cluster)&&loaded)
  {
    print("list files")
    print(eod_cluster$getUploadedFiles())
    
    modified_file<-eod_cluster$getUploadedFile(as.numeric(i))
    print("MOD")
    print(modified_file)
    if(! modified_file %in% updated_files)
    {
      updated_files<<-c(updated_files,modified_file)
    }
    eod_cluster$removeEodAtIndex(as.numeric(i))
  }
  i<-1
  tmp_choices<-list()
  lapply(eod_cluster$getSourceFiles(),
         function(x)
         {
           print("x=")
           print(x)
           tmp_choices[[i]]<<-paste0(i, " ", x)
           i<<-i+1
         })
  print(tmp_choices)
  updateSelectInput(session, "select_eod",
                    
                    choices = setNames(seq(1,length(eod_cluster$getSourceFiles())),
                                       tmp_choices)
                    
  )
 
}

superimpose<-function(flag, output, session)
{
  
  if(flag)
  {
    max_size<-eod_cluster$getSize()
    #force normalization
    for(i in 1:max_size)
    {
      tmp_eod<-eod_cluster$getEODS(i)
      print("TEST_NORMALIZED")
      print(tmp_eod$isNormalized())
      #if(! tmp_eod$isNormalized())
      #{
      
        print("NORMALIZE")
        print(i)
        if(normalized)
        {
          tmp_eod$detect_landmarks_after_normalization()
        }
        else
        {
          tmp_eod$normalize()
        }
      #}
    }
    superimposed_plot<<-eod_cluster$superimposePlots()
    output$plot_eods<-renderPlot(eod_cluster$superimposePlots())
  }
  else
  {
    redraw_plot(current_eod_idx, output, session)
  }
}

shift_wave<-function(i, shift_interval, direction, output, session)
{
  tmp_eod<-eod_cluster$getEODS(as.numeric(i))
  tmp_eod$pad_normalized(shift_interval, direction)
  current_chart<-tmp_eod$getMainPlot()
  output$plot_eods<-renderPlot(current_chart)
}

create_averaged_file<-function(output_file)
{
  if(loaded)
  {
    eod_cluster$generateAveragedNormalizedSignal()
    eod_cluster$averaged_to_mormyroscope_file(output_file)
  }
}

change_time_normalization<-function(type_normalization,output, session)
{
   print(type_normalization)
   eod_cluster$setTimeNormalization(type_normalization)
   normalize_all_plots()
   tmp_eod<-eod_cluster$getEODS(as.numeric(current_eod_idx))
   current_chart<-tmp_eod$getMainPlot()
   output$plot_eods<-renderPlot(current_chart)
}

manage_batch_files<-function(file_array, target_folder, output)
{
    print("LOAD")
    print(file_array)
   
    eod_cluster<<-EodCluster$new(file_array$datapath, true_filenames=file_array$name)
    print(eod_cluster$getSize())
    print("DONE")
    #print(l_frames)
    l_files<-eod_cluster$getFilenameForMergedData()
    print(l_files)
    invisible(lapply(
        l_files,
        function(x)
        {
            new_file<-paste0(target_folder,"/",x)
            print(new_file)
            #print(l_frames[new_file])
            if(file.exists(new_file))
            {
                print("EXISTS")
                shinyalert(
                    type = "input",
                    inputId="shiny_file_confirm",
                    inputType = "number",
                    size = "l",
                    inputValue=1,
                    title=paste0("File exists\r\n", str_replace(new_file,target_folder,""), ".csv",
                                 "\r\n 1 append or create \r\n 2 recreate \r\n 3 cancel"),
                    callbackR= function(y)
                    {
                        print("CALLBACK")
                        
                        if(y==1)
                        {
                            print("update_file")
                            #print(l_frames[new_file])
                            append_metadata_batch(x, target_folder)
                        }
                        else if(y==2)
                        {
                            print("recreate_file")
                            file.create(new_file)
                            #print(l_frames[new_file])
                            #print(l_frames[new_file])
                            append_metadata_batch(x, target_folder)
                            
                        }
                        else
                        {
                            print("cancel")
                        }
                        
                    }
                )
            }
            else
            {
                print("create_file")
                file.create(new_file)
                #print(l_frames[new_file])
                append_metadata_batch(x,target_folder)
                
            }
            print("after_alert")
   
        }
        
    ))
    
}

# Define UI for application that draws a histogram
ui <- dashboardPage(

    dashboardHeader(title = "EOD dashboard"),
    dashboardSidebar(
        
        fileInput("eod_files", label= "Choose Mormyroscope files",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        textInput("output_folder", label="output  folder", value=getwd()),
        actionButton("change_output_folder", "Change output folder"),
        actionButton("trigger_merge", "Merge files"),
        fileInput("eod_cluster", label= "Choose cluster file",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        actionButton("load_cluster", "Load cluster")
        
    ),
    dashboardBody(useShinyalert(),
                  fluidRow(
                    textOutput("summary"),
                      box(
                        
                        plotOutput("plot_eods")
                      ),
                    textOutput("baseline_text"),
                    actionButton("invert_phase", "Invert phase"),
                    actionButton("remove_eod", "Remove"),
                    actionButton("save_update", "Save updates"),
                    selectInput("select_eod", label="Current EOD", choices=c()),
                    selectInput("time_normalization", label="Time normalization", choices=c(
                        "halfway"="halfway",
                        "positive_peak"= "positive_peak",
                        "negative_peak"= "negative_peak"
                      
                    )),
                    checkboxInput("superimpose", "Superimpose", value = FALSE),
                    numericInput(
                      "shift",
                      "Shift",
                      0,
                      min = 0,
                      max = 1.0,
                      step = 0.01,
                    ),
                    actionButton("shift_right", "Shift right"),
                    actionButton("shift_left", "Shift left"),
                    actionButton("save_superimposed", "Save superimposed plot"),
                    actionButton("create_averaged", "Save averaged signal"),
                      
                    ),
                    htmlOutput("summary_data"),
                  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    ##BLOCK TO MANAGE OUTPUT FILES
    observeEvent(input$eod_files,{
        file_array<<- input$eod_files
        print(input$eod_files$datapath)
        
        
        
    })
    
    observeEvent(input$change_output_folder,
                 {
                     tmp<-choose.dir(default=getwd(), caption="Select folder for EODs")
                     updateTextInput(session, "output_folder", value=tmp)
                 }
    )
    
    ###
    ### Block handling button click
    observeEvent(input$trigger_merge,
                 {
                     #print("click")
                     #print(file_array)
                     
                     output_folder <<-input$output_folder
                     if(length(file_array)>0 && nchar(output_folder)>0)
                     {
                         #print("go")
                         manage_batch_files(file_array,output_folder, output)
                     }
                 }
    )
    
    observeEvent(input$load_cluster,
                 {
                    print("read merge")
                    if(length(input$eod_cluster)>0)
                    {
                      manage_cluster(input$eod_cluster,output, session)
                    }
                  })
    
    observeEvent(input$select_eod,
                  {
                    redraw_plot(input$select_eod,output, session)
                    if(loaded)
                    {
                      output$summary<-renderText(paste0(input$select_eod,"/", eod_cluster$getSize(), " file(s)" ))
                    }
                  })
    observeEvent(input$invert_phase,
                 {
                   invert_phase(output, session)
                 })
    observeEvent(input$time_normalization,
                 {
                   if(loaded)
                   {
                     print("tried_to_normalize")
                     change_time_normalization(input$time_normalization,output, session)
                     redraw_plot(input$select_eod,output, session)
                   }
                 })
    observeEvent(input$remove_eod,
                 {
                   remove_at_index(input$select_eod,output, session)
                 })
    observeEvent(input$save_update,
                 {
                   print("try to save")
                   output_folder <<-input$output_folder
                  
                   if( nchar(output_folder)>0)
                   {
                     
                    save_update( output_folder, output, session)
                   }
                 })
    observeEvent(input$superimpose,
                 {
                  superimpose(input$superimpose, output, session)
                 })
    observeEvent(input$shift_right,
                 {
                   if(loaded)
                   {
                     shift_wave(input$select_eod, input$shift, "right", output, session)
                   }
                 })
    observeEvent(input$shift_left,
                 {
                   if(loaded)
                   {
                     shift_wave(input$select_eod, input$shift, "left", output, session)
                   }
                 })
    observeEvent(input$save_superimposed,
                 {
                   if(! is.null(superimposed_plot))
                   {
                     output_folder <<-input$output_folder
                     print(paste0(output_folder,"/",cluster_file,".png"))
                     ggsave(paste0(output_folder,"/",cluster_file,".png"),superimposed_plot)
                   }
                 }
                 )
    observeEvent(input$create_averaged,
                 {
                   output_folder <<-input$output_folder
                   create_averaged_file(paste0(output_folder,"/",cluster_file,"_averaged.csv"))
                 })
}

# Run the application 
shinyApp(ui = ui, server = server)
