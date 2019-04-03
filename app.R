
#install packages
# packages<-c("shiny","rsconnect","shinyFiles","rdrop2")
# for(i in packages){
# if (!i %in% installed.packages()) {install.packages(i)}}

#load packages

library(rdrop2)
library(shiny)
library(shinyFiles)

#dropboxdir
outputDir <- "dreiFragezeichenApp"

############################
#Functions to save Data to Dropbox
##################################
saveData <- function(data,fileName) {
  # Write the data to a temporary file locally
  #filePath <- file.path(tempdir(), fileName)
  if(length(grep("\\.csv",fileName))==1){
    write.csv(data, fileName, row.names = FALSE, quote = TRUE)
  }
  if(length(grep("\\.R",fileName))==1){
    save(data,file=fileName)
  }
  # Upload the file to Dropbox
  drop_upload(fileName, path = outputDir)
}

######################################
#Function to load data from dropbox
#######################################
loadData <- function(fileName) {
  filePath <- file.path(outputDir, fileName)
  if(length(grep("\\.csv",fileName))==1){
    data <- drop_read_csv(filePath)
    return(t(data))
  }
  if(length(grep("\\.R",fileName))==1){
    drop_download(filePath,overwrite = T,local_path = file.path(getwd(),fileName))
    load(file.path(getwd(),fileName),envir =  .GlobalEnv)
  }
}

####################
#set input 
####################
input.path<-file.path(outputDir,"input.csv")
 
  if(drop_exists(input.path)){
  n_folgen2<-as.numeric(drop_read_csv(input.path))
  #oad("input.R",envir =  .GlobalEnv)
}else{
n_folgen2<-200
}

###################################
#UI
#####################################
#build User Interface
ui<-pageWithSidebar(
  
  
  
  # Application title
  headerPanel("??? Zufallsgenerator"),
  
  #
  sidebarPanel(    
    
    numericInput("n_folgen","Anzahl Folgen",n_folgen2),
    actionButton("button", "Folge generieren")
),
  mainPanel(
    textOutput("folge"),
    textOutput("weitere_folgen"),
    br(),
    textOutput("datum"),
    tags$head(tags$style("#folge{
                                 font-size: 20px;
                         }"
                         )
    )
  )
)

###############################
#dreifragezeichen Funktion
###############################
random_drei_fragezeichen<-
  function(n_folgen=200,#Anzahl Folgen
           #name der Datei in der der Vektor gespeichert wird
           file="R_drei_Fragezeichen-Folgen_"){
    
  #Zusammenfügen von Pfad und Dateiname mit Anzahl Folgen im Namen
  file_n<-paste0(file,n_folgen,".R")
  
  #Falls noch keine Datei mit dem Namen vorhanden ist 
  #wird sie jetzt erstellt
  if(!drop_exists(file.path(outputDir,file_n))){
    #ein Vektor von 1 bis anzahl folgen wird erstellt
    vec<-1:n_folgen
    #der Vektor wird unter dem angegebenen Namen gespeichert
    saveData(data=vec,fileName = file_n)
    #save(vec,file=file_n)
  }#Ende if
  
  #Falls die Anzahl Folgen geändert wurde sind nun zwei Dateien im Ordner
  #die nicht mehr aktuelle Datei wird gelöscht
  #erst werden alle Dateien abgefragt die den Name enthalten
  dropbox_list<-drop_dir(outputDir)
  files<-dropbox_list$name[grep(file,dropbox_list$name)]
  #die aktuelle Datei ist die bei der n_folgen am Ende steht
  cur_file<-grep(paste0(file,n_folgen),files)
  #falls weitere Dateien vorhanden sind werden sie nun gelöscht 
  if(length(files[-cur_file])!=0){
    for(i in files[-cur_file]){
    drop_delete(file.path(outputDir,i))
    }
  }
  
  #Nun wird der gespeicherte Vektor geladen
  #load(file_n)
  loadData(fileName = file_n)
  vec<-data
  #if(save==T){
   #falls er die Länge Null hat wird erneut 
   #ein Vektor von 1 bis n_folgen erstellt
   if(length(vec)==0){
     vec<-1:n_folgen
   }
   if(length(vec)==n_folgen){
     #Dieser Wert entspricht der Folge die jetzt gehört wird 
     folge<-vec[n_folgen]
     #diese Folge wird vom gesamt Vektor entfernt
     vec<-vec[-n_folgen]
   }else{
   #dann wird ein zufälliger Wert des Vektors gezogen
   sampl<-sample(1:length(vec),1,replace = F)
   #Dieser Wert entspricht der Folge die jetzt gehört wird 
   folge<-vec[sampl]
   #diese Folge wird vom gesamt Vektor entfernt
   vec<-vec[-sampl]
   }
   #der gekürzte Vektor wird gespeichert
   saveData(vec,fileName=file_n)
   #Die aktuelle Folge wird ausgegeben
  # return(folge)
  #}else{
  # return(length(vec))
  #}
   return(list(folge,length(vec)))
}

##################################
#Server
##################################
#build server
server<-function(input, output,session) {
  
observe({
  
  if(drop_exists(input.path)){
    n_folgen2<-as.numeric(drop_read_csv(input.path))
    #oad("input.R",envir =  .GlobalEnv)
  }else{
    n_folgen2<-200
  }
  #session$sendInputMessage("n_folgen",  list(value=inputvalues[[i]]) )
  updateNumericInput(session,"n_folgen",value=n_folgen2)
})
    
observeEvent(input$button,{
  
  n_folgen2<-(input$n_folgen)
  out<-random_drei_fragezeichen(n_folgen = n_folgen2)
  weitere<-out[[2]]
  folge<-out[[1]]
  saveData(n_folgen2, fileName = 'input.csv')
  
output$folge<-renderText({


  formulierungen<-c(paste("heute hörst du Folge",folge),
                    paste("heute ist Folge",folge,"dran"),
                    paste("die heutige Auswahl ist Folge",folge),
                    paste("für heute hat der Zufallsgenerator Folge",folge,"ausgesucht"),
                    paste("dieses Mal ist Folge",folge,"dran"),
                    
                    paste("der Algorithmus hat sich für Folge",folge,"entschieden"),
                    paste("der Computer schlägt dir heute Folge",folge,"vor"),
                    paste("die Planetenkonstellation spricht heute deutlich für Folge",folge),
                    paste("die Götter haben entschieden, dass heute Zeit für Folge",folge,"ist"),
                    paste0("wie wäre es mit Folge ",folge,"?"),
                    paste("dieses Mal hörst du Folge",folge))
  sample(formulierungen,1)

  })


output$weitere_folgen<-renderText({
  #weitere<-random_drei_fragezeichen(n_folgen = input$n_folgen,save=F)
  if(weitere>0){
    paste("jetzt sind noch",weitere,"Folgen übrig")
  }else{
    "du hast jetzt alle Folgen durchgehört und es beginnt wieder von vorn!"
  }
})
  
output$datum<-renderText({
  datum<-format(Sys.Date(),"%d.%m")
  apptag<-as.numeric(format(Sys.Date(),"%Y"))-2019
  if(datum=="14.10"){
    "Alles Gute zum Geburtstag!"
  }
  if(datum=="24.12"){
    "Frohe Weihnachten!"
  }
  if(datum=="03.04"&apptag>0){
    paste0("heute hat die App ihren ",apptag,"ten Geburtstag!")
  }

})
})

}
#run app
shinyApp(ui, server)


