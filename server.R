library(shiny)
library(shinyjs)
library(ggplot2)
library(DiagrammeR)
library(stringr)

#Globally define place where all users can share reactive data

vars=reactiveValues(chat=NULL,flow=NULL,mcount=NULL)

#Restore from previous session, if available
if (file.exists("chat.Rds")){
  vars$chat=readRDS("chat.Rds")
}else{
  vars$chat="Traditional Chat Log"
}

if (file.exists("mcount.Rds")){
  vars$mcount=readRDS("mcount.Rds")
}else{
  vars$mcount=0
}

if (file.exists("flow.Rds")){
  vars$flow=readRDS("flow.Rds")
}else{
  vars$flow=rbind("digraph D{",
                  "#Node List",
                  "",
                  "#Connections",
                  "",
                  "}")
}


server=shinyServer(function(input,output,session){
  
  shinyjs::disable("collbtn")
  shinyjs::disable("trunbtn")
  shinyjs::disable("send")
  
  observeEvent(input$login,{
    if (!input$username==""){
      shinyjs::enable("collbtn")
      shinyjs::enable("trunbtn")
      shinyjs::enable("send")
      shinyjs::disable("login")
      shinyjs::disable("username")
    }
  })
  
  #Make clickable
  txt <- reactive({
    req(input$flowgraph_click)
    nodeval <- input$flowgraph_click$nodeValues[[1]]
    return(word(nodeval,1,sep="@"))
  })
  
  observeEvent(input$flowgraph_click,{
    updateNumericInput(session,"toin",value=as.numeric(txt()))
  })
  
  output$chatbox=renderUI({
    if(length(vars$chat)>500){
      vars$chat=vars$chat[length(vars$chat)-500:length(vars$chat)]
    }
    saveRDS(vars$chat,"chat.Rds")
    HTML(vars$chat)
  })
  
  #Collapse Functionality
  observeEvent(input$collbtn,{
    collist=unique(c(as.numeric(unlist(strsplit(input$collapses,","))),input$toin))
    updateTextInput(session,"collapses",value=paste(collist,sep=","))
  })
  
  #Trunk Functionality
  observeEvent(input$trunbtn,{
    trunlist=unique(c(as.numeric(unlist(strsplit(input$trunks,","))),input$toin))
    updateTextInput(session,"trunks",value=paste(trunlist,sep=","))
  })
  
  #Construct digraphs
  observeEvent(input$send,{
    if(!input$message==""){
      vars$mcount=vars$mcount+1
      vars$chat=c(vars$chat,paste0("<br/>",tags$span(class="username",tags$abbr(title=Sys.time(),input$username)),
                                   "@",
                                   tagList(vars$mcount),
                                   ": ",
                                   tagList(input$message)))
      updateTextInput(session,"message",value="")
    }
    
    flowc=vars$flow
    
    ni=grep("Node List",flowc)+1
    ci=grep("Connections",flowc)+1
    valtbl=vars$chat[-1]
    if(length(valtbl)>0){
      valsave=c("",paste0(
        word(word(valtbl,2,sep="@"),1,sep=":"),
        "@",
        word(word(valtbl,4,sep=">"),1,sep="<"),
        ":",
        word(word(valtbl,2,sep="@"),2,sep=":")
      ))
      valsave=gsub("'","",valsave)
      connections=which(gsub("(.+)@.*", "\\1", valsave)==input$toin)
      
      if (!input$message==""){
        for (icc in connections){
          flowc[ci]=paste0(flowc[ci],paste(paste0("\"",valsave[icc],"\""),"->",paste0("\"",valsave[length(valsave)],"\"")))
        }
      }
      
      cons=unlist(strsplit(flowc[ci],"\""))
      cons=cons[!(cons=="  "|cons==""|grepl("->",cons))]
      conmat=matrix(cons,ncol = 2,byrow=T)
      
      colorgrade=rep("white",length(valsave))
      
      cramp=colorRampPalette(c("green", "white"))(10)
      for (icol in length(cramp):1){
        #Trace from most recent up
        tracu=valsave[max(0,length(valsave)-icol+1)]
        
        lenprev=2
        while(!length(tracu)==lenprev){
          lensave=length(tracu)
          tracu=unique(c(tracu,conmat[which(conmat[,2]%in%tracu),1]))
          lenprev=lensave
        }
        colorgrade[match(tracu,valsave)]=paste0("\"",cramp[icol],"\"")
      }
      
      
      prop=paste0("[shape=box,fontname=Helvetica,style=filled,fillcolor=",colorgrade[-1],"]")
      
      
      flowc[ni]=paste0("  ",paste(paste0("\"",valsave[-1],"\"",prop),collapse = ";"))
      vars$flow=flowc
      saveRDS(vars$flow,"flow.Rds")
      saveRDS(vars$mcount,"mcount.Rds")
      updateNumericInput(session,"toin",value=vars$mcount)
    }
  })
  observe({
    collapses=as.numeric(unlist(strsplit(input$collapses,",")))
    trunks=as.numeric(unlist(strsplit(input$trunks,",")))
    
    flowc=vars$flow
    
    ni=grep("Node List",flowc)+1
    ci=grep("Connections",flowc)+1
    
    if (!flowc[ni]==""){
      
      valtbl=vars$chat[-1]
      if(length(valtbl)>0){
        valsave=c("",paste0(
          word(word(valtbl,2,sep="@"),1,sep=":"),
          "@",
          word(word(valtbl,4,sep=">"),1,sep="<"),
          ":",
          word(word(valtbl,2,sep="@"),2,sep=":")
        ))
        valsave=gsub("'","",valsave)
        connections=which(gsub("(.+)@.*", "\\1", valsave)==input$toin)
        
        cons=unlist(strsplit(flowc[ci],"\""))
        cons=cons[!(cons=="  "|cons==""|grepl("->",cons))]
        conmat=matrix(cons,ncol = 2,byrow=T)
        
        colorgrade=rep("white",length(valsave))
        
        cramp=colorRampPalette(c("green", "white"))(10)
        for (icol in length(cramp):1){
          #Trace from most recent up
          tracu=valsave[max(0,length(valsave)-icol+1)]
          
          lenprev=2
          while(!length(tracu)==lenprev){
            lensave=length(tracu)
            tracu=unique(c(tracu,conmat[which(conmat[,2]%in%tracu),1]))
            lenprev=lensave
          }
          colorgrade[match(tracu,valsave)]=paste0("\"",cramp[icol],"\"")
        }
        
        
        prop=paste0("[shape=box,fontname=Helvetica,style=filled,fillcolor=",colorgrade[-1],"]")
        
        #Tracedown
        tracd=valsave[-1][collapses]
        lenprev=length(tracd)*5
        while(!length(tracd)==lenprev){
          lensave=length(tracd)
          tracd=unique(c(tracd,conmat[which(conmat[,1]%in%tracd),2]))
          lenprev=lensave
        }
        
        #Trace roots
        if (length(trunks)==0){
          trunks=which(!valsave[-1]%in%conmat[,2])
        }
        
        tracr=valsave[-1][trunks]
        lenprev=length(tracr)*5
        while(!length(tracr)==lenprev){
          lensave=length(tracr)
          tracr=unique(c(tracr,conmat[which(conmat[,1]%in%tracr),2]))
          lenprev=lensave
        }
        
        col_sel=which((!valsave[-1]%in%tracd)&(valsave[-1]%in%tracr))
        flowc[ni]=paste0("  ",paste(paste0("\"",valsave[-1][col_sel],"\"",prop[col_sel]),collapse = ";"))
        conmat=matrix(conmat[which(conmat[,1]%in%tracr),],ncol=2)
        if(length(collapses)>0){
          conmat=matrix(conmat[which((!conmat[,1]%in%tracd)&(!conmat[,2]%in%tracd)),],ncol=2)
        }
        
        if (dim(conmat)[1]>1){
          flowc[ci]=paste(do.call(paste, c(data.frame(apply(conmat,2,function(cx)paste0("\"",cx,"\""))), sep="->")),collapse = "")
        }else if(dim(conmat)[1]==0){
          flowc[ci]=""
        }else if (dim(conmat)[1]==1){
          flowc[ci]=paste(paste0("\"",conmat[,1],"\""),"->",paste0("\"",conmat[,2],"\""))
        }
        
        output$flowgraph = renderGrViz({grViz((flowc))})
      }
    }
  })
})
