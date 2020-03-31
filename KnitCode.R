
for (i in 1:99) {
     rmarkdown::render("RASP_MarkdownReport.Rmd",params=list(id=idlist$ID[i]))
     file.rename(from="RASP_MarkdownReport.docx", to =paste(idlist$ID[i],"Report.docx"))
     file.copy(from=paste0(getwd(),"/",idlist$ID[i]," Report.docx"), 
               to = paste0(getwd(),"/Reports/",idlist$ID[i]," Report.docx"))
     file.remove(paste0(getwd(),"/",idlist$ID[i]," Report.docx"))
} 

