#adding new column with removal pre/after to all metadatafiles
addsitedata<-function(file){stop()
  require(readxl) #install package
  sites<-read_excel(file, sheet=1, col_names=TRUE, col_type= NULL) #read excel file
  sites.data<-lapply(1:nrow(sites), function(i){print(i)
    r<-sites[i, ]
   # browser()
    f<-read.table(r$meta, header=TRUE, fill=TRUE, stringsAsFactors = FALSE)
    #f$removal<-r$removal
    f$MST<-r$MST
    f$MAP<-r$MAP
    write.table(f, file=r$meta,quote = FALSE, sep="\t", row.names = FALSE ) 
    #write.table(f, file="d:\\tmp.txt",quote = FALSE, sep="\t", row.names = FALSE ) 
    
  })
}

newmetadata<-addsitedata("\\\\eir.uib.no\\home6\\ial008\\FunCab\\Data\\CO2 flux\\RcodeCO2\\sitefiles2015.xlsx")
  
# it does not take the tab for the flag column when this place does not have a value "x"

