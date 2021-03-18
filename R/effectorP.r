# extract the 'refresh' header (used to check if result is done)
refresh_header <- function(req){
    headers(req)[["refresh"]]
    
}

#' Submit a protein file to effectorP
#' @import rvest 
#' @import stringr
#' @import httr
#' @import progress
#' @import xml2
#' @export

effectorP <- function(fasta_file, version="1", fasta=TRUE, outfile="effectorP.fasta"){
    software_version <- paste0("version", match.arg("1", c("1", "2")))
    handle <- POST(base_url(), 
                   body=list( userfile  = upload_file(fasta_file), 
                   mode=software_version))
    res_url <- str_extract(refresh_header(handle), "http://.+")
    pb <- progress_bar$new(format=paste("Contacting", res_url, "...(:spin)"), 
                           total=1e6)
    calculating <- TRUE
    while( calculating ){
        for( i in 1:40){
            pb$tick()
            Sys.sleep(1/20)
        }
        res <- read_html(res_url)
        if (html_text(html_node(res, "title")) == "EffectorP Results"){
            calculating <- FALSE
        }
    }   
    cat("\n")    
    if(fasta){
        links <- html_nodes(res, "a")
        fasta_URL <- html_attr(links[str_detect(links, ".+\\.fasta")], "href")
        cat(as.character(GET(fasta_URL)), file= outfile)
        message("Fasta file written to", outfile)
    }
    html_table(res, header=TRUE)[[1]]    
}

    

funanno_table <- function(effector_tab){
    genes <- sapply(strsplit(effector_tab[,1], " "), "[[",1)
    data.frame(genes, "note", paste0("effectorP:", effector_tab[,2]))
}

