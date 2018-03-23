#' Submit a protein file to effectorP

refresh_header <- function(req){
    headers(req)[["refresh"]]
    
}

effectorP <- function(fasta_file, version="1", fasta=TRUE, outfile="effectorP.fasta"){
    software_version <- paste0("version", match.arg("1", c("1", "2")))
    handle <- POST(URL, 
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

    



}
