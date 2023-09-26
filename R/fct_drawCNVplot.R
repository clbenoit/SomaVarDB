#' drawCNVplot
#' @description
#' A short description...
#' @param name description
#' @import org.Hs.eg.db 
#' @importFrom dplyr %>% select filter mutate inner_join group_by
#' @importFrom RSQLite SQLite
#' @importFrom plyranges bind_ranges
#' @importFrom plotly subplot ggplotly
#' @importFrom GenomicFeatures exonicParts exons
#' @importFrom DBI dbConnect dbExistsTable dbReadTable
#' @importFrom ggplot2 ggplot geom_segment geom_text geom_rect scale_fill_manual aes scale_color_manual element_blank
#' @return return a CNV plot for selected gene and sample.
#'
#' @noRd
#' @export
drawCNVplot_seqone <- function(prefix = NULL,
                          cnvfile_path = NULL,
                          db_path = NULL,
                          selected_sample = NULL,
                          background_samples = NULL,
                          selected_gene = NULL, session = NULL) {
  
  progressSweetAlert(session = session, id = "progresscnvplot",title = "Building cnv plot...",display_pct = TRUE, value = 9)
  
  
  db_name <- file.path(db_path, paste0(prefix, ".db"))
  ## add samples ##
  con <- dbConnect(SQLite(), db_name)
  if(DBI::dbExistsTable(conn = con, name="cnv_geno")){
    cnv_geno <- DBI::dbReadTable(conn = con, name="cnv_geno")
  } else { print("nodata")}

  if(DBI::dbExistsTable(conn = con, name="cnv_geno_raw")){
    cnv_geno_raw <- DBI::dbReadTable(conn = con,
                                     name="cnv_geno_raw") %>% dplyr::select(c("sample_name","interval","padj",
                                                                                "chr", "ratio", "gene", "refseq_accession_number",
                                                                                "start","end")) %>% 
                                       mutate(chr = paste0("chr",chr)) %>%
                                       filter(sample_name %in% c(selected_sample,background_samples)) %>% 
                                       filter(gene ==  selected_gene) 
  }else {print("nodata")}
  
  gtf <- AnnotationDbi::loadDb(system.file("extdata/annotations","gencode.v43lift37.annotation_canonical", package = "SomaVarDB"))
  
  exons <- GenomicFeatures::exonicParts(gtf)
  
  annots <- AnnotationDbi::select(org.Hs.eg.db, keys= selected_gene, 
                                  columns=c("ENSEMBL","ENSEMBLTRANS"), keytype="SYMBOL")
  
  ## La ligne dessous marche en deors du package mais pas dedans ##
  exons <- exons[unlist(unique(BiocGenerics::grepl(annots$ENSEMBL,elementMetadata(exons)[,"gene_id"])))]
  
  cnv_figure_data <- cnv_geno_raw %>% dplyr::select(c("start","end","ratio","sample_name","gene","interval")) %>% 
    filter(sample_name %in% c(selected_sample,background_samples)) %>%
    filter(gene == selected_gene) %>%
    mutate(CNV_status = case_when(ratio <= 0.7   ~ "DEL",
                                  ratio >= 1.3   ~ "DUP",
                                  ratio >= 2   ~ "AMP",
                                  (0.8 <= ratio & ratio <= 1.2) ~ "UNKNOWN")) %>%
    mutate(CNV_status_color = case_when(sample_name %in% background_samples ~ "BACKGROUND",.default = CNV_status)) %>%
    mutate(tooltip = paste0("\n Sample = ",sample_name,
                            "\n Start = ",start,
                            "\n End = ",end,
                            "\n Status = ",CNV_status))

  cnv_figure_probe <-  ggplot(data= cnv_figure_data) +
                        geom_segment(mapping = aes(x=start,xend = end, y = ratio,
                                                yend=ratio,color = CNV_status_color, group = tooltip), size = 2) +    
                        scale_color_manual(breaks = c("DEL", "DUP", "AMP","UNKNOWN","BACKGROUND"),
                                          values=c("blue", "purple", "red","black","grey"))


  exons_by_gene <- exonsBy(gtf,"gene")
  tx_by_gene <- transcriptsBy(gtf,"gene")
  tx_by_gene <- unlist(tx_by_gene[grepl(annots$ENSEMBL,names(tx_by_gene))])
  exons <- unlist(exons_by_gene[grepl(annots$ENSEMBL,names(exons_by_gene))])
  mcols(exons)$model <- "exon"
  mcols(exons)$y <- 2
  introns <- unlist(intronsByTranscript(gtf,use.names = TRUE)[tx_by_gene$tx_name])
  names(introns) <- rep(unique(names(exons)),1,length(names(introns)))
  mcols(introns)$model <- "intron"
  mcols(introns)$y <- 1
  cds <- unlist(cdsBy(gtf, "tx",use.names = TRUE)[tx_by_gene$tx_name])
  names(cds) <- rep(unique(names(exons)),1,length(names(cds)))
  mcols(cds)$model <- "exon"
  mcols(cds)$y <- 2
  fiveutr <- unlist(fiveUTRsByTranscript(gtf,use.names = TRUE)[tx_by_gene$tx_name])
  mcols(fiveutr)$model <- "utr"
  mcols(fiveutr)$exon_rank <- NULL
  mcols(fiveutr)$y <- 1
  threeutr <- unlist(threeUTRsByTranscript(gtf,use.names = TRUE)[tx_by_gene$tx_name])
  mcols(threeutr)$model <- "utr"
  mcols(threeutr)$y <- 1
  mcols(threeutr)$exon_rank <- NULL
  features <- bind_ranges(introns,cds,fiveutr,threeutr,exons, .id = NULL)
  mcols(features)$model <- c(introns$model,cds$model,fiveutr$model,threeutr$model,exons$model)
  features <- GRangesList(features)
  names(features) <- annots$SYMBOL

  features <- as.data.frame(features)
  model <- ggplot(data = features) + 
    geom_rect(aes(xmin = start, xmax = end, ymin = 1 - y, ymax = 1 + y, fill = model)) + 
    geom_text(aes(x=(start+end)/2, y=1, label=exon_rank), size=4) +
    ggplot2::theme(axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
    scale_fill_manual(breaks = c("exon", "intron", "utr"),
                       values=c("blue", "red", "black"))# +

  DBI::dbDisconnect(con)
  
  closeSweetAlert(session = session)
  
  return(subplot(ggplotly(model, tooltip = NULL),ggplotly(cnv_figure_probe,tooltip = "group"),shareX = TRUE,nrows = 2))

}
