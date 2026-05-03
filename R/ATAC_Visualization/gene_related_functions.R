#函数：通过基因ID获取MACS2结果内的峰
getLocationByID<-
  function(GeneID,
           ID_Type = c('gene_name','ensembl_gene_id','ensembl_transcript_id',
                       'ensembl_exon_id','ensembl_CDS_id'),
           Genome,
           macs2_dir,
           sig_level=0.05, #显著性阈值
           FC_cutoff=2 #FC阈值
           ){
    # 示例：
    # macs2_dir<-'D:/OneDrive/PROJECTS/UoB/M6_GroupProject_EHMT2/ATAC_Seq/CallPeak_MACS2/C2_vs_C1'
    # Genome<-rtracklayer::import('D:/OneDrive/PROJECTS/UoB/M6_GroupProject_EHMT2/Homo_sapiens.GRCh38.115.chr.gtf.gz')
    # Genome<-as.data.frame(Genome)
    # GeneID<-'KSR2'
    # ID_Type<-'gene_name'
    # sig_level=0.05 #显著性阈值
    # FC_cutoff=2 #FC阈值

    ID_Type <- match.arg(ID_Type)

    # 筛选后的基因组表格信息
    if(ID_Type =='gene_name'){
      selectedGenome_df<-dplyr::filter(Genome, gene_name==GeneID)
      selectedGenome_df<-dplyr::filter(selectedGenome_df, type=='gene')
      selectedGenome_df$Label<-selectedGenome_df$gene_name
    }else if(ID_Type == 'ensembl_gene_id'){
      selectedGenome_df<-dplyr::filter(Genome, gene_id==GeneID)
      selectedGenome_df<-dplyr::filter(selectedGenome_df, type=='gene')
      selectedGenome_df$Label<-selectedGenome_df$gene_id
    }else if(ID_Type == 'ensembl_transcript_id'){
      selectedGenome_df<-dplyr::filter(Genome, transcript_id==GeneID)
      selectedGenome_df<-dplyr::filter(selectedGenome_df, type=='transcript')
      selectedGenome_df$Label<-selectedGenome_df$transcript_id
    }else if(ID_Type == 'ensembl_exon_id'){
      selectedGenome_df<-dplyr::filter(Genome, exon_id==GeneID)
      selectedGenome_df<-dplyr::filter(selectedGenome_df, type=='exon')
      selectedGenome_df$Label<-selectedGenome_df$exon_id
    }else if(ID_Type == 'ensembl_CDS_id'){
      selectedGenome_df<-dplyr::filter(Genome, protein_id==GeneID)
      selectedGenome_df<-dplyr::filter(selectedGenome_df, type=='CDS')
      selectedGenome_df$Label<-selectedGenome_df$protein_id
    }

    # 调整基因组的染色体编号
    selectedGenome_df <- selectedGenome_df %>%
      mutate(seqnames = as.character(seqnames),
             seqnames = if_else(seqnames %in% as.character(1:22),
                                paste0("chr", seqnames),
                                seqnames))

    # 读取MACS2结果
    xls_files<-list.files(path = macs2_dir,pattern = '.xls')
    if(length(xls_files)>1){ # 如果不止一个xls文件就报错
      stop(errorCondition(
        message = "More than one xls files are found!",
        class = "not_single_file_error",
        value = xls_files          # 附加原始值，便于调试
      ))
    }
    MACS2_peak_read<-
      read.table(paste0(macs2_dir,'/',xls_files),
                 comment.char = '#',header = TRUE) %>%
      dplyr::filter(`X.log10.pvalue.`>=(-log10(sig_level)) &
                    fold_enrichment >= FC_cutoff)
    if(nrow(MACS2_peak_read)<1){ # 如果不止一个xls文件就报错
      stop(errorCondition(
        message = "No significant peaks are found!",
        class = "no_sig_result_error"
      ))
    }

    # 筛选结果
    ## 建立空白结果新变量
    result<-MACS2_peak_read[1,][-1,] %>% as.data.frame() %>%
      dplyr::mutate(Label=NA)
    for(i in 1:nrow(selectedGenome_df)){
      MACS2_peak_filtered<-MACS2_peak_read %>%
        dplyr::filter(end >= selectedGenome_df$start[i]) %>%
        dplyr::filter(start <= selectedGenome_df$end[i]) %>%
        dplyr::filter(chr == selectedGenome_df$seqnames[i])
      result<-rbind(
        result,
        MACS2_peak_filtered %>%
          dplyr::mutate(Label=selectedGenome_df$Label[i])
      )
    }
    result<-result %>% dplyr::filter(!is.na(chr))
    return(
      list(
        gene_region=selectedGenome_df,
        macs2_peaks=result
      )
    )
  }

