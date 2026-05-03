#函数：绘制基因组位置示意图
GeneLocationPlot<-
  function(
    GeneID,
    ID_Type = c('gene_name','ensembl_gene_id','ensembl_transcript_id',
                'ensembl_exon_id','ensembl_CDS_id'),
    Genome,
    plot_location=NULL, #绘图X轴区域
    XAsixText=F
  ){
    # 示例：
    # Genome<-rtracklayer::import('D:/OneDrive/PROJECTS/UoB/M6_GroupProject_EHMT2/Homo_sapiens.GRCh38.115.chr.gtf.gz')
    # Genome<-as.data.frame(Genome)
    # GeneID<-'KSR2'
    # ID_Type<-'gene_name'
    
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
    selectedGenome_df_plot<-selectedGenome_df %>% 
      dplyr::select(c('seqnames','start','end','Label'))
    
    # 增加一个 y 轴位置列。行号越小（越靠前），y值越小。
    selectedGenome_df_plot$y_pos <- 1:nrow(selectedGenome_df_plot)
    # 开始使用 ggplot2 绘图
    p <- ggplot2::ggplot(selectedGenome_df_plot) +
      # 绘制箭头：使用 geom_segment，从 start 指向 end
      ggplot2::geom_segment(
        aes(x = start, y = y_pos, xend = end, yend = y_pos),
        arrow = arrow(length = unit(0.3, "cm"), type = "closed"), # 设置箭头样式
        color = "darkblue", 
        linewidth = 1 # 控制箭头线条粗细
      ) +
      # 绘制文本标签：定位在 start 位置
      geom_text(
        aes(
          x = (start + end) / 2, # 计算中点作为 x 坐标
          y = y_pos, 
          label = Label
        ),
        vjust = -0.4,           # 负值表示向上偏移（数值越大离箭头越远）
        hjust = 0.5,            # 0.5 表示水平居中
        fontface = "bold",
        color = "darkblue"
      ) +
      # 翻转 y 轴，这样 y_pos = 1 (即第一行) 会出现在画面的最上方
      ggplot2::scale_y_reverse() +
      # 美化图表，去除多余的背景和 y 轴的数字刻度
      ggplot2::labs(x = paste("Genomic Position", unique(selectedGenome_df_plot$seqnames)), y = NULL) +
      ggplot2::theme_void() +
      ggplot2::labs(x=NULL)+
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),   # 隐藏 y 轴文本
        axis.ticks.y = ggplot2::element_blank(),  # 隐藏 y 轴刻度
        panel.grid.minor = ggplot2::element_blank(), # 去除次要网格线
        panel.grid.major.y = ggplot2::element_blank() # 去除 y 轴水平网格线，使画面更干净
      )+
      ggplot2::coord_cartesian(
        ylim = c(0.8, nrow(selectedGenome_df_plot) + 0.5)  # 缩小 Y 轴范围。0.8 让第一行靠近顶部，+0.5 让最后一行靠近底部
      )
    
    # 绘图范围
    if(length(plot_location)!=0){
      range_nums <- as.numeric(strsplit(plot_location, "-")[[1]])
      # 使用 coord_cartesian 代替 scale_x_continuous(limits=...)
      p <- p + ggplot2::coord_cartesian(xlim = range_nums)
    }else{
      # 向左扩展 X 轴的显示范围，防止左侧的文本标签被画面边缘截断
      p<-p+ggplot2::scale_x_continuous(expand = expansion(mult = c(0.15, 0.05)))
    }
    
    # X轴文本
    if(XAsixText==F){
      p<-p+ggplot2::theme(axis.text.x = element_blank())
    }
    
    return(p)
  }
