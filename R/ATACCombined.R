#calculation_functions.R
#根据区间估计整个区间
##为什么写这个函数呢？因为要求左右数据背景没有类似的峰才能显得区间内的峰更显著
##所以需要调整可视化区间
getAppropriateVisualSegment<-
  function(location, # 需要关注的高亮区域
           left_times, #位于高亮区域X轴左侧的区域相较于高亮区域的长度倍数
           right_times){ #位于高亮区域X轴右侧的区域相较于高亮区域的长度倍数
    
    # 高亮区域
    # location示例："15112456-15125852"
    library(stringr)
    library(dplyr)
    
    # 高亮区域开始位置
    location_start<-location %>%
      str_sub(1,(str_locate(location,'-')[1,1]-1)) %>%
      as.numeric()
    # 高亮区域结束位置
    location_end<-location %>%
      str_sub((str_locate(location,'-')[1,1]+1),nchar(location)) %>%
      as.numeric()
    # 计算高亮区域长度
    location_length<-location_end - location_start
    
    # 计算背景区段长度
    left_length<-location_length*left_times # 左侧
    right_length<-location_length*right_times # 右侧
    
    # 计算背景区段起始位点
    backgroud_start<-(location_start-left_length) %>% round(0)
    backgroud_end<-(location_end+right_length) %>% round(0)
    
    #输出
    return(paste0(backgroud_start,'-',backgroud_end))
    
  }

#合并（取并集Union）对照组和实验组区间
merge_highlight_intervals <- function(
    highlight_location_case,
    highlight_location_ctl
) {
  # 合并两个输入向量并去除可能的空值
  all_intervals <- c(highlight_location_case, highlight_location_ctl)
  if (length(all_intervals) == 0) return(character(0))
  
  # 解析区间字符串为数据框，包含 start 和 end 两列
  parts <- strsplit(all_intervals, "-")
  valid <- lengths(parts) == 2
  if (!all(valid)) {
    warning("Some interval strings are malformed and will be ignored.")
    parts <- parts[valid]
  }
  if (length(parts) == 0) return(character(0))
  
  # 转换为数值矩阵
  intervals <- t(sapply(parts, as.numeric))
  colnames(intervals) <- c("start", "end")
  
  # 按起始位置排序
  intervals <- intervals[order(intervals[, "start"]), , drop = FALSE]
  
  # 合并重叠或相邻的区间（闭区间相邻即合并：如 [1,5] 与 [6,10] 合并为 [1,10]）
  merged <- list()
  current_start <- intervals[1, "start"]
  current_end   <- intervals[1, "end"]
  
  for (i in seq_len(nrow(intervals))[-1]) {
    next_start <- intervals[i, "start"]
    next_end   <- intervals[i, "end"]
    if (next_start <= current_end + 1) {
      # 区间重叠或相邻，扩展当前区间的结束位置
      current_end <- max(current_end, next_end)
    } else {
      # 无重叠且不相邻，保存当前区间并开始新区间
      merged[[length(merged) + 1]] <- c(current_start, current_end)
      current_start <- next_start
      current_end   <- next_end
    }
  }
  # 保存最后一个区间
  merged[[length(merged) + 1]] <- c(current_start, current_end)
  
  # 格式化输出为 "start-end"
  result <- vapply(merged, function(x) paste(x[1], x[2], sep = "-"), character(1))
  return(result)
}

#单个高亮区间内有哪些对照组和实验组区间
filter_highlights_within_interval <- function(
    interval_str,
    highlight_locations) {
  # 若高亮区域为空，直接返回 NULL
  if (length(highlight_locations) == 0) return(NULL)
  
  # 解析查询区间
  parts <- strsplit(interval_str, "-")[[1]]
  if (length(parts) != 2) {
    warning("Invalid interval_str format. Expected 'start-end'.")
    return(NULL)
  }
  query_start <- as.numeric(parts[1])
  query_end   <- as.numeric(parts[2])
  
  # 解析所有高亮区域
  highlight_parts <- strsplit(highlight_locations, "-")
  valid <- lengths(highlight_parts) == 2
  if (!all(valid)) {
    warning("Some highlight_locations strings are malformed and will be ignored.")
    highlight_locations <- highlight_locations[valid]
    highlight_parts   <- highlight_parts[valid]
  }
  if (length(highlight_parts) == 0) return(NULL)
  
  # 转换为数值并判断是否在查询区间内（闭区间）
  result <- character(0)
  for (i in seq_along(highlight_parts)) {
    h_start <- as.numeric(highlight_parts[[i]][1])
    h_end   <- as.numeric(highlight_parts[[i]][2])
    if (h_start >= query_start && h_end <= query_end) {
      result <- c(result, highlight_locations[i])
    }
  }
  
  # 返回结果
  if (length(result) == 0) return(NULL) else return(result)
}


#gene_plot.R
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

#gene_related_functions.R
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


#peak_plot.R

#----------------------------0级基础模块----------------------------------------
#单个样本、多区域、连续绘图
singleBedGraphSamplePlot<-
  function(
    bedGraphData, #数据,read.table读取
    chrom_A, #染色体序号
    plot_location, #绘图X轴区域
    highlight_location=NULL, #高亮区域
    sample_names, #样本名称
    DataRange, #绘图Y轴范围
    x_axis_label=T #默认要不要X轴的标度标签
  ){
    
    #参数部分示例
    # chrom_A<-'chr11'
    # plot_location<-'31700000-31850000'
    # highlight_location<-c('31720000-31750000','31784779-31817961')
    # sample_names<-'B1-C'
    # DataRange<-c(0,50)
    
    #加载R包
    library(stringr)
    library(dplyr)
    library(scales)
    library(ggplot2)
    
    message('Reading Data...')
    colnames(bedGraphData)<-c('chromA','chromStartA','chromEndA','peakName','dataValue') #重命名列
    
    message('Processing Location Info...')
    #根据序列位置的字符串切割出绘图区域开始和结束位置
    chr_start_plot<-str_sub(plot_location,1,
                            (as.numeric(str_locate(plot_location,'-')[1,1])-1)) %>%
      as.numeric()
    chr_end_plot<-str_sub(plot_location,
                          (as.numeric(str_locate(plot_location,'-')[1,1])+1),
                          nchar(plot_location)) %>%
      as.numeric()
    
    #筛选出对应染色体的数据
    bedGraphData_selected<-bedGraphData %>%
      dplyr::filter(chromA==chrom_A) %>%
      dplyr::filter(chromEndA>chr_start_plot) %>%
      dplyr::filter(chromStartA<=chr_end_plot)
    
    message('Creating Plot...')
    #绘图风格
    if(x_axis_label==T){
      x_axis_label_size=12
    }else if(x_axis_label==F){
      x_axis_label_size=0
    }
    theme.set = ggplot2::theme(
      axis.title  = ggplot2::element_text(size=12,face = "bold", family = "Times"),
      axis.text.x = ggplot2::element_text(size=x_axis_label_size,
                                          face = "bold", family = "Times",
                                          angle = 90,vjust = 0.5),
      axis.text.y = ggplot2::element_text(size=12,face = "bold", family = "Times"),
      legend.text = ggplot2::element_text(size=12,face = "bold", family = "Times"),
      legend.title = ggplot2::element_text(size=12,face='bold',family = "Times"),
      plot.title = ggplot2::element_text(size=12,face = "bold", family = "Times"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "Times"))
    #绘图
    
    if(length(highlight_location[which(!is.na(highlight_location))])>0){ #如果存在输入的高亮区域
      #根据序列位置的字符串切割出高亮区域开始和结束位置
      ## 建立高亮区域起止位置变量
      Highlight_Locations<-data.frame(matrix(nrow=length(highlight_location),
                                             ncol = 2))
      colnames(Highlight_Locations)<-c('start','end')
      for(i in 1:nrow(Highlight_Locations)){
        Highlight_Locations$start[i]<-str_sub(highlight_location[i],1,
                                              (as.numeric(str_locate(highlight_location[i],'-')[1,1])-1)) %>%
          as.numeric()
        Highlight_Locations$end[i]<-str_sub(highlight_location[i],
                                            (as.numeric(str_locate(highlight_location[i],'-')[1,1])+1),
                                            nchar(highlight_location[i])) %>%
          as.numeric()
      }
      #绘图
      result<-
        ggplot() +
        geom_rect(data=Highlight_Locations,
                  mapping=aes(xmin = start,
                              xmax = end,
                              ymin = DataRange[1],
                              ymax = DataRange[2]),
                  fill = 'yellow', alpha = 0.3)+ #高亮区域
        geom_rect(data=bedGraphData_selected,
                  mapping=aes(xmin = chromStartA,
                              xmax = chromEndA,
                              ymin = 0, ymax = dataValue),
                  fill = "steelblue", color = "steelblue") + #整体区域
        scale_x_continuous(breaks = c(as.numeric(chr_start_plot),
                                      as.numeric(Highlight_Locations$start),
                                      as.numeric(Highlight_Locations$end),
                                      as.numeric(chr_end_plot)),
                           labels = comma,
                           limits = c(chr_start_plot,chr_end_plot),
                           expand = c(0, 0)) +
        scale_y_continuous(limits = c(min(DataRange),max(DataRange)),
                           breaks = c(as.numeric(quantile(seq(0,max(DataRange))))[1:5]),
                           labels = c(as.numeric(quantile(seq(0,max(DataRange))))[1:5]),
                           expand = c(0, 0)) +
        theme_bw() +
        labs(y = sample_names,
             x = NULL,
             title = paste0('[',DataRange[1],',',DataRange[2],']'))+
        theme.set
    }else{
      #没有高亮区域的绘图
      result<-
        ggplot() +
        # geom_rect(data=Highlight_Locations,
        #           mapping=aes(xmin = start,
        #                       xmax = end,
        #                       ymin = DataRange[1],
        #                       ymax = DataRange[2]),
        #           fill = 'yellow', alpha = 0.3)+ #高亮区域
        geom_rect(data=bedGraphData_selected,
                  mapping=aes(xmin = chromStartA,
                              xmax = chromEndA,
                              ymin = 0, ymax = dataValue),
                  fill = "steelblue", color = "steelblue") + #整体区域
        scale_x_continuous(breaks = c(as.numeric(chr_start_plot),
                                      # as.numeric(Highlight_Locations$start),
                                      # as.numeric(Highlight_Locations$end),
                                      as.numeric(chr_end_plot)),
                           labels = comma,
                           limits = c(chr_start_plot,chr_end_plot),
                           expand = c(0, 0)) +
        scale_y_continuous(limits = c(min(DataRange),max(DataRange)),
                           breaks = c(as.numeric(quantile(seq(0,max(DataRange))))[1:5]),
                           labels = c(as.numeric(quantile(seq(0,max(DataRange))))[1:5]),
                           expand = c(0, 0)) +
        theme_bw() +
        labs(y = sample_names,
             x = NULL,
             title = paste0('[',DataRange[1],',',DataRange[2],']'))+
        theme.set
    }
    
    
    #输出
    return(result)
  }

#多个样本相同区域纵向堆叠绘图
multiBedGraphSamplesPlot<-
  function(
    bedGraphDataList, #数据,read.table读取,list()按意向数据储存为列表
    chrom_A, #染色体序号
    plot_location, #绘图X轴区域
    highlight_location=NULL, #高亮区域
    sample_names, #样本名称
    DataRange, #绘图Y轴范围
    x_axis_label=T #默认要不要X轴的标度标签
  ){
    
    #参数部分示例
    # chrom_A<-'chr11'
    # plot_location<-'31700000-31850000'
    # highlight_location<-c('31720000-31750000','31784779-31817961')
    # sample_names<-c('B1-C','B2-KD')
    # DataRange<-c(0,50)
    
    #加载R包
    library(stringr)
    library(dplyr)
    library(scales)
    library(patchwork)
    library(ggplot2)
    
    #拼图用的结果变量
    assign('result_plots',list())
    
    #X轴文本标签大小，仅保留最下方的文本
    if(x_axis_label==T){
      x_axis_text_size<-c(rep(0,(length(bedGraphDataList)-1)),12)
    }else if(x_axis_label==F){
      x_axis_text_size<-c(rep(0,(length(bedGraphDataList))))
    }
    
    #建立循环绘制不同文件对应的图
    for(m in 1:length(bedGraphAdds)){
      print(paste0('Reading Sample ',m,'...'))
      bedGraphData<-bedGraphDataList[[m]] #读取数据
      colnames(bedGraphData)<-c('chromA','chromStartA','chromEndA','dataValue') #重命名列
      
      print(paste0('Processing Data of Sample ',m,'...'))
      #根据序列位置的字符串切割出绘图区域开始和结束位置
      chr_start_plot<-str_sub(plot_location,1,
                              (as.numeric(str_locate(plot_location,'-')[1,1])-1)) %>%
        as.numeric()
      chr_end_plot<-str_sub(plot_location,
                            (as.numeric(str_locate(plot_location,'-')[1,1])+1),
                            nchar(plot_location)) %>%
        as.numeric()
      
      #筛选出对应染色体的数据
      bedGraphData_selected<-bedGraphData %>%
        dplyr::filter(chromA==chrom_A) %>%
        dplyr::filter(chromEndA>chr_start_plot) %>%
        dplyr::filter(chromStartA<=chr_end_plot)
      
      print(paste0('Creating Plot of Sample ',m,'...'))
      #绘图风格
      theme.set = theme(
        axis.title = element_text(size=12,face = "bold", family = "Times"),
        axis.text.x = element_text(size=x_axis_text_size[m],
                                   face = "bold", family = "Times",
                                   angle = 90,vjust = 0.5),
        axis.text.y = element_text(size=12,face = "bold", family = "Times"),
        legend.text = element_text(size=12,face = "bold", family = "Times"),
        legend.title = element_text(size=12,face='bold',family = "Times"),
        plot.title = element_text(size=12,face = "bold", family = "Times"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Times"))
      
      if(length(highlight_location[which(!is.na(highlight_location))])>0){ #如果存在输入的高亮区域
        #根据序列位置的字符串切割出高亮区域开始和结束位置
        ## 建立高亮区域起止位置变量
        Highlight_Locations<-data.frame(matrix(nrow=length(highlight_location),
                                               ncol = 2))
        colnames(Highlight_Locations)<-c('start','end')
        for(i in 1:nrow(Highlight_Locations)){
          Highlight_Locations$start[i]<-str_sub(highlight_location[i],1,
                                                (as.numeric(str_locate(highlight_location[i],'-')[1,1])-1)) %>%
            as.numeric()
          Highlight_Locations$end[i]<-str_sub(highlight_location[i],
                                              (as.numeric(str_locate(highlight_location[i],'-')[1,1])+1),
                                              nchar(highlight_location[i])) %>%
            as.numeric()
        }
        #绘图
        result_plots[[m]]<-
          ggplot() +
          geom_rect(data=Highlight_Locations,
                    mapping=aes(xmin = start,
                                xmax = end,
                                ymin = DataRange[1],
                                ymax = DataRange[2]),
                    fill = 'yellow', alpha = 0.3)+ #高亮区域
          geom_rect(data=bedGraphData_selected,
                    mapping=aes(xmin = chromStartA,
                                xmax = chromEndA,
                                ymin = 0, ymax = dataValue),
                    fill = "steelblue", color = "steelblue") + #整体区域
          scale_x_continuous(breaks = c(as.numeric(chr_start_plot),
                                        as.numeric(Highlight_Locations$start),
                                        as.numeric(Highlight_Locations$end),
                                        as.numeric(chr_end_plot)),
                             labels = comma,
                             limits = c(chr_start_plot,chr_end_plot),
                             expand = c(0, 0)) +
          scale_y_continuous(limits = c(min(DataRange),max(DataRange)),
                             breaks = c(as.numeric(quantile(seq(0,max(DataRange))))[1:5]),
                             labels = c(as.numeric(quantile(seq(0,max(DataRange))))[1:5]),
                             expand = c(0, 0)) +
          theme_bw() +
          labs(y = sample_names,
               x = NULL,
               title = paste0('[',DataRange[1],',',DataRange[2],']'))+
          theme.set
      }else{
        #没有高亮区域的绘图
        result_plots[[m]]<-
          ggplot() +
          # geom_rect(data=Highlight_Locations,
          #           mapping=aes(xmin = start,
          #                       xmax = end,
          #                       ymin = DataRange[1],
          #                       ymax = DataRange[2]),
          #           fill = 'yellow', alpha = 0.3)+ #高亮区域
          geom_rect(data=bedGraphData_selected,
                    mapping=aes(xmin = chromStartA,
                                xmax = chromEndA,
                                ymin = 0, ymax = dataValue),
                    fill = "steelblue", color = "steelblue") + #整体区域
          scale_x_continuous(breaks = c(as.numeric(chr_start_plot),
                                        # as.numeric(Highlight_Locations$start),
                                        # as.numeric(Highlight_Locations$end),
                                        as.numeric(chr_end_plot)),
                             labels = comma,
                             limits = c(chr_start_plot,chr_end_plot),
                             expand = c(0, 0)) +
          scale_y_continuous(limits = c(min(DataRange),max(DataRange)),
                             breaks = c(as.numeric(quantile(seq(0,max(DataRange))))[1:5]),
                             labels = c(as.numeric(quantile(seq(0,max(DataRange))))[1:5]),
                             expand = c(0, 0)) +
          theme_bw() +
          labs(y = sample_names,
               x = NULL,
               title = paste0('[',DataRange[1],',',DataRange[2],']'))+
          theme.set
      }
      message(paste0('Got Plot ',m,'.'))
    }
    
    #输出
    return(wrap_plots(result_plots,ncol = 1))
  }

#双样本对照和实验组X轴两侧相反方向的绘图
doubleBedGraphCtlCasePlot<-
  function(
    bedGraphData_case, #实验组数据,read.table读取（X轴上方）
    bedGraphData_ctl, #对照组数据,read.table读取（X轴下方）
    chrom_A, #染色体序号
    plot_location, #绘图X轴区域
    highlight_location_ctl=NULL, #对照组高亮区域
    highlight_location_case=NULL, #实验组高亮区域
    sample_name_ctl,
    sample_name_case, #样本名称
    DataRange, #绘图Y轴范围
    x_axis_label=T #默认要不要X轴的标度标签
  ){
    
    #参数部分示例
    # chrom_A<-'chr11'
    # plot_location<-'31700000-31850000'
    # highlight_location<-c('31720000-31750000','31784779-31817961')
    # DataRange<-c(0,50)
    
    #加载R包
    library(stringr)
    library(dplyr)
    library(scales)
    library(ggplot2)
    
    message('Reading Data of Control Sample...') #读取对照组数据提示
    bedGraphDataCTL<-bedGraphData_ctl #读取对照组数据
    colnames(bedGraphDataCTL)<-c('chromA','chromStartA','chromEndA','peakName','dataValue') #重命名列
    bedGraphDataCTL$Group<-'Control' #分组信息，后面绘图填色会用到
    bedGraphDataCTL$dataValue<- bedGraphDataCTL$dataValue*(-1) #因为在X轴下方所以乘-1
    
    message('Reading Data of Case Sample...') #读取实验组数据提示
    bedGraphDataCASE<-bedGraphData_case #读取实验组数据
    colnames(bedGraphDataCASE)<-c('chromA','chromStartA','chromEndA','peakName','dataValue') #重命名列
    bedGraphDataCASE$Group<-'Case'#分组信息，后面绘图填色会用到
    
    message('Binding Data...') #组合数据提示
    bedGraphData<-rbind(bedGraphDataCASE,bedGraphDataCTL) #组合数据
    bedGraphData$Group<-factor(bedGraphData$Group,
                               levels = c('Case','Control'))
    
    message('Processing Locations...') #处理位置字符串提示
    #根据序列位置的字符串切割出绘图区域开始和结束位置
    chr_start_plot<-str_sub(plot_location,1,
                            (as.numeric(str_locate(plot_location,'-')[1,1])-1)) %>%
      as.numeric()
    chr_end_plot<-str_sub(plot_location,
                          (as.numeric(str_locate(plot_location,'-')[1,1])+1),
                          nchar(plot_location)) %>%
      as.numeric()
    
    #筛选出对应染色体的数据
    message('Filtering Data...') #筛选数据提示
    bedGraphData_selected<-bedGraphData %>%
      dplyr::filter(chromA==chrom_A) %>%
      dplyr::filter(chromEndA>chr_start_plot) %>%
      dplyr::filter(chromStartA<=chr_end_plot)
    bedGraphData_selected$Group<-factor(bedGraphData_selected$Group,
                                        levels = c('Case','Control'))
    message('Creating Plot...') #正在绘图提示
    #绘图风格
    if(x_axis_label==T){
      x_axis_text_size<-12
    }else if(x_axis_label==F){
      x_axis_text_size<-0
    }
    theme.set = ggplot2::theme(
      axis.title = ggplot2::element_text(size=12,face = "bold", family = "Times"),
      axis.text.x = ggplot2::element_text(size=x_axis_text_size,
                                          face = "bold", family = "Times",
                                          angle = 90,vjust = 0.5),
      axis.text.y = ggplot2::element_text(size=12,face = "bold", family = "Times"),
      legend.text = ggplot2::element_text(size=12,face = "bold", family = "Times"),
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size=12,face = "bold", family = "Times"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "Times"))
    
    #根据序列位置的字符串切割出高亮区域开始和结束位置
    ## 建立高亮区域起止位置变量
    ###对照组
    if(length(highlight_location_ctl)==0){
      Highlight_Locations_ctl<-NA
    }else{
      Highlight_Locations_ctl<-data.frame(matrix(nrow=length(highlight_location_ctl),
                                                 ncol = 2))
      colnames(Highlight_Locations_ctl)<-c('start','end')
      for(i in 1:nrow(Highlight_Locations_ctl)){
        Highlight_Locations_ctl$start[i]<-str_sub(highlight_location_ctl[i],1,
                                                  (as.numeric(str_locate(highlight_location_ctl[i],'-')[1,1])-1)) %>%
          as.numeric()
        Highlight_Locations_ctl$end[i]<-str_sub(highlight_location_ctl[i],
                                                (as.numeric(str_locate(highlight_location_ctl[i],'-')[1,1])+1),
                                                nchar(highlight_location_ctl[i])) %>%
          as.numeric()
      }
    }
    ###实验组
    if(length(highlight_location_case)==0){
      Highlight_Locations_case<-NA
    }else{
      Highlight_Locations_case<-data.frame(matrix(nrow=length(highlight_location_case),
                                                  ncol = 2))
      colnames(Highlight_Locations_case)<-c('start','end')
      for(i in 1:nrow(Highlight_Locations_case)){
        Highlight_Locations_case$start[i]<-str_sub(highlight_location_case[i],1,
                                                   (as.numeric(str_locate(highlight_location_case[i],'-')[1,1])-1)) %>%
          as.numeric()
        Highlight_Locations_case$end[i]<-str_sub(highlight_location_case[i],
                                                 (as.numeric(str_locate(highlight_location_case[i],'-')[1,1])+1),
                                                 nchar(highlight_location_case[i])) %>%
          as.numeric()
      }
    }
    
    
    #绘图
    ##基础部分
    p_base <- ggplot2::ggplot() +
      ggplot2::scale_x_continuous(breaks = c(as.numeric(chr_start_plot),
                                             # as.numeric(Highlight_Locations$start),
                                             # as.numeric(Highlight_Locations$end),
                                             as.numeric(chr_end_plot)),
                                  labels = comma,
                                  limits = c(chr_start_plot,chr_end_plot),
                                  expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c((-max(DataRange)),max(DataRange)),
                                  breaks = c(as.numeric(quantile(seq(0,max(DataRange))))[1:5],
                                             -(as.numeric(quantile(seq(0,max(DataRange))))[1:5])),
                                  labels = c(as.numeric(quantile(seq(0,max(DataRange))))[1:5],
                                             (as.numeric(quantile(seq(0,max(DataRange))))[1:5])),
                                  expand = c(0, 0)) +
      ggplot2::theme_bw() +
      ggplot2::labs(y = NULL,
                    x = NULL,
                    title = paste0('[',DataRange[1],',',DataRange[2],']'))+
      theme.set
    ##峰部分
    p_peak<-list(
      ggplot2::geom_rect(data=bedGraphData_selected,
                         mapping=aes(xmin = chromStartA,
                                     xmax = chromEndA,
                                     ymin = 0, ymax = dataValue,
                                     fill=Group,color=Group )), #整体区域
      ggplot2::scale_fill_manual(
        values = c("Control" = "blue", "Case" = "red")
      ),
      ggplot2::scale_color_manual(
        values = c("Control" = "blue", "Case" = "red")
      ),
      ggplot2::geom_text(mapping = aes(x=mean(c(chr_start_plot,chr_end_plot)),
                                       y=max(DataRange)*0.9),
                         label=sample_name_case,
                         show.legend = FALSE),
      ggplot2::geom_text(mapping = aes(x=mean(c(chr_start_plot,chr_end_plot)),
                                       y=-max(DataRange)*0.9),
                         label=sample_name_ctl,
                         show.legend = FALSE),
      ggplot2::geom_hline(yintercept = 0, color='gray',
                          show.legend = FALSE)
    )
    ###实验组高亮部分
    if(length(which(is.na(Highlight_Locations_case)))==0){
      p_highlight_case<-
        ggplot2::geom_rect(data=Highlight_Locations_case,
                           mapping=aes(xmin = start,
                                       xmax = end,
                                       ymin = 0,
                                       ymax = DataRange[2]),
                           fill = 'yellow', alpha = 0.3) #高亮区域
    }else{p_highlight_case<-NULL}
    ###对照组高亮部分
    if(length(which(is.na(Highlight_Locations_ctl)))==0){
      p_highlight_ctl<-
        ggplot2::geom_rect(data=Highlight_Locations_ctl,
                           mapping=aes(xmin = start,
                                       xmax = end,
                                       ymin = -DataRange[2],
                                       ymax = 0),
                           fill = 'yellow', alpha = 0.3) #高亮区域
    }else{p_highlight_ctl<-NULL}
    
    #合成图
    result<-
      p_base+
      p_highlight_case+p_highlight_ctl+
      p_peak
    
    #输出
    return(result)
  }


#----------------------------1级基础模块----------------------------------------
#单个样本、多区域、根据高亮区域间断绘图
singleBedGraphSamplePlot_Break<-
  function(
    bedGraphData, #数据,read.table读取
    chrom_A, #染色体序号
    plot_location, #绘图X轴区域
    highlight_location=NULL, #高亮区域
    sample_names, #样本名称
    DataRange, #绘图Y轴范围
    x_axis_label=T, #默认要不要X轴的标度标签
    expand_times #高亮区间左右背景区间是高亮区间的多少倍数
  ){
    if(length(highlight_location)==1){
      outputPlotResult<-
        singleBedGraphSamplePlot(
          bedGraphData, #数据,read.table读取
          chrom_A, #染色体序号
          plot_location=
            getAppropriateVisualSegment(
              highlight_location[1],
              expand_times,
              expand_times
            ), #绘图X轴区域
          highlight_location=highlight_location[1], #高亮区域
          sample_names, #样本名称
          DataRange, #绘图Y轴范围
          x_axis_label=T #默认要不要X轴的标度标签
        )
    }else if(length(highlight_location)>1){
      #计算多个区域的长度比值
      # 提取每个区段的长度（闭区间，长度 = end - start + 1）
      Highlight_Locs_lengths <- sapply(strsplit(highlight_location, "-"), function(x) {
        start <- as.numeric(x[1])
        end   <- as.numeric(x[2])
        end - start + 1
      })
      # 计算比值，使总和为 1
      Highlight_Locs_length_ratios <- Highlight_Locs_lengths / sum(Highlight_Locs_lengths)
      
      #主题列表：除了第一张图，其他不要左上角范围和Y轴标题
      SinglePlotTheme<-list(
        ggplot2::theme()
      )
      for(i in 2:length(highlight_location)){
        SinglePlotTheme[[i]]<-
          ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                         plot.title = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank())+
          ggplot2::labs(y=NULL)
      }
      #峰值绘图区域的结果变量
      resultPlotList<-list()
      for(i in 1:length(highlight_location)){
        resultPlotList[[i]]<-
          singleBedGraphSamplePlot(
            bedGraphData, #数据,read.table读取
            chrom_A, #染色体序号
            plot_location=
              getAppropriateVisualSegment(
                highlight_location[i],
                expand_times,
                expand_times
              ), #绘图X轴区域
            highlight_location=highlight_location[i], #高亮区域
            sample_names=ifelse(i==1,sample_names,''), #样本名称
            DataRange, #绘图Y轴范围
            x_axis_label=T #默认要不要X轴的标度标签
          )+SinglePlotTheme[[i]]
      }
      outputPlotResult<-
        patchwork::wrap_plots(resultPlotList,nrow = 1)+
        plot_layout(widths = Highlight_Locs_length_ratios)
    }
    return(outputPlotResult)
  }

#对照和实验双样本、多区域、根据高亮区域间断绘图
doubleBedGraphCtlCasePlot_Break<-
  function(
    bedGraphData_case, #实验组数据,read.table读取（X轴上方）
    bedGraphData_ctl, #对照组数据,read.table读取（X轴下方）
    chrom_A, #染色体序号
    plot_location, #绘图X轴区域
    highlight_location_ctl=NULL, #对照组高亮区域
    highlight_location_case=NULL, #实验组高亮区域
    sample_name_ctl,
    sample_name_case, #样本名称
    DataRange, #绘图Y轴范围
    x_axis_label=T, #默认要不要X轴的标度标签
    expand_times,
    title_whole_plot=NULL
  ){
    
    #合并区间
    highlight_location<-
      merge_highlight_intervals(
        highlight_location_case,
        highlight_location_ctl
      )
    
    #区分单个和多个区间的情况
    if(length(highlight_location)==1){
      outputPlotResult<-
        doubleBedGraphCtlCasePlot(
          bedGraphData_case, #实验组数据,read.table读取（X轴上方）
          bedGraphData_ctl, #对照组数据,read.table读取（X轴下方）
          chrom_A, #染色体序号
          plot_location=
            getAppropriateVisualSegment(
              highlight_location[1],
              expand_times,
              expand_times
            ), #绘图X轴区域
          highlight_location=highlight_location[1], #高亮区域
          sample_name_ctl,
          sample_name_case, #样本名称
          DataRange, #绘图Y轴范围
          x_axis_label=T #默认要不要X轴的标度标签
        )
    }else if(length(highlight_location)>1){
      #计算多个区域的长度比值
      # 提取每个区段的长度（闭区间，长度 = end - start + 1）
      Highlight_Locs_lengths <- sapply(strsplit(highlight_location, "-"), function(x) {
        start <- as.numeric(x[1])
        end   <- as.numeric(x[2])
        end - start + 1
      })
      # 计算比值，使总和为 1
      Highlight_Locs_length_ratios <- Highlight_Locs_lengths / sum(Highlight_Locs_lengths)
      
      #主题列表：除了第一张图，其他不要左上角范围和Y轴标题。除了最后一张图，其他不要图例。
      SinglePlotTheme<-list()
      SinglePlotTheme[[1]]<-
        ggplot2::theme(legend.position = 'none')
      for(i in 2:(length(highlight_location))){
        SinglePlotTheme[[i]]<-
          ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                         plot.title = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank(),
                         legend.position = 'none')+
          ggplot2::labs(y=NULL)
      }
      #峰值绘图区域的结果变量
      resultPlotList<-list()
      for(i in 1:length(highlight_location)){
        resultPlotList[[i]]<-
          doubleBedGraphCtlCasePlot(
            bedGraphData_case, #实验组数据,read.table读取（X轴上方）
            bedGraphData_ctl, #对照组数据,read.table读取（X轴下方）
            chrom_A, #染色体序号
            plot_location=
              getAppropriateVisualSegment(
                highlight_location[i],
                expand_times,
                expand_times
              ), #绘图X轴区域
            highlight_location_ctl=
              filter_highlights_within_interval(
                highlight_location[i],
                highlight_location_ctl
              ), #对照组高亮区域
            highlight_location_case=
              filter_highlights_within_interval(
                highlight_location[i],
                highlight_location_case
              ), #实验组高亮区域
            sample_name_ctl=ifelse(i==1,sample_name_ctl,''),
            sample_name_case=ifelse(i==1,sample_name_case,''), #样本名称
            DataRange, #绘图Y轴范围
            x_axis_label=T #默认要不要X轴的标度标签
          )+SinglePlotTheme[[i]]
      }
      p_legend_manual<-
        ggplot()+
        ggplot2::geom_rect(aes(xmin=0,xmax=1,ymin=0.5,ymax=1.5),fill='red')+
        ggplot2::geom_rect(aes(xmin=0,xmax=1,ymin=-0.5,ymax=-1.5),fill='blue')+
        ggplot2::geom_text(aes(x=2,y=1),fontface = "bold",
                           label='Case')+
        ggplot2::geom_text(aes(x=2.5,y=-1),fontface = "bold",
                           label='Control')+
        ggplot2::scale_y_continuous(limits = c((-10),10))+
        ggplot2::scale_x_continuous(limits = c((0),4))+
        ggplot2::theme_void()+
        ggplot2::coord_fixed()
      resultPlotList[[length(highlight_location)+1]]<-p_legend_manual
      outputPlotResult<-
        patchwork::wrap_plots(resultPlotList,nrow = 1)+
        patchwork::plot_layout(widths = c(Highlight_Locs_length_ratios,0.075))+
        patchwork::plot_annotation(
          title=title_whole_plot,
          theme=ggplot2::theme(
            plot.title = ggplot2::element_text(face = 'bold', hjust =0.5)
          )
        )
      
    }
    return(outputPlotResult)
  }


#MACS2结果区段绘图
plotPeaksMACS2<-function(
    dirs, #MACS2结果的文件夹地址，多个样本，向量
    chrom_A, #染色体序号
    plot_location, #绘图X轴区域
    sample_names, #样本名称，和dirs对应顺序
    sig_item='p', #显著性，看p值还是q值
    sig_level=0.05, #显著性阈值
    logFC_cutoff=2, #logFC阈值
    title_label=F #要不要在左上角加上标题标签 MACS2 Significant Peaks
){
  #参数部分示例
  # chrom_A<-'chr11'
  # plot_location<-'31700000-31850000'
  # highlight_location<-c('31720000-31750000','31784779-31817961')
  # sample_names<-c('B1-C','B2-KD')
  # DataRange<-c(0,50)
  
  #加载R包
  library(stringr)
  library(dplyr)
  library(scales)
  library(patchwork)
  library(ggplot2)
  library(ggrepel)
  
  #拼图用的结果变量
  assign('result_plots',list())
  
  #X轴文本标签大小，仅保留最下方的文本
  x_axis_text_size<-c(rep(0,(length(dirs)-1)),12)
  #标题标签
  if(title_label==T){
    plot_titles<-c('MACS2 Significant Peaks',rep('',(length(dirs)-1)))
  }else if(title_label==F){
    plot_titles<-c('',rep('',(length(dirs)-1)))
  }
  
  #建立循环绘制不同文件对应的图
  for(m in 1:length(dirs)){
    print(paste0('Reading Data ',m,'...'))
    peak_data<-#读取数据
      read.table(list.files(path = dirs[[m]], pattern = '.xls',full.names = T),
                 header = T,
                 comment.char = '#')
    
    print(paste0('Processing Data of Sample ',m,'...'))
    #根据序列位置的字符串切割出绘图区域开始和结束位置
    chr_start_plot<-str_sub(plot_location,1,
                            (as.numeric(str_locate(plot_location,'-')[1,1])-1)) %>%
      as.numeric()
    chr_end_plot<-str_sub(plot_location,
                          (as.numeric(str_locate(plot_location,'-')[1,1])+1),
                          nchar(plot_location)) %>%
      as.numeric()
    
    #筛选出显著的数据
    if(sig_item=='p'){
      peak_data_selected<-peak_data %>%
        dplyr::filter(chr==chrom_A) %>%
        dplyr::filter(end>chr_start_plot) %>%
        dplyr::filter(start<=chr_end_plot) %>%
        dplyr::filter(`X.log10.pvalue.`>(-log10(sig_level))) %>%
        dplyr::filter(fold_enrichment>logFC_cutoff)
    }else if(sig_item=='q'){
      peak_data_selected<-peak_data %>%
        dplyr::filter(chr==chrom_A) %>%
        dplyr::filter(end>chr_start_plot) %>%
        dplyr::filter(start<=chr_end_plot) %>%
        dplyr::filter(`X.log10.qvalue.`>(-log10(sig_level))) %>%
        dplyr::filter(fold_enrichment>logFC_cutoff)
    }
    #峰名称标签文本的x轴坐标
    peak_data_selected$label_x_locs<-
      (peak_data_selected$start+peak_data_selected$end)/2
    
    message(paste0('Creating Plot of Sample ',m,'...'))
    #绘图风格
    theme.set = theme(
      axis.title = element_text(size=12,face = "bold", family = "Times"),
      axis.text.x = element_text(size=x_axis_text_size[m],
                                 face = "bold", family = "Times",
                                 angle = 90,vjust = 0.5),
      axis.text.y = element_text(size=0,face = "bold", family = "Times"),
      legend.text = element_text(size=12,face = "bold", family = "Times"),
      legend.title = element_text(size=12,face='bold',family = "Times"),
      plot.title = element_text(size=12,face = "bold", family = "Times"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks=element_blank(),
      text = element_text(family = "Times"))
    
    result_plots[[m]]<-
      ggplot()+
      geom_rect(data=peak_data_selected,
                mapping=aes(xmin = start,
                            xmax = end,
                            ymin = 0, ymax = 1),
                fill = "steelblue", color = "steelblue") + #整体区域
      geom_text_repel(data=peak_data_selected,
                      mapping=aes(x = label_x_locs,
                                  y = 0.9,
                                  label=name),family = "Times")+
      scale_x_continuous(breaks = c(as.numeric(chr_start_plot),
                                    as.numeric(chr_end_plot)),
                         labels = comma,
                         limits = c(chr_start_plot,chr_end_plot),
                         expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1),
                         expand = c(0, 0)) +
      theme_bw() +
      labs(y = sample_names[m],
           x = NULL,
           title = plot_titles[m])+
      theme.set
    print(paste0('Got Plot ',m,'.'))
  }
  
  #输出
  return(wrap_plots(result_plots,ncol = 1))
  
}




