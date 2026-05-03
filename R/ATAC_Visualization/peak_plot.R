
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



