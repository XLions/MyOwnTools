singleBedGraphSamplePlot<-
  function(
    bedGraphAdd, #文件地址
    chrom_A, #染色体序号 'chr11'
    plot_location, #绘图X轴区域 '31700000-32000000'
    highlight_location, #高亮区域 '31784779-31817961'
    sample_names, #样本名称 'B1-C'
    DataRange #绘图Y轴范围 c(0,50)
  ){
    
    # #参数部分示例
    # chrom_A<-'chr11'
    # plot_location<-'31700000-32000000'
    # highlight_location<-'31784779-31817961'
    # sample_names<-'B1-C'
    # DataRange<-c(0,50)
    
    #加载R包
    library(stringr)
    library(dplyr)
    library(scales)
    
    bedGraphData<-read.table(bedGraphAdd) #读取数据
    colnames(bedGraphData)<-c('chromA','chromStartA','chromEndA','dataValue') #重命名列
    
    #根据序列位置的字符串切割出绘图区域开始和结束位置
    chr_start_plot<-str_sub(plot_location,1,
                            (as.numeric(str_locate(plot_location,'-')[1,1])-1)) %>%
      as.numeric()
    chr_end_plot<-str_sub(plot_location,
                          (as.numeric(str_locate(plot_location,'-')[1,1])+1),
                          nchar(plot_location)) %>%
      as.numeric()
    #根据序列位置的字符串切割出高亮区域开始和结束位置
    chr_start_highlight<-str_sub(highlight_location,1,
                                 (as.numeric(str_locate(highlight_location,'-')[1,1])-1)) %>%
      as.numeric()
    chr_end_highlight<-str_sub(highlight_location,
                               (as.numeric(str_locate(highlight_location,'-')[1,1])+1),
                               nchar(highlight_location)) %>%
      as.numeric()
    #筛选出对应染色体的数据
    bedGraphData_selected<-bedGraphData %>%
      dplyr::filter(chromA==chrom_A) %>%
      dplyr::filter(chromEndA>chr_start_plot) %>%
      dplyr::filter(chromStartA<=chr_end_plot)
    
    #绘图风格
    theme.set = theme(
      axis.title = element_text(size=12,face = "bold", family = "Times"),
      axis.text.x = element_text(size=12,face = "bold", family = "Times",
                                 angle = 90,vjust = 0.5),
      axis.text.y = element_text(size=12,face = "bold", family = "Times"),
      legend.text = element_text(size=12,face = "bold", family = "Times"),
      legend.title = element_text(size=12,face='bold',family = "Times"),
      plot.title = element_text(size=12,face = "bold", family = "Times"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      text = element_text(family = "Times"))
    #绘图
    result<-
      ggplot() +
      geom_rect(mapping=aes(xmin = as.numeric(chr_start_highlight), 
                            xmax = as.numeric(chr_end_highlight),
                            ymin = DataRange[1], 
                            ymax = DataRange[2]),
                fill = 'yellow', alpha = 0.3)+ #高亮区域
      geom_rect(data=bedGraphData_selected,
                mapping=aes(xmin = chromStartA, 
                            xmax = chromEndA, 
                            ymin = 0, ymax = dataValue),
                fill = "steelblue", color = "steelblue") + #整体区域
      scale_x_continuous(breaks = c(as.numeric(chr_start_plot),
                                    as.numeric(chr_start_highlight),
                                    as.numeric(chr_end_highlight),
                                    as.numeric(chr_end_plot)),
                         labels = comma,
                         limits = c(chr_start_plot,chr_end_plot),
                         expand = c(0, 0)) +
      scale_y_continuous(limits = c(min(DataRange),max(DataRange)),
                         breaks = c(as.numeric(quantile(seq(0,50)))[1:5]),
                         labels = c(as.numeric(quantile(seq(0,50)))[1:5]),
                         expand = c(0, 0)) + 
      theme_bw() +
      labs(y = sample_names,
           x = NULL,
           title = paste0('[',DataRange[1],',',DataRange[2],']'))+
      theme.set
    
    #输出
    return(result)
  }
