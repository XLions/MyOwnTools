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
