  #将一堆数值转换为连续的区间开始结束位点的函数
  #输入：所有数值
  #输出：列表：开始位点列和结束位点列
  Number_PointsToSegment<-function(numbers){
    if(length(numbers)>1){
      numbers<-sort(numbers)
      starts<-c(numbers[1])
      ends<-c()
      for(i in 2:length(numbers)){
        if((numbers[i]-numbers[i-1])>1){
          ends<-c(ends,numbers[i-1])
          starts<-c(starts,numbers[i])
        }
      }
      ends<-c(ends,numbers[length(numbers)])
      if(length(ends)==length(starts)){
        result<-list(
          start=starts,
          end=ends,
          length=length(ends)
        )
      }else{
        result<-list(
          start=starts,
          end=ends,
          length='Ends and Starts have different lengths')
      }
      return(result) 
    }else if(length(numbers)==1){
      return(list(
        start=numbers,
        end=numbers,
        length=1))
    }
  }
