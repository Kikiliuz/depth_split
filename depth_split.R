
library('dplyr')
set.seed(1)
x<-round(runif(min=100,max=10000,n=1231))
y<-runif(min=0,max=1,n=1231)
data<-data.frame(cbind(x,y))
depth_split = function(data, k, variable) {
	n = nrow(data) # 获取data的行数
	data = arrange(data, variable) # 对data进行排序
	depth_list=rep(0, n) # 创建新行，初始化为0
	for(i in c(1:n)) {
		depth_list[i] =i/k # 对每行的depth列，赋值组号
	}
	data$depth = depth_list # 创建新列
	data$split=cut(depth_list,k) #等宽分组
	return(data) # 返回
}
a<-depth_split(data=data,k=10,variable=y)
summarise(group_by(a,split),length(x),min(y),max(y))#检查排序
