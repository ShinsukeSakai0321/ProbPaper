#グンベル確率紙へのプロット
PgumbelL <- function( Variable,Xlabel="Observed Value",MainTitle="Gumbel Probability Paper"
                     ,color="gray",names=c("1","2","3"),pchs=c(0,1,2))                             # データベクトル

{
  gumb <- function(p) -log(-log(p))    # グンベル分布の尺度に変更
  Lnum<-length(Variable) #number of label elements
  x<-NULL
  nmax<-0
  for(i in 1:Lnum){
    x <- c(x,Variable[[i]])
    nn<-length(Variable[[i]])
    if(nn>nmax)nmax<-nn
  }
  x <- x[!is.na(x)]
  xmin<-min(x); xmax<-max(x)
  y0 <- c(10^(-10:0), 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99, 99.9, 99.99, 99.999)
  y0 <- y0[y0 > 10/nmax]
  probs <- gumb(y0/100)                                # 目盛数値の位置
  plot(c(xmin, xmax), c(probs[1], probs[length(probs)]), type="n", xaxt="n", yaxt="n",
       xlab=Xlabel, ylab="Cumulative Percent",
       main=MainTitle)
  abline(h=probs, col=color)                 # 水平の格子線
  abline(v=axTicks(1),col=color)             # 垂直の格子線axTicksは目盛の値を得る関数
  axis(side=1,labels=TRUE)                    # 横軸を描く
  axis(2, at=probs, labels=y0)                    # 縦軸を描く
  for(i in 1:Lnum){
    x <- Variable[[i]]
    x <- x[!is.na(x)]                            # 欠損値を除く
    n <- length(x)                                       # 有効データ数
    y <- gumb(1:n/(n+1))                         # グンベル分布における累積密度の位置(ミーンランク)
    x <- sort(x)                                 # 昇順に並べ替える
    points(x, y,pch=pchs[i])                                #データ点を描く
  }
  legend("topleft",names,pch=pchs)
}
