#グンベル確率紙
# 測定値をグンベル確率紙にプロットする
Pgumbel <- function( Variable,Xlabel="Observed Value",MainTitle="Gumbel Probability Paper"
                     ,color="gray")                             # データベクトル

{
  x <- Variable
  gumb <- function(p) -log(-log(p))    # グンベル分布の尺度に変更
  x <- x[!is.na(x)]                            # 欠損値を除く
  n <- length(x)                                       # 有効データ数
  x <- sort(x)                                 # 昇順に並べ替える
  y <- gumb(1:n/(n+1))                         # グンベル分布における累積密度の位置(ミーンランク)
  y0 <- c(10^(-10:0), 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99, 99.9, 99.99, 99.999)
  y0 <- y0[y0 > 10/n]
  probs <- gumb(y0/100)                                # 目盛数値の位置
  plot(c(x[1], x[n]), c(probs[1], probs[length(probs)]), type="n", xaxt="n", yaxt="n",
       xlab=Xlabel, ylab="Cumulative Percent",
       main=MainTitle)
  abline(h=probs, col=color)                 # 水平の格子線
  abline(v=axTicks(1),col=color)             # 垂直の格子線axTicksは目盛の値を得る関数
  axis(side=1,labels=TRUE)                    # 横軸を描く
  axis(2, at=probs, labels=y0)                    # 縦軸を描く
  points(x, y)                                #データ点を描く
}
#Frechet確率紙
# 測定値をFrechet確率紙にプロットする
Pfrechet <- function( Variable,Xlabel="Observed Value",MainTitle="Frechet Probability Paper"
                      ,color="gray")                             # データベクトル

{
  x <- Variable
  frech <- function(p) -log(-log(p))    #frechet分布の尺度に変更
  log.axis <- function(z)                              # 対数軸を描く関数
  {
    z <- floor(log10(z))                 # 対数にしたときの整数部
    log.min <- min(z)                    # 最小値
    bb <- c(1.0,5.0,10.0)
    z2 <- 1:10*10^log.min                        # 値の範囲をカバーするように
    zz <- bb*10^log.min
    n <- max(z)-log.min                  # 10 倍しながら順次，右の位置に目盛りを描く
    z2 <- rep(z2, n+1)*10^rep(0:n, each=10)      # 対数目盛り位置の数値
    zz <- rep(zz, n+1)*10^rep(0:n, each=3)
    log.z2 <- log10(z2)                  # 目盛りを描く位置
    log.zz <- log10(zz)
    #axis(1, at=log.z2, labels=z2)           # log.z2 の位置に，z2 という数値を描く
    axis(1, at=log.zz, labels=zz)
    abline(v=log.z2, col=color)             # 垂直格子線を描く
    #abline(v=log.zz, col=color)
  }
  x <- x[!is.na(x)]                            # 欠損値を除く
  n <- length(x)                                       # 有効データ数
  x <- sort(x)                                 # 昇順に並べ替える
  log.x <- log10(x)                            # 常用対数を取る
  y <- frech(1:n/(n+1))                         # グンベル分布における累積密度の位置(ミーンランク)
  y0 <- c(10^(-10:0), 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99, 99.9, 99.99, 99.999)
  y0 <- y0[y0 > 10/n]
  probs <- frech(y0/100)                                # 目盛数値の位置
  plot(c(log.x[1], log.x[n]), c(probs[1], probs[length(probs)]), type="n", xaxt="n", yaxt="n",
       xlab=Xlabel, ylab="Cumulative Percent",
       main=MainTitle)
  abline(h=probs, col=color)                 # 水平の格子線
  log.axis(x)                                     # 横軸を描く
  #abline(v=axTicks(1),col="grey")             # 垂直の格子線axTicksは目盛の値を得る関数
  axis(2, at=probs, labels=y0)                    # 縦軸を描く
  points(log.x, y)                                #データ点を描く
}
#ワイブル確率紙
# 測定値をワイブル確率紙にプロットする
Pweibull <- function( Variable,Xlabel="Observed Value",MainTitle="Weibull Probability Paper",                            # データベクトル
                      color="gray")                   # 格子線を描く色
{
  x <- Variable
  weib <- function(p) log10(log10(1/(1-p)))    # ワイブル分布の尺度に変更
  log.axis <- function(z)                              # 対数軸を描く関数
  {
    z <- floor(log10(z))                 # 対数にしたときの整数部
    log.min <- min(z)                    # 最小値
    bb <- c(1.0,5.0,10.0)
    z2 <- 1:10*10^log.min                        # 値の範囲をカバーするように
    zz <- bb*10^log.min
    n <- max(z)-log.min                  # 10 倍しながら順次，右の位置に目盛りを描く
    z2 <- rep(z2, n+1)*10^rep(0:n, each=10)      # 対数目盛り位置の数値
    zz <- rep(zz, n+1)*10^rep(0:n, each=3)
    log.z2 <- log10(z2)                  # 目盛りを描く位置
    log.zz <- log10(zz)
    #             axis(1, at=log.z2, labels=z2)           # log.z2 の位置に，z2 という数値を描く
    axis(1, at=log.zz, labels=zz)
    abline(v=log.z2, col=color)             # 垂直格子線を描く
    abline(v=log.zz, col=color)
  }

  x <- x[!is.na(x)]                            # 欠損値を除く
  n <- length(x)                                       # 有効データ数
  x <- sort(x)                                 # 昇順に並べ替える
  log.x <- log10(x)                            # 常用対数を取る
  y <- weib(1:n/(n+1))                             # ワイブル分布における累積密度の位置
  y0 <- c(10^(-10:0), 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99, 99.9, 99.99, 99.999)
  y0 <- y0[y0 > 10/n]
  probs <- weib(y0/100)                                # 目盛数値の位置
  plot(c(log.x[1], log.x[n]), c(probs[1], probs[length(probs)]), type="n", xaxt="n", yaxt="n",
       xlab=Xlabel, ylab="Cumulative Percent",
       main=MainTitle)
  abline(h=probs, col=color)                     # 水平の格子線
  log.axis(x)                                     # 横軸を描く
  axis(2, at=probs, labels=y0)                    # 縦軸を描く
  points(log.x, y)                                #データ点を描く
}

# 正規確率紙に累積相対度数をプロットする
Pnorm <- function(Variable,Xlabel="Observed Value",MainTitle="Normal Probability Paper",                            # データベクトル
                  color="gray")          # データベクトル
{
  x <- x[!is.na(x)]    # 欠損値を持つケースを除く
  n <- length(x)               # データの個数
  x <- sort(x)         # 昇順に並べ替える
  y <- (1:n-0.5)/n     # 累積相対度数
  probs <- c(0.01, 0.1, 1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99, 99.9, 99.99)/100
  plot(c(x[1], x[n]), qnorm(c(probs[1], probs[17])), type="n", yaxt="n",
       xlab=Xlabel, ylab="Cumulative Percent",
       main=MainTitle)
  abline(h=qnorm(probs), col=color)                     # 水平の格子線
  abline(v=axTicks(1),col=color)             # 垂直の格子線axTicksは目盛の値を得る関数
  points(x, qnorm(y))
  axis(2, qnorm(probs), probs*100)
}
# 対数正規確率紙に累積相対度数をプロットする
Plnorm <- function(Variable,Xlabel="Observed Value",MainTitle="Log Normal Probability Paper",                            # データベクトル
                   color="gray")                                  # データベクトル
{
  x<-Variable
  log.axis <- function(z)                              # 対数軸を描く関数
  {
    color="gray"
    z <- floor(log10(z))                 # 対数にしたときの整数部
    log.min <- min(z)                    # 最小値
    bb <- c(1.0,5.0,10.0)
    z2 <- 1:10*10^log.min                        # 値の範囲をカバーするように
    zz <- bb*10^log.min
    n <- max(z)-log.min                  # 10 倍しながら順次，右の位置に目盛りを描く
    z2 <- rep(z2, n+1)*10^rep(0:n, each=10)      # 対数目盛り位置の数値
    zz <- rep(zz, n+1)*10^rep(0:n, each=3)
    log.z2 <- log10(z2)                  # 目盛りを描く位置
    log.zz <- log10(zz)
    #             axis(1, at=log.z2, labels=z2)           # log.z2 の位置に，z2 という数値を描く
    axis(1, at=log.zz, labels=zz)
    abline(v=log.z2, col=color)             # 垂直格子線を描く
    abline(v=log.zz, col=color)
  }
  n <- length(x)                                       # データの個数
  log.x <- log10(sort(x))                              # データをプロットするときの横座標
  y <- 1:n/n                                   # 累積確率
  probs <- c(0.01, 0.1, 1, 5, 10, 20, 30, 40, 50,      # 縦軸の目盛り
             60, 70, 80, 90, 95, 99, 99.9, 99.99)/100
  plot(log.x[c(1,n)], qnorm(probs[c(1,17)]),      # 枠組み
       type="n", xaxt="n", yaxt="n",
       xlab=Xlabel, ylab="Cumulative Percent",
       main=MainTitle)
  log.axis(x)                                     # 横軸（対数目盛）を描く
  axis(2, qnorm(probs), probs*100)                # 縦軸（正規確立目盛）を描く
  abline(h=qnorm(probs), col="grey")              # 水平格子線を描く
  points(log.x, qnorm(y))                         # データ点を描く
}
# 逆ワイブル確率紙
# 測定値を逆ワイブル確率紙にプロットする
Prweibull <- function(Variable,Xlabel="Observed Value",MainTitle="Reverse Weibull Probability Paper",          # データベクトル
                       color="gray")                   # 格子線を描く色
{
  x <- Variable
  rweib <- function(p) -log10(-log10(p))    # ワイブル分布の尺度に変更
  log.axis <- function(z)                              # 対数軸を描く関数
  {
    z <- floor(log10(z))                 # 対数にしたときの整数部
    log.min <- min(z)                    # 最小値
    bb <- c(1.0,5.0,10.0)
    z2 <- 1:10*10^log.min                        # 値の範囲をカバーするように
    zz <- bb*10^log.min
    n <- max(z)-log.min                  # 10 倍しながら順次，右の位置に目盛りを描く
    z2 <- rep(z2, n+1)*10^rep(0:n, each=10)      # 対数目盛り位置の数値
    zz <- rep(zz, n+1)*10^rep(0:n, each=3)
    log.z2 <- log10(z2)                  # 目盛りを描く位置
    log.zz <- log10(zz)
    #             axis(1, at=log.z2, labels=z2)           # log.z2 の位置に，z2 という数値を描く
    axis(1, at=log.zz, labels=zz)
    abline(v=log.z2, col=color)             # 垂直格子線を描く
    abline(v=log.zz, col=color)
  }

  x <- x[!is.na(x)]                            # 欠損値を除く
  n <- length(x)                                       # 有効データ数
  x <- sort(x,T)                                 #降順に並べ替える
  log.x <- log10(x)                            # 常用対数を取る
  y <- rweib(1:n/(n+1))                            # ワイブル分布における累積密度の位置
  y0 <- c(10^(-10:0), 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99, 99.9, 99.99, 99.999)
  y0 <- y0[y0 > 10/n]
  probs <- rweib(y0/100)                                # 目盛数値の位置
  plot(c(log.x[1], log.x[n]), c(probs[1], probs[length(probs)]), type="n", xaxt="n", yaxt="n",
       xlab=Xlabel, ylab="Cumulative Percent",
       main=MainTitle)
  abline(h=probs, col="grey")                     # 水平の格子線
  log.axis(x)                                     # 横軸を描く
  axis(2, at=probs, labels=y0)                    # 縦軸を描く
  points(log.x, y)                                #データ点を描く
}
