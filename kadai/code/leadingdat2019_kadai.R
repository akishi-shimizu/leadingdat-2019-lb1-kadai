#### リーディンDAT　LーB1　課題

# データ取得元
# https://sites.google.com/site/shunichinomu/datkadai

##################################### セットアップ ##############################################

# RパッケージKFASの読み込み(R の起動ごとに必要)
library(KFAS) 

# 描画にplotlyを使う
#install.packages("plotly") #はじめの１回だけ必要
library(plotly)

# plotlyで日本語表示させるためのフォント指定
par(family= "HiraKakuProN-W3")

# グラフのフォントサイズ指定
t <- list(size = 20)


# 体重データの読み込み（パス指定が必要。難しければ、データ読み込み済.RDataを起動すれば最初からデータが入っています）
Foreigners <- read.csv("../data/訪日外客数_utf8.csv")
Foreigners <- transform(Foreigners,YearMonth=c(
  "2003-01","2003-02","2003-03","2003-04","2003-05","2003-06","2003-07","2003-08","2003-09","2003-10","2003-11","2003-12",
  "2004-01","2004-02","2004-03","2004-04","2004-05","2004-06","2004-07","2004-08","2004-09","2004-10","2004-11","2004-12",
  "2005-01","2005-02","2005-03","2005-04","2005-05","2005-06","2005-07","2005-08","2005-09","2005-10","2005-11","2005-12",
  "2006-01","2006-02","2006-03","2006-04","2006-05","2006-06","2006-07","2006-08","2006-09","2006-10","2006-11","2006-12",
  "2007-01","2007-02","2007-03","2007-04","2007-05","2007-06","2007-07","2007-08","2007-09","2007-10","2007-11","2007-12",
  "2008-01","2008-02","2008-03","2008-04","2008-05","2008-06","2008-07","2008-08","2008-09","2008-10","2008-11","2008-12",
  "2009-01","2009-02","2009-03","2009-04","2009-05","2009-06","2009-07","2009-08","2009-09","2009-10","2009-11","2009-12",
  "2010-01","2010-02","2010-03","2010-04","2010-05","2010-06","2010-07","2010-08","2010-09","2010-10","2010-11","2010-12",
  "2011-01","2011-02","2011-03","2011-04","2011-05","2011-06","2011-07","2011-08","2011-09","2011-10","2011-11","2011-12",
  "2012-01","2012-02","2012-03","2012-04","2012-05","2012-06","2012-07","2012-08","2012-09","2012-10","2012-11","2012-12",
  "2013-01","2013-02","2013-03","2013-04","2013-05","2013-06","2013-07","2013-08","2013-09","2013-10","2013-11","2013-12",
  "2014-01","2014-02","2014-03","2014-04","2014-05","2014-06","2014-07","2014-08","2014-09","2014-10","2014-11","2014-12",
  "2015-01","2015-02","2015-03","2015-04","2015-05","2015-06","2015-07","2015-08","2015-09","2015-10","2015-11","2015-12",
  "2016-01","2016-02","2016-03","2016-04","2016-05","2016-06","2016-07","2016-08","2016-09","2016-10","2016-11","2016-12",
  "2017-01","2017-02","2017-03","2017-04","2017-05","2017-06","2017-07","2017-08","2017-09","2017-10","2017-11","2017-12",
  "2018-01","2018-02","2018-03","2018-04","2018-05","2018-06","2018-07","2018-08","2018-09","2018-10","2018-11","2018-12"
  ))

# 全ての訪日外客数の描画
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~Foreigners[, "総数"]/10000, name = '総数', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "中国"]/10000, name = '中国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "香港"]/10000, name = '香港', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "台湾"]/10000, name = '台湾', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "韓国"]/10000, name = '韓国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "フィリピン"]/10000, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "タイ"]/10000, name = 'タイ', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "シンガポール"]/10000, name = 'シンガポール', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "マレーシア"]/10000, name = 'マレーシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "インドネシア"]/10000, name = 'インドネシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "インド"]/10000, name = 'インド', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "ベトナム"]/10000, name = 'ベトナム', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "フランス"]/10000, name = 'フランス', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "ロシア"]/10000, name = 'ロシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "イタリア"]/10000, name = 'イタリア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "米国"]/10000, name = '米国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "カナダ"]/10000, name = 'カナダ', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "豪州"]/10000, name = '豪州', type = 'scatter', mode = 'lines') %>%
  layout(
         xaxis = list(title = "年月"),
         yaxis = list (title = "月別訪日外客数 (万人)"),font=t)
p

##################################### 必須課題１ ##############################################

# フィリピンを選択する。
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~Foreigners[, "フィリピン"]/10000, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "年月"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (万人)"),font=t)
p


### 課題１
# 基本構造時系列モデルを適応し、最も当てはまりの
# 良いトレンド成分モデル＋季節成分モデルの組合せ
# をAICで選択しましょう

philippines <- ts(Foreigners[,"フィリピン"]/10000) # フィリピンのデータ（万人単位にスケール変換）

philippines

# モデル定義
mod1<-SSModel(philippines ~ SSMtrend(1,Q=NA) + SSMseasonal(12,Q=0 ), H=NA)
mod2<-SSModel(philippines ~ SSMtrend(1,Q=NA) + SSMseasonal(12,Q=NA), H=NA)
mod3<-SSModel(philippines ~ SSMtrend(2,Q=list(0,NA)) + SSMseasonal(12,Q=0 ), H=NA)
mod4<-SSModel(philippines ~ SSMtrend(2,Q=list(0,NA)) + SSMseasonal(12,Q=NA), H=NA)

# 未知パラメータの推定
fit1 <- fitSSM(mod1, numeric(2), method = "BFGS")
fit2 <- fitSSM(mod2, numeric(3), method = "BFGS")
fit3 <- fitSSM(mod3, numeric(2), method = "BFGS")
fit4 <- fitSSM(mod4, numeric(3), method = "BFGS")

# カルマンフィルタ・カルマンスムーザの実行
kfs1 <- KFS(fit1$model)
kfs2 <- KFS(fit2$model)
kfs3 <- KFS(fit3$model)
kfs4 <- KFS(fit4$model) 

# 最大対数尤度
logLik1 <- kfs1$logLik - sum(kfs1$Finf>0) * log(2*pi)/2 # -159.3184
logLik2 <- kfs2$logLik - sum(kfs2$Finf>0) * log(2*pi)/2 # -13.51217
logLik3 <- kfs3$logLik - sum(kfs3$Finf>0) * log(2*pi)/2 # -164.7324
logLik4 <- kfs4$logLik - sum(kfs4$Finf>0) * log(2*pi)/2 # -16.91601

logLik1
logLik2
logLik3
logLik4

# AIC (赤池情報量規準)
AIC1 <- -2*logLik1 + 2*( 2 + 12 ) # 346.6368
AIC2 <- -2*logLik2 + 2*( 3 + 12 ) # 57.02434（最良）
AIC3 <- -2*logLik3 + 2*( 2 + 13 ) # 359.4648
AIC4 <- -2*logLik4 + 2*( 3 + 13 ) # 65.83203

AIC1
AIC2
AIC3
AIC4


##################################### 必須課題２ ##############################################
# 残差分析を行って異常値を検出し、その原因を考察
# した上で、対処しましょう

# 水準成分の図
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~philippines, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs1$alphahat[,"level"], name = 'ローカルレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs2$alphahat[,"level"], name = 'ローカルレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs3$alphahat[,"level"], name = '平滑化トレンドレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs4$alphahat[,"level"], name = '平滑化トレンドレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "原系列とトレンド（水準）成分値"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (万人)")) %>%
  layout(legend = list(x = 0.1, y = 1),
         font=t)
p

# 季節成分の図
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  # add_trace(y = ~philippines, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs1$alphahat[,"sea_dummy1"], name = 'ローカルレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs2$alphahat[,"sea_dummy1"], name = 'ローカルレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs3$alphahat[,"sea_dummy1"], name = '平滑化トレンドレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs4$alphahat[,"sea_dummy1"], name = '平滑化トレンドレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "季節成分"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (万人)")) %>%
  layout(legend = list(x = 0.1, y = 1),
         font=t)
  
p

# 残差の図
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  #add_trace(y = ~Foreigners[, "フィリピン"]/10000, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~philippines-kfs1$muhat, name = 'ローカルレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~philippines-kfs2$muhat, name = 'ローカルレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~philippines-kfs3$muhat, name = '平滑化トレンドレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~philippines-kfs4$muhat, name = '平滑化トレンドレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "平滑化後の残差"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (万人)")) %>%
  layout(legend = list(x = 0.1, y = 1), font=t)
p


########################################################


# 残差がとても大きいので、当てはまりが良いとは言えない。
# 年月が進むにつれて、来客者数が増えているため、
# 季節変動の幅が来客者数の規模に応じて変化している。
# => 前処理として対数変換を施す。
# 
# 対数変換バージョン
#

# モデル定義
mod1Log<-SSModel(log(philippines) ~ SSMtrend(1,Q=NA) + SSMseasonal(12,Q=0,sea.type = "dummy"), H=NA)
mod2Log<-SSModel(log(philippines)  ~ SSMtrend(1,Q=NA) + SSMseasonal(12,Q=NA,sea.type = "dummy"), H=NA)
mod3Log<-SSModel(log(philippines)  ~ SSMtrend(2,Q=list(0,NA)) + SSMseasonal(12,Q=0,sea.type = "dummy" ), H=NA)
mod4Log<-SSModel(log(philippines)  ~ SSMtrend(2,Q=list(0,NA)) + SSMseasonal(12,Q=NA,sea.type = "dummy"), H=NA)


# 未知パラメータの推定
fit1Log <- fitSSM(mod1Log, numeric(2)) 
fit2Log <- fitSSM(mod2Log, numeric(3)) 
fit3Log <- fitSSM(mod3Log, numeric(2)) 
fit4Log <- fitSSM(mod4Log, numeric(3)) 


# カルマンフィルタ・カルマンスムーザの実行
kfs1Log <- KFS(fit1Log$model, smoothing=c("state","mean","disturbance"))
kfs2Log <- KFS(fit2Log$model, smoothing=c("state","mean","disturbance"))
kfs3Log <- KFS(fit3Log$model, smoothing=c("state","mean","disturbance"))
kfs4Log <- KFS(fit4Log$model, smoothing=c("state","mean","disturbance")) 


# 最大対数尤度
logLik1Log <- kfs1Log$logLik - sum(kfs1Log$Finf>0) * log(2*pi)/2
logLik2Log <- kfs2Log$logLik - sum(kfs2Log$Finf>0) * log(2*pi)/2
logLik3Log <- kfs3Log$logLik - sum(kfs3Log$Finf>0) * log(2*pi)/2
logLik4Log <- kfs4Log$logLik - sum(kfs4Log$Finf>0) * log(2*pi)/2

# AIC (赤池情報量規準)
AIC1Log <- -2*logLik1Log + 2*( 2 + 12 ) # -41.22831
AIC2Log <- -2*logLik2Log + 2*( 3 + 12 ) # -109.5595（最良）
AIC3Log <- -2*logLik3Log + 2*( 2 + 13 ) # -14.59486
AIC4Log <- -2*logLik4Log + 2*( 3 + 13 ) # -12.59486

AIC1Log
AIC2Log
AIC3Log
AIC4Log

# 水準成分の図
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~log(philippines), name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs1Log$alphahat[,"level"], name = 'ローカルレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs2Log$alphahat[,"level"], name = 'ローカルレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs3Log$alphahat[,"level"], name = '平滑化トレンドモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfs4Log$alphahat[,"level"], name = '平滑化トレンドモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "対数系列とトレンド（水準）成分の平滑化推定値"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (対数)")) %>%
  layout(legend = list(x = 0, y = 1), font=t)
p


# 観測値撹乱項の標準残差（ピアソン残差）
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~rstandard(kfs1Log, "pearson"), name = 'ローカルレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs2Log, "pearson"), name = 'ローカルレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs3Log, "pearson"), name = '平滑化トレンドモデル、固定季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs4Log, "pearson"), name = '平滑化トレンドモデル、可変季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~1.96, name = '95%信頼区間（上限）', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot'), showlegend = FALSE) %>%
  add_trace(y = ~-1.96, name = '95%信頼区間（下限）', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot'), showlegend = FALSE)  %>%
  layout(xaxis = list(title = "観測値撹乱項の補助残差（ピアソン残差）"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (対数)")) %>%
  layout(legend = list(x = 0.1, y = 1), font=t)
p


# 状態撹乱項の標準残差（ピアソン残差）
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~rstandard(kfs1Log, "state")[,1], name = 'ローカルレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs2Log, "state")[,1], name = 'ローカルレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs3Log, "state")[,1], name = '平滑化トレンドモデル、固定季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs4Log, "state")[,1], name = '平滑化トレンドモデル、可変季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~1.96, name = '95%信頼区間（上限）', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot'), showlegend = FALSE) %>%
  add_trace(y = ~-1.96, name = '95%信頼区間（下限）', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot'), showlegend = FALSE)  %>%
  layout(xaxis = list(title = "状態撹乱項（水準成分）の補助残差"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (対数)")) %>%
  layout(legend = list(x = 0.1, y = 1), font=t)
p



#
# 原因の考察と対処
#
# 東日本大震災の激減を捉えられていない。
# 対処：東日本大震災の激減を一時的な変化とみなし、
# 2011年3,4,5,6月のデータを異常値として除外（NAとして欠損値扱い）する。

philippinesNA <- philippines
philippinesNA[99] <- NA # 2011年3月は99番目
philippinesNA[100] <- NA # 2011年4月は100番目
philippinesNA[101] <- NA # 2011年5月は101番目
philippinesNA[102] <- NA # 2011年6月は101番目

# モデル定義
mod1Log<-SSModel(log(philippinesNA) ~ SSMtrend(1,Q=NA) + SSMseasonal(12,Q=0,sea.type = "dummy"), H=NA)
mod2Log<-SSModel(log(philippinesNA)  ~ SSMtrend(1,Q=NA) + SSMseasonal(12,Q=NA,sea.type = "dummy"), H=NA)
mod3Log<-SSModel(log(philippinesNA)  ~ SSMtrend(2,Q=list(0,NA)) + SSMseasonal(12,Q=0,sea.type = "dummy" ), H=NA)
mod4Log<-SSModel(log(philippinesNA)  ~ SSMtrend(2,Q=list(0,NA)) + SSMseasonal(12,Q=NA,sea.type = "dummy"), H=NA)

# 未知パラメータの推定
fit1Log <- fitSSM(mod1Log, numeric(2)) 
fit2Log <- fitSSM(mod2Log, numeric(3)) 
fit3Log <- fitSSM(mod3Log, numeric(2)) 
fit4Log <- fitSSM(mod4Log, numeric(3)) 

# カルマンフィルタ・カルマンスムーザの実行
kfs1Log <- KFS(fit1Log$model, smoothing=c("state","mean","disturbance"))
kfs2Log <- KFS(fit2Log$model, smoothing=c("state","mean","disturbance"))
kfs3Log <- KFS(fit3Log$model, smoothing=c("state","mean","disturbance"))
kfs4Log <- KFS(fit4Log$model, smoothing=c("state","mean","disturbance")) 

# 最大対数尤度
logLik1Log <- kfs1Log$logLik - sum(kfs1Log$Finf>0) * log(2*pi)/2
logLik2Log <- kfs2Log$logLik - sum(kfs2Log$Finf>0) * log(2*pi)/2
logLik3Log <- kfs3Log$logLik - sum(kfs3Log$Finf>0) * log(2*pi)/2
logLik4Log <- kfs4Log$logLik - sum(kfs4Log$Finf>0) * log(2*pi)/2

# AIC (赤池情報量規準)
AIC1Log <- -2*logLik1Log + 2*( 2 + 12 ) # -42.90388
AIC2Log <- -2*logLik2Log + 2*( 3 + 12 ) # -53.27841（最良）
AIC3Log <- -2*logLik3Log + 2*( 2 + 13 ) # -32.03087
AIC4Log <- -2*logLik4Log + 2*( 3 + 13 ) # -30.03087

AIC1Log
AIC2Log
AIC3Log
AIC4Log

# 観測値撹乱項の標準残差（ピアソン残差）
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~rstandard(kfs1Log, "pearson"), name = 'ローカルレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs2Log, "pearson"), name = 'ローカルレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs3Log, "pearson"), name = '平滑化トレンドモデル、固定季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs4Log, "pearson"), name = '平滑化トレンドモデル、可変季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~1.96, name = '95%信頼区間（上限）', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot'), showlegend = FALSE) %>%
  add_trace(y = ~-1.96, name = '95%信頼区間（下限）', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot'), showlegend = FALSE)  %>%
  layout(xaxis = list(title = "東日本大震災を異常値として扱う場合の観測値撹乱項の補助残差（ピアソン残差）"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (対数)")) %>%
  layout(legend = list(x = 0.2, y = 1), font=t)
p


# 状態撹乱項の標準残差（ピアソン残差）
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~rstandard(kfs1Log, "state")[,1], name = 'ローカルレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs2Log, "state")[,1], name = 'ローカルレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs3Log, "state")[,1], name = '平滑化トレンドモデル、固定季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~rstandard(kfs4Log, "state")[,1], name = '平滑化トレンドモデル、可変季節変動', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~1.96, name = '95%信頼区間（上限）', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot'), showlegend = FALSE) %>%
  add_trace(y = ~-1.96, name = '95%信頼区間（下限）', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot'), showlegend = FALSE)  %>%
  layout(xaxis = list(title = "東日本大震災を異常値（欠損）として扱った場合の状態撹乱項（水準成分）の補助残差"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (対数)")) %>%
  layout(legend = list(x = 0.1, y = 1), font=t)
p

# 残差の図
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~log(philippinesNA)-kfs1Log$muhat, name = 'ローカルレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~log(philippinesNA)-kfs2Log$muhat, name = 'ローカルレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~log(philippinesNA)-kfs3Log$muhat, name = '平滑化トレンドモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~log(philippinesNA)-kfs4Log$muhat, name = '平滑化トレンドモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "東日本大震災を異常値（欠損）として扱った場合の平滑化後の残差"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (対数)")) %>%
  layout(legend = list(x = 0.2, y = 0.9), font=t)
p

# 原系列との比較の図
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~philippines, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1Log$muhat), name = 'ローカルレベルモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs2Log$muhat), name = 'ローカルレベルモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs3Log$muhat), name = '平滑化トレンドモデル、固定季節変動', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs4Log$muhat), name = '平滑化トレンドモデル、可変季節変動', type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "東日本大震災を異常値として扱う場合の値と原系列"),
         yaxis = list (title = "月別訪日フィリピン旅行客数（万人）")) %>%
  layout(legend = list(x = 0.2, y = 0.9), font=t)
p


#
# 原系列の年周期を図示する
#
# 原系列との比較の図
philippines
Months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13")

p1 <- plot_ly(Foreigners, x = ~Months) %>%
  add_trace(y = ~log(philippines[1:13]), name = '2003', type = 'scatter', mode = 'lines', legendgroup = 'group1') %>%
  add_trace(y = ~log(philippines[13:25]), name = '2004',type = 'scatter', mode = 'lines', legendgroup = 'group1') %>%
  add_trace(y = ~log(philippines[25:37]), name = '2005',type = 'scatter', mode = 'lines', legendgroup = 'group1') %>%
  add_trace(y = ~log(philippines[37:49]), name = '2006',type = 'scatter', mode = 'lines', legendgroup = 'group1') %>%
  add_trace(y = ~log(philippines[49:61]), name = '2007',type = 'scatter', mode = 'lines', legendgroup = 'group1') %>%
  add_trace(y = ~log(philippines[61:73]), name = '2008',type = 'scatter', mode = 'lines', legendgroup = 'group1') %>%
  add_trace(y = ~log(philippines[73:85]), name = '2009',type = 'scatter', mode = 'lines', legendgroup = 'group1') %>%
  layout(xaxis = list(title = "原系列の年周期性"))
p2 <- plot_ly(Foreigners, x = ~Months) %>%
  add_trace(y = ~log(philippines[85:97]), name = '2010',type = 'scatter', mode = 'lines', legendgroup = 'group2')　%>%
  add_trace(y = ~log(philippines[97:109]), name = '2011',type = 'scatter', mode = 'lines', legendgroup = 'group2') %>%
  add_trace(y = ~log(philippines[109:121]), name = '2012',type = 'scatter', mode = 'lines', legendgroup = 'group2') %>%
  add_trace(y = ~log(philippines[121:133]), name = '2013',type = 'scatter', mode = 'lines', legendgroup = 'group2') %>%
  add_trace(y = ~log(philippines[133:145]), name = '2014',type = 'scatter', mode = 'lines', legendgroup = 'group2') %>%
  add_trace(y = ~log(philippines[145:157]), name = '2015',type = 'scatter', mode = 'lines', legendgroup = 'group2') %>%
  add_trace(y = ~log(philippines[157:169]), name = '2016',type = 'scatter', mode = 'lines', legendgroup = 'group2') %>%
  add_trace(y = ~log(philippines[169:181]), name = '2017',type = 'scatter', mode = 'lines', legendgroup = 'group2') %>%
  add_trace(y = ~log(philippines[181:193]), name = '2018',type = 'scatter', mode = 'lines', legendgroup = 'group2') %>%
  layout(xaxis = list(title = "原系列の年周期性"))
#p <-subplot(p1, p2, nrows = 2)  %>%
p <-subplot(p1, p2, margin = 0.1)  %>%
  layout(yaxis = list (title = "月別訪日フィリピン旅行客数（対数）")) %>%
  layout(legend = list(x = 0.45, y = 0.9, shareY = TRUE), font=t)
p



#
# 原因の考察と対処（その２）
#
# 東日本大震災（2011年3,4,5月）の激減を捉えられていない。
# 2013年3月から2015年12月の急増を捉えられていない。
# 2010年1月前後で周期性が変わる。
# 対処：2011年3,4,5月を異常値として除外する代わりに、
# 　　　短期変動（定常AR）成分を加える。
#

# モデル定義
# 状態空間モデルの定義
# 平滑化トレンドモデル、可変季節変動に短期変動（定常AR）成分を加える。
modWithAR <- SSModel(philippines ~ SSMtrend(2, Q = list(0,NA)) + SSMseasonal(12, Q=NA) + SSMarima(ar = 0, Q = NA), H = NA)

# ARモデルの自己回帰係数は自動で推定できず，
# updatefnというパラメータを更新するための
# 関数を定義してfitSSMの引数に加える必要がある
updatefn <- function(pars, model){
  SSModel(philippines ~ SSMtrend(2, Q = list(0,exp(pars[1])))
          + SSMseasonal(12, Q=exp(pars[2]))
          + SSMarima(ar = artransform(pars[3]), Q = exp(pars[4])), H = exp(pars[5]))
}

# 対数尤度最大化による未知パラメータの推定
fitWithARL <- fitSSM(modWithAR, numeric(5), updatefn, method="BFGS")

# カルマンフィルタ・カルマンスムーザの実行
# 状態推定（平滑化）
kfsWithAR <- KFS(fitWithARL$model)
# 推定されたAR係数を確認
tail(fitWithARL$model$T,1) # 0.801992

# 最大対数尤度
logLikWithAR <- kfsWithAR$logLik - sum(kfsWithAR$Finf>0) * log(2*pi)/2

# AIC (赤池情報量規準)
AICWithAR <- -2*logLikWithAR + 2*( 4 + 13 ) # 203.1782（最良）
AICWithAR

#
# ARモデルの図
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~philippines, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfsWithAR$alphahat[,"level"], name = '水準成分', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~kfsWithAR$alphahat[,"sea_dummy1"], name = '季節成分', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~kfsWithAR$alphahat[,"arima1"], name = '定常AR成分', type = 'scatter', mode = 'lines') %>% 
  layout(xaxis = list(title = "原系列とモデルの各成分"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (万人)")) %>%
  layout(legend = list(x = 0.1, y = 1), font=t)
p

# 原系列との比較
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~philippines, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~kfsWithAR$muhat, name = '平滑化トレンドモデル（可変季節変動、定常AR成分）', type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "原系列と比較"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (万人)")) %>%
  layout(legend = list(x = 0.1, y = 1), font=t)
p

# 残差の図
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~philippines-kfsWithAR$muhat, name = '残差', type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "平滑化後の残差"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (万人)")) %>%
  layout(legend = list(x = 0.1, y = 1), font=t)
p



##################################### 必須課題３ ##############################################
#
# 2019年の予測をする。
#
#
# ローカルレベルモデル、可変季節変動、東日本大震災を欠陥値として扱う。
# 平滑化トレンドモデルモデル、可変季節変動、東日本大震災を欠陥値として扱う。
# 平滑化トレンドモデルモデル、可変季節変動、短期変動（定常AR）成分を加える。


# 
# 2019年1〜12月の推移を予測する。
# 
philippinesForecast <- c(philippines[1:192],rep(NA,12)) # 直近1年間(2019年1月〜12月)をNA(欠測＝予測対象)
philippinesForecast

# 対処：東日本大震災の激減を一時的な変化とみなし、
# 2011年3,4,5,6月のデータを異常値として除外（NAとして欠損値扱い）する。

philippinesNAForecast <- philippinesForecast
philippinesNAForecast[99] <- NA # 2011年3月は99番目
philippinesNAForecast[100] <- NA # 2011年4月は100番目
philippinesNAForecast[101] <- NA # 2011年5月は101番目
philippinesNAForecast[102] <- NA # 2011年6月は101番目


# 状態空間モデル(のみ)の定義
# モデル定義
mod1Log<-SSModel(log(philippinesNAForecast) ~ SSMtrend(1,Q=NA) + SSMseasonal(12,Q=0,sea.type = "dummy"), H=NA)
mod2Log<-SSModel(log(philippinesNAForecast) ~ SSMtrend(1,Q=NA) + SSMseasonal(12,Q=NA,sea.type = "dummy"), H=NA)
mod3Log<-SSModel(log(philippinesNAForecast) ~ SSMtrend(2,Q=list(0,NA)) + SSMseasonal(12,Q=0,sea.type = "dummy" ), H=NA)
mod4Log<-SSModel(log(philippinesNAForecast) ~ SSMtrend(2,Q=list(0,NA)) + SSMseasonal(12,Q=NA,sea.type = "dummy"), H=NA)

# 未知パラメータの推定
fit1Log <- fitSSM(mod1Log, numeric(2), method = "BFGS") 
fit2Log <- fitSSM(mod2Log, numeric(3), method = "BFGS") 
fit3Log <- fitSSM(mod3Log, numeric(2), method = "BFGS") 
fit4Log <- fitSSM(mod4Log, numeric(3), method = "BFGS") 

confLog1Forecast <- predict(fit1Log$model, interval="confidence", level=0.95)
confLog2Forecast <- predict(fit2Log$model, interval="confidence", level=0.95)
confLog3Forecast <- predict(fit3Log$model, interval="confidence", level=0.95)
confLog4Forecast <- predict(fit4Log$model, interval="confidence", level=0.95)

preLog1Forecast  <- predict(fit1Log$model, interval="prediction", level=0.95)
preLog2Forecast  <- predict(fit2Log$model, interval="prediction", level=0.95)
preLog3Forecast  <- predict(fit3Log$model, interval="prediction", level=0.95)
preLog4Forecast  <- predict(fit4Log$model, interval="prediction", level=0.95)


# 状態空間モデル(のみ)の定義
# 平滑化トレンドモデル、可変季節変動に短期変動（定常AR）成分を加える。
modWithARForecast <- SSModel(philippinesForecast ~ SSMtrend(2, Q = list(0,NA)) + SSMseasonal(12, Q=NA) + SSMarima(ar = 0, Q = NA), H = NA)
# ARモデルの自己回帰係数は自動で推定できず，
# updatefnというパラメータを更新するための
# 関数を定義してfitSSMの引数に加える必要がある
updatefn <- function(pars, model){
  SSModel(philippinesForecast ~ SSMtrend(2, Q = list(0,exp(pars[1])))
          + SSMseasonal(12, Q=exp(pars[2]))
          + SSMarima(ar = artransform(pars[3]), Q = exp(pars[4])), H = exp(pars[5]))
}

# 対数尤度最大化による未知パラメータの推定
fitWithARForecast <- fitSSM(modWithARForecast, numeric(5), updatefn, method="BFGS")
confARForecast <- predict(fitWithARForecast$model, interval="confidence", level=0.95)
preARForecast  <- predict(fitWithARForecast$model, interval="prediction", level=0.95)
confARForecast
preARForecast



#
# ARモデルの図
#
#
# 予測用データを作成する。
ForeignersForecast <- read.csv("../data/訪日外客数_2019年12月まで_utf8.csv")
ForeignersForecast <- transform(ForeignersForecast,YearMonth=c(
  "2003-01","2003-02","2003-03","2003-04","2003-05","2003-06","2003-07","2003-08","2003-09","2003-10","2003-11","2003-12",
  "2004-01","2004-02","2004-03","2004-04","2004-05","2004-06","2004-07","2004-08","2004-09","2004-10","2004-11","2004-12",
  "2005-01","2005-02","2005-03","2005-04","2005-05","2005-06","2005-07","2005-08","2005-09","2005-10","2005-11","2005-12",
  "2006-01","2006-02","2006-03","2006-04","2006-05","2006-06","2006-07","2006-08","2006-09","2006-10","2006-11","2006-12",
  "2007-01","2007-02","2007-03","2007-04","2007-05","2007-06","2007-07","2007-08","2007-09","2007-10","2007-11","2007-12",
  "2008-01","2008-02","2008-03","2008-04","2008-05","2008-06","2008-07","2008-08","2008-09","2008-10","2008-11","2008-12",
  "2009-01","2009-02","2009-03","2009-04","2009-05","2009-06","2009-07","2009-08","2009-09","2009-10","2009-11","2009-12",
  "2010-01","2010-02","2010-03","2010-04","2010-05","2010-06","2010-07","2010-08","2010-09","2010-10","2010-11","2010-12",
  "2011-01","2011-02","2011-03","2011-04","2011-05","2011-06","2011-07","2011-08","2011-09","2011-10","2011-11","2011-12",
  "2012-01","2012-02","2012-03","2012-04","2012-05","2012-06","2012-07","2012-08","2012-09","2012-10","2012-11","2012-12",
  "2013-01","2013-02","2013-03","2013-04","2013-05","2013-06","2013-07","2013-08","2013-09","2013-10","2013-11","2013-12",
  "2014-01","2014-02","2014-03","2014-04","2014-05","2014-06","2014-07","2014-08","2014-09","2014-10","2014-11","2014-12",
  "2015-01","2015-02","2015-03","2015-04","2015-05","2015-06","2015-07","2015-08","2015-09","2015-10","2015-11","2015-12",
  "2016-01","2016-02","2016-03","2016-04","2016-05","2016-06","2016-07","2016-08","2016-09","2016-10","2016-11","2016-12",
  "2017-01","2017-02","2017-03","2017-04","2017-05","2017-06","2017-07","2017-08","2017-09","2017-10","2017-11","2017-12",
  "2018-01","2018-02","2018-03","2018-04","2018-05","2018-06","2018-07","2018-08","2018-09","2018-10","2018-11","2018-12",
  "2019-01","2019-02","2019-03","2019-04","2019-05","2019-06","2019-07","2019-08","2019-09","2019-10","2019-11","2019-12"
))

# 2019年の実データを作成する。
Foreigners2019Actual <- read.csv("../data/訪日外客数_2019年12月まで実際値_utf8.csv")
Foreigners2019Actual <- transform(Foreigners2019Actual,YearMonth=c(
  "2003-01","2003-02","2003-03","2003-04","2003-05","2003-06","2003-07","2003-08","2003-09","2003-10","2003-11","2003-12",
  "2004-01","2004-02","2004-03","2004-04","2004-05","2004-06","2004-07","2004-08","2004-09","2004-10","2004-11","2004-12",
  "2005-01","2005-02","2005-03","2005-04","2005-05","2005-06","2005-07","2005-08","2005-09","2005-10","2005-11","2005-12",
  "2006-01","2006-02","2006-03","2006-04","2006-05","2006-06","2006-07","2006-08","2006-09","2006-10","2006-11","2006-12",
  "2007-01","2007-02","2007-03","2007-04","2007-05","2007-06","2007-07","2007-08","2007-09","2007-10","2007-11","2007-12",
  "2008-01","2008-02","2008-03","2008-04","2008-05","2008-06","2008-07","2008-08","2008-09","2008-10","2008-11","2008-12",
  "2009-01","2009-02","2009-03","2009-04","2009-05","2009-06","2009-07","2009-08","2009-09","2009-10","2009-11","2009-12",
  "2010-01","2010-02","2010-03","2010-04","2010-05","2010-06","2010-07","2010-08","2010-09","2010-10","2010-11","2010-12",
  "2011-01","2011-02","2011-03","2011-04","2011-05","2011-06","2011-07","2011-08","2011-09","2011-10","2011-11","2011-12",
  "2012-01","2012-02","2012-03","2012-04","2012-05","2012-06","2012-07","2012-08","2012-09","2012-10","2012-11","2012-12",
  "2013-01","2013-02","2013-03","2013-04","2013-05","2013-06","2013-07","2013-08","2013-09","2013-10","2013-11","2013-12",
  "2014-01","2014-02","2014-03","2014-04","2014-05","2014-06","2014-07","2014-08","2014-09","2014-10","2014-11","2014-12",
  "2015-01","2015-02","2015-03","2015-04","2015-05","2015-06","2015-07","2015-08","2015-09","2015-10","2015-11","2015-12",
  "2016-01","2016-02","2016-03","2016-04","2016-05","2016-06","2016-07","2016-08","2016-09","2016-10","2016-11","2016-12",
  "2017-01","2017-02","2017-03","2017-04","2017-05","2017-06","2017-07","2017-08","2017-09","2017-10","2017-11","2017-12",
  "2018-01","2018-02","2018-03","2018-04","2018-05","2018-06","2018-07","2018-08","2018-09","2018-10","2018-11","2018-12",
  "2019-01","2019-02","2019-03","2019-04","2019-05","2019-06","2019-07","2019-08","2019-09","2019-10","2019-11","2019-12"
))

Foreigners2019Actual[,"フィリピン"]
philippines2019 <- ts(Foreigners2019Actual[,"フィリピン"]/10000) # フィリピンのデータ（万人単位にスケール変換）

philippines2019

p <- plot_ly(ForeignersForecast, x = ~ForeignersForecast[, "YearMonth"]) %>%
  add_ribbons(x = ~ForeignersForecast[, "YearMonth"][193:204], ymin = exp(preLog4Forecast[,2][193:204]), ymax = exp(preLog4Forecast[,3][193:204]),
            color = I("gray80"), name = "95%予測区間(異常値導入モデル)") %>%
  add_ribbons(x = ~ForeignersForecast[, "YearMonth"][193:204], ymin = preARForecast[,2][193:204], ymax = preARForecast[,3][193:204],
              color = I("pink"), name = "95%予測区間(短期変動導入モデル)") %>%
  add_lines(x = ~ForeignersForecast[, "YearMonth"][193:204], y = exp(preLog4Forecast[,1][193:204]),
            line = list(color = 'green', width = 4, dash = 'dash'), name = "予測値(異常値導入モデル)") %>%
  add_lines(x = ~ForeignersForecast[, "YearMonth"][193:204], y = preARForecast[,1][193:204],
            line = list(color = 'red', width = 4, dash = 'dash'), name = "予測値(短期変動導入モデル)") %>%
   add_lines(x = ~ForeignersForecast[, "YearMonth"][193:204], y = ~philippines2019[193:204], name = 'フィリピン（2019年）',
            line = list(color = 'rgb(22,96,167)', width = 4)) %>%
  layout(xaxis = list(title = "予測値と実際値"),
         yaxis = list (title = "月別訪日フィリピン旅行客数 (万人)")) %>%
  layout(legend = list(x = 0.4, y = 1), font=t)
p



###########################  課題4. 選択課題② #################################
####
### ②適当な２つの国のデータに対して、SUTSEモデルを適応する。
####

# フィリピンとタイを選択する。
p <- plot_ly(Foreigners, x = ~Foreigners[, "YearMonth"]) %>%
  add_trace(y = ~Foreigners[, "フィリピン"]/10000, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Foreigners[, "タイ"]/10000, name = 'タイ', type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "年月"),
         yaxis = list (title = "月別訪日旅行客数 (万人)"),font=t,legend = list(x = 0.1, y = 1))
p


philippines # フィリピンのデータ（万人単位にスケール変換）
philippinesNA # 東日本大震災（2011年3,4,5,6月）を欠損値としたフィリピンのデータ

thai <- ts(Foreigners[,"タイ"]/10000) # タイのデータ（万人単位にスケール変換）
thaiNA <- thai # 東日本大震災（2011年3,4,5,6月）を欠損値としたタイのデータ
thaiNA[99] <- NA # 2011年3月は99番目
thaiNA[100] <- NA # 2011年4月は100番目
thaiNA[101] <- NA # 2011年5月は101番目
thaiNA[102] <- NA # 2011年6月は101番目

# 欠損なしの場合
modSUTSE <- SSModel(cbind(thai, philippines) ~
                      SSMtrend(1, Q = matrix(NA,2,2)), H = matrix(NA,2,2))
fitSUTSE <- fitSSM(modSUTSE, numeric(6), method="BFGS")
kfsSUTSE <- KFS(fitSUTSE$model)
confSUTSE <- predict(fitSUTSE$model, interval="confidence", level=0.95)
preSUTSE  <- predict(fitSUTSE$model, interval="prediction", level=0.95)

# 欠損あり（21~40日目の体重がNA）の場合
modSUTSENA <- SSModel(cbind(thaiNA, philippinesNA) ~
                        SSMtrend(1, Q = matrix(NA,2,2)), H = matrix(NA,2,2))
fitSUTSENA <- fitSSM(modSUTSENA, numeric(6), method="BFGS")
kfsSUTSENA <- KFS(fitSUTSENA$model)
confSUTSENA <- predict(fitSUTSENA$model, interval="confidence", level=0.95)
preSUTSENA  <- predict(fitSUTSENA$model, interval="prediction", level=0.95)

# スライド番号26の分散共分散行列の表示
fitSUTSE$model$H
fitSUTSE$model$Q
fitSUTSENA$model$H
fitSUTSENA$model$Q





###########################  課題4. 選択課題③#################################
####
### ③全ての国の訪日外客数の年次推移に対して動的因子モデルを適応し、２つ以上の因子を抽出して因子の解釈をする。
####

# RパッケージKFASの読み込み(R の起動ごとに必要)
library(KFAS) 

# 描画にplotlyを使う
library(plotly)

# plotlyで日本語表示させるためのフォント指定
par(family= "HiraKakuProN-W3")

# グラフのフォントサイズ指定
t <- list(size = 20)



# 訪日外客数年別データ取り込み
MonthlyForeigners <- read.csv("../data/訪日外客数（年次推移）_utf8.csv")
MonthlyForeigners
MonthlyForeigners <- transform(MonthlyForeigners,Year=c(
  "2003",  "2004",  "2005",  "2006",  "2007",  "2008",  "2009",
  "2010",  "2011",  "2012",  "2013",  "2014",  "2015",  "2016",  "2017",  "2018"))


# 実データの図
# 全ての訪日外客数の描画
p <- plot_ly(MonthlyForeigners, x = ~MonthlyForeigners[, "Year"]) %>%
#  add_trace(y = ~MonthlyForeigners[, "総数"]/10000, name = '総数', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "中国"]/10000, name = '中国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "香港"]/10000, name = '香港', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "台湾"]/10000, name = '台湾', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "韓国"]/10000, name = '韓国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "フィリピン"]/10000, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "タイ"]/10000, name = 'タイ', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "シンガポール"]/10000, name = 'シンガポール', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "マレーシア"]/10000, name = 'マレーシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "インドネシア"]/10000, name = 'インドネシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "インド"]/10000, name = 'インド', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "ベトナム"]/10000, name = 'ベトナム', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "フランス"]/10000, name = 'フランス', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "ロシア"]/10000, name = 'ロシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "イタリア"]/10000, name = 'イタリア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "米国"]/10000, name = '米国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "カナダ"]/10000, name = 'カナダ', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~MonthlyForeigners[, "豪州"]/10000, name = '豪州', type = 'scatter', mode = 'lines') %>%
  layout(
    xaxis = list(title = "年"),
    yaxis = list (title = "年別訪日外客数 (万人)"),font=t)
p

#y <- ts(MonthlyForeigners[,2:21]/10000) # 20ヶ国（総数〜豪州）のデータ(万人単位,対数変換)
y <- ts(log(MonthlyForeigners[,2:21])) # 20ヶ国（総数〜豪州）のデータ(対数変換)

# 動的因子モデル（１因子ローカルレベルモデル）
mod1 <- SSModel(y~SSMtrend(1,Q=1,"common")+SSMtrend(1,Q=0,"distinct"), tol=.Machine$double.eps^0.8)


# 更新関数を定義
updatefn1 <- function(pars,mod1){
  mod1 <- SSModel(y~SSMtrend(1,Q=1,"common")+SSMtrend(1,Q=0,"distinct"), tol=.Machine$double.eps^0.8)
  mod1$Z[,1,] <- pars[1:20]
  diag(mod1$H[,,]) <- exp(pars[21])
  mod1
}

# 対数尤度最大化による未知パラメータの推定
fit1 <- fitSSM(mod1,c(rep(0.1,20),-5),updatefn1,method="BFGS")
kfs1 <- KFS(fit1$mod) # Worningが出る
loglik1 <- kfs1$logLik - sum(kfs1$Finf>0) * log(2*pi)/2
aic1 <- -2*loglik1 + 2*(21+20+2) 
aic1　# -82.08243


p <- plot_ly(MonthlyForeigners, x = ~MonthlyForeigners[, "Year"]) %>%
  add_trace(y = ~exp(kfs1$muhat[, "総数"])/10000, name = '総数', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "中国"])/10000, name = '中国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "香港"])/10000, name = '香港', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "台湾"])/10000, name = '台湾', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "韓国"])/10000, name = '韓国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "フィリピン"])/10000, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "タイ"])/10000, name = 'タイ', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "シンガポール"])/10000, name = 'シンガポール', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "マレーシア"])/10000, name = 'マレーシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "インドネシア"])/10000, name = 'インドネシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "インド"])/10000, name = 'インド', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "ベトナム"])/10000, name = 'ベトナム', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "フランス"])/10000, name = 'フランス', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "ロシア"])/10000, name = 'ロシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "イタリア"])/10000, name = 'イタリア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "米国"])/10000, name = '米国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "カナダ"])/10000, name = 'カナダ', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs1$muhat[, "豪州"])/10000, name = '豪州', type = 'scatter', mode = 'lines') %>%
  layout(
    xaxis = list(title = "年"),
    yaxis = list (title = "年別訪日外客数 (万人)"),font=t)
p




# 動的因子モデル（２因子 1つはローカルレベルモデル＆もう1つは平滑化トレンドモデル）
mod5 = SSModel(y~SSMtrend(2,Q=diag(c(0,1)),"common",state_names=c("factor1_level","factor1_slope"))+SSMtrend(1,Q=1,"common",state_names="factor2_level")+SSMtrend(1,Q=0,"distinct"), tol=.Machine$double.eps^0.8)

# updatefn（更新関数）の定義
updatefn5 <- function(pars,mod5){
  mod5 = SSModel(y~SSMtrend(2,Q=diag(c(0,1)),"common",state_names=c("factor1_level","factor1_slope"))+SSMtrend(1,Q=1,"common",state_names="factor2_level")+SSMtrend(1,Q=0,"distinct"), tol=.Machine$double.eps^0.8)
  mod5$Z[,"factor1_level",] = pars[1:20]
  mod5$Z[,"factor2_level",] = pars[21:40]
  diag(mod5$H[,,])=exp(pars[41])
  mod5
}

# 対数尤度最大化による未知パラメータの推定
fit5 = fitSSM(mod5,c(fit1$opt$par[1:20], rep(0.1,20), -4),updatefn5,method="BFGS") #-155.2473
kfs5 = KFS(fit5$mod)
loglik5 = kfs5$logLik - sum(kfs5$Finf>0) * log(2*pi)/2
aic5 = -2*loglik5 + 2*(41+20+4)
aic5


p <- plot_ly(MonthlyForeigners, x = ~MonthlyForeigners[, "Year"]) %>%
  add_trace(y = ~exp(kfs5$muhat[, "総数"])/10000, name = '総数', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "中国"])/10000, name = '中国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "香港"])/10000, name = '香港', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "台湾"])/10000, name = '台湾', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "韓国"])/10000, name = '韓国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "フィリピン"])/10000, name = 'フィリピン', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "タイ"])/10000, name = 'タイ', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "シンガポール"])/10000, name = 'シンガポール', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "マレーシア"])/10000, name = 'マレーシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "インドネシア"])/10000, name = 'インドネシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "インド"])/10000, name = 'インド', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "ベトナム"])/10000, name = 'ベトナム', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "フランス"])/10000, name = 'フランス', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "ロシア"])/10000, name = 'ロシア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "イタリア"])/10000, name = 'イタリア', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "米国"])/10000, name = '米国', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "カナダ"])/10000, name = 'カナダ', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~exp(kfs5$muhat[, "豪州"])/10000, name = '豪州', type = 'scatter', mode = 'lines') %>%
  layout(
    xaxis = list(title = "年"),
    yaxis = list (title = "年別訪日外客数 (万人)"),font=t)
p


pYear <- plot_ly(MonthlyForeigners, x = ~MonthlyForeigners[, "Year"]) %>%
  add_trace(y = ~kfs5$alphahat[,"factor1_level"], name = '第１因子', type = 'scatter', mode = 'lines', line = list(color = 'blue', width = 2)) %>%
  add_trace(y = ~kfs5$alphahat[,"factor2_level"], name = '第２因子', type = 'scatter', mode = 'lines', line = list(color = 'red', width = 2)) %>%
  layout(
    xaxis = list(title = "年"),
    yaxis = list (title = "因子"),font=t)


CountryNames <- c("00:総数","01:中国","02:香港","03:台湾","04:韓国",
                  "05:フィリピン","06:タイ","07:シンガポール","08:マレーシア","09:インドネシア","10:インド","11:ベトナム",
                  "12:英国","13:ドイツ","14:フランス","15:ロシア","16:イタリア","17:米国","18:カナダ","19:豪州")

pCountry <- plot_ly(MonthlyForeigners, x = ~CountryNames) %>%
  add_trace(y = ~fit5$opt$par[1:20], name = '第１因子の負荷量', type = 'scatter', mode = 'lines', line = list(color = 'blue', width = 2, dash = 'dot')) %>%
  add_trace(y = ~fit5$opt$par[21:40], name = '第２因子の負荷量', type = 'scatter', mode = 'lines', line = list(color = 'red', width = 2, dash = 'dot')) %>%
  layout(
    xaxis = list(title = "国別"),
    yaxis = list (title = "因子負荷量"),font=t)

p <- subplot(pYear,pCountry,nrows = 2)
p

########################### ここまで #################################
