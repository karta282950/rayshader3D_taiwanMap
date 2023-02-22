# Load library
library(ggplot2)
library(xlsx)
library(rayshader)
library(rgl)
library(png)
library(grid)
library(viridis)
library(sf)
# Read data
## 台灣人口數
tw_population = read.xlsx2('各鄉鎮市區人口密度.xlsx',1)
tw_population = tw_population[,c(2,3)]
colnames(tw_population) = c("區域別", "人口數")

# 繪製台灣地圖
taiwan_shap2=read_sf("政府開放資料/TOWN_MOI_1111118.shp")
taiwan_shap2$區域別 = paste0(taiwan_shap2$COUNTYNAME,taiwan_shap2$TOWNNAME)
taiwan_shap2 = merge(taiwan_shap2, tw_population, by='區域別', all = TRUE)
taiwan_shap2 = dplyr::filter(taiwan_shap2, !區域別 %in% c("東沙群島", "南沙群島"))
taiwan_shap2$人口數 = as.numeric(taiwan_shap2$人口數)
taiwan_shap2$人口數[is.na(taiwan_shap2$人口數)] = 0
row.names(taiwan_shap2) <- NULL

## 畫圖參數
theme <- theme_get()
theme$text$family <- "STFangsong"
theme_set(theme)
my_breaks = c(0, 100, 1000,  5000, 10000, 50000 ,100000, 400000)
my_breaks2 = c('0', '100', '1千', '5千', '1萬', '5萬','10萬', '40萬')
population = taiwan_shap2$人口數

## 畫圖
tw_population_map = ggplot(taiwan_shap2, aes(fill = population)) + 
  geom_sf() + 
  ggtitle('台灣各行政區人口數量(2020)') + 
  scale_fill_gradient(low="#fffbe3", high="#b5781d", name='Population', trans = "log", breaks = my_breaks, labels = my_breaks2) +
  coord_sf(xlim = c(118, 122.5), ylim = c(21.5, 25.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line=element_blank(),
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank())
#tw_population_map

xlim = ggplot_build(tw_population_map)$layout$panel_scales_x[[1]]$range$range
ylim = ggplot_build(tw_population_map)$layout$panel_scales_y[[1]]$range$range
### 用ChatGPT寫的函數😂(plot_gg畫布長寬需要偶數長度)
nearest_even <- function(x){
  int_part <- floor(x)  # 取整數部分
  if (int_part %% 2 == 0){  # 如果整數部分本身就是偶數
    return(round(x))  # 四捨五入
  } else {  # 如果整數部分是奇數
    return(round(x-0.5))  # 小數部分減去0.5再四捨五入
  }
}

## 3D圖
plot_gg(tw_population_map, multicore = TRUE, width = diff(xlim)*1 ,height=diff(ylim)*1, fov = 15, scale = 50)

## 錄製影片
### Video1(https://search.r-project.org/CRAN/refmans/rayshader/html/render_movie.html)
par3d(windowRect = c(0, 0, nearest_even(diff(xlim)) * 2500, nearest_even(diff(ylim)) * 2500))
phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = 0 + 45 * sin(seq(0,359,length.out = 360) * pi/180)
zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull = c(zoomvec, rev(zoomvec))
render_movie(filename = 'video/output1', type = "custom", frames = 360,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)

### Video2(https://github.com/geoffreylarnold/pittsburgh-mapdeck/blob/master/rayshader-gif.R#L115)
transition_values <- function(from, to, steps = 10, one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin"))){stop("type must be one of: 'cos', 'lin'")}
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range)/2
  if (type == "cos") {scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))}
  else if (type == "lin"){
    if (one_way) {xout <- seq(1, -1, length.out = steps)} 
    else {xout <- c(seq(1, -1, length.out = floor(steps/2)), seq(-1, 1, length.out = ceiling(steps/2)))}
    scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y }
  middle - half_width * scaling
}
theta <- transition_values(from = 0, to = 360, steps = 360, one_way = TRUE, type = "lin")
phi <- transition_values(from = 10, to = 70, steps = 360, one_way = FALSE, type = "cos")
zoom <- transition_values(from = 0.4, to = 0.8, steps = 360, one_way = FALSE, type = "cos")
render_movie(filename = 'video/output2', type = "custom", frames = 360,  phi = phi, zoom = zoom, theta = theta)

rgl.close()