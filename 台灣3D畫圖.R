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

## 繪製台灣地圖
taiwan_shap2=read_sf("政府開放資料/TOWN_MOI_1111118.shp")
taiwan_shap2$區域別 = paste0(taiwan_shap2$COUNTYNAME,taiwan_shap2$TOWNNAME)
taiwan_shap2 = merge(taiwan_shap2, tw_population, by='區域別', all = TRUE)
taiwan_shap2 = dplyr::filter(taiwan_shap2, !區域別 %in% c("東沙群島", "南沙群島"))
taiwan_shap2$人口數 = as.numeric(taiwan_shap2$人口數)
taiwan_shap2$人口數[is.na(taiwan_shap2$人口數)] = 0
row.names(taiwan_shap2) <- NULL

# 畫圖參數
theme < -theme_get()
theme$text$family <- "STFangsong"
theme_set(theme)
my_breaks = c(0, 100, 1000,  5000, 10000, 50000 ,100000, 400000)
my_breaks2 = c('0', '100', '1千', '5千', '1萬', '5萬','10萬', '40萬')
population = taiwan_shap2$人口數

# 畫圖
tw_population_map = ggplot(taiwan_shap2, aes(fill = population)) + 
  geom_sf() + 
  ggtitle('Taiwan') + 
  scale_fill_gradient(low="#fffbe3", high="#b5781d", name='Population', trans = "log", breaks = my_breaks, labels = my_breaks2) +
  coord_sf(xlim = c(118, 122.5), ylim = c(21.5, 25.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.line=element_blank(),
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank())

xlim = ggplot_build(tw_population_map)$layout$panel_scales_x[[1]]$range$range
ylim = ggplot_build(tw_population_map)$layout$panel_scales_y[[1]]$range$range

# 3D圖
plot_gg(tw_population_map, multicore = TRUE, width = diff(xlim)*1 ,height=diff(ylim)*1, fov = 15, scale = 50)
