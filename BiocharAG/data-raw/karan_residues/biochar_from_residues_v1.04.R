# check that bc.100 has all crops

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(dplyr)
library(rasterVis)
library(data.table)
library(terra)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

waterfall = function (.data = NULL, values, labels, rect_text_labels = values, 
                      rect_text_size = 1, rect_text_labels_anchor = "centre", put_rect_text_outside_when_value_below = 0.05 * 
                        (max(cumsum(values)) - min(cumsum(values))), calc_total = FALSE, 
                      total_axis_text = "Total", total_rect_text = sum(values), 
                      total_rect_color = "black", total_rect_border_color = "black", 
                      total_rect_text_color = "white", fill_colours = NULL, fill_by_sign = TRUE, 
                      rect_width = 0.7, rect_border = "black", draw_lines = TRUE, 
                      lines_anchors = c("right", "left"), linetype = "dashed", 
                      draw_axis.x = "behind", theme_text_family = "", scale_y_to_waterfall = TRUE, 
                      print_plot = FALSE, ggplot_object_name = "mywaterfall") 
{
  if (!is.null(.data)) {
    if (!is.data.frame(.data)) {
      stop("`.data` was a ", class(.data)[1], ", but must be a data.frame.")
    }
    if (ncol(.data) < 2L) {
      stop("`.data` had fewer than two columns, yet two are required: labels and values.")
    }
    dat <- as.data.frame(.data)
    char_cols <- vapply(dat, is.character, FALSE)
    factor_cols <- vapply(dat, is.factor, FALSE)
    num_cols <- vapply(dat, is.numeric, FALSE)
    if (!xor(num_cols[1], num_cols[2]) || sum(char_cols[1:2], 
                                              factor_cols[1:2], num_cols[1:2]) != 2L) {
      const_width_name <- function(noms) {
        if (is.data.frame(noms)) {
          noms <- names(noms)
        }
        max_width <- max(nchar(noms))
        formatC(noms, width = max_width)
      }
      stop("`.data` did not contain exactly one numeric column and exactly one character or factor ", 
           "column in its first two columns.\n\t", "1st column: '", 
           const_width_name(dat)[1], "'\t", sapply(dat, 
                                                   class)[1], "\n\t", "2nd column: '", const_width_name(dat)[2], 
           "'\t", sapply(dat, class)[2])
    }
    if (num_cols[1L]) {
      .data_values <- .subset2(dat, 1L)
      .data_labels <- .subset2(dat, 2L)
    }
    else {
      .data_values <- .subset2(dat, 2L)
      .data_labels <- .subset2(dat, 1L)
    }
    if (!missing(values) && !missing(labels)) {
      warning(".data and values and labels supplied, .data ignored")
    }
    else {
      values <- .data_values
      labels <- as.character(.data_labels)
    }
  }
  if (!(length(values) == length(labels) && length(values) == 
        length(rect_text_labels))) {
    stop("values, labels, fill_colours, and rect_text_labels must all have same length")
  }
  if (rect_width > 1) 
    warning("rect_Width > 1, your chart may look terrible")
  number_of_rectangles <- length(values)
  north_edge <- cumsum(values)
  south_edge <- c(0, cumsum(values)[-length(values)])
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[seq_len(n)]
  }
  if (fill_by_sign) {
    if (!is.null(fill_colours)) {
      warning("fill_colours is given but fill_by_sign is TRUE so fill_colours will be ignored.")
    }
    fill_colours <- ifelse(values >= 0, gg_color_hue(2)[2], 
                           gg_color_hue(2)[1])
  }
  else {
    if (is.null(fill_colours)) {
      fill_colours <- gg_color_hue(number_of_rectangles)
    }
  }
  rect_border_matching <- length(rect_border) == number_of_rectangles
  if (!(rect_border_matching || length(rect_border) == 1)) {
    stop("rect_border must be a single colour or one colour for each rectangle")
  }
  if (!(grepl("^[lrc]", lines_anchors[1]) && grepl("^[lrc]", 
                                                   lines_anchors[2]))) 
    stop("lines_anchors must be a pair of any of the following: left, right, centre")
  if (grepl("^l", lines_anchors[1])) 
    anchor_left <- rect_width/2
  if (grepl("^c", lines_anchors[1])) 
    anchor_left <- 0
  if (grepl("^r", lines_anchors[1])) 
    anchor_left <- -1 * rect_width/2
  if (grepl("^l", lines_anchors[2])) 
    anchor_right <- -1 * rect_width/2
  if (grepl("^c", lines_anchors[2])) 
    anchor_right <- 0
  if (grepl("^r", lines_anchors[2])) 
    anchor_right <- rect_width/2
  if (!calc_total) {
    p <- if (scale_y_to_waterfall) {
      ggplot2::ggplot(data.frame(x = c(labels, labels), 
                                 y = c(south_edge, north_edge)), ggplot2::aes_string(x = "x", 
                                                                                     y = "y"))
    }
    else {
      ggplot2::ggplot(data.frame(x = labels, y = values), 
                      ggplot2::aes_string(x = "x", y = "y"))
    }
    p <- p + ggplot2::geom_blank() #+ ggplot2::theme(axis.title = ggplot2::element_blank())
  }
  else {
    p <- if (scale_y_to_waterfall) {
      ggplot2::ggplot(data.frame(x = c(labels, total_axis_text, 
                                       labels, total_axis_text), y = c(south_edge, north_edge, 
                                                                       south_edge[number_of_rectangles], north_edge[number_of_rectangles])), 
                      ggplot2::aes_string(x = "x", y = "y"))
    }
    else {
      ggplot2::ggplot(data.frame(x = c(labels, total_axis_text), 
                                 y = c(values, north_edge[number_of_rectangles])), 
                      ggplot2::aes_string(x = "x", y = "y"))
    }
    p <- p + ggplot2::geom_blank() #+ ggplot2::theme(axis.title = ggplot2::element_blank())
  }
  if (grepl("behind", draw_axis.x)) {
    p <- p + ggplot2::geom_hline(yintercept = 0)
  }
  for (i in seq_along(values)) {
    p <- p + ggplot2::annotate("rect", xmin = i - rect_width/2, 
                               xmax = i + rect_width/2, ymin = south_edge[i], ymax = north_edge[i], 
                               colour = rect_border[[if (rect_border_matching) 
                                 i
                                 else 1]], fill = fill_colours[i])
    if (i > 1 && draw_lines) {
      p <- p + ggplot2::annotate("segment", x = i - 1 - 
                                   anchor_left, xend = i + anchor_right, linetype = linetype, 
                                 y = south_edge[i], yend = south_edge[i])
    }
  }
  for (i in seq_along(values)) {
    if (abs(values[i]) > put_rect_text_outside_when_value_below) {
      p <- p + ggplot2::annotate("text", x = i, y = 0.5 * 
                                   (north_edge[i] + south_edge[i]), family = theme_text_family, 
                                 label = ifelse(rect_text_labels[i] == values[i], 
                                                ifelse(values[i] < 0, paste0("−", -1 * values[i]), 
                                                       values[i]), rect_text_labels[i]), size = rect_text_size/(5/14))
    }
    else {
      p <- p + ggplot2::annotate("text", x = i, y = north_edge[i], 
                                 family = theme_text_family, label = ifelse(rect_text_labels[i] == 
                                                                              values[i], ifelse(values[i] < 0, paste0("−", 
                                                                                                                      -1 * values[i]), values[i]), rect_text_labels[i]), 
                                 vjust = ifelse(values[i] >= 0, -0.2, 1.2), size = rect_text_size/(5/14))
    }
  }
  if (calc_total) {
    p <- p + ggplot2::annotate("rect", xmin = number_of_rectangles + 
                                 1 - rect_width/2, xmax = number_of_rectangles + 1 + 
                                 rect_width/2, ymin = 0, ymax = north_edge[number_of_rectangles], 
                               colour = total_rect_border_color, fill = total_rect_color) + 
      ggplot2::annotate("text", x = number_of_rectangles + 
                          1, y = 0.5 * north_edge[number_of_rectangles], 
                        family = theme_text_family, label = ifelse(total_rect_text == 
                                                                     sum(values), ifelse(north_edge[number_of_rectangles] < 
                                                                                           0, paste0("−", -1 * north_edge[number_of_rectangles]), 
                                                                                         north_edge[number_of_rectangles]), total_rect_text), 
                        color = total_rect_text_color, size = rect_text_size/(5/14)) + 
      ggplot2::scale_x_discrete(labels = c(labels, total_axis_text))
    if (draw_lines) {
      p <- p + ggplot2::annotate("segment", x = number_of_rectangles - 
                                   anchor_left, xend = number_of_rectangles + 1 + 
                                   anchor_right, y = north_edge[number_of_rectangles], 
                                 yend = north_edge[number_of_rectangles], linetype = linetype)
    }
  }
  else {
    p <- p + ggplot2::scale_x_discrete(labels = labels)
  }
  if (grepl("front", draw_axis.x)) {
    p <- p + ggplot2::geom_hline(yintercept = 0)
  }
  if (print_plot) {
    if (ggplot_object_name %in% ls(.GlobalEnv)) 
      warning("Overwriting ", ggplot_object_name, " in global environment.")
    assign(ggplot_object_name, p, inherits = TRUE)
    print(p)
  }
  else {
    return(p)
  }
}



# ------------ total residue production ----------------
res_path = '~/gis/residues/' # replace with local path to raster files
res_files = list.files(paste0(res_path, 'spam2010V2r0_151122_global_residue_productionV1.geotiff/'), full.names = TRUE)
res_files = grep("tif", res_files, ignore.case=TRUE, value=TRUE)
res = rast(res_files) # units are Mg DM per pixel 
# names(res) = sub('.+_R_(.+)_.+', '\\1', names(res))
names(res) = sub('.+_R_(.+)', '\\1', names(res))
res_tot = global(res, fun = 'sum', na.rm=TRUE) # Mg
sum(res_tot)/1e9 # world total residues (Pg DM)

# plot residues map
res_tot_rast = sum(res, na.rm = TRUE)
res_tot_rast = classify(res_tot_rast, cbind(-Inf,1,1))

library(RColorBrewer)
library(colorspace)
myPalette <- colorRampPalette(rev(brewer.pal(11, "PuBuGn")))

sf = scale_fill_continuous_sequential(palette = "Viridis",
                                      # trans = "log",
                                      # breaks = (c(1e0, 1e1, 1e2, 1e3, 1e4, 3e5)),
                                      # limits = (c(0.99,300000)),
                                      na.value = NA,
                                      name = expression("Crop residue production (Mg ha"^-1*")"),
                                      rev = FALSE
                                      )
# myPalette <- colorRampPalette(rev(brewer.pal(9, "GnBu")))
# myPalette <- colorRampPalette(rev(brewer.pal(9, "RdBu")))

# sf = scale_fill_gradientn(
#   colours = myPalette(50),
#   trans = "log",
#   breaks = (c(1e0, 1e1, 1e1, 1e3, 1e4, 1e5, 300000)),
#   limits = (c(0.99,300000)),
#   na.value = NA,
#   name = expression("Crop residue production (Mg ha"^-1*")"))

t <- theme(axis.line=element_blank(),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank(),
           legend.position=c(0.42, 0.1),
           legend.title.align=1,
           legend.title = element_text(size=12),
           legend.text = element_text(size = 12),
           legend.direction="horizontal",
           legend.justification = c(0.4,0),
           legend.key.width = unit(2, 'cm'),
           plot.margin = unit(c(0,0,0,0), "cm"))

library(colorspace)
library(classInt)
library(rnaturalearth)

n=6
classes = classIntervals(values(res_tot_rast), n = n, style = "fisher")$brks
classes = signif(classes, 1)
classes[1] = 0
class_labels = c(paste(c(0, format(classes[2:n], scientific = TRUE)), 
                       format(classes[-1], scientific = TRUE), sep = ' — '))

res_tot_cut = classify(res_tot_rast, cbind(classes[1:n], classes[-1], seq_len(n)))
res_tot_cut = classify(res_tot_cut, cbind(NaN, NA))

world <- ne_countries(scale = 'small', returnclass = 'sf')
world = world[world$region_un != "Antarctica",]

gplot(res_tot_cut, maxpixels=5e6) + 
  geom_tile(aes(fill = factor(value))) +
  geom_sf(data = world, colour = "grey30", fill = NA, inherit.aes = FALSE) +
  scale_fill_discrete_sequential(palette = "Viridis",
                                 na.value = NA,
                                 na.translate = F,
                                 labels = class_labels,
                                 name = expression("Crop residue production (Mg ha"^-1*")"),
                                 rev = FALSE) +
  theme_map() +
  # theme(axis.line=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.ticks=element_blank(),
  #       axis.title.x=element_blank(),
  #       axis.title.y=element_blank(),
  #       legend.position=c(0.42, 0.1),
  #       legend.title.align=1,
  #       
  #       legend.title = element_text(size=12),
  #       legend.text = element_text(size = 12),
  #       legend.direction="horizontal",
  #       legend.justification = c(0.4,0),
  #       # legend.key.width = unit(2, 'cm'),
  #       plot.margin = unit(c(0,0,0,0), "cm")) +
  coord_sf()


# scale_fill_gradient(low = 'white', high = 'blue') +
  # geom_sf(data = countries, colour = "grey30", fill = NA, inherit.aes = FALSE) +
  # scale_fill_viridis_c(name = expression(Crop~residue~production), na.value = "#FFFFFF", trans='log') +
  # theme_map() +
  # theme(plot.background = element_rect(fill = 'white', color=NA), legend.position = c(0.15,0.35))
  coord_equal()


# ------------ residue characterization ----------------
res_tbl = fread('Residue_Characterization_2023.csv')[, c('reference', 'comment', 'harvestable_notes') := NULL]

# convert percent to fraction
percent_cols = c("moisture_pc",  "ash_pc", "C_dm", "N_dm", "C_daf", "N_daf", "lignin_dm")
res_tbl[, (percent_cols) := lapply(.SD, \(x) x/100), .SDcols = percent_cols]
setnames(res_tbl, \(jname) sub('_pc', '', jname))

all(names(res) %in% res_tbl$name) # sanity check that all layer have values in characterization table
names(res)[!(names(res) %in% res_tbl$name)]
# [1] "TEMF" "TROF"
res_tbl[name=='TEMPF', name := 'TEMF']
res_tbl[name=='TROPF', name := 'TROF']

all(names(res) %in% res_tbl$name) # sanity check that all layer have values in characterization table

res_tbl = res_tbl[match(names(res), name)]
res_tbl[, prod_global := res_tot[name,]]
res_tbl[, prod_global := prod_global * C_dm]
res_tbl[, round(sum(prod_global)/1e9, 2)] # Pg C /yr

fwrite(res_tbl[, .(category, TgC = round((prod_global)/1e6, 2)), crop], 'res_tbl_bars.csv') # Tg C /yr


# ------------ biochar ----------------
py_temp = 550
res_tbl[, biochar_yield    := 0.1260917 + 0.27332*lignin_dm + 0.5391409*exp(-0.004*py_temp)] # Woolf, Lehmann & Lee 2016
res_tbl[, biochar_C        := 0.99 - 0.78*exp(-0.0042*py_temp)] # Woolf, Lehmann & Lee 2016
res_tbl[, biochar_C_yield  := biochar_yield * biochar_C / C_daf] # mass biochar C per biomass C
res_tbl[, fperm            := 0.71] 
res_tbl[, biochar_tech_seq := prod_global * biochar_C_yield * fperm] 
res_tbl[, sum(biochar_tech_seq)/1e9] 
res_tbl[, sum(prod_global)/1e9] 

fwrite(res_tbl, 'res_with_biochar.csv')

fperm_data = fread('fperm_data.csv')[Tclass == "Medium"]
fperm.calc = function (soil_temperature, FpermYears = 100) {
  fperm = numeric(length(soil_temperature))
  for (i in seq_along(soil_temperature)) {
    TT = soil_temperature[i]
    fperm_data[, q10 :=(1.1 *(T_expt-TT) - 63.1579 * exp(-0.19*T_expt) + 63.1579 *exp(-0.19*TT)) / (T_expt-TT)]
    fperm_data[T_expt == TT, q10 := 1.1+12*exp(-0.19*T_expt)]
    fperm_data[, fT := exp(log(q10) * (TT-T_expt)/10)]
    fperm_data[, k1_fperm := k1 * fT]
    fperm_data[, k2_fperm := k2 * fT]
    fperm_data[, k3_fperm := k3 * fT]
    fperm_data[, fperm := 
                 C1*exp(-k1_fperm*FpermYears) + 
                 C2*exp(-k2_fperm*FpermYears) + 
                 C3*exp(-k3_fperm*FpermYears)]
    fperm[i] = fperm_data[, mean(fperm)]
  }
  return(fperm)
}

TT = seq(-55,40)
fperm_approx = data.table(soil_temperature = TT, fperm = fperm.calc(TT))
fperm_fun = function(TT) {
  fperm_approx[match(round(TT), soil_temperature), fperm]
}

if (!file.exists('fperm.tif')) {
  # mat = rast('/home/dominic/gis/climate/worldclim/wc2.1_5m_bio_1.tif') # mean annual temperature from WorldClim
  # we now use soil temp from https://onlinelibrary.wiley.com/doi/10.1111/gcb.16060
  # data available at https://zenodo.org/record/7134169
  mat0_5  = rast('/home/dominic/gis/climate/soil_temperature/SBIO1_0_5cm_Annual_Mean_Temperature.tif')
  mat5_15 = rast('/home/dominic/gis/climate/soil_temperature/SBIO1_5_15cm_Annual_Mean_Temperature.tif')
  mat = (mat0_5 + 3*mat5_15) / 4 # 0-15cm weighted mean
  fperm = app(mat, fperm_fun, filename = 'fperm.tif')
} else {
  fperm = rast('fperm.tif')
}
# plot(fperm, col = viridis::viridis(50), plg=list(title=expression(F[perm]), horiz=TRUE), ylim=c(-55,80))
# coast = vect('/home/dominic/gis/boundaries/WB_countries_Admin0_10m/WB_countries_Admin0_10m.shp')
# plot(coast, lwd=0.3, add=TRUE)

# countries = st_read('/home/dominic/gis/boundaries/WB_countries_Admin0_10m/WB_countries_Admin0_10m.shp')
countries = st_read('~/gis/boundaries/gadm_410-levels.gpkg')
countries = st_simplify(countries, dTolerance = 0.1)
countries = countries[countries$COUNTRY != "Antarctica", ]

# countries = st_make_valid(countries)
# countries = st_buffer(countries, 10)
# countries = st_simplify(countries, dTolerance = 0.2)
# countries = countries[countries$COUNTRY != "Antarctica",]
# cnt = countries[countries$COUNTRY != "Antarctica",]
# cnt = st_buffer(cnt, 10)
# cnt = st_simplify(cnt, dTolerance = 0.2)

gplot(fperm, maxpixels = 1e6) +
  geom_tile(aes(fill = value)) +
  geom_sf(data = world, colour = "grey30", fill = NA, inherit.aes = FALSE) +
  scale_fill_viridis_c(name = expression(F[perm]), na.value = "#FFFFFF") +
  theme_map() +
  theme(plot.background = element_rect(fill = 'white', color=NA),
        legend.position = c(0.15,0.35))

gplot(fperm, maxpixels=5e5) + 
  geom_tile(aes(fill = value)) +
  geom_sf(data = world, colour = "grey30", fill = NA, inherit.aes = FALSE) +
  scale_fill_continuous_sequential(palette = "Viridis",
                                 na.value = NA,
                                 # na.translate = F,
                                 # labels = class_labels,
                                 name = expression(F[perm]),
                                 rev = FALSE) +
  theme_map() +
  theme(legend.position =  c(0.05,0.4),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)) +
  coord_sf()
ggsave2('fperm.eps', width = 6, height = 4)

fperm = resample(fperm, res)

# ------------ Competing uses ----------------
# Add residue production to countries
country_res = extract(res, countries, fun = sum, na.rm=TRUE)
countries = cbind(countries, country_res[,-1])
countries$res.prod = rowSums(country_res[,-1], na.rm = TRUE)
plot(countries['res.prod'])

# Add Herrero.region field to countries
# w.regions = fread('regions.csv')
w.regions = fread('gadm_table.csv')
countries = merge(countries, w.regions, all.x = TRUE, by = 'COUNTRY')

if (file.exists('Herrero.res.csv')) {
  Herrero.res =fread('Herrero.res.csv')
} else {
  Herrero.res = countries %>%
    st_make_valid() %>%
    group_by(Herrero.region) %>%
    summarise(res.prod = sum(res.prod, na.rm = TRUE)) %>%
  st_drop_geometry() %>%
    setDT()
  fwrite(Herrero.res, 'Herrero.res.csv')
}
#  table of harvestable fraction by crop 
res_tbl[, .(name, harvestable_res_fraction)]

#  create table of livestock fraction by country
residue_consumption = fread("Herrero_stover_consumption_by_region.csv")
Herrero.res = Herrero.res[residue_consumption, on='Herrero.region']
Herrero.res[, res.livestock := stover_feed_mt * 1e6]
Herrero.res[, res.livestock.frac := res.livestock / res.prod]
countries = merge(countries, Herrero.res[, .(Herrero.region, res.livestock.frac)], all.x = TRUE, by = 'Herrero.region')
ggplot(countries) + geom_sf(aes(fill = res.livestock.frac))

#  create table of available fraction by crop x region
crop.region = expand.grid(Herrero.region = Herrero.res$Herrero.region, crop = res_tbl$name)
setDT(crop.region)
crop.region = crop.region[res_tbl[, .(crop = name, harv.frac = harvestable_res_fraction)], on = 'crop']
crop.region = crop.region[Herrero.res[,.(Herrero.region, livestock.frac = res.livestock.frac)], on='Herrero.region']
crop.region[, avail.frac := pmax(0, harv.frac-livestock.frac)]

res.harv.frac = dcast(crop.region, Herrero.region~crop, value.var = 'harv.frac')
res.live.frac = dcast(crop.region, Herrero.region~crop, value.var = 'livestock.frac')
res.avai.frac = dcast(crop.region, Herrero.region~crop, value.var = 'avail.frac')

setnames(res.harv.frac, -1, \(x) paste0(x, '.harv'), skip_absent=T)
setnames(res.live.frac, -1, \(x) paste0(x, '.live'), skip_absent=T)
setnames(res.avai.frac, -1, \(x) paste0(x, '.avai'), skip_absent=T)
countries = merge(countries, res.harv.frac, all.x = TRUE, by = 'Herrero.region')
countries = merge(countries, res.live.frac, all.x = TRUE, by = 'Herrero.region')
countries = merge(countries, res.avai.frac, all.x = TRUE, by = 'Herrero.region')

# ------------ Calculate available residues per crop ----------------
# ------------ Convert available residues to biochar ----------------
# ------------ Calculate remaining biochar after 100 years ----------------
harv                = list() 
avai                = list()
res.prod            = list()
res.harv            = list()
res.avai            = list()
bc.prod.tech        = list()
bc.100.tech         = list()
bc.prod.constrained = list()
bc.100.constrained  = list()
i=0
for (.crop in names(res)) {
  i = i+1
  cat(round(100 * i / nlyr(res)), "% ", .crop, '\n', sep='')
  harv     [[.crop]] = rasterize(countries, res, paste0(.crop, '.harv')) # harvestable fraction by crop x country
  avai     [[.crop]] = rasterize(countries, res, paste0(.crop, '.avai')) # availability of residues by crop x country
  res.prod [[.crop]] = res[[.crop]] * res_tbl[name == .crop, C_dm] # Mg C /pixel/year
  res.harv [[.crop]] = res.prod[[.crop]] * harv[[.crop]]           # Mg C /pixel/year           
  res.avai [[.crop]] = res.prod[[.crop]] * avai[[.crop]]           # Mg C /pixel/year
  bc.prod.tech[[.crop]] = res.prod[[.crop]] * res_tbl[name == .crop, biochar_C_yield] # Mg C /pixel/year
  bc.100.tech [[.crop]] = bc.prod.tech[[.crop]] * fperm                               # Mg C /pixel/year
  bc.prod.constrained[[.crop]] = bc.prod.tech[[.crop]] * avai[[.crop]]                # Mg C /pixel/year
  bc.100.constrained [[.crop]] = bc.prod.tech[[.crop]] * avai[[.crop]] * fperm        # Mg C /pixel/year
} 
# all in Mg C /pixel/year
sum.res.prod            = sum(rast(res.prod), na.rm = TRUE)          
sum.res.harv            = sum(rast(res.harv), na.rm=TRUE)
sum.res.avai            = sum(rast(res.avai), na.rm=TRUE)
sum.bc.prod.tech        = sum(rast(bc.prod.tech), na.rm=TRUE)
sum.bc.100.tech         = sum(rast(bc.100.tech), na.rm=TRUE)
sum.bc.prod.constrained = sum(rast(bc.prod.constrained), na.rm=TRUE)
sum.bc.100.constrained  = sum(rast(bc.100.constrained), na.rm=TRUE)

# writeRaster(sum.res.prod, 'results/res_prod.tif', gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite = TRUE)
# writeRaster(sum.res.harv, 'results/res_harv.tif', gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite = TRUE)
# writeRaster(sum.res.avai, 'results/res_avail.tif', gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite = TRUE)
# writeRaster(sum.bc.prod.tech, 'results/bc_prod_tech.tif', gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite = TRUE)
# writeRaster(sum.bc.100.tech, 'results/bc_100_tech.tif', gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite = TRUE)
# writeRaster(sum.bc.prod.constrained, 'results/bc_prod_constrained.tif', gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite = TRUE)
# writeRaster(sum.bc.100.constrained, 'results/bc_100_constrained.tif', gdal=c("COMPRESS=ZSTD", "PREDICTOR=2"), overwrite = TRUE)

gplot(sum.bc.100.tech, maxpixels = 5e5) +
  geom_tile(aes(fill = value)) +
  geom_sf(data = countries, colour = "grey30", fill = NA, inherit.aes = FALSE) +
  scale_fill_viridis_c(na.value = "#FFFFFF") +
  theme_map()

# plot residues production map
world <- ne_countries(scale = 'small', returnclass = 'sf')
world = world[world$region_un != "Antarctica",]
n=6
a = cellSize(sum.res.prod, unit='ha')
res.prod.ha = sum.res.prod / a
classes = classIntervals(values(res.prod.ha), n = n, style = "fisher")$brks

res_tot_cut = classify(res.prod.ha, cbind(classes[1:n], classes[-1], seq_len(n)))
res_tot_cut = classify(res_tot_cut, cbind(NaN, NA))

classes.1 = signif(classes, 1)
class_labels = c(paste(c(0, format(classes.1[2:n], scientific = F)), 
                       format(classes.1[-1], scientific = F), sep = ' - '))

gplot(res_tot_cut, maxpixels=5e6) + 
  geom_tile(aes(fill = factor(value))) +
  geom_sf(data = world, colour = "grey30", fill = NA, inherit.aes = FALSE) +
  scale_fill_discrete_sequential(palette = "Viridis",
                                 na.value = NA,
                                 na.translate = F,
                                 labels = class_labels,
                                 name = expression(atop("Crop residue production", "(Mg ha"^-1*")")),
                                 rev = FALSE) +
  theme_map() +
  theme(legend.position = c(0.05,0.4),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10)) +
  coord_sf()

ggsave2('res_map.eps', width = 9, height=6)

# --------------------- Summary Results -------------------------------
global.res.prod            = unlist(global(sum.res.prod, 'sum', na.rm=TRUE)) / 1e9
global.res.harv            = unlist(global(sum.res.harv, 'sum', na.rm=TRUE)) / 1e9
global.res.avai            = unlist(global(sum.res.avai, 'sum', na.rm=TRUE)) / 1e9
global.bc.prod.tech        = unlist(global(sum.bc.prod.tech, 'sum', na.rm=TRUE)) / 1e9
global.bc.100.tech         = unlist(global(sum.bc.100.tech, 'sum', na.rm=TRUE)) / 1e9
global.bc.prod.constrained = unlist(global(sum.bc.prod.constrained, 'sum', na.rm=TRUE)) / 1e9
global.bc.100.constrained  = unlist(global(sum.bc.100.constrained, 'sum', na.rm=TRUE)) / 1e9

df = data.table(x = c(
  'Residue production',
  'Harvest losses',
  'Livestock production',
  'Biochar production',
  'Biochar decomposition'))
df[, y := c(
  global.res.prod,
  global.res.harv - global.res.prod,
  global.res.avai - global.res.harv,
  global.bc.prod.constrained - global.res.avai,
  global.bc.100.constrained - global.bc.prod.constrained)]
df$x = factor(df$x, levels = df$x)

gw = waterfall(df, calc_total = TRUE, total_axis_text = 'Sequestered biochar',
               rect_text_labels = round(df$y, 2), total_rect_text = round(sum(df$y), 2)) +
  scale_y_continuous('Carbon stock changes (Pg C)', expand = c(0,0), limits = c(0,3)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12), 
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = 'white'),
        axis.title.y = element_text(size=12))
gw
ggsave('waterfall.png', gw, width = 6, height = 4)
ggsave2('waterfall.eps', gw, width = 6, height = 5)


global.res = data.table(
  C.type = c(
    'res.prod',
    'res.harv',
    'res.avail',
    'bc.prod.tech',
    'bc.100.tech',
    'bc.prod.constrained',
    'bc.100.constrained'),
  value = c(
    global.res.prod,
    global.res.harv,
    global.res.avai,
    global.bc.prod.tech,
    global.bc.100.tech,
    global.bc.prod.constrained,
    global.bc.100.constrained
  ))
  
knitr::kable(global.res, digits = 2)

# --------------- compare to current emissions ------------------
bc.100.constrained.country = extract(sum.bc.100.constrained, countries, fun = 'sum', na.rm=TRUE)
countries$bc.100.constrained = bc.100.constrained.country$sum 
bc.100.tech.country = extract(sum.bc.100.tech, countries, fun = 'sum', na.rm=TRUE)
countries$bc.100.tech = bc.100.tech.country$sum

emissions = fread('ghg-emissions_countrynames.csv')
# setnames(emissions, 'Country', 'NAME_EN')
# setdiff(unique(countries$NAME_EN), unique(emissions$Country))
# setdiff(emissions$Country, countries$NAME_EN)

countries = merge(countries, emissions[, .(COUNTRY, GHG_2019 = Y2019)], on='COUNTRY', all.x=TRUE)

countries %<>% 
  group_by(COUNTRY) %>% 
  mutate(bc.tech_ghg_pc = (100 * sum(bc.100.tech, na.rm = TRUE) * 44/12 /1e6) / mean(GHG_2019, na.rm = TRUE),
         bc.constrain_ghg_pc = (100 * sum(bc.100.constrained, na.rm = TRUE) * 44/12 /1e6) / mean(GHG_2019, na.rm = TRUE))

countries = countries %>% 
  st_crop(xmin = -180, ymin = -57, xmax = 180, ymax = 85)

# countries$bc.tech_ghg_pc = (100 * countries$bc.100.tech * 44/12 /1e6) / countries$GHG_2019
# countries[countries$bc.tech_ghg_pc < 0,]$bc.tech_ghg_pc = NA
# countries$bc.constrain_ghg_pc = (100 * countries$bc.100.constrained * 44/12 /1e6) / countries$GHG_2019
# countries[countries$bc.constrain_ghg_pc < 0,]$bc.constrain_ghg_pc = NA

breaks = c(0,1,2,4,8,16,32,64,Inf) 
labels = c('0-1','1-2','2-4','4-8','8-16','16-32','32-64','>64')

countries$ghg.pc.tech = cut(countries$bc.tech_ghg_pc, include.lowest=TRUE, 
                            breaks = breaks, labels = labels)

countries$ghg.pc.constrain = cut(countries$bc.constrain_ghg_pc, include.lowest=TRUE, 
                                 breaks = breaks, labels = labels)

gg.tech.pc = ggplot(countries) +
  geom_sf(aes(fill = ghg.pc.tech))+
  scale_fill_viridis_d(na.translate = FALSE, drop=FALSE, name='Percent of emissions', guide="none") +
  annotate("text", x = -160, y = 90, label = "c", size = 10) +
  theme_map() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal', 
        plot.background = element_rect(fill = 'white', color=NA))

gg.constrain.pc = ggplot(countries) +
  geom_sf(aes(fill = ghg.pc.constrain))+
  scale_fill_viridis_d(na.translate = FALSE, drop=FALSE, name='Percent of emissions', guide="none") +
  annotate("text", x = -160, y = 90, label = "d", size = 10) +
  theme_map() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal', 
        plot.background = element_rect(fill = 'white', color=NA))

gg.legend.pc = ggplot(countries) +
  geom_sf(aes(fill = ghg.pc.constrain)) +
  scale_fill_viridis_d(na.translate = FALSE, drop=FALSE, name='Percent of emissions') +
  theme_map() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.justification = 'center', 
        plot.background = element_rect(fill = 'white', color=NA),
        legend.background = element_rect(fill = 'white', color=NA)) +
  guides(fill = guide_legend(nrow = 1, title.position = 'top'))
gg.legend.pc = get_legend(gg.legend.pc)

plot_grid(gg.tech.pc, gg.legend.pc, gg.constrain.pc, ncol = 1, rel_heights = c(1,0.2,1))
ggsave2('ghg_pc.png', height = 8, width = 10)

file.remove('countries_res_bc_ghg.gpkg')
st_write(countries, 'countries_res_bc_ghg.gpkg')
fwrite(st_drop_geometry(countries), 'countries_1.csv')


# plots of country biochar potential
sf_use_s2(TRUE)
sf_use_s2(FALSE)


# st_layers('~/gis/boundaries/gadm_410-levels.gpkg')
# gadm = st_read('~/gis/boundaries/gadm_410-levels.gpkg')
# gadm = st_simplify(gadm, dTolerance = 0.1)
# gadm = gadm[gadm$COUNTRY != "Antarctica",]

country_bc.tech = extract(sum.bc.100.tech, countries, fun = sum, na.rm=TRUE)
country_bc.constrain = extract(sum.bc.100.constrained, countries, fun = sum, na.rm=TRUE)
countries$bc.tech = country_bc.tech$sum
countries$bc.cons = country_bc.constrain$sum
countries$area = st_area(countries)

# bc.XXX:  units = Mg C
# area:    units = m^2 
countries$bc.tech.dens = countries$bc.tech / (countries$area / 1e6)
countries$bc.cons.dens = countries$bc.cons / (countries$area / 1e6)


# hist(countries$bc.tech.dens)


breaks = c(0, 1, 5, 10, 15, 20, 25, 30, Inf) 
labels = c('0-1','1-2','2-4','4-8','8-16','16-32','32-64', '>64')
labels = c('0-1','1-5','5-10','10-15','15-20','20-25','25-30', '>30')

countries$bc.tech.cut = cut(countries$bc.tech.dens, include.lowest=TRUE, 
                            breaks = breaks, labels = labels)
countries$bc.cons.cut = cut(countries$bc.cons.dens, include.lowest=TRUE, 
                       breaks = breaks, labels = labels)
option = "F"

gg.tech = ggplot(countries) +
  geom_sf(aes(fill = bc.tech.cut))+
  scale_fill_viridis_d(na.translate = FALSE, drop=FALSE, name='', 
                       guide="none", option = option) +
  annotate("text", x = -160, y = 90, label = "a", size = 10) +
  theme_map() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal', 
        plot.background = element_rect(fill = 'white', color=NA))

gg.cons = ggplot(countries) +
  geom_sf(aes(fill = bc.cons.cut))+
  scale_fill_viridis_d(na.translate = FALSE, drop=FALSE, name='', 
                       guide="none", option = option) +
  annotate("text", x = -160, y = 90, label = "b", size = 10) +
  theme_map() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal', 
        plot.background = element_rect(fill = 'white', color=NA))

gg.legend = ggplot(countries) +
  geom_sf(aes(fill = bc.tech.cut)) +
  scale_fill_viridis_d(na.translate = FALSE, drop=FALSE, 
                       name=expression('Biochar density ('*Mg~C~km^{-2}~yr^{-1}*')'), 
                       option = option) +
  theme_map() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.justification = 'center', 
        plot.background = element_rect(fill = 'white', color=NA),
        legend.background = element_rect(fill = 'white', color=NA)) +
  guides(fill = guide_legend(nrow = 1, title.position = 'top'))
gg.legend = get_legend(gg.legend)

# plot_grid(gg.tech, gg.legend, gg.cons, ncol = 1, rel_heights = c(1,0.2,1))
plot_grid(gg.tech,    gg.tech.pc, 
          NULL, NULL,
          gg.legend,  gg.legend.pc, 
          NULL, NULL,
          gg.cons,    gg.constrain.pc,
          ncol = 2, rel_heights = c(1, -0.13, 0.1, -0.13, 1))


# ggsave2('bc_maps.jpg', height = 8, width = 14)
ggsave2('bc_maps.eps', height = 8, width = 14)




# bc.100.constrained.country = vect("results/bc100_constrained.shp")
# sapply(unique(countries$NAME_EN), \(x) sum(countries[countries$NAME_EN == x, 'bc'], na.rm=TRUE), USE.NAMES =TRUE)
# x = countries$NAME_EN[1]

# --------- fperm in croplands
range(fperm)
cropland = classify(sum.res.prod, cbind(-Inf, Inf, 1))
fperm.cropland = fperm * cropland
gplot(fperm.cropland) +
  geom_raster(aes(fill=value)) +
  geom_sf(data = countries, colour = "grey30", fill = NA, inherit.aes = FALSE) +
  scale_fill_viridis_c(na.value = "#FFFFFF") +
  theme_map()
x = ecdf(values(fperm.cropland))
plot(x)
quantile(values(fperm.cropland), probs = c(0.025,0.975), na.rm=TRUE)

# ---------------
country_mitigation = setDT(st_drop_geometry(countries))[, 
    .(NAME_EN, res.prod, bc.100.constrained, bc.100.tech, 
      GHG_2019, bc.tech_ghg_pc, bc.constrain_ghg_pc, ghg.pc.tech, ghg.pc.constrain)]
setorder(country_mitigation, -bc.100.tech)
country_mitigation[, cum_pc := cumsum(bc.100.tech) / sum(bc.100.tech)]

crs(gadm)
crs(countries)

# -------- EU 27 results

# Austria, Belgium, Bulgaria, Cyprus, Czech Republic, 
# Denmark, Estonia, Finland, France, Germany, Greece, Hungary, 
# Ireland, Italy, Latvia, Lithuania, Luxembourg, Malta, the Netherlands, 
# Poland, Portugal, Romania, Slovakia, Slovenia, Spain, Sweden, UK
EU27 = c(
  'Austria', 'Belgium', 'Bulgaria', 'Cyprus', 'Czechia', 
  'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 
  'Greece', 'Hungary', 'Ireland', 'Italy', 'Latvia', 'Lithuania', 
  'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 
  'Romania', 'Slovakia', 'Slovenia', 'Spain', 'Sweden', 'United Kingdom'
)
EU27 = EU27[which(EU27 %in% countries$COUNTRY)]

country_res_C = extract(sum.res.prod, countries, fun = sum, na.rm=TRUE)
countries$res.prod.C = country_res_C[,-1]

countries %>% 
  filter(COUNTRY %in% EU27) %>% 
  summarise(sum(bc.100.tech), sum(bc.100.constrained), sum(res.prod.C)) %>% 
  st_drop_geometry()

countries %>% 
  st_drop_geometry() %>% 
  group_by(COUNTRY) %>% 
  summarise(bc.100.tech = sum(bc.100.tech)/1e6, 
            bc.100.constrained = sum(bc.100.constrained)/ 1e6, 
            res.prod.C = sum(res.prod.C)/1e6) %>% 
  arrange(desc(bc.100.tech)) 

countries[countries$COUNTRY == "People's Republic of China",]
  