library(raadfiles)
library(dplyr)
files <- polarview_files(type = "jpeg") |>
  transmute(date, fullname)
files <- tail(files, 100)

xmin <- xmax <- ymin <- ymax <- rep(NA_real_, nrow(files))
for (i in seq_along(xmin)) {
  tarball <- raadfiles:::polarview_jpeg_tarball(files$fullname[i])
  if (is.na(tarball)) next;
  info1 <- try(vapour::vapour_raster_info(glue::glue("/vsitar/{tarball}/{raadfiles:::polarview_tifname(tarball)}")))
  if (inherits(info1, "try-error")) next;
  xmin[i] <- info1$extent[1L]
  xmax[i] <- info1$extent[2L]
  ymin[i] <- info1$extent[3L]
  ymax[i] <- info1$extent[4L]
  print(i)
}

files <- files |> mutate(xmin = xmin, xmax = xmax, ymin  = ymin, ymax = ymax)


#
# library(DBI)
# con = dbConnect(duckdb::duckdb(), dbdir="polarview_raad.duckdb", read_only=FALSE)
# dbWriteTable(con, "polarview", files)
# rm(con)

library(DBI)
library(dplyr)
con = dbConnect(duckdb::duckdb(), dbdir="polarview_raad.duckdb", read_only=TRUE)
files <- tbl(con, "polarview") |> collect()
files <- files |> filter(!is.na(xmin))

library(whatarelief)
afile <- files$fullname[2]
info1 <- vapour::vapour_raster_info(afile)
info1$projection <- vapour::vapour_srs_wkt("+proj=stere +lat_0=-90")

plot(range(files$xmin), range(files$ymin), asp = 1)
ex <-list(x = c(2e+06, 3e+06), y = c(-112489.769462839, -1914345.41768303
))

files1 <- files |> filter(between(xmin, ex$x[1], ex$x[2]) |  between(xmax, ex$x[1], ex$x[2]) &
                            between(ymin, ex$y[1], ex$y[2]) |  between(ymax, ex$y[1], ex$y[2]))
vrt <- character(nrow(files1))
for (i in seq_along(vrt)) vrt[i] <- vapour::vapour_vrt(files1$fullname[i], extent = c(files1$xmin[i], files1$xmax[i],
                                                                                     files1$ymin[i], files1$ymax[i]),
                                                       projection = info1$projection)

ext <- c(range(unlist(files1[c("xmin", "xmax")])),
         range(unlist(files1[c("ymin", "ymax")])))
par(mfrow = n2mfrow(22), mar = rep(0, 4))
for (i in seq_along(vrt)) {
  im <- imagery(source = vrt[i], extent = ext, projection = info1$projection,
              dimension = dev.size("px")/4)
ximage::ximage(im, extent = ext, asp = 1)
}

m <- do.call(cbind, maps::map(plot = F)[1:2])
lines(reproj::reproj_xy(m, info1$projection), lwd = 1, col = "yellow")
