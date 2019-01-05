# Move Sanne and Mayra's data to correct WD

library(fs)
library(openxlsx)

pad_path <- 'C:/Users/sl13sise/Dropbox/PADRINO/Digitization/Padrino_progress_'

if(file_exists('data-raw/all_progress.xlsx')) {
  file_delete('data-raw/all_progress.xlsx')
  file_delete('data-raw/Padrino_progress_Mayra.xlsx')
  file_delete('data-raw/Padrino_progress_Sanne.xlsx')
}

file_copy(paste(pad_path, c("Sanne", "Mayra"), '.xlsx', sep = ""),
          rep('data-raw/', 2))
sanne <- RPadrino:::.read_all_sheets('data-raw/Padrino_progress_Sanne.xlsx')
# remove some weird columns
sanne <- lapply(sanne, function(x) x[ ,!grepl('X__[0-9]', names(x))])

mayra <- RPadrino:::.read_all_sheets('data-raw/Padrino_progress_Mayra.xlsx')


nms_s <- lapply(sanne, names)
nms_m <- lapply(mayra, names)

identical(unlist(nms_m), unlist(nms_s))

all_data <- lapply(seq_len(length(sanne)), function(x) rbind(sanne[[x]],
                                                             mayra[[x]]))

names(all_data) <- names(mayra)

write.xlsx(all_data, file = 'data-raw/all_progress.xlsx')
