# sample script for building an IPM from the database

library(RPadrino)

# This part will likely change to a function that hooks into an online instance
# of the database rather than reading an excel sheet
padrino <- RPadrino:::.read_all_sheets('../Padrino/metadata/Test_Padrino_methods.xlsx')

# This isn't really necessary any more
padrino[[9]]$model_type <- gsub('Integrated', 'Substituted', padrino[[9]]$model_type)

oenethra_proto <- make_proto_ipm(padrino, ipm_id == 'xxxxx1')
oenethra_kernels <- make_ipm(oenethra_proto)
oenethra_k <- make_k(oenethra_kernels, oenethra_proto)

target <- 1.059307
actual <- max(Re(eigen(oenethra_k)$values))
actual - target

