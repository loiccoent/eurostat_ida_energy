library(futile.logger)

print_chart <- function(p, 
                        filename, 
                        output_path,
                        width,
                        height,
                        res){
    flog.info(paste("Save the chart", filename))
    jpeg(
        file = paste0(output_path, filename),
        width = width,
        height = height,
        res = res
    )
    print(p)
    dev.off()
}