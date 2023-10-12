print_chart <- function(p, 
                        filename, 
                        output_path,
                        width,
                        height,
                        res){
    print(filename)
    jpeg(
        file = paste0(output_path, filename),
        width = width,
        height = height,
        res = res
    )
    print(p)
    dev.off()
}