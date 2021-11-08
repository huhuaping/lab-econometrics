(function(inputFile, encoding) { 
  outFile <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(inputFile));
  out_dir <- 'public';
  rmarkdown::render(inputFile,
                    encoding=encoding,
                    output_file=file.path(dirname(inputFile), out_dir, outFile)) })