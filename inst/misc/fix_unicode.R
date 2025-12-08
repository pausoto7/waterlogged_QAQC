# Fix Unicode escape sequences
files <- list.files('R', pattern='*.R$', full.names=TRUE)
for(f in files) {
  content <- readLines(f, warn=FALSE, encoding='UTF-8')
  content <- gsub('00B0', '\\u00B0', content, fixed=TRUE)
  writeLines(content, f, useBytes=FALSE)
}
cat('Fixed', length(files), 'R files\n')

# Fix test files
test_files <- list.files('tests/testthat', pattern='*.R$', full.names=TRUE)
for(f in test_files) {
  content <- readLines(f, warn=FALSE, encoding='UTF-8')
  content <- gsub('00B0', '\\u00B0', content, fixed=TRUE)
  writeLines(content, f, useBytes=FALSE)
}
cat('Fixed', length(test_files), 'test files\n')
