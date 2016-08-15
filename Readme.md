# ezdf: Easy data.frame



# Installation


```r
install.packages('haven', 'data.table')
library(devtools)
install_github('huashan/ezdf')
```

For those having trouble with the error message:

```
Error in curl::curl_fetch_memory(url, handle = handle) :
  Timeout was reached
```

may try these:

```r
install.packages('haven', 'data.table')
options(download.file.method = "wininet")
install_git("https://github.com/huashan/ezdf")
```

You may also install an R package [pander](https://github.com/huashan/pander), an R pandoc writer. This is a fixed version with better support for CJK characters.

```r
devtools::install_github('huashan/pander')
```