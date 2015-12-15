% Readme

# What is Easy data.frame?

# Install

```r
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
options(download.file.method = "wininet")
install_git("https://github.com/huashan/ezdf")
```
