Package only availaible on [github](http://github.com/). In order to install you will need the [devtools](https://cran.r-project.org/package=devtools) package installed on your [R program](https://www.r-project.org/). Please follow the code bellow to install the package:

To install the latest released version:

```r
devtools::install_github("ConservaSom/monitoraSom",
                         dependencies = T,
                         auth_token = "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"
)
```


Alternatively,

Install the following packages

```r
install.packages("pak")
install.packages("gitcreds")
```

Execute the following command and paste the token below in the prompt
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

```r
gitcreds::gitcreds_set()
```

Install `monitoraSom`

```r
pak::pkg_install("ConservaSom/monitoraSom")
```
