Package only availaible on [github](http://github.com/). In order to install you will need the [devtools](https://cran.r-project.org/package=devtools) package installed on your [R program](https://www.r-project.org/). Please follow the code bellow to install the package:

To install the latest released version:

```r
devtools::install_github("ConservaSom/monitoraSom",
                         dependencies = T,
                         auth_token = "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"
)


Alternatively,

```r
# instale estes dois pacotes
install.packages("pak")
install.packages("gitcreds")

# Execute esse comando e insira as credenciais para ter acesso ao pacote
# monitoraSom. Quando executar, cole o token
# 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 e pressione enter.
gitcreds::gitcreds_set()

# Instale o pacote monitoraSom
pak::pkg_install("ConservaSom/monitoraSom")
```
