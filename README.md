# Modelo de precificação de imóveis: Aplicação em Melbourne, AU

## Bibliotecas
```{r}
library('ggplot2')
library('Boruta')
library('caTools')
library('statmod')
library('hnp')
library('fmsb')
```
## Resumo
Modelo linear generalizado para respostas com distribuição gama foi utilizado para precificação de imóveis na região de Melbourne, Austrália. 
Um conjunto de 11.237  ofertas com um conjunto de 9 variáveis explicativas foram utilizados para modelar o problema. 
Com um coeficiente de determinação (R$^2$) de 0,8095 os imóveis mais caros são os que apresentam um maior terreno; quartos, banheiros e vagas para garagem extra; os mais próximos do distrito comercial que estão localizados a sudeste da cidade cuja densidade populacional seja mais baixa. 

## Licença
[MIT](https://choosealicense.com/licenses/mit/)
