---
layout: post
title: "some quick thoughts on gerrymandering"
date: 2023-06-18
---

> This post demonstrates a simple summary of the famous **Iris dataset** and a visualization.

### Summary of Data

    summary(iris)

    ##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    ##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
    ##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
    ##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
    ##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
    ##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
    ##        Species  
    ##  setosa    :50  
    ##  versicolor:50  
    ##  virginica :50  
    ##                 
    ##                 
    ## 

### else

    iris |> head() |> DT::datatable()

<div class="datatables html-widget html-fill-item" id="htmlwidget-7719428dd96a930c1400" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-7719428dd96a930c1400">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6"],[5.1,4.9,4.7,4.6,5,5.4],[3.5,3,3.2,3.1,3.6,3.9],[1.4,1.4,1.3,1.5,1.4,1.7],[0.2,0.2,0.2,0.2,0.2,0.4],["setosa","setosa","setosa","setosa","setosa","setosa"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Sepal.Length<\/th>\n      <th>Sepal.Width<\/th>\n      <th>Petal.Length<\/th>\n      <th>Petal.Width<\/th>\n      <th>Species<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,2,3,4]},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"Sepal.Length","targets":1},{"name":"Sepal.Width","targets":2},{"name":"Petal.Length","targets":3},{"name":"Petal.Width","targets":4},{"name":"Species","targets":5}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
