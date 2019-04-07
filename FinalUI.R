library(wordnet)
library(e1071)
library(RTextTools)
library(shiny)
library(tau)
library(tm)

shinyUI<-
  
  fluidPage(
    
    titlePanel(
      
      h2("PREFERENCED BASED ENHANCED CLASSIFICATION")),hr(),
    
    sidebarLayout(
      
      sidebarPanel( h3("USER PREFERENCES"),
                    
                    textInput(inputId = "search1", label = h4("Enter Preference1 :"), 
                              value = ""), 
                    br(),
                    hr(),
                    
                    textInput(inputId = "search2", label = h4("Enter Preference2 :"), 
                              value = ""), 
                    br(),
                    hr(),
                    
                    
                    textInput(inputId = "search3", label = h4("Enter Preference3 :"), 
                             value = ""),br(),hr(),
                    
                    actionButton(inputId = "buttonx",label = "Segregate"),width=3),
      
      mainPanel(h3("Results ",align="center"),br(),
                
               
                textOutput(outputId = "txtout"),hr(),br(),
                actionButton(inputId = "button1", label = "NaiveBaye's"),hr(),br(), 
                textOutput(outputId = "txtout1"),hr(),br(),
                actionButton(inputId = "button2", label = "SVM"),hr(),br(),
                textOutput(outputId = "txtout2"),hr(),br(),
                actionButton(inputId = "button3", label = "DecisionTree"),hr(),br(),
                textOutput(outputId = "txtout3"))
    )
    
  )

shinyServer<-function(input, output) {
  
  synx <- function(p1,p2,p3) {
    getwd()
    setwd("C:/Users/Dell/Desktop")
    
    getwd()
    df <- read.csv("yelp.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
                   strip.white = TRUE)
    dfd<-as.character(df[,2])
    df2<-as.character(df[,1])
    Sys.setenv(WNHOME="C:/Program Files (x86)/WordNet/2.1")
    initDict()
    getwd()
    setwd("C:/Program Files (x86)/WordNet/2.1/dict")
    getDict()
    getDictInstance()
    
    if(initDict()) 
    {
      filter1 <- getTermFilter("ExactMatchFilter",p1,TRUE)
      filter2 <- getTermFilter("ExactMatchFilter",p2,TRUE)
      filter3 <- getTermFilter("ExactMatchFilter",p3,TRUE)
      terms1 <- getIndexTerms("NOUN", 5, filter1)
      terms2 <- getIndexTerms("NOUN", 5, filter2)
      terms3 <- getIndexTerms("NOUN", 5, filter3)
      x1=getSynonyms(terms1[[1]])
      x2=getSynonyms(terms2[[1]])
      x3=getSynonyms(terms3[[1]])
    }
    x<-c(x1,x2,x3)
    words <- readLines(system.file("stopwords", "english.dat",
                                   package = "tm"))
    s<-remove_stopwords(dfd, words, lines = TRUE)
    n<-removeNumbers(s)
    t<-removePunctuation(n, preserve_intra_word_dashes = FALSE)
    a<-1
    a
    count=1
    for(i in 1:(length(x)))
    { 
      for(j in 1:(length(t)))
      {
        m=0
        e<-tokenize(t[j])
        for(k in 1:(length(e)))
        {
          if((e[k]==x[i]))
          {
            if(m==0)
            {
              
              m=j
              a[count]=df2[j]
              p1[count]=m
              count=count+1
              
            }
          }
        }
      }
    }
    p1<-unique(p1)
    print(paste("No of documents segregated along with their document id is::",length(p1)))
    print(paste("  "))
  }
  syn1 <- function(p1,p2,p3) {
    getwd()
    setwd("C:/Users/Dell/Desktop")
    
    getwd()
    df <- read.csv("yelp.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
                   strip.white = TRUE)
    dfd<-as.character(df[,2])
    df2<-as.character(df[,1])
    Sys.setenv(WNHOME="C:/Program Files (x86)/WordNet/2.1")
    initDict()
    getwd()
    setwd("C:/Program Files (x86)/WordNet/2.1/dict")
    getDict()
    getDictInstance()
  
    if(initDict()) 
    {
      filter1 <- getTermFilter("ExactMatchFilter",p1,TRUE)
      filter2 <- getTermFilter("ExactMatchFilter",p2,TRUE)
      filter3 <- getTermFilter("ExactMatchFilter",p3,TRUE)
      terms1 <- getIndexTerms("NOUN", 5, filter1)
      terms2 <- getIndexTerms("NOUN", 5, filter2)
      terms3 <- getIndexTerms("NOUN", 5, filter3)
      x1=getSynonyms(terms1[[1]])
      x2=getSynonyms(terms2[[1]])
      x3=getSynonyms(terms3[[1]])
    }
    x<-c(x1,x2,x3)
    words <- readLines(system.file("stopwords", "english.dat",
                                   package = "tm"))
    s<-remove_stopwords(dfd, words, lines = TRUE)
    n<-removeNumbers(s)
    t<-removePunctuation(n, preserve_intra_word_dashes = FALSE)
    a<-1
    a
    count=1
    for(i in 1:(length(x)))
    { 
      for(j in 1:(length(t)))
      {
        m=0
        e<-tokenize(t[j])
        for(k in 1:(length(e)))
        {
          if((e[k]==x[i]))
          {
            if(m==0)
            {
              
              m=j
              a[count]=df2[j]
              p1[count]=m
              count=count+1
              
            }
          }
        }
      }
    }
    p1<-unique(p1)
    for(l in 1:(length(p1)))   
    {
      segdoc<-df[c(p1),]
    }
    segdoc
    new1<-rbind(df,segdoc)
    setwd("C:/Users/Dell/Desktop")
    getwd()
    write.csv(new1[,], file = "segredoc1.CSV",row.names=FALSE)
    
    df1 <- read.csv("segredoc1.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
                    strip.white = TRUE)
    matrix= create_matrix(df1[,2], language="english", 
                          removeStopwords=TRUE, removeNumbers=TRUE, 
                          stemWords=TRUE)
    mat = as.matrix(matrix)
    classifier = naiveBayes(mat[1:1000,], as.factor(df1[1:1000,3]) )
    predicted = predict(classifier, mat[1001:(1000+(length(p1))),])
    print(paste("Accuracy WithOut Weighting:"))
    re<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted)
    re<-(re*100)
    print(paste(re)) 
    print(paste("Accuracy With Weighting:"))
    matrix1= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "bpn"))
    mat1 = as.matrix(matrix1)
    classifier1 = naiveBayes(mat1[1:1000,], as.factor(df1[1:1000,3]) )
    
    predicted1= predict(classifier1, mat1[1001:(1000+(length(p1))),])
    re1<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted1)
    re1<-(re1*100)
    print(paste(re1))
    
    matrix2= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "btn"))
    mat2 = as.matrix(matrix2)
    classifier2 = naiveBayes(mat2[1:1000,], as.factor(df1[1:1000,3]) )
    
    predicted2= predict(classifier2, mat2[1001:(1000+(length(p1))),])
    re2<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted2)
    re2<-(re2*100)
    print(paste(re2))
    
    matrix3= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "apn"))
    mat3 = as.matrix(matrix3)
    classifier3 = naiveBayes(mat3[1:1000,], as.factor(df1[1:1000,3]) )
    predicted3= predict(classifier3, mat3[1001:(1000+(length(p1))),])
    re3<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted3)
    re3<-(re3*100)
    print(paste(re3))
    
    matrix4= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "atn"))
    mat4 = as.matrix(matrix4)
    classifier4 = naiveBayes(mat4[1:1000,], as.factor(df1[1:1000,3]) )
    predicted4= predict(classifier4, mat4[1001:(1000+(length(p1))),])
    re4<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted4)
    re4<-(re4*100)
    print(paste(re4))
    
    matrix5= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "lpn"))
    mat5 = as.matrix(matrix5)
    classifier5 = naiveBayes(mat5[1:1000,], as.factor(df1[1:1000,3]) )
    predicted5= predict(classifier5, mat5[1001:(1000+(length(p1))),])
    re5<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted5)
    re5<-(re5*100)
    print(paste(re5))
    
    matrix6= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "ltn"))
    mat6 = as.matrix(matrix6)
    classifier6 = naiveBayes(mat6[1:1000,], as.factor(df1[1:1000,3]) )
    predicted6= predict(classifier6, mat6[1001:(1000+(length(p1))),])
    re6<-recall_accuracy(df1[1001:(1000+(length(p1))), 3], predicted6)
    re6<-(re6*100)
    re6
   
  }
  
  syn2 <- function(p1,p2,p3) {
    
    getwd()
    setwd("C:/Users/Dell/Desktop")
    
    getwd()
    df <- read.csv("yelp.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
                   strip.white = TRUE)
    dfd<-as.character(df[,2])
    df2<-as.character(df[,1])
    Sys.setenv(WNHOME="C:/Program Files (x86)/WordNet/2.1")
    initDict()
    getwd()
    setwd("C:/Program Files (x86)/WordNet/2.1/dict")
    getDict()
    getDictInstance()
    
    if(initDict()) 
    {
      filter1 <- getTermFilter("ExactMatchFilter",p1,TRUE)
      filter2 <- getTermFilter("ExactMatchFilter",p2,TRUE)
      filter3 <- getTermFilter("ExactMatchFilter",p3,TRUE)
      terms1 <- getIndexTerms("NOUN", 5, filter1)
      terms2 <- getIndexTerms("NOUN", 5, filter2)
      terms3 <- getIndexTerms("NOUN", 5, filter3)
      x1=getSynonyms(terms1[[1]])
      x2=getSynonyms(terms2[[1]])
      x3=getSynonyms(terms3[[1]])
    }
    x<-c(x1,x2,x3)
    words <- readLines(system.file("stopwords", "english.dat",
                                   package = "tm"))
    s<-remove_stopwords(dfd, words, lines = TRUE)
    n<-removeNumbers(s)
    t<-removePunctuation(n, preserve_intra_word_dashes = FALSE)
    a<-1
    a
    count=1
    for(i in 1:(length(x)))
    { 
      for(j in 1:(length(t)))
      {
        m=0
        e<-tokenize(t[j])
        for(k in 1:(length(e)))
        {
          if((e[k]==x[i]))
          {
            if(m==0)
            {
              
              m=j
              a[count]=df2[j]
              p1[count]=m
              count=count+1
              
            }
          }
        }
      }
    }
    p1<-unique(p1)
    for(l in 1:(length(p1)))   
    {
      segdoc<-df[c(p1),]
    }
    segdoc
    new1<-rbind(df,segdoc)
    setwd("C:/Users/Dell/Desktop")
    getwd()
    write.csv(new1[,], file = "segredoc1.CSV",row.names=FALSE)
    
    df1 <- read.csv("segredoc1.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
                    strip.white = TRUE)
    matrix= create_matrix(df1[,2], language="english", 
                          removeStopwords=TRUE, removeNumbers=TRUE, 
                          stemWords=TRUE)
    mat = as.matrix(matrix)
    
    container = create_container(matrix, as.numeric(as.factor(df1[,3])),
                                 trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models = train_models(container, algorithms=c("SVM","TREE"))
    results = classify_models(container, models)
    print(paste("Accuracy WithOut Weighting:"))
    svm<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results[,"SVM_LABEL"])
    svm<-(svm*100)
    print(paste(svm))
    print(paste("SVM With Weighting:"))
    matrix1= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "bpn"))
   
    
    container1 = create_container(matrix1, as.numeric(as.factor(df1[,3])),
                                  trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models1 = train_models(container1, algorithms=c("SVM","TREE"))
    results1 = classify_models(container1, models1)
    svm1<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results1[,"SVM_LABEL"])
    svm1<-(svm1*100)
    print(paste(svm1))
    matrix2= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "btn"))
    container2 = create_container(matrix2, as.numeric(as.factor(df1[,3])),
                                  trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models2 = train_models(container2, algorithms=c("SVM","TREE"))
    results2 = classify_models(container2, models2)
    svm2<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results2[,"SVM_LABEL"])
    svm2<-(svm2*100)
    print(paste(svm2))
    
    matrix3= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "apn"))
    
    
    container3 = create_container(matrix3, as.numeric(as.factor(df1[,3])),
                                  trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models3 = train_models(container3, algorithms=c("SVM","TREE"))
    results3 = classify_models(container3, models3)
    svm3<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results3[,"SVM_LABEL"])
    svm3<-(svm3*100)
    print(paste(svm3))
    
    matrix4= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "atn"))
    container4= create_container(matrix4, as.numeric(as.factor(df1[,3])),
                                 trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models4 = train_models(container4, algorithms=c("SVM","TREE"))
    results4 = classify_models(container4, models4)
    svm4<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results4[,"SVM_LABEL"])
    svm4<-(svm4*100)
    print(paste(svm4))
    
    matrix5= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "lpn"))
    container5= create_container(matrix5, as.numeric(as.factor(df1[,3])),
                                 trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    container5
    models5 = train_models(container5, algorithms=c("SVM","TREE"))
    models5
    results5 = classify_models(container5, models5)
    results5
    svm5<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results5[,"SVM_LABEL"])
    svm5<-(svm5*100)
    print(paste(svm5))
    
    matrix6= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "ltn"))
    container6= create_container(matrix6, as.numeric(as.factor(df1[,3])),
                                 trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models6 = train_models(container6, algorithms=c("SVM","TREE"))
    results6 = classify_models(container6, models6)
    svm6<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results6[,"SVM_LABEL"])
    svm6<-(svm6*100)
    svm6
    
    
  }
  
  syn3 <- function(p1,p2,p3) {
    
    getwd()
    setwd("C:/Users/Dell/Desktop")
    
    getwd()
    df <- read.csv("yelp.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
                   strip.white = TRUE)
    dfd<-as.character(df[,2])
    df2<-as.character(df[,1])
    Sys.setenv(WNHOME="C:/Program Files (x86)/WordNet/2.1")
    initDict()
    getwd()
    setwd("C:/Program Files (x86)/WordNet/2.1/dict")
    getDict()
    getDictInstance()
    if(initDict()) 
    {
      filter1 <- getTermFilter("ExactMatchFilter",p1,TRUE)
      filter2 <- getTermFilter("ExactMatchFilter",p2,TRUE)
      filter3 <- getTermFilter("ExactMatchFilter",p3,TRUE)
      terms1 <- getIndexTerms("NOUN", 5, filter1)
      terms2 <- getIndexTerms("NOUN", 5, filter2)
      terms3 <- getIndexTerms("NOUN", 5, filter3)
      x1=getSynonyms(terms1[[1]])
      x2=getSynonyms(terms2[[1]])
      x3=getSynonyms(terms3[[1]])
    }
    x<-c(x1,x2,x3)
    words <- readLines(system.file("stopwords", "english.dat",
                                   package = "tm"))
    s<-remove_stopwords(dfd, words, lines = TRUE)
    n<-removeNumbers(s)
    t<-removePunctuation(n, preserve_intra_word_dashes = FALSE)
    a<-1
    a
    count=1
    for(i in 1:(length(x)))
    { 
      for(j in 1:(length(t)))
      {
        m=0
        e<-tokenize(t[j])
        for(k in 1:(length(e)))
        {
          if((e[k]==x[i]))
          {
            if(m==0)
            {
              
              m=j
              a[count]=df2[j]
              p1[count]=m
              count=count+1
              
            }
          }
        }
      }
    }
   
    p1<-unique(p1)
    for(l in 1:(length(p1)))   
    {
      segdoc<-df[c(p1),]
    }
    segdoc
    new1<-rbind(df,segdoc)
    setwd("C:/Users/Dell/Desktop")
    getwd()
    write.csv(new1[,], file = "segredoc1.CSV",row.names=FALSE)
    
    df1 <- read.csv("segredoc1.CSV",header = TRUE,quote="\"",stringsAsFactors= TRUE,
                    strip.white = TRUE)
    matrix= create_matrix(df1[,2], language="english", 
                          removeStopwords=TRUE, removeNumbers=TRUE, 
                          stemWords=TRUE)
    mat = as.matrix(matrix)
    container = create_container(matrix, as.numeric(as.factor(df1[,3])),
                                 trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models = train_models(container, algorithms=c("SVM","TREE"))
    results = classify_models(container, models)
    print(paste("Accuracy WithOut Weighting:"))
    tr<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results[,"TREE_LABEL"])
    tr<-(tr*100)
    print(paste(tr))
    print(paste("DT With Weighting:"))
    matrix1= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "bpn"))
    container1 = create_container(matrix1, as.numeric(as.factor(df1[,3])),
                                  trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models1 = train_models(container1, algorithms=c("SVM","TREE"))
    results1 = classify_models(container1, models1)
    tr1<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results1[,"TREE_LABEL"])
    tr1<-(tr1*100)
    print(paste(tr1))
    matrix2= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "btn"))
    
    container2 = create_container(matrix2, as.numeric(as.factor(df1[,3])),
                                  trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models2 = train_models(container2, algorithms=c("SVM","TREE"))
    results2 = classify_models(container2, models2)
    tr2<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results2[,"TREE_LABEL"])
    tr2<-(tr2*100)
    print(paste(tr2))
    
    matrix3= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "apn"))
    container3 = create_container(matrix3, as.numeric(as.factor(df1[,3])),
                                  trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models3 = train_models(container3, algorithms=c("SVM","TREE"))
    results3 = classify_models(container3, models3)
    tr3<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results3[,"TREE_LABEL"])
    tr3<-(tr3*100)
    print(paste(tr3))
    
    matrix4= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "atn"))
    container4= create_container(matrix4, as.numeric(as.factor(df1[,3])),
                                 trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models4 = train_models(container4, algorithms=c("SVM","TREE"))
    results4 = classify_models(container4, models4)
    tr4<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results4[,"TREE_LABEL"])
    tr4<-(tr4*100)
    print(paste(tr4))
    
    matrix5= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "lpn"))
   
    container5= create_container(matrix5, as.numeric(as.factor(df1[,3])),
                                 trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    container5
    models5 = train_models(container5, algorithms=c("SVM","TREE"))
    models5
    results5 = classify_models(container5, models5)
    results5
    tr5<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results5[,"TREE_LABEL"])
    tr5<-(tr5*100)
    print(paste(tr5))
    
    matrix6= create_matrix(df1[,2], language="english", 
                           removeStopwords=TRUE, removeNumbers=TRUE, 
                           stemWords=TRUE ,weighting = function(x)
                             weightSMART(x, spec = "ltn"))
    
    container6= create_container(matrix6, as.numeric(as.factor(df1[,3])),
                                 trainSize=1:1000, testSize=(1001:(1000+(length(p1)))),virgin=FALSE)
    models6 = train_models(container6, algorithms=c("SVM","TREE"))
    results6 = classify_models(container6, models6)
    tr6<-recall_accuracy(as.numeric(as.factor(df1[1001:(1000+(length(p1))), 3])), results6[,"TREE_LABEL"])
    tr6<-(tr6*100)
    tr6
   
  }
  
  output$txtout<-renderPrint({
    
    data1<-eventReactive(input$buttonx,{synx(input$search1,input$search2,input$search3)})
    print(data1())
    
  }
  )
  
  output$txtout1<-renderPrint({
    
    data1<-eventReactive(input$button1,{syn1(input$search1,input$search2,input$search3)})
    print(data1())
  
  }
  )
  
  output$txtout2<-renderPrint({
  
    data2<-eventReactive(input$button2,{syn2(input$search1,input$search2,input$search3)})
    print(data2())
   
  }
  )
  
  output$txtout3<-renderPrint({
    
    data3<-eventReactive(input$button3,{syn3(input$search1,input$search2,input$search3)})
    print(data3())
    
  }
  )
  
  
}
shinyApp(shinyUI, shinyServer)


