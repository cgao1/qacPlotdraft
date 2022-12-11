#' thePlot
#'
#' @description
#'  This \code{thePlot} takes nearly all variables and returns you a plot
#'
#' @param data The dataset from where the variables will be taken from
#' @param x The first variable
#' @param y The second variable
#' @param group The variable to group the plot with
#'
#' @return A plot
#' @export
#' @import ggplot2
#' @import stats
#' @import visreg
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' data(mtcars)
#' thePlot(mtcars, hp)
#' }
thePlot<- function(data,x,y=NULL,group=NULL){
  # all three variables need to be in the dataset
  # find function that skips test
  # with Q, histogram or density
  #make sure they do not put the same variable
  x_sub<-substitute(x)
  x_quote<-as.character(x)
  y_sub<-substitute(y)
  y_quote<-as.character(y)
  group_sub<-substitute(group)
  group_quote<-as.character(group)
  if(!is.data.frame(data)){
    return(cat("This is not a proper dataframe"))
  }


  classQ<-c("numeric", "integer")
  classC<-c("character","logical","factor")
  colplot<-colnames(data)


  if(!any(colplot==x_sub)){
    return(cat(x_quote, "is not a variable in the dataframe"))
  }

  xclass<-class(data[[x]])

  if(any(classQ==xclass)){
    xclass<-"Q"
  }
  else if(any(classC==xclass)){
    xclass<-"C"
  }




  if(!is.null(y_sub)){
    if(!any(colplot==y_sub)){
      return(cat(y_quote, "is not a variable in the dataframe"))
    }
    yclass<-class(data[[y_sub]])
    if(any(classQ==yclass)){
      yclass<-"Q"
    }
    else if(any(classC==yclass)){
      yclass<-"C"
    }
  }



  if(!is.null(group)){
    if(!any(colplot==group_sub)){
      return(cat(group_quote, "is not a variable in the dataframe"))
    }
    groupclass<-class(data[[group]])
    if(any(classQ==groupclass)){
      groupclass<-"Q"
    }
    else if(any(classC==groupclass)){
      groupclass<-"C"
    }
  }


  # only X
  if(is.null(y) & is.null(group)){

    if(xclass=="C"){
      p<-ggplot(data=data,
             aes(x=data[[x]])) +
        geom_bar(fill = "steelblue",
                 color = "black")  +
        labs(x = x_quote,
             y = "Frequency",
             title = paste("Frequency by",x_quote)) +
        theme_minimal()
    }
    if(xclass=="Q"){
      p<-ggplot(data,
             aes(x=data[[x]])) +
        geom_density(fill = "steelblue",
                     color = "black")  +
        labs(x=x_quote,
          y = "Frequency",
          title = paste("Frequency by", x_quote)) +
        theme_minimal()
    }

  }

  # X and Y
  if(!is.null(y) & is.null(group)){
    if(xclass=="C" & yclass=="C"){
      p<-ggplot(data,
             aes(x=data[[x]],
               fill = data[[y_sub]])) +
        geom_bar(position = "dodge")  +
        labs(#change [fill in]
          x = x_quote,
          y = y_quote,
          fill = y_quote,
          title = paste("Bar Chart of", x_quote,
                        ""),
          subtitle= "Grouped Bar Chart") +
        theme_minimal()
    }
    if(xclass=="C" & yclass=="Q"){
      p<-ggplot(data,
            aes(x = data[[y_sub]],
                fill = data[[x]])) +
        geom_density(alpha = 0.4) +
        labs(#change fill in
          title= paste(y_quote, "by" ,x_quote),
          subtitle = "Grouped Kernel Density Plot",
          x=y_quote,
          y="Density",
          fill= x_quote)
    }

    if(xclass=="Q" & yclass=="Q"){
      p<-ggplot(data=data,aes(x=data[[x]], y=data[[y_sub]]))+
        geom_point()+
        labs(x=x_quote,
             y=y_quote,
             title=paste(x_quote, "and", y_quote,"Scatterplot"),
             subtitle="Scatterplot with Line of Best Fit")+
        geom_smooth(method="lm")+
        theme_minimal()
    }
    if(xclass=="Q" & yclass=="C"){
      data_result<-glm(data[[y_sub]]~data[[x]],
                       family="binomial",
                       data=data)
      p<-visreg::visreg(data_result, "data[[x]]",
                gg=TRUE,
                scale="response")+
        labs(y=paste("Prob(",y_quote,")"),
             x=x_quote,
             title=paste("Relationship of",x_quote,"and",y_quote),
             subtitle="Logistic Regression")
    }
  }


  # X Y G
  if(!is.null(y) & !is.null(group)){
    if(xclass=="C" & yclass=="C" & groupclass=="C"){
      p<-ggplot(data=data,
             aes(x=data[[x]],
               fill = data[[y]])) +
        geom_bar(position = "dodge")  +
        facet_wrap(~data[[group]])+
        labs(#change [fill in]
          x = x_quote,
          title = paste(x_quote,"on",y_quote,"by", group_quote),
          subtitle = "Grouped Bar Charts with a Grouping Variable",
          fill=y_quote) +
        theme_minimal()
    }
    if(xclass=="C" & yclass=="Q" & groupclass=="C"){
      p<-ggplot(data=data,
             aes(x = data[[y]],
                 fill = data[[x]])) +
        geom_density(alpha = 0.4) +
        facet_wrap(~data[[group]]) +
        labs(#change fill in
          x=y_quote,
          y="Density",
          title = paste(x_quote,"with",y_quote,"by",group_quote),
          subtitle = "Grouped Kernel Density Plot with a Grouping Variable",
          fill=x_quote)+
        theme_minimal()
    }
    if(xclass=="Q" & yclass=="C" & groupclass=="C"){
      data_result<-glm(data[[y_sub]]~data[[x]],
                       family="binomial",
                       data=data)
      p<-visreg::visreg(data_result, "data[[x]]",
                gg=TRUE,
                scale="response")+
        labs(y=paste("Prob(",y_quote,")"),
             x=x_quote,
             title=paste("Relationship of",x_quote,"and",y_quote),
             subtitle="Logistic Regression")+
        facet_grid(. ~ data[[group]])
    }
    if(xclass=="Q" & yclass=="Q" & groupclass=="C"){
      p<-ggplot(data=data,aes(x=data[[x]], y=data[[y]]))+
        geom_point()+
        geom_smooth(method="lm")+
        facet_grid(. ~ data[[group]])+
        labs(x=x_quote,
             y=y_quote,
             title="Scatterplot",
             subtitle="Scatterplot with Line of Best Fit by a Grouping Variable")
        theme_minimal()
    }

  }
p
}







