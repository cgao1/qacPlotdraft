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
  # find function that skips test
  # with Q, histogram or density

  x_sub<-substitute(x)
  x_quote<-as.character(substitute(x))
  y_sub<-substitute(y)
  y_quote<-as.character(substitute(y))
  group_sub<-substitute(group)
  group_quote<-as.character(substitute(group))

  inputs<-c(x_sub,y_sub,group_sub)
  check_duplicate<-duplicated(inputs)
  if(any(check_duplicate==TRUE)){
    return(cat("There are duplicated variables provided. Please check your inputted variables"))
  }


  if(!is.data.frame(data)){
    return(cat("This is not a proper dataframe"))
  }


  classQ<-c("numeric", "integer")
  classC<-c("character","logical","factor")
  colplot<-colnames(data)


  if(!any(colplot==x_sub)){
    return(cat(x_quote, "is not a variable in the dataframe"))
  }

  xclass<-class(data[[x_sub]])

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



  if(!is.null(group_sub)){
    if(!any(colplot==group_sub)){
      return(cat(group_quote, "is not a variable in the dataframe"))
    }
    groupclass<-class(data[[group_sub]])
    if(any(classQ==groupclass)){
      groupclass<-"Q"
      return(cat("This function currently can not handle quantitative grouping variables. Please try with a categorical grouping variable!"))
    }
    else if(any(classC==groupclass)){
      groupclass<-"C"
    }
  }


  # only X
  if(is.null(y_sub) & is.null(group_sub)){

    if(xclass=="C"){
      p<-ggplot(data=data,
             aes(x=data[[x_sub]])) +
        geom_bar(fill = "steelblue",
                 color = "black")  +
        labs(x = x_quote,
             y = "Frequency",
             title = paste("Frequency by",x_quote)) +
        theme_minimal()
    }
    if(xclass=="Q"){
      p<-ggplot(data,
             aes(x=data[[x_sub]])) +
        geom_density(fill = "steelblue",
                     color = "black")  +
        labs(x=x_quote,
          y = "Frequency",
          title = paste("Frequency by", x_quote)) +
        theme_minimal()
    }

  }

  # X and Y
  if(!is.null(y_sub) & is.null(group_sub)){
    if(xclass=="C" & yclass=="C"){
      p<-ggplot(data,
             aes(x=data[[x_sub]],
               fill = data[[y_sub]])) +
        geom_bar(position = "dodge")  +
        labs(x = x_quote,
             fill = y_quote,
             title = paste(x_quote, "by", y_quote),
             subtitle= "Bar Chart") +
        theme_minimal()
    }
    if(xclass=="C" & yclass=="Q"){
      p<-ggplot(data,
            aes(x = data[[y_sub]],
                fill = data[[x_sub]])) +
        geom_density(alpha = 0.4) +
        labs(title= paste(y_quote, "by" ,x_quote),
             subtitle = "Kernel Density Plot",
             x=y_quote,
             y="Density",
             fill= x_quote)+
        theme_minimal()
    }

    if(xclass=="Q" & yclass=="Q"){
      p<-ggplot(data=data,aes(x=data[[x_sub]], y=data[[y_sub]]))+
        geom_point()+
        labs(x=x_quote,
             y=y_quote,
             title=paste(x_quote, "by", y_quote),
             subtitle="Scatterplot with Line of Best Fit")+
        geom_smooth(method="lm")+
        theme_minimal()
    }
    if(xclass=="Q" & yclass=="C"){
      if(nlevels(data[[y_sub]])==2){

        data2 <- data.frame(
          myx <- data[[x_quote]],
          myy <- data[[y_quote]]
        )
        data_result<-glm(myy~myx,
                         family="binomial",
                         data=data2)
        p <-visreg(data_result, "myx",
                   gg = TRUE,
                   scale="response") +
          labs(y=paste("Prob(",y_quote,")"),
               x=x_quote,
               title=paste("Relationship of",x_quote,"and",y_quote),
               subtitle="Logistic Regression")+
          theme_minimal()

      }
      else if(nlevels(data[[y_sub]])>2){
        return(cat("Our function currently can not handle categorical variables with more than 2 levels. \nFor more information on plots made for categorical variables with more than two level, please try different models"))
      }
    }
  }


  # X Y G
  if(!is.null(y_sub) & !is.null(group_sub)){
    if(xclass=="C" & yclass=="C" & groupclass=="C"){
      p<-ggplot(data=data,
             aes(x=data[[x_sub]],
               fill = data[[y_sub]])) +
        geom_bar(position = "dodge")  +
        facet_wrap(~data[[group_sub]], ncol = 1)+
        labs(x = x_quote,
             title = paste(x_quote,"on",y_quote,"by", group_quote),
             subtitle = "Grouped Bar Charts",
             fill=y_quote)
    }
    if(xclass=="C" & yclass=="Q" & groupclass=="C"){
      if(nlevels(data[[x_sub]])<=3){
      p<-ggplot(data=data,
             aes(x = data[[y_sub]],
                 fill = data[[x_sub]])) +
        geom_density(alpha = 0.4) +
        facet_wrap(~data[[group_sub]]) +
        labs(x=y_quote,
             y="Density",
             title = paste(x_quote,"with",y_quote,"by",group_quote),
             subtitle = "Grouped Kernel Density Plot",
             fill=x_quote)
      }
      else if(nlevels(data[[x_sub]]>3)){
        return(cat("We see that your "))
      }
    }

    if(xclass=="Q" & yclass=="C" & groupclass=="C"){
      if(nlevels(data[[y_sub]])==2){

        mydata <- data.frame(
          myx <- data[[x_quote]],
          myy <- data[[y_quote]],
          mygroup <- data[[group_quote]]
        )
        myplot <-list()
        count <- 1
        for(l in levels(mydata$mygroup)){
          d <- mydata[mydata$mygroup == l,]
          colnames(d) <- c("myx", "myy", "mygroup")
          data_result <- glm(myy~myx,
                             family="binomial",
                             data=d)
          p<-visreg::visreg(data_result, "myx",
                            gg=TRUE,
                            scale="response")+
            labs(x=x_quote,
                 y=y_quote,
                 title=paste(x_quote,"on",y_quote,"by",group_quote,"Plot",count),
                 subtitle="Grouped Logistic Regression")
          myplot[[count]] <- p
          count <- count+1
        }
        return(for(p in myplot){
          plot(p)
        })

      }
      else if(nlevels(data[[y_sub]])>2){
        return(cat("Our function currently can not handle categorical variables with more than 2 levels. \nFor more information on plots made for categorical variables with more than two level, please try different models"))
      }
    }
    if(xclass=="Q" & yclass=="Q" & groupclass=="C"){
      p<-ggplot(data=data,aes(x=data[[x_sub]], y=data[[y_sub]]))+
        geom_point()+
        geom_smooth(method="lm")+
        facet_wrap(. ~ data[[group_sub]])+
        labs(x=x_quote,
             y=y_quote,
             title=paste(x_quote,"on", y_quote,"by",group_quote),
             subtitle="Grouped Scatterplot with Line of Best Fit")
    }

  }
p
}







