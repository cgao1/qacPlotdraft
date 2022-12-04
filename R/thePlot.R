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
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' data(mtcars)
#' thePlot(mtcars, hp)
#' }
thePlot<- function(data,x,y=NULL,group=NULL){
  # needs to be dataframe
  # needs at least x
  # all three variables need to be in the dataset
  # find function that skips test
  # what to do with complex variables, is there types of classes and what should di with them
  # with Q, hitogram or density
  x_sub<-substitute(x)
  x_quote<-as.character(x)
  y_sub<-substitute(y)
  y_quote<-as.character(y)
  group_sub<-substitute(group)
  group_quote<-as.character(group)
  if(!is.data.frame(data)){
    return(cat("This is not a proper dataframe"))
  }

  # character, numeric, integer, complex, and logical (factor??)
  classQ<-c("numeric", "integer") # double check with user
  classC<-c("character","logical")
  colplot<-colnames(data)


  if(!any(colplot==x_quote)){
    return(cat(x_quote, "is not a variable in the dataframe"))
  }

  xclass<-class(x)

  if(any(classQ==xclass)){
    xclass<-"Q"
  }
  else if(any(classC==xclass)){
    xclass<-"C"
  }




  if(!is.null(y)){
    if(!any(colplot==y)){
      return(cat(y_quote, "is not a variable in the dataframe"))
    }
    yclass<-class(y)
    if(any(classQ==yclass)){
      yclass<-"Q"
    }
    else if(any(classC==yclass)){
      yclass=="C"
    }
  }



  if(!is.null(group)){
    if(!any(colplot==group)){
      return(cat(group_quote, "is not a variable in the dataframe"))
    }
    groupclass<-class(group)
    if(any(classQ==groupclass)){
      groupclass=="Q"
    }
    else if(any(classC==groupclass)){
      groupclass=="C"
    }
  }


  # only X
  if(is.null(y) & is.null(group)){
    if(xclass=="C"){
      ggplot(data=data,
             aes(x=x)) +
        geom_bar(fill = "steelblue",
                 color = "black")  +
        labs(x = x_quote,
             y = "Frequency",
             title = paste("Frequency by",x_quote)) +
        theme_minimal()
    }
    if(xclass=="Q"){
      ggplot(data,
             aes(x=x)) +
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
      ggplot(data,
             aes(x=x,
               fill = y)) +
        geom_bar(position = "dodge")  +
        labs(#change [fill in]
          x = x_quote,
          y = y_quote,
          title = paste("",
                        "")) +
        theme_minimal()
    }
    if(xclass=="C" & yclass=="Q"){
      ggplot(data,
            aes(x = x,
                fill = y)) +
        geom_density(alpha = 0.4) +
        labs(#change fill in
          title = "")
    }

    if(xclass=="Q" & yclass=="Q"){
      ggplot(data=data)+
        geom_point(aes(x=x, y=y))+
        theme_minimal()
    }
  }


  # X Y G
  if(!is.null(y) & !is.null(group)){
    if(xclass=="C" & yclass=="C" & groupclass=="C"){
      ggplot(data=data,
             aes(x=x,
               fill = y)) +
        geom_bar(position = "dodge")  +
        facet_wrap(~group)
      labs(#change [fill in]
        x = x_quote,
        y = y_quote,
        title = paste("",
                      "")) +
        theme_minimal()
    }
    if(xclass=="Q" & yclass=="Q" & groupclass=="C"){
      ggplot(data,
             aes(x = x,
                 fill = y)) +
        geom_density(alpha = 0.4) +
        facet_wrap(~group)
      labs(#change fill in
        title = "")+
        theme_minimal()
    }
    if(xclass=="Q" & yclass=="Q" & groupclass=="C"){
      ggplot(data=data)+
        geom_point(aes(x=x, y=y))+
        facet_grid(. ~ group)+
        theme_minimal()
    }
  }








}







