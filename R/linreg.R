#' Lab4: Computations using ordinary least squares, implement methods in RC class
#'
#' @param FunctionFormula formula object passed by user
#' @param data dataframe passed by the user
#' @field VarianceOfTheRegressionCoefficients matrix.
#' @field RegressionCoeficientMatrix matrix.
#' @field TValues t-values in vec form
#' @field Residuals matrix.
#' @field ResidualVariance matrix.
#' @field DegreesOfFreedom numeric.
#' @field DataName Name of the dataframe passed.
#' @field FittedValues matrix.
#' @field PValues t-values in vec form
#' @examples
#' newobj= linreg$new(frm, iris)
#' newobj$summary()
#'
#' @return class
#' @import methods
#' @export
linreg <- setRefClass("linreg",

                      fields = list(
                        FunctionFormula = "formula",
                        RegressionCoeficientMatrix = "matrix",
                        FittedValues = "matrix",
                        Residuals = "matrix",
                        DegreesOfFreedom = "numeric",
                        ResidualVariance = "matrix",
                        VarianceOfTheRegressionCoefficients ="vector",
                        TValues = "vector",
                        DataName = "character",
                        PValues = "vector"
                      ),


                      methods = list(

                        #' Constructor
                        #' @description
                        #' Constructor to initialize the data fields of this class
                        #' @param formula formula object passed by the user
                        #' @param data dataframe passed by the user
                        #' @return creates an object based on the arguments passed
                        initialize = function(formula, data)
                        {
                          FunctionFormula <<- formula
                          DataName <<- deparse(substitute(data)) #get the name of dataframe
                          X <- model.matrix(FunctionFormula, data)
                          y <- data[all.vars(FunctionFormula)[1]]
                          y <- unname(data.matrix(y))
                          setRegCofficient(X,y)
                          setFittedValues(RegressionCoeficientMatrix, X)
                          setResidual(y)
                          setDegreesOfFreedom(RegressionCoeficientMatrix, X)
                          setResidualVariance()
                          setVarianceOfTheRegressionCoefficients(X)
                          setTValues()
                          setPValues()

                        }, #constructor

                        #' Setter for regression coefficient
                        #' @description
                        #' Sets the value for RegressionCoeficientMatrix field
                        #' @param X independent variables matrix
                        #' @param y dependent variables matrix
                        #' @return the value that has been set
                        setRegCofficient = function(X, y)
                        {
                          return (RegressionCoeficientMatrix <<- solve(t(X) %*% X) %*% t(X) %*% y)
                        },


                        #' Setter for Fitted Values
                        #' @description
                        #' Sets the value for FittedValues
                        #' @param RegCofMatrix independent variables matrix
                        #' @param X independent variables matrix
                        #' @return the value that has been set
                        setFittedValues = function(RegCofMatrix, X)
                        {
                          return (FittedValues <<- X %*% RegCofMatrix)
                        },


                        #' Setter for Fitted Values
                        #' @description
                        #' Sets the value for Residuals
                        #' @param y dependent variables matrix
                        #' @return the value that has been set
                        setResidual = function(y)
                        {
                          return (Residuals <<- y - FittedValues)
                        },

                        #' Setter for degree of freedom
                        #' @description
                        #' Sets the value for DegreesOfFreedom
                        #' @param RegCofMatrix independent variables matrix
                        #' @param X independent variables matrix
                        #' @return the value that has been set
                        setDegreesOfFreedom = function(RegCofMatrix, X)
                        {
                          return (DegreesOfFreedom <<- dim(X)[1] - dim(RegCofMatrix)[1])
                        },

                        #' Setter for degree of residual variance
                        #' @description
                        #' Sets the value for ResidualVariance
                        #' @return the value that has been set
                        setResidualVariance = function()
                        {
                          return (ResidualVariance <<- (t(Residuals) %*% Residuals) / DegreesOfFreedom)
                        },

                        #' Setter for degree of variance of reg coefficients
                        #' @description
                        #' Sets the value for VarianceOfTheRegressionCoefficients
                        #' @param X independent variables matrix
                        #' @return the value that has been set
                        setVarianceOfTheRegressionCoefficients = function(X)
                        {
                          ans= ResidualVariance[1,1] * (solve(t(X) %*% X))
                          return (VarianceOfTheRegressionCoefficients <<- diag(ans))
                        },

                        #' Setter T Values
                        #' @description
                        #' Sets the value for TValues
                        #' @return the value that has been set
                        setTValues = function()
                        {
                          return (TValues <<- as.vector(RegressionCoeficientMatrix)/sqrt(VarianceOfTheRegressionCoefficients))
                        },

                        #' Setter P Values
                        #' @description
                        #' Sets the value for TValues
                        #' @return the value that has been set
                        setPValues = function()
                        {
                          return (PValues <<- pt(as.vector(RegressionCoeficientMatrix),df=DegreesOfFreedom))
                        },


                        ############################################# <1.3 Implementing methods for your class> ##########################################

                        #' Print like lm
                        #' @description
                        #' Prints out the coefficient and coefficent names
                        #' @return nothing
                        print = function()
                        {
                          cat("Call:\n")
                          cat("linreg(formula = ", format(FunctionFormula), ", data = ", DataName ,")\n\n", sep = "")
                          cat("Coefficients:\n")
                          cat(dimnames(RegressionCoeficientMatrix)[[1]], "\n")
                          cat(RegressionCoeficientMatrix)

                          #cat("Call:\n", "linreg(formula = ", format(FunctionFormula), ", data = ", DataName ,")\n\n","Coefficients:\n" )
                          #cat(dimnames(RegressionCoeficientMatrix)[[1]], "\n", RegressionCoeficientMatrix)
                        },


                        #' Plot using ggplot2
                        #' @description
                        #' Plots 2 graphs on a grid that are mention in the lab manual
                        #' @return nothing
                        plot = function()
                        {
                          VecFittedValues= unlist (FittedValues)
                          VecResiduals= unlist (Residuals)
                          DataFrame2PlotResidualFit <- data.frame(VecResiduals, VecFittedValues, c(1:length(FittedValues)))
                          names(DataFrame2PlotResidualFit) <- c("Residuals", "Fitted_values", "Number")

                          ResidualFit <-
                            ggplot2::ggplot(DataFrame2PlotResidualFit) +
                            ggplot2::aes(Fitted_values, Residuals) +
                            #to add a trend line:
                            ggplot2::geom_smooth(ggplot2::aes(Fitted_values, Residuals), formula = y~0+x, se = FALSE, span = 1, color = "#FF0000") +
                            #plot geom points
                            ggplot2::geom_point(size = 2,  shape = 1) +
                            # title of the graph, appears on top
                            ggplot2::ggtitle("\t\t\t\t Residuals vs Fitted") +
                            #this part appears at the bottom
                            ggplot2::xlab(paste("Fitted values", "\n lm(", format( FunctionFormula), ")", sep = ""))


                          StandardizedResiduals = unlist(sqrt(abs(Residuals)))
                          VecStandardizedResiduals = unlist(StandardizedResiduals)
                          DataFrame2PlotScaleLocation = data.frame(VecStandardizedResiduals, VecFittedValues ,c(1:length(FittedValues)))
                          names(DataFrame2PlotScaleLocation) <- c("StandResiduals", "Fitted_values", "Number")


                          ScaleLocation <-
                            ggplot2::ggplot(data = DataFrame2PlotScaleLocation) +
                            ggplot2::aes(Fitted_values,StandResiduals) +
                            ggplot2::geom_point(size = 2,  shape = 1) +
                            # title of the graph, appears on top
                            ggplot2::ggtitle("\t\t\t\t Scale - Location") +
                            ggplot2::geom_smooth(ggplot2::aes(Fitted_values, StandResiduals), formula = y~0+x, color = "#FF0000") +
                            ggplot2::ggtitle("\t\t\t\tScale-Location") +
                            ggplot2::xlab(paste("Fitted Values", "\n lm(", format( FunctionFormula), ")", sep = "")) +
                            ggplot2::ylab(expression(sqrt("Standardized residuals")))

                          gridExtra::grid.arrange(ResidualFit,ScaleLocation)

                        },


                        #' Residuals calculation
                        #' @description
                        #' return only residuals as vec form
                        #' @return Residuals
                        resid = function()
                        {
                          return(as.vector(Residuals))
                        },


                        #' predicted values
                        #' @description
                        #' return the preticted values (y-hat)
                        #' @return FittedValues
                        pred = function()
                        {
                          return(FittedValues)
                        },

                        #' coefficeints as named vec
                        #' @description
                        #' return the coefficeints as named vec
                        #' @return coefficeints as named vec
                        coef = function()
                        {
                          c= as.vector(RegressionCoeficientMatrix)
                          names(c) <- rownames(RegressionCoeficientMatrix)
                          return(c)
                        },

                        #' Print the Summary
                        #' @description
                        #' Print the simmary along with the pvalue, tvalue, sigma and degree of freedom
                        #' @return coefficeints as named vec
                        summary = function(){ "returns the summary of linear regression model"

                          summaryMatrix <- matrix(round(c(as.vector(RegressionCoeficientMatrix), as.vector(sqrt(VarianceOfTheRegressionCoefficients)), as.vector(TValues), as.vector(PValues)),4), ncol = 4)
                          Pval_thresh <- ifelse(PValues<0.001, "***",no = ifelse(PValues<0.01, "**", ifelse(PValues<0.05, "*", ifelse(PValues<0.1, ".", " "))))
                          summaryMatrix <- cbind(summaryMatrix, Pval_thresh)

                          colnames(summaryMatrix) <- c("    Coefficients", "Standard Error" ,"Tvalues", "PValues", " ")
                          rownames(summaryMatrix) <- dimnames(RegressionCoeficientMatrix)[[1]]
                          cat("Call:\n")
                          cat("linreg(formula = ", format(FunctionFormula), ", data = ", DataName ,")\n\n", sep = "")
                          write.table((summaryMatrix), quote = FALSE)
                          cat("\n Residual standard error:", sqrt(ResidualVariance),"on", DegreesOfFreedom,"degrees of freedom")
                          cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
                        }) #methods

)#class

# newobj= linreg$new(frm, iris)
# newobj$summary()
