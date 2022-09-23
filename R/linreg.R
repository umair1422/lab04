#' Lab4: Computations using ordinary least squares, implement methods in RC class
#' @importFrom ggplot2 %+replace%
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

                        },

                        setRegCofficient = function(X, y)
                        {"Sets the value for RegressionCoeficientMatrix field"
                          return (RegressionCoeficientMatrix <<- solve(t(X) %*% X) %*% t(X) %*% y)
                        },


                        #' @description
                        #' Sets the value for FittedValues
                        #' @param RegCofMatrix independent variables matrix
                        #' @param X independent variables matrix
                        #' @return the value that has been set
                        setFittedValues = function(RegCofMatrix, X)
                        {"Sets the value for FittedValues"
                          return (FittedValues <<- X %*% RegCofMatrix)
                        },

                        setResidual = function(y)
                        {"Sets the value for Residuals"
                          return (Residuals <<- y - FittedValues)
                        },

                        setDegreesOfFreedom = function(RegCofMatrix, X)
                        {"Sets the value for DegreesOfFreedom"
                          return (DegreesOfFreedom <<- dim(X)[1] - dim(RegCofMatrix)[1])
                        },

                        setResidualVariance = function()
                        {"Sets the value for ResidualVariance"
                          return (ResidualVariance <<- (t(Residuals) %*% Residuals) / DegreesOfFreedom)
                        },

                        setVarianceOfTheRegressionCoefficients = function(X)
                        {"Sets the value for VarianceOfTheRegressionCoefficients"
                          ans= ResidualVariance[1,1] * (solve(t(X) %*% X))
                          return (VarianceOfTheRegressionCoefficients <<- diag(ans))
                        },

                        setTValues = function()
                        {"Sets the value for TValues"
                          return (TValues <<- as.vector(RegressionCoeficientMatrix)/sqrt(VarianceOfTheRegressionCoefficients))
                        },


                        setPValues = function()
                        {"Sets the value for TValues"
                          return (PValues <<- pt(as.vector(RegressionCoeficientMatrix),df=DegreesOfFreedom))
                        },


                        ############################################# <1.3 Implementing methods for your class> ##########################################

                        print = function()
                        {"Prints out the coefficient and coefficent names"

                          cat("Call:\n","linreg(formula = ", format(FunctionFormula), ", data = ", DataName ,")\n\n", sep = "")
                          cat("Coefficients:\n",dimnames(RegressionCoeficientMatrix)[[1]], "\n",RegressionCoeficientMatrix)

                           },

                        plot = function()
                        {"Plots 2 graphs on a grid that are mention in the lab manual"
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

                        resid = function()
                        {"return only residuals as vec form"
                          return(as.vector(Residuals))
                        },

                        pred = function()
                        {"return the preticted values (y-hat)"
                          return(FittedValues)
                        },

                        coef = function()
                        {
                          c= as.vector(RegressionCoeficientMatrix)
                          names(c) <- rownames(RegressionCoeficientMatrix)
                          return(c)
                        },

                        summary = function(){"Print the simmary along with the pvalue, tvalue, sigma and degree of freedom"

                          summaryDetails <- matrix(round(c(as.vector(RegressionCoeficientMatrix), as.vector(sqrt(VarianceOfTheRegressionCoefficients)), as.vector(TValues), as.vector(PValues)),4), ncol = 4)
                          PThreshold <- ifelse(PValues<0.001, "***",no = ifelse(PValues<0.01, "**", ifelse(PValues<0.05, "*", ifelse(PValues<0.1, ".", " "))))
                          summaryDetails <- cbind(summaryDetails, PThreshold)

                          colnames(summaryDetails) <- c("Coefficients", "Std Error" ,"T-values", "P-Values", " ")
                          rownames(summaryDetails) <- dimnames(RegressionCoeficientMatrix)[[1]]
                          cat("Call:\n")
                          cat("linreg(formula = ", format(FunctionFormula), ", data = ", DataName ,")\n\n", sep = "")
                          write.table((summaryDetails), quote = FALSE)
                          cat("\n Residual standard error:", sqrt(ResidualVariance),"on", DegreesOfFreedom,"degrees of freedom")
                          cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
                        }) #methods

)#class

# newobj= linreg$new(frm, iris)
# newobj$summary()
