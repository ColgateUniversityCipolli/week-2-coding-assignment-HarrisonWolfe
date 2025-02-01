riemann.sums <- function(fnct,                        # function to integrate
                         a,                           # lower bound of integral
                         b,                           # upper bound of integral
                         n.rect,                      # number of  bound of integral
                         method = "Trapezoidial"){    # method to use (trap by default)
  ######################################
  # Check Input
  ######################################
  if(!is.numeric(a)){ # if a is not numeric
    stop("The lower bound of the integral (a) must be numeric.")
  }
  if(!is.numeric(b)){ # if b is not numeric
    stop("The lower bound of the integral (a) must be numeric.")
  }
  if(!(is.numeric(n.rect)) | (n.rect%%1!=0)){ # if n.rect is not a whole number
    stop("The number of rectangles must be a positive whole number.")
  }
  ######################################
  # Compute Area
  ######################################
  if(method == "Left"){
    # Add necessary code here
    delta.x = (b-a)/n.rect
    left.points <- a + (0:(n.rect-1))*(delta.x)
    (left.area <- sum(delta.x*(fnct(left.points))))
  }else if(method == "Right"){
    # Add necessary code here
    delta.x = (b-a)/n.rect
    right.points <- a + (1:(n.rect))*(delta.x)
    (right.area <- sum(delta.x*(fnct(right.points))))
  }else if(method == "Midpoint"){
    # Add necessary code here
    delta.x = (b-a)/n.rect
    left.points <- a + (0:(n.rect-1))*(delta.x)
    right.points <- a + (1:(n.rect))*(delta.x)
    mid.points <- (left.points+right.points)/2
    (mid.area <- sum(delta.x*(fnct(mid.points))))
  }else if(method == "Trapezoidial"){
    # Add necessary code here
    delta.x = (b-a)/n.rect
    point.for.trap = a + (1:(n.rect-1)*(delta.x)
    0.5*delta.x*sum(fnct(a) + fnct(b) + sum(2*fnct(a+point.for.trap)))
  }else{
    stop("Please select a valid method (e.g., 'Left', 'Right', 'Midpoint', 'Trapezoidial')")
  }
  ######################################
  # Return the area
  ######################################
  return(area)
}
######################################
# Test the function
######################################
riemann.sums(fnct = integrand,
             a = 0,
             b = 2,
             n.rect = 100)
######################################
# Compare to numerical integral
######################################
integrate(f = integrand, # integrate() is an R function
          lower = 0,     # that completes numerical
          upper = 2)     # integration