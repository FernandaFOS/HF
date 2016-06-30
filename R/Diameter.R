#' Diameter of pipes
#'
#' \code{Diameter} calculates the diameter of pipes for a given head loss
#'
#' @param J Unitary head loss  in meters per meter or Total head loss in meters
#' @param L Length of pipe in meters. If \code{J} is unitary head loss this parameter
#' is unnecessary.
#' @param Q Flow rate in cubic meters per second
#' @param RC Roughness coefficient. Absolute roughness (E in meters) for
#'  Darcy-Weisbach equation or C (dimensionless) for Hazen-Willians equation or
#'  b (dimensionless) for Flamant equation
#' @param Eq To choose the equation that will be used. By default \code{Eq="CW"} for
#'  Colebrook-White. Other options are:  \code{Eq="SJ"} for Swamee-Jain; \code{Eq="HW"} for
#'  Hazen-Willians; \code{Eq="FL"} for Flamant
#' @param friend If friend=TRUE the results are presented in a friendly way
#' @param v Kinematic viscosity of fluiyd in square meters per second. By default use the
#' value for water at 20oC \code{v=1.01e-6 m2/s}.  Unnecessary for empirical equations.
#' @param g Gravitational acceleration g=9.81 meters per square second.  Unnecessary for
#' empirical equations.
#' @param x1 Initial parameter of f for Newthon-Raphson. By default \code{x1=0.0625}.
#' Unnecessary for empirical or explicity equations.
#'
#' @return D Diameter of pipe in meters
#' @export
#'
#' @examples
#' Diameter(J=0.197,Q=0.005,RC=1e-4,Eq="CW", friend=TRUE)
#' Diameter(J=0.197,Q=0.005,RC=1e-4,Eq="CW")
#' Diameter(J=0.197,Q=0.005,RC=1e-4,Eq="SJ", friend=TRUE)
#' Diameter(J=0.197,Q=0.005,RC=140, Eq="HW", friend=TRUE)
#' Diameter(J=0.197,Q=0.005,RC=140, Eq="HW")

Diameter <-
  function(J,  L = 1, Q,RC, Eq = "CW", friend = FALSE, v = 1.01e-6, g = 9.81,x1 = 0.0625)
  {
    J = J / L
    if (Eq == "CW")
    {
      x = x2 = x1 + 1
      p <- 2 * sqrt(12.1 * J) / Q
      q <- RC / 3.7
      r <- (2.51 * v) / sqrt(2 * g * J)
      while (x >= 0.00001) {
        x2 <-
          x1 - ((x1 ^ 5 + p * log10(q * x1 ^ 2 + r * x1 ^ 3))) / (5 * x1 ^ 4 + p *
                                                                    log10(exp(1) * (2 * q + 3 * r * x1 ^ 3) / (q * x1 + 2 * r * x1 ^ 2)))
        x = abs(x2 - x1)
        x1 = x2
      }
      D = 1 / (x1) ^ 2
    }
    else if (Eq == "HW") {
      D = (((10.643 * (Q ^ 1.852) * J) / ((RC ^ 1.852)))) ^ (1 / 4.871)
    }
    else if (Eq == "SJ") {
      D = 0.66 * (RC ^ 1.25 * (L * Q ^ 2 / (g * J * L)) ^ 4.75 + v * Q ^ 9.4 *
                    (Q / (g * J * L)) ^ 5.2) ^ 0.04
    }


    #laminar flow recalculate
    A = ((pi * (D ^ 2)) / 4)
    V = Q / A
    Re = V * D / v

    if (Re < 2000) {
      print("laminar")
      #colocar a equacao laminar para o diametro

    }
    else
      0

    #show the results in friendly way
    if (friend == TRUE)
    {
      if (Eq == "CW")
      {
        valor = paste("Diameter = ", D, "meters. Calculated by the Colebrook-White")
      }
      else if (Eq == "HW")
      {
        valor = paste("Diameter = ", D, "meters. Calculated by the Hazen-Willians")
      }
      else if (Eq == "SJ")
      {
        valor = paste("Diameter = ", D, "meters. Calculated by the Swamee-Jain")
      }
      else if (Eq == "FL")
      {
        valor = paste("Diameter = ", D, "meters. Calculated by the Flamant")
      }

      return(valor)
    }
    else
    {
      return(D)
    }
  }
