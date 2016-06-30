#'Head loss in meters over the length of pipe
#'
#'\code{headLoss} calculate the head loss by fricition in pipes by different
#'methods
#'
#'@param D  Diameter in meters
#'@param Q  Flow rate in cubic meters per second
#'@param L  Length of pipe in meters
#'@param RC  Roughness coefficient. Absolute roughness (E in meters) for
#'  Darcy-Weisbach equation or C (dimensionless) for Hazen-Willians equation or
#'  b (dimensionless) for Flamant equation
#'@param Eq  To choose the equation that will be used. By default \code{Eq="CW"} for
#'  Colebrook-White. Other options are:  \code{Eq="SJ"} for Swamee-Jain; \code{Eq="HW"} for
#'  Hazen-Willians; \code{Eq="FL"} for Flamant
#'@param friend If friend=TRUE the results are presented in a friendly way
#'@param v  Kinematic viscosity of fluiyd in square meters per second. By
#'  default use the value for water at 20ºC \code{v=1.01e-6 m2/s}.  Unnecessary for empirical equations.
#'@param g  Gravitational acceleration \code{g=9.81} meters per square second.  Unnecessary for empirical equations.
#'@param x1  Initial parameter of f for Newthon-Raphson. By default \code{x1=0.0625}.  Unnecessary for empirical or explicity equations.
#'
#'@return Hf Head loss in meters
#'@export
#'
#'@examples
#'headLoss(D=0.025,Q=0.001,L=200,RC=0.0001,Eq="CW",friend=TRUE)
#'headLoss(D=0.025,Q=0.001,L=200,RC=0.0001,Eq="CW")
#'headLoss(D=0.025,Q=0.001,L=200,RC=0.0001,Eq="SJ")
#'headLoss(D=0.200,Q=0.05447,L=3200,RC=130,Eq="HW")
#'headLoss(D=0.025,Q=0.001,L=200,RC=0.000135,Eq="FL")
#'



headLoss <-
  function (D,Q,L,RC,Eq = "CW", friend = FALSE, v = 1.01e-6, g = 9.81, x1 = 0.0625)
  {
    A = ((pi * (D ^ 2)) / 4)
    #area
    V = Q / A
    #velocity
    Re = V * D / v
    #Reynolds number

    if (Re < 2000)
      #laminar regime
    {
      f = 64 / Re
      hf = f * (L / D) * ((V ^ 2) / (2 * g))
    }
    else
    {
      if (Eq == "CW")
        #Colebrook-White
      {
        x1 = (1 / x1) ^ 0.5
        x = 1
        x2 = x1 + 1
        w = (RC / (3.7 * D)) + ((2.51 * x1) / Re)
        h = (2.18 / (((RC * Re) / (3.7 * D)) + (2.51 * x1)))
        while (x >= 0.00001) {
          x2 = x1 - (((x1 + (2 * log10(
            w
          ))) / (1 + h)))
          x = abs(x2 - x1)
          x1 = x2
        }
        f = 1 / x1 ^ 2
        hf = f * (L / D) * ((V ^ 2) / (2 * g))
      }
      else if (Eq == "HW")
        #Hazen-Willians
      {
        hf = (10.643 * (Q ^ 1.852) * L) / ((RC ^ 1.852) * (D ^ 4.871))
      }
      else if (Eq == "SJ")
        #Swamee-Jain
      {
        hf = (0.203 * Q ^ 2 * L / (g * D ^ 5)) / (log10(RC / (3.7 * D) +
                                                          5.74 / Re ^ 0.9)) ^ 2
      }
      else if (Eq == "FL")
        #Flamant
      {
        hf = 6.107 * RC * ((L * (Q ^ 1.75)) / (D ^ 4.75))
      }
    }

    #show the results in friendly way
    if (friend == TRUE)
    {
      if (Re < 2000) {
        valor = paste(
          "Head loss = ", hf, "meters. Calculated by the Darcy-Weisbach equation for laminar flow"
        )
      }
      else
      {
        if (Eq == "CW")
        {
          valor = paste("Head loss = ", hf, "meters. Calculated by the Colebrook-White")
        }
        else if (Eq == "HW")
        {
          valor = paste("Head loss = ", hf, "meters. Calculated by the Hazen-Willians")
        }
        else if (Eq == "SJ")
        {
          valor = paste("Head loss = ", hf, "meters. Calculated by the Swamee-Jain")
        }
        else if (Eq == "FL")
        {
          valor = paste("Head loss = ", hf, "meters. Calculated by the Flamant")
        }
      }
      return(valor)
    }
    else
    {
      return(hf)
    }
  }
