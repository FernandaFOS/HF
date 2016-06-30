#' Flow rate
#'
#' \code{flowRate} calculate the flow rate or flow velocity for a given head loss
#'
#' @param Hf Head loss  in meters per meter or Total head loss in meters
#' @param L Length of pipe in meters. If \code{J} is unitary head loss this parameter
#' is unnecessary.
#' @param D Diamter of pipe in meters
#' @param RC Roughness coefficient. Absolute roughness (E in meters) for
#'  Darcy-Weisbach equation or C (dimensionless) for Hazen-Willians equation or
#'  b (dimensionless) for Flamant equation
#' @param Vel Velocity. If \code{Vel=TRUE} then the return are the flow velocity.
#' @param Eq To choose the equation that will be used. By default \code{Eq="CW"} for
#'  Colebrook-White. Other options are:  \code{Eq="SJ"} for Swamee-Jain; \code{Eq="HW"} for
#'  Hazen-Willians; \code{Eq="FL"} for Flamant
#' @param friend If \code{friend=TRUE} the results are presented in a friendly way
#' @param v Kinematic viscosity of fluiyd in square meters per second. By default use the
#' value for water at 20oC \code{v=1.01e-6 m2/s}.  Unnecessary for empirical equations.
#' @param g Gravitational acceleration g=9.81 meters per square second.  Unnecessary for
#' empirical equations.
#'
#' @return Q Flow rate in cubic meters per second or V velocity flow in meters per second
#' @export
#'
#' @examples

#' flowRate(Hf=0.097,L=1,D=0.05,RC=1e-4, Vel=TRUE, Eq="CW", friend=TRUE)
#' flowRate(Hf=0.097,L=1,D=0.0005,RC=1e-4, Eq="CW", friend=TRUE)
#' flowRate(Hf=0.097,L=1,D=0.0005,RC=140, Eq="HW")
#' flowRate(Hf=0.097,L=1,D=0.05,RC=1e-4, Eq="CW", friend=TRUE)
#' flowRate(Hf=0.097,L=1,D=0.05,RC=1e-4, Eq="SJ", friend=TRUE)
#' flowRate(Hf=0.097,L=1,D=0.05,RC=140, Eq="HW", friend=TRUE)
#' flowRate(Hf=0.097,L=1,D=0.05,RC=0.000135, Eq="FL", friend=TRUE)

flowRate <-
  function(Hf, L, D, RC, Vel = FALSE, Eq = "CW", friend = FALSE, v = 1.01e-6,g = 9.81) {
    lam = FALSE
    J = Hf / L
    A = (pi * (D ^ 2)) / 4
    if (Eq == "CW") {
      Ref = (D / v) * (sqrt(2 * g * D * J))
      f = (1 / (-2 * log10(RC / (3.7 * D) + (2.51 / Ref)))) ^ 2
      V = sqrt((J * D * 2 * g) / (f))
      Q = A * V
    }
    else if (Eq == "HW") {
      Q = ((J * (RC ^ 1.852) * (D ^ 4.871)) / (10.643)) ^ (1 / 1.852)
      V = Q / A
    }
    else if (Eq == "SJ") {
      Q = -pi / sqrt(2) * log10(RC / (3.7 * D) + 1.78 * v / (D * sqrt(g * D *
                                                                        J))) * D ^ 2 * sqrt(g * D * J)
      V = Q / A
    }
    else if (Eq == "FL") {
      Q = ((J * D ^ 4.75) / (6.107 * RC)) ^ (1 / 1.75)
      V = Q / A

    }

    #laminar flow recalculate
    Re = V * D / v

    if (Re < 2000) {
      if (Eq == "HW" || Eq == "FL") {
        pskill
        stop(paste0(
          "Laminar flow. Re=",Re," (< 2000). You can't use empirical equations."
        ))
      }
      lam = TRUE
      #colocar a equacao laminar para a vazão
      V = (J * D ^ 2 * 2 * g) / (64 * RC)
    }


    #show the results in friendly way
    if (friend == TRUE)
    {
      if (Vel == TRUE)
      {
        QoV <- V
        ini <- "Velocity="
        end <- " meters per second."
      }
      else
      {
        QoV <- Q
        ini <- "Flow rate="
        end <- " cubic meters per second."
      }
      if (Eq == "CW")
      {
        valor = paste(ini, QoV, end,"Calculated by the Colebrook-White.")
      }
      else if (Eq == "HW")
      {
        valor = paste(ini, QoV, end,"Calculated by the Hazen-Willians.")
      }
      else if (Eq == "SJ")
      {
        valor = paste(ini, QoV,end, "Calculated by the Swamee-Jain.")
      }
      else if (Eq == "FL")
      {
        valor = paste(ini, QoV, end,"Calculated by the Flamant.")
      }
      if (lam == TRUE) {
        valor <-
          paste0(
            ini, QoV, end," Calculated by the Darcy-Weisbach equation for laminar flow. Re=",format(Re,digits =
                                                                                                      4)," (<2000)."
          )
      }
      return(valor)
    }
    else
    {
      return(Q)
    }
  }
