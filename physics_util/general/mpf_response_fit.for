      FUNCTION mpf_response_fit(e_meas)

C----------------------------------------------------------------------
C-   Purpose and Methods :  for fit of missing et projection fraction derived
C-            response curve.
C-   Returned value  :  r_par, e_par
C-   Inputs  :  measured jet energy, e_meas
C-
C-   Created  27-MAY-1993   Bob Kehoe
C-   Updated  Mar-27-1994   Bob Kehoe -- minor modifications
C-   Updated  Sep-18-1995   Bob Kehoe -- change to logarithmic function
C----------------------------------------------------------------------

      IMPLICIT NONE
      REAL e_meas
      double precision r_par(5),e_par(5),mpf_response_fit
      COMMON /mpf_fit/ r_par,e_par

      IF (r_par(3).gt.0) mpf_response_fit = r_par(1) + 
     &        r_par(2)*log(e_meas*r_par(3))

      RETURN
      END
