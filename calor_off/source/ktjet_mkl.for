C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_MKL.FOR
C *1     3-FEB-1994 14:39:47 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_MKL.FOR
      FUNCTION KTJET_MKL(J,I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates Mkl squared values for pairs of particles
C-   and jets. Use mkl = 2*min(e1,e2)*(1-cos12) definition.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-JUL-1992   Kate C. Frame
C-   Modified 20-JAN-1993   R. Astur "Get 4 vectors from zebra bank"
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      REAL RIJ, PJ4, PI4, KTJET_MKL, KTJET_DELTA_R
      INTEGER K,J,I
      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
C----------------------------------------------------------------------
C      IF ( I .LT. 1 .OR. J .LT. 1 .OR. I .GT. IQ(LKVEC+3) .OR. J .GT.
C     &  IQ(LKVEC+3) ) THEN
C        TYPE *,' BOMB, RUN/EVENT IS ',IQ(LHEAD+9),IQ(LHEAD+6)
C        TYPE *,' J,I VALUES ARE ', J, I
C      ENDIF
C: SC: dij=min(Eti**2,Etj**2)((etai-etaj)**2+(phii-phij)**2)/RGIAN
C
C: Kt: dij = 2*min( Ei**2, Ej**2 )(1 - cosij)
C: ET: dij = 2*min( Eti**2, Etj**2)( 1 - cosij )
      RIJ=KTJET_DELTA_R(I,J)

        KTJET_MKL = MIN( Q(KTET( I ))**2, Q(KTET( J ))**2 )
     &    *(RIJ/D_SEP**2)

  999 RETURN
      END
