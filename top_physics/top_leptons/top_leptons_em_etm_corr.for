      SUBROUTINE TOP_LEPTONS_EM_ETM_CORR(LOBJ,DMET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute Missing Et Correction vector
C-
C-   Inputs  : LOBJ : link 
C-
C-   Outputs : DMET : vectorial correction to Missing Et
C-   Controls: 
C-
C-   Created  12-JUL-1993   Stephe J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LOBJ
C
      REAL    DMET(3),FSCALE
      REAL    E,EX,EY,EZ
      REAL    TOP_LEPTONS_EM_CORRECTION
C
      CALL VZERO(DMET,3)
C
C *** missing et correction for a single electron or photon 
C
        E   =Q(LOBJ+6) 
        EX  =Q(LOBJ+3) 
        EY  =Q(LOBJ+4) 
        EZ  =Q(LOBJ+5) 
        FSCALE = TOP_LEPTONS_EM_CORRECTION(LOBJ)
        DMET(1)=-(FSCALE-1.)*EX
        DMET(2)=-(FSCALE-1.)*EY
        DMET(3)=-(FSCALE-1.)*EZ
C
  999 RETURN
      END
