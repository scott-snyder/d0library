      FUNCTION TOP_LEPTONS_UTIL_COPLAN(VECT1,VECT2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute the coplanarity of two leptons
C-
C-   Returned value  : Coplan    
C-          Coplan = ABS( Px1Py2-Px2Py1 / E1E1 )
C-
C-   Inputs  : VECT1     4-vector of lepton 1
C-             VECT2     4-vector of lepton 2
C-
C-   Outputs : None
C-
C-   Controls: None
C-
C-   Created   4-SEP-1991   Stephen J. Wimpenny
C-   Modified 17-Mar-1993   Routine name changed for library compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL TOP_LEPTONS_UTIL_COPLAN,VECT1,VECT2,TEMP
C
      DIMENSION VECT1(4),VECT2(4)
C
      IF(VECT1(4).GT.0..AND.VECT2(4).GT.0.) THEN
        TEMP=(VECT1(1)*VECT2(2)-VECT2(1)*VECT1(2))/(VECT1(4)*VECT2(4))
        TOP_LEPTONS_UTIL_COPLAN=ABS(TEMP)
      ELSE
        TOP_LEPTONS_UTIL_COPLAN=-99999.9
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
