      SUBROUTINE GET_P2NEUT(IPNUT,P2_NEUT,VP2_NEUT,ETSCALAR,PNUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET MISSING ET 2-VECTOR: PX, PY, 
C-                         VARIANCES OF PX and PY, and ETSCALAR
C-
C-   Inputs  : IPNUT       [I]   CORRECTION LEVEL, IF ZERO THEN FIRST IN 
C-                               CHAIN = LAST BOOKED = LATEST CORRECTION
C-   Outputs : P2_NEUT(2)  [R]   MISSING PT 2-VECTOR: PX,PY 
C-             VP2_NEUT(2) [R]   VARIANCE (SIGMA**2) OF PX,PY
C-             ETSCALAR    [R]   
C-             PNUT        [L]   TRUE IF PNUT BANK FOUND
C-   Controls: 
C-
C-   Created  12-NOV-1991   Stan M. Krzywdzinski
C-   Modified 10-JUL-1992   Stan M. Krzywdzinski Added VP2_NEUT and ETSCALAR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPNUT
      REAL P2_NEUT(2),VP2_NEUT(2),ETSCALAR
      LOGICAL PNUT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C----------------------------------------------------------------------
      INTEGER GZPNUT
C----------------------------------------------------------------------
      CALL VZERO(P2_NEUT,2)
      CALL VZERO(VP2_NEUT,2)
      ETSCALAR = 0.
      PNUT = .FALSE.
C
      LPNUT = GZPNUT(IPNUT)
      IF(LPNUT.LE.0)THEN
        CALL ERRMSG('NOPNUT','GET_P2NEUT',
     &  'NO PNUT BANK FOR THIS EVENT ','W')
        GO TO 999
      ENDIF
C
      PNUT = .TRUE.
      CALL UCOPY(Q(LPNUT+ 3), P2_NEUT,2)
      CALL UCOPY(Q(LPNUT+11),VP2_NEUT,2)
      ETSCALAR = Q(LPNUT+14)
C
  999 RETURN
      END
