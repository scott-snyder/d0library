      SUBROUTINE UDST_GET_MAGPOL(IPOL)
C----------------------------------------------------------------------
C-      PURPOSE: fetch toroid polarity info from GLOB bank for UDST
C-
C-      INPUTS: none
C-      OUTPUTS:  INTEGER IPOL 
C-                = toroid polarity words = 10*(wam+2) + sam+2
C-                      e.g. IPOL = 33 ==> forward,forward
C-                           IPOL = 11 ==> reverse,reverse
C-                           IPOL = 22 ==> off,off
C-                           IPOL = 0  ==> unknown,unknown
C-
C-   Created  24-APR-1993   Darien R. Wood
C-   Updated  14-AUG-1993   Ulrich Heintz - extended to get words 2-18
C-   Updated  16-DEC-1993   Ian Adam  - rename routine UDST_GET_GLOBWORDS
C-    to UDST_GET_MAGPOL.  Move GLOB word fetching to MAKE_UDST.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER   LGLOB,GZGLOB,IPWAM,IPSAM,IVER_GLOB,IPOL,I,JPOL,LMTRH
      INTEGER   GZMTRH
      REAL      R
      EQUIVALENCE(R,I)
C----------------------------------------------------------------------
C check GLOB bank:
      LGLOB = GZGLOB()
      IPWAM = -2
      IPSAM = -2

C magnet polarities are saved in GLOB bank version 3 or higher:
      IF(LGLOB.GT.0) THEN
        IVER_GLOB = IQ(LGLOB+1)
        IF(IVER_GLOB.GE.2) THEN
        ENDIF
        IF(IVER_GLOB.GE.3) THEN
          IPWAM = IQ(LGLOB+19)
          IPSAM = IQ(LGLOB+20)
          IPOL=10*(IPWAM+2) + (IPSAM+2)
        ELSE
          CALL ERRMSG('GLOB VERSION','UDST_GET_MAGPOL','NO MAGNET POL',
     &      'W')
          IPOL=0
          GOTO 999
        ENDIF
      ELSE
        CALL ERRMSG('no GLOB bank','UDST_GET_GLOBWORDS',' ','W')
        IPOL=0
        GOTO 999
      ENDIF
  999 RETURN
C
      ENTRY UDST_PUT_MAGPOL(JPOL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : unpack magnet polarity and fill GLOB and MTRH
C-
C-   Created   1-FEB-1995   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IPWAM=JPOL/10-2
      IPSAM=JPOL-10*(IPWAM+2)-2
      LGLOB=GZGLOB()
      IF(LGLOB.GT.0)THEN
        IQ(LGLOB+19) = IPWAM
        IQ(LGLOB+20) = IPSAM
      ENDIF
      LMTRH=GZMTRH()
      IF(LMTRH.GT.0)THEN
        Q(LMTRH+14) = FLOAT(IPWAM)
        Q(LMTRH+15) = FLOAT(IPSAM)
      ENDIF
C----------------------------------------------------------------------
      END
