      REAL FUNCTION MUDRER(IPOINT,IDRFT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS error (cm) for a given drift time
C-   hit in the MUOH bank
C-   Inputs  : IPOINT = which MUOH hit
C-             IDRFT  = which drift solution 
C-   Controls: 
C-
C-   Created  26-AUG-1992   David Hedin
C-  extremely preliminary version
C-   Revised  12-sep-1993   Darien Wood
C-     include module dependence, but no drift time dependence yet
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPOINT,IDRFT
      INTEGER IWADD,IFW1,IFW2,INRAW,IORIEN,NHWIR
      REAL CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1
      REAL DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR
C
      INTEGER NMOD,NPLN,NWIR,IERR,JERR
      REAL E_DRIFT,E_NONDRIFT
C
C----------------------------------------------------------------------
      MUDRER=99999.
      IF(IABS(IDRFT).LE.0.OR.IABS(IDRFT).GE.3) GO TO 999
C
      CALL GTMUOH(IPOINT,IWADD,IFW1,IFW2,INRAW,IORIEN,NHWIR,
     &  CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,
     &  DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
C
      CALL MUADD(IWADD,NMOD,NPLN,NWIR,IERR)
      IF(IERR.EQ.0) THEN
        CALL MU_MOD_ERRORS(NMOD,E_DRIFT,E_NONDRIFT,JERR)
        IF(JERR.EQ.0) THEN
          MUDRER = E_DRIFT
        ENDIF
      ENDIF  
C----------------------------------------------------------------------
  999 RETURN
      END
