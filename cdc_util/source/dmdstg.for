      SUBROUTINE DMDSTG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Modify sense wire staggering from 200 to 150 um.
C-
C-   Inputs  : 
C-   Outputs : none
C-
C-   Created  16-DEC-1992   Domenico Pizzuto
C-   Updated  31-DEC-1992   Qizhong Li-Demarteau   added EZRSET and update
C-                                                 DALS banks
C-
C----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'

      INTEGER LDRFT,GZDRFT,NWIR,IER,WIRE
      INTEGER LDALS, GZDALS, IPAL, LAY, SEC
      REAL EFFSTG,STAG, STGDIF, OLDSTG(0:6)
      LOGICAL FIRST,EZERROR

      DATA FIRST /.TRUE./

      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DMDSTG',
     &      'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('EFFSTG',EFFSTG,IER)
        CALL EZRSET
      END IF

      LDRFT = GZDRFT ()
      IF (ABS (C (LDRFT+26)).EQ.EFFSTG) GOTO 999
      STAG = -EFFSTG
      DO 10 WIRE = 0, MXSENS
        OLDSTG(WIRE) = C(LDRFT+26+WIRE)
        C(LDRFT+26+WIRE) = STAG
        STAG = -STAG
   10 CONTINUE
C
      DO 100 LAY = 0, 3
        DO 200 SEC = 0, 31
          LDALS = GZDALS(LAY,SEC)
          DO 50 WIRE = 0, MXSENS
            IPAL  = LDALS + 6 + IC(LDALS+6) * WIRE
            STAG = C(LDRFT+26+WIRE)
            STGDIF = STAG - OLDSTG(WIRE)
            C(IPAL+1) = C(IPAL+1) + STGDIF * C(LDALS+3)
            C(IPAL+2) = C(IPAL+2) + STGDIF * C(LDALS+4)
   50     CONTINUE
  200   CONTINUE
  100 CONTINUE
C 
  999 RETURN
      END
