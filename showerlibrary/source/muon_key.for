      FUNCTION MUON_KEY(RKEY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WORKS OUT LINEAR INDEX INTO CYCLES
C-                         ARRAY
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-OCT-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MULCON.INC'
      INTEGER RKEY(*)
      INTEGER MUON_KEY
      LOGICAL FIRST
      INTEGER IER,LREC,MAX_CYCLE,IDIV
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('MUONLIBRARY_RCP')
        CALL EZGET('MAXIMUM_CYCLES',MAX_CYCLE,IER)
        CALL EZRSET
        IDIV = (MAX_CYCLE+1)
      ENDIF
      CALL MULIB_PACK_REC(RKEY,1,LREC)
      MUON_KEY = 1 + LREC/IDIV
C
C ****  MUON_KEY STARTS WITH 1, FOR KEY(1-NKEY) = 1
C
      IF(MUON_KEY.GT.NDATA_CYCLES)THEN
        CALL ERRMSG('MUONLIBRARY','MUON_KEY',
     &    'MUON_KEY OUT OF RANGE','F')
      ENDIF
  999 RETURN
      END
