      FUNCTION INDX_KEY(RKEY)
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
      INCLUDE 'D0$INC:SHLCON.INC'
      INTEGER RKEY(*)
      INTEGER INDX_KEY
      LOGICAL FIRST
      INTEGER IER,LREC,MAX_CYCLE,IDIV
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET('MAXIMUM_CYCLES',MAX_CYCLE,IER)
        CALL EZRSET
        IDIV = (MAX_CYCLE+1)
      ENDIF
      CALL PACK_REC(RKEY,1,LREC)         
      INDX_KEY = 1 + LREC/IDIV
C
C ****  INDX_KEY STARTS WITH 1, FOR KEY(1-NKEY) = 1
C
      IF(INDX_KEY.GT.NDATA_CYCLES)THEN
        CALL ERRMSG('SHOWERLIBRARY','INDX_KEY',
     &    'INDX_KEY OUT OF RANGE','F')
      ENDIF
  999 RETURN
      END
