      LOGICAL FUNCTION MULIB_END_RUN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-FEB-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MULCON.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER IERR
      INTEGER ICYCLE
      INTEGER RCPAR(20),RCPTYP(20),TOTAL
      CHARACTER*80 FILNAM
      INTEGER NCHAR,TRULEN
C
      INTEGER PRTUN,SSUNIT
      INTEGER NRUN
      INTEGER IER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA NRUN/0/
      INTEGER KK
C
      LOGICAL DO_MAKE_LIB
C----------------------------------------------------------------------
      MULIB_END_RUN = .TRUE.
      CALL ERRMSG('MUONLIBRARY','MULIB_END_RUN',
     &  'DOING MULIB_END_RUN ','W')
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('MUONLIBRARY_RCP')              ! select MUONLIB bank
        CALL EZGET('DO_MAKE_LIB',DO_MAKE_LIB,IER)
      ENDIF
C
      IF (ISUNIT_AUX.NE.0.AND.DO_MAKE_LIB)THEN
C
        CALL EZPICK('CALFRAME_RCP')
        CALL EZ_GET_CHARS('INPUT_FILENAME',NCHAR,FILNAM,IER)
        CALL EZRSET
C
        PRTUN = SSUNIT()
C
        NFILE = NFILE + 1
C
        CALL MULIB_AUX_WRITE(ISUNIT_AUX,NFILE,NFILE,FILNAM,CYCLES,
     &   NO_CYCLES, NDATA_CYCLES,NCYCLES,NCYCLES,.TRUE.)
C
C WRITE AUXILLIARY INFO
C
        WRITE(PRTUN,2)NFILE,FILNAM(1:TRULEN(FILNAM))
    2   FORMAT(' INPUT FILE NAME : '1X,I5,1X,A,/,' AND CYCLES ARRAY '
     &  'SAVED TO AUXILLIARY FILE')
C
      ENDIF
C
      IF(IERR.NE.0)MULIB_END_RUN = .FALSE.
C
  999 RETURN
      END
