      LOGICAL FUNCTION SHLEND_RUN()
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
C-   Updated  24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SHLCON.INC'
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
      SHLEND_RUN = .TRUE.
      CALL ERRMSG('SHOWERLIBRARY','SHLEND_RUN',
     &  'DOING SHLEND_RUN ','W')
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')              ! select SHOWERLIB bank
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
        CALL SHLAUX_WRITE(ISUNIT_AUX,NFILE,NFILE,FILNAM,CYCLES,
     &    NDATA_CYCLES,NCYCLES,NCYCLES,.TRUE.)
C
C WRITE AUXILLIARY INFO
C
        WRITE(PRTUN,2)NFILE,FILNAM(1:TRULEN(FILNAM))
    2   FORMAT(' INPUT FILE NAME : ',1X,I5,1X,A,/,' AND CYCLES ARRAY ',
     &  'SAVED TO AUXILLIARY FILE')
C
      ENDIF
C
      IF(IERR.NE.0)SHLEND_RUN = .FALSE.
C
  999 RETURN
      END
