      FUNCTION TOP_MASS_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize TOP_MASS package
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Updated   3-APR-1993   Rajendran Raja   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:KINEQ.INC'
      LOGICAL TOP_MASS_INI
      LOGICAL FIRST
      INTEGER IER,NWARN
      SAVE FIRST
      DATA FIRST/.TRUE./
      INTEGER ILEN,INUM,I,IPT,ERROR(100)
      LOGICAL LERR(100)
      LOGICAL CONTIN,COUNT,TYPE,LOG
      INTEGER ERR_NUM,MAX
      EQUIVALENCE (LERR,ERROR)
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        TOP_MASS_INI=.FALSE.
C       read in files
        CALL INRCP('TOP_MASS_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
C
        CALL INRCPE('TOP_MASS_RCPE',IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &  CALL ERRMSG('TOP_MASS','TOP_MASS_INI',
     &  ' Default TOP_MASS_RCP modified','W')
C
        CALL EZPICK('TOP_MASS_RCP')              ! select TOP_MASS bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          CALL EZGET('NUMBER_OF_WARNINGS',NWARN,IER)
          CALL ERRMAX ('TOP_MASS',-1,NWARN)
          TOP_MASS_INI= .TRUE.
        ELSE
          CALL ERRMSG('TOP_MASS','TOP_MASS_INI',
     &      ' TOP_MASS_RCP file does not have a TOP_MASS bank.','W')
          TOP_MASS_INI = .FALSE.
        ENDIF
C
C ****  do error handling in program
C
        CALL EZGETA('ERROR_HANDLING',0,0,0,ILEN,IER)
        CALL EZGETA('ERROR_HANDLING',1,ILEN,1,ERROR,IER)
        INUM = ILEN/6
        IPT = 1
        DO I = 1 , INUM
          ERR_NUM = ERROR(IPT)
          CONTIN = LERR(IPT+1)
          COUNT = LERR(IPT+2)
          TYPE = LERR(IPT+3)
          LOG = LERR(IPT+4)
          MAX = ERROR(IPT+5)
          IPT = IPT + 6
          CALL ERRSET(ERR_NUM,CONTIN,COUNT,TYPE,LOG,MAX)
        ENDDO
C
C ****  Not first entry into TOP_MASS_INI - set TOP_MASS_INI .TRUE.
C
        TOP_MASS_INI=.TRUE.
      ENDIF
      ITOT_EV = 0  !INITIALIZING
      IACC_EV = 0
      ISOL_EV = 0
  999 RETURN
      END

