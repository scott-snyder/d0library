      FUNCTION LSQ_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize LSQ package
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Updated  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_PARS.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
      LOGICAL LSQ_INI
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      INTEGER IER,NWARN,I
      LOGICAL READONLY,OK
      INTEGER NCHARS
      INTEGER NLEN
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        LSQ_INI=.FALSE.
C
C       read in files
        CALL INZBRA                       ! INITIALIZE ZEBRA
        CALL INZCOM(2)                    ! Initialize /ZEBCOM/
        CALL INZLNK                       ! Initialize ZLINKA
        CALL INZSTP                       ! Initialize /ZEBSTP/
        CALL INPAWC                       ! Initialize HBOOK4
C
        CALL INRCP('LSQ_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
C
        CALL EZPICK('LSQ_RCP')              ! select LSQ bank
        CALL EZERR(IER)
C
        IF(IER.EQ.0) THEN
          CALL EZGET('NUMBER_OF_WARNINGS',NWARN,IER)
          CALL ERRMAX ('LSQ',-1,NWARN)
          LSQ_INI= .TRUE.
        ELSE
          CALL ERRMSG('LSQ','LSQ_INI',
     &      ' LSQ_RCP file does not have a LSQ bank.','W')
          LSQ_INI = .FALSE.
        ENDIF
C
C ****  MAKE ZLSQ into a link area
C
        CALL LSQ_MAKE_LINK_AREA
        NMAT = 0
        DO I = 1 , LSQ_MAX
          M_NAME(I) = ' '
          M_DELETE(I) = 0
        ENDDO
C
        CALL EZGET('NEW_RZ',NEW_RZ,IER)
        CALL EZGETS('SUB_DIRECTORY',1,SUB_DIRECTORY,NLEN,IER)
C
        IF ( NEW_RZ ) THEN
          READONLY = .FALSE.
          CALL LSQ_RZ_MAKE(.TRUE.,READONLY,RZ_UNIT,OK)
        ELSE
          READONLY = .FALSE.
          CALL LSQ_RZ_MAKE(.FALSE.,READONLY,RZ_UNIT,OK)
          CALL LSQ_RZ_GET(' ',SUB_DIRECTORY,'ALL',IER)
        ENDIF
C
        CALL EZRSET
      ELSE
        LSQ_INI=.TRUE.
      ENDIF
C
  999 RETURN
      END

