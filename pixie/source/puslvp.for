      SUBROUTINE PUSLVP( XVIN,YVIN, PACKAGE, COMMAND, IDX, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SELECT VIEWPORT ACCORDING TO XVIN,YVIN
C-
C-   Inputs  : XVIN,YVIN  :Virtual coordinates of a point
C-   Outputs :
C-   Controls:
C-
C-   Created  01-JUN-1990   Michael W. Peters
C-   Updated   6-DEC-1990   Harrison B. Prosper, Nobu Oshima
C-      Make compatible with PIXIE RCP
C-   Updated  14-MAY-1991   Harrison B. Prosper
C-   Updated  18-MAY-1991   Harrison B. Prosper
C-      Use PU_SETUP_VIEWPORT
C-   Updated  18-JUN-1991   Nobuaki Oshima  
C-      Add PACKAGE argument 
C-   Updated  03-JUL-1991   Nobuaki Oshima( Fix a Bug by MAXPAC )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL XVIN,YVIN
      CHARACTER*(*) PACKAGE, COMMAND
      INTEGER IDX, IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:PXUSER.INC'
C----------------------------------------------------------------------
      INTEGER MAXPAC
      PARAMETER( MAXPAC =  10 )         ! Should be d0$params:pixie.def
C----------------------------------------------------------------------
      REAL    VPORTXMIN(MAXPAC),VPORTXMAX(MAXPAC)
      REAL    VPORTYMIN(MAXPAC),VPORTYMAX(MAXPAC)
      REAL    VIEWREFX, VIEWREFY, VIEWREFZ
      INTEGER IDDX(10),NPORT
      CHARACTER*32 PACK(MAXPAC),ACTIVE_PACKAGE
      CHARACTER*40 COMM(MAXPAC),COMD
      LOGICAL PU_SET_RCP_BANK
C----------------------------------------------------------------------
C
C ****  Return the current command (could be a mult-port command)
C
      CALL PU_GET_ACTIVE_COMMAND(COMD)
      CALL PU_ACTIVE_PACKAGE(ACTIVE_PACKAGE)
C
C **** Which viewport are we in?
C
      NPORT =-1                         ! Return 1st viewport ONLY
      CALL PU_GET_VIEWPORT(ACTIVE_PACKAGE,COMD,
     &                     XVIN,XVIN,YVIN,YVIN,
     &                     VPORTXMIN,VPORTXMAX,
     &                     VPORTYMIN,VPORTYMAX,
     &                     PACK,COMM,IDDX,NPORT)
C
      IF ( NPORT .GT. 0 ) THEN
        PACKAGE = PACK(1)
        COMMAND = COMM(1)
        IDX = IDDX(1)
C
C ****  Select correct RCP-bank
C
        IF ( PU_SET_RCP_BANK(PACK) ) THEN
          CALL PU_SETUP_VIEWPORT(IDX,IER)
          CALL PU_RESET_RCP_BANK
        ELSE
          CALL ERRMSG('BAD_BANK','PUSLVP',
     &    ' Unable to select Bank for package '//PACK(1),'W')
        ENDIF
      ELSE
        COMMAND = ' '
        IDX = 0
        CALL ERRMSG('BAB_POINT','PUSLVP',
     &    ' **** Point outside viewport ****','W')
        GOTO 999
      ENDIF
C
  999 RETURN
      END
