      SUBROUTINE JLOCAT(DSPDV, PHYDV, ECHLV, BUT, VX, VY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
CD   This module returns a 2D virtual coordinate system point and the
CD   button trigger used to trigger the LOCATOR input. The parameters
CD   passed are the display device (DSPDV), the physical input device
CD   (PHYDV), and the echo level (only one supported) (ECHLV). The
CD   parameters returned are the button value (BUT), and the Virtual
CD   coordinates of the LOCATOR (VX, VY).
C-
C-   Inputs  :DSPDV, PHYDV, ECHLV
C-   Outputs :BUT, VX, VY
C-   Controls:
C-
C-   CREATED    18-NOV-1988   A. VIRGO
C-   Modified   22-FEB-1990   SHAHRIAR ABACHI    Picking by keyboard included
C-                                               PHYDV shoulb be > 1
C-   UPDATED    20-NOV-1989   SHAHRIAR ABACHI
C-   UPDATED    23-SEP-1992   SHAHRIAR ABACHI    Added button identifiaction
C-   Updated    20-OCT-1992   Nobuaki Oshima     Now Button ID works!!!
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL ERRHND
      INTEGER DSPDV, PHYDV, ECHLV, BUT, HDBUT
      REAL VX, VY, HDVX, HDVY
      INTEGER LOCLEN, LOCLEN2, LOCLEN3, IST,IBUT, MRK
      REAL XPOS, YPOS
      CHARACTER*40 LOCATE,LOCATE2,LOCATE3
      CHARACTER*1 ICH
      INCLUDE 'D0$INC:DEVSTS.INC'
      CHARACTER*20 VXVY
      COMMON /TEMP3/ VXVY
      LOGICAL FIRST1,HDPICK
      DATA    FIRST1,HDPICK / .TRUE., .FALSE. /
C----------------------------------------------------------------------
C
C-
C--- PICK WAS DONE OR NOT INSIDE THE HARDWARE ROTATION MODE?
C-
      IF ( HDPICK ) THEN
        HDPICK = .FALSE.
        VX  = HDVX
        VY  = HDVY
        BUT = HDBUT
        GO TO 999
      ENDIF
C-
      LOCATE = ' '
      LOCATE2 = ' '
      LOCATE3 = ' '
      CALL KUPDV
C
      IF( FIRST1 .OR. LASTCO ) THEN
        CALL PFN ('DEL', 'DELTA', ERRHND)
        CALL PFN ('ADD', 'ADDC', ERRHND)
        CALL PFNN('RTE', 'ROUTE', 9, ERRHND)
        CALL PFN ('SUB', 'SUBC', ERRHND)
        CALL PFN ('PRT3', 'PRINT', ERRHND)
        CALL PFN ('PRT6', 'PRINT', ERRHND)
        CALL PFN ('CONCAT', 'CONCATENATE', ERRHND)
        FIRST1 = .FALSE.
      ENDIF
C---
      CALL PSNFIX( 1, 2, 'DEL', ERRHND)
      CALL PSNFIX( 1, 2, 'ADD', ERRHND)
      CALL PSNFIX( 1, 2, 'SUB', ERRHND)
C-
      CALL PDIALL('RTE', ERRHND)
      CALL PDIALL('CONCAT', ERRHND)
C---
      CALL PCONN('TABLETIN', 3, 1, 'DEL', ERRHND)
      CALL PCONN('DEL', 1, 1, 'ADD', ERRHND)
      CALL PCONN('ADD', 1, 1, 'RTE', ERRHND)
      CALL PCONN('ADD', 1, 2, 'RTE', ERRHND)
      CALL PCONN('RTE', 2, 1, 'SUB', ERRHND)
      CALL PCONN('RTE', 3, 1, 'SUB', ERRHND)
      CALL PCONN('RTE', 5, 1, 'SUB', ERRHND)
      CALL PCONN('RTE', 9, 1, 'SUB', ERRHND)
      CALL PCONN('SUB', 1, 1, 'PRT3', ERRHND)
      CALL PCONN('TABLETIN', 6, 1, 'PRT6', ERRHND)
      CALL PCONN('PRT3', 1, 1, 'CONCAT', ERRHND)
      CALL PCONN('PRT6', 1, 2, 'CONCAT', ERRHND)
      CALL PCONN('CONCAT', 1, 1, 'HOST_MESSAGE', ERRHND)
C-
      CALL PSNST('INPUT [ WHITE BUTTON FOR QUIT ]',1, 'MESSAGE_DISPLAY',
     &  ERRHND)
C---
      CALL PPURGE(ERRHND)
      CALL PGETW(LOCATE, LOCLEN, ERRHND)
C---
      CALL PDI('TABLETIN', 3, 1, 'DEL', ERRHND)
      CALL PDI('DEL', 1, 1, 'ADD', ERRHND)
      CALL PDI('ADD', 1, 1, 'RTE', ERRHND)
      CALL PDI('ADD', 1, 2, 'RTE', ERRHND)
      CALL PDI('RTE', 2, 1, 'SUB', ERRHND)
      CALL PDI('RTE', 3, 1, 'SUB', ERRHND)
      CALL PDI('RTE', 5, 1, 'SUB', ERRHND)
      CALL PDI('RTE', 9, 1, 'SUB', ERRHND)
      CALL PDI('SUB', 1, 1, 'PRT3', ERRHND)
      CALL PDI('TABLETIN', 6, 1, 'PRT6', ERRHND)
      CALL PDI('PRT3', 1, 1, 'CONCAT', ERRHND)
      CALL PDI('PRT6', 1, 2, 'CONCAT', ERRHND)
      CALL PDI('CONCAT', 1, 1, 'HOST_MESSAGE', ERRHND)
C
      CALL PSNST(' ', 1, 'MESSAGE_DISPLAY', ERRHND)
C
      CALL PPURGE(ERRHND)
C
      BUT = 0
      XPOS = 0.
      YPOS = 0.
      VXVY = ' '
C
      MRK = INDEX(LOCATE, 'V')
      IF(MRK .EQ. 0) THEN
        READ (LOCATE(1:LOCLEN), '(A1)', ERR=9999) ICH
        BUT = ICHAR(ICH)
      ELSEIF (LOCLEN .EQ. 0) THEN
        GOTO 1
      ELSE
        READ (LOCATE(MRK+1:LOCLEN), *, ERR=9999) XPOS, YPOS
        READ (LOCATE(1:1), *, ERR=9999) IBUT
        IF(IBUT .EQ. 0) THEN
          BUT = 1
        ELSE
          BUT = IBUT
        ENDIF
      ENDIF
C
      VX = XPOS
      VY = YPOS
      IF ( BUT .GT. 47 .AND. BUT .LT. 58) BUT = BUT - 48
C
      IF (ECHLV.EQ.-2 .AND. BUT.NE.8) THEN
        HDPICK = .TRUE.
        HDVX  = VX
        HDVY  = VY
        HDBUT = BUT
      ENDIF
      GOTO 999
C
C- Error exit.
C
    1 CONTINUE
      BUT = 999
      GOTO 999
 9999 CONTINUE
      CALL ERROR('ERROR DECODING INPUT: '//LOCATE(:LOCLEN))
  999 RETURN
      END
