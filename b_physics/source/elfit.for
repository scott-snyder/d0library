      FUNCTION ELFIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : perform a global fit of electron trajectory
C-   using vertex pont, VTX track, CDC/FDC track and CAL cluster position.
C-   Save the fit results in Zebra. 
C-
C-   Returned value  : 
C-   Inputs  : contents of Zebra banks PELC, VERT, VTXT, DTRK, FDCT
C-   Outputs : updated electron momentum in PELC  
C-   Controls: 
C-
C-   Created  11-JUL-1993   Andrzej Zieminski, Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INTEGER LPARH,GZPARH,LPELC,GZPELC
      INTEGER NZBANK,NPELC
      LOGICAL ELFIT
      INTEGER IERR
      REAL POINT(3),EPOINT(3),CLIST(100)
C
      ELFIT = .TRUE.
      LPARH=GZPARH()
      LPELC=LQ(LPARH-IZPELC)
      IF(LPELC.LE.0) GOTO 999
C
C Loop over electron banks PELC, perform the fit.
C
  100 CONTINUE
      CALL CLUCEN(LPELC,POINT,EPOINT,IERR)  ! get cluster position and errors
      IF(IERR.GT.0) GOTO 101
C
      IF (ABS(POINT(3)).LT.150.) THEN
        CALL ELFITCEN(LPELC,POINT,EPOINT,CLIST,IERR)  ! central region
      ELSE
        CALL ELFITFWD(LPELC,POINT,EPOINT,CLIST,IERR)  ! forward region
      END IF
C
C  Store fit results 
C
      CALL ELFIT_SAVE(LPELC,POINT,EPOINT,CLIST)
      LPELC=LQ(LPELC)
  101 IF (LPELC.GT.0) GO TO 100
C
  999 RETURN
      END
