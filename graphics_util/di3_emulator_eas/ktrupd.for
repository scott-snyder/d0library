      SUBROUTINE KTRUPD(TRVEC, NODEN, TRNAM, IRST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-FEB-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL  TRVEC(3), TTRV(3)
      INTEGER  IRST
      CHARACTER*7  NODEN
      CHARACTER*2  TRNAM
      REAL  TRX, TRY, TRZ
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      EXTERNAL  ERRHND
C
        CALL PFN(TRNAM//'XV', 'XVECTOR', ERRHND)
        CALL PFN(TRNAM//'YV', 'YVECTOR', ERRHND)
        CALL PFN(TRNAM//'ZV', 'ZVECTOR', ERRHND)
        CALL PFN(TRNAM//'AC', 'ACCUMULATE', ERRHND)
C
        CALL PCONN(TRNAM//'XV', 1, 1, TRNAM//'AC', ERRHND)
        CALL PCONN(TRNAM//'YV', 1, 1, TRNAM//'AC', ERRHND)
        CALL PCONN(TRNAM//'ZV', 1, 1, TRNAM//'AC', ERRHND)
        CALL PCONN(TRNAM//'AC', 1, 1, NODEN(1:7)//'"', ERRHND)
C
        TTRV(1) = 0.0
        TTRV(2) = 0.0
        TTRV(3) = 0.0
        TRX = TRVEC(1)
        TRY = TRVEC(2)
        TRZ = TRVEC(3)
        IF(IRST .EQ. 0) THEN
          CALL PSNV3D(TTRV, 2, TRNAM//'AC', ERRHND)
        ENDIF
        CALL PSNREA(TRX, 1, TRNAM//'XV', ERRHND)
        CALL PSNREA(TRY, 1, TRNAM//'YV', ERRHND)
        CALL PSNREA(TRZ, 1, TRNAM//'ZV', ERRHND)
CC        CALL PSNBOO(.TRUE., 1, TRNAM//'AC', ERRHND)
        CALL PPURGE(ERRHND)
C
      RETURN
      END
