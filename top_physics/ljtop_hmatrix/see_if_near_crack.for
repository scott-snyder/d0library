      SUBROUTINE SEE_IF_NEAR_CRACK(P4,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SEE IF ELECTRON HITS CC NEAR CRACK.
C-   USES SHOWERLIBRARY CODE.
C-
C-   Inputs  : IER = NON ZERO IF IN CRACK
C-   Outputs : P4 = 4 VECTOR OF ELECTRON.
C-   Controls: 
C-
C-   Created   5-JAN-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    P4(*)
      INTEGER IER
      INCLUDE 'D0$INC:SHLCON.INC'   
      LOGICAL LMONTE
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      REAL    VERTEX(3),ZV(3),DZ
      INTEGER NVER,ISAPART
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET('MONTE_CARLO_DATA',LMONTE,IER)
        CALL EZRSET
      ENDIF
      IF ( LMONTE ) THEN
        CALL ZVERTX(ZV,DZ)                ! Isajet Vertex
      ELSE
        CALL ZVERTE(NVER,ZV,DZ)                ! Vertex from tracking
        IF(NVER.EQ.0) THEN
          CALL ERRMSG('No Vertices','SEE_IF','z set to 0','W')
          ZV(1)=0.0
        ENDIF
      ENDIF
      VERTEX(1) = 0.
      VERTEX(2) = 0.
      VERTEX(3) = ZV(1)
      ISAPART = 12                      ! ELECTRON
      CALL GETBIN_NOCD(ISAPART,VERTEX,P4) 
      IF(KEY(5).EQ.2)THEN
        CALL ERRMSG('HMATRIX','SEE_IF_NEAR_CRACK',
     &    'ELECTRON TOO CLOSE TO CRACK ','W')
        IER = 1
      ENDIF
  999 RETURN
      END
