      SUBROUTINE ISREAD(INEWEV,PTCUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : READS ISAJET EVENT AND FAKES JXYZ POINTS.
C-                          IF INEWEV=0 NEW EVENT IS NOT READ IN.
C-                          ONLY FAKES JXYZ TRACKS FOR EVENTS WITH
C-                               ISAJET PT .GT. PTCUT
C-
C-   Inputs  : INEWEV   Read new event flag
C-             PTCUT    Minimum track Pt to process
C-   Outputs :
C-   Controls:
C-
C-   Created  11-OCT-1988   R.RAJA
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCBANK.INC/LIST'
      INCLUDE 'D0$INC:GCLINK.INC/LIST'
      INCLUDE 'D0$INC:GCNUM.INC/LIST'
      INTEGER INEWEV
      REAL PTCUT
C----------------------------------------------------------------------
C
      IF ( INEWEV.NE.0 ) THEN
        IF ( JKINE.NE.0 ) CALL MZDROP(IXSTOR,JKINE,' ')  !Drop previous JKINE
        NTRACK=0
        CALL ISKINE
      ENDIF
      CALL FAJXYZ(PTCUT)
C
  999 RETURN
      END
