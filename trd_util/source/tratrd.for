      SUBROUTINE TRATRD(ITR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : ITR      =     0    : All tracks (in the sense of
C-                                     option OPT)
C-                      >     0    : Track ITR
C-           : OPT      =    'tra' : ZTRK track
C-       (via TRD_RCP)  =    'vtx' : VTX  track
C-                      =    'cdc' : CDC  track
C-                      =    'cal' : CALO track (not available yet)
C-   Outputs :                                                 
C-   Controls: 
C-
C-   Created   2-NOV-1989   J.Fr. Glicenstein
C-   Updated  17-NOV-1989   A. Zylberstejn  :Cleaning 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INTEGER ITR,LL,IDISA,LGTRH,GZGTRH
C----------------------------------------------------------------------
C
C  Build a table containing all the tracks contained in
C  the TRD
c      CALL VZERO(ANOINF(1,1),3*NTOTTR)
c      CALL VZERO(ANOSUP(1,1),3*NTOTTR)
c      CALL VZERO(CATINF(1,1),3*NTOTTR)
c      CALL VZERO(CATSUP(1,1),3*NTOTTR)
C
C  TRISTR deals with ISAJET tracks
C  TRISRC deals  with ZTRAKS tracks
      IF (TYPPRO.EQ.0) THEN !Ztraks only
        CALL TRISRC(ITR)
      ELSE IF (TYPPRO.EQ.1) THEN ! Geant tracks only
        CALL TRISTR
      ELSE IF (TYPPRO.EQ.2) THEN
        CALL TRISTR
        CALL TRISRC(ITR)
      ENDIF
         IF (TYPPRO.GT.0) THEN          ! Isajet type
          LGTRH = GZGTRH()
          IF (LGTRH.GE.0) THEN
           LL = LQ(LGTRH-1)             ! Kept only for debugging
           IF (LL.GT.0) THEN
            IDISA = IQ(LL+14)
            IF (IDISA.LE.3) IDENTR(NGOODT) = IDENTR(NGOODT)+1
           ENDIF
           ENDIF
          ENDIF
  999 RETURN
      END
