      SUBROUTINE MUGET_VERT(MODE,LMUON,POS,ERR,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get VERT information from USER bank
C-
C-   Inputs  : MODE : 1 for Run 1a SSQ files
C-                    2 for Run 1b SSQ files
C-                    .ne. 1,2 for any other kind of file
C-             LMUON : Pointer to MUON bank. Necessary only for MODE .ne. 1,2 
C-
C-   Outputs : POS : X, Y, Z of vertex
C              ERR : DX, DY, DZ
C-   Controls: 
C-
C-   Created   16-MAY-1994    Taka Yasuda
C-   Modified  14-Nov-1994    R. Markeloff. Renamed from GET_VERT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'
C
      REAL     POS(3), ERR(3)
      INTEGER  MODE
      INTEGER  IER
C
      INTEGER  LVERT, LMUON, LMSSQ, LZTRK
C----------------------------------------------------------------------
      IER = 1
C
      IF ( MODE.EQ.1 ) THEN ! Run 1a SSQ files
        LVERT = 0
      ELSE IF( MODE.EQ.2 ) THEN ! Run 1b SSQ files
        LMSSQ = LQ(LHEAD-IZUSER)
        IF (LMSSQ.GT.0) THEN
          LVERT = LQ(LMSSQ-4)
        ENDIF
      ELSE
        LZTRK = LQ(LMUON-13)
        IF ( LZTRK.LE.0 ) GOTO 999
        LVERT = LQ(LZTRK-2)
      ENDIF
      IF ( LVERT.EQ.0 ) GOTO 999
C
      POS(1) = Q(LVERT+3)
      POS(2) = Q(LVERT+4)
      POS(3) = Q(LVERT+5)
      ERR(1) = Q(LVERT+6)
      ERR(2) = Q(LVERT+7)
      ERR(3) = Q(LVERT+8)
C
      IER = 0
C
  999 RETURN
      END
