      SUBROUTINE FLVSN_MTRH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill MUON STP version information into MTRH 
C-                         Bank
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-JAN-1993   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INTEGER LMTRH, GZMTRH
      INTEGER LUP
      INTEGER GZMGEH,GZMMAH, GZSMAH, GZSSTH, GZSBPH
      INTEGER LMUD1, GZMUD1
      INTEGER W1
      INTEGER M8
      PARAMETER (M8=255)   ! 8bit mask
C----------------------------------------------------------------------
      LMTRH = GZMTRH(1)            ! find MTRH bank
      IF ( LMTRH.EQ.0 ) GOTO 999
C
      IF ( LHEAD.EQ.0 ) GOTO 999
      IF ( IQ(LHEAD+1).LE.1000 ) GOTO 1000
C
C  for MC
C
      LMUD1 = GZMUD1(1)
      IF ( LMUD1.EQ.0 ) GOTO 1000
C
      IQ(LMTRH+2) = IQ(LMUD1+5)
      IQ(LMTRH+3) = IQ(LMUD1+6)
C
C  for recontstruction
C
 1000 CONTINUE    ! reconstruction information
C                                  ! MGEH
      W1 = 0
      LUP = GZMGEH(1)
      IF ( LUP.NE.0 ) THEN
        W1 = IAND( IC(LUP+1), M8 ) 
      END IF
C
      LUP = GZSSTH()
      IF ( LUP.NE.0 ) THEN
        W1 = IOR(W1, ISHFT(IAND( IC(LUP+1), M8 ),8) ) 
      END IF
C
      LUP = GZSMAH()
      IF ( LUP.NE.0 ) THEN
        W1 = IOR(W1, ISHFT(IAND( IC(LUP+1), M8 ),16) ) 
      END IF
C
      LUP = GZSBPH()
      IF ( LUP.NE.0 ) THEN
        W1 = IOR(W1, ISHFT(IAND( IC(LUP+1), M8 ),24) ) 
      END IF
      IQ(LMTRH+4) = W1
C
      W1 = 0
      LUP = GZMMAH(1)
      IF ( LUP.NE.0 ) THEN
        W1 = IAND( IC(LUP+1), M8 ) 
      END IF
      IQ(LMTRH+5) = W1
C
  999 RETURN
      END
