      SUBROUTINE MUIFW2(ITRAK)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill flag word 2 in MUOT with
C-                         hits/planes type information
C-
C-    Created :  11-DEC-93  M. Fortner
C-    Modified : 6/94 MF   Use IBSET for setting bits, tag single tracks
C-    DH 3/95 donot use 150/250 modules for missing flag
C-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER I,ITRAK,NON,ION(10),NOFF,IOFF(10),LMUOT,GZMUOT
      INTEGER IA,IBC,K
      LOGICAL IHITPLNCUT
C
      LMUOT = GZMUOT(ITRAK)
      IF (LMUOT.EQ.0) RETURN
C
C   Flag bad hit-plane combinations
C
      IF (IQ(LMUOT+3).GE.5) THEN
          CALL MU_HITPLN_CUT(LMUOT,IHITPLNCUT)
          IF (IHITPLNCUT) THEN
              IQ(LMUOT+5) = IBSET(IQ(LMUOT+5),9)
          ENDIF
      ENDIF
C
C   Flag missing layers
C
      CALL MUMISS(ITRAK,NON,ION,NOFF,IOFF)
      IF (NOFF.GE.1) THEN
          IA = 0
          IBC = 0
          DO I = 1,NOFF
            K=IOFF(I)
            IF(K.NE.150.AND.K.NE.153.AND.K.NE.180.AND.K.NE.183.
     A AND.K.NE.250.AND.K.NE.251.AND.K.NE.253.AND.K.NE.255.AND.
     A     K.NE.280.AND.K.NE.281.AND.K.NE.283.AND.K.NE.285) THEN
              IF (IOFF(I).LE.99) IA=1
              IF (IOFF(I).GE.100) IBC=1
            ENDIF
          ENDDO
          IF (IA.EQ.1) IQ(LMUOT+5)=IBSET(IQ(LMUOT+5),4)
          IF (IBC.EQ.1) IQ(LMUOT+5)=IBSET(IQ(LMUOT+5),5)
      ENDIF
C
C    Flag cosmic rays
C
      CALL MUCTAG(ITRAK)
C
  999 RETURN
      END
