      SUBROUTINE MUIFW4(ITRAK)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill flag word 4 in MUOT with
C-                         track quality information by summing
C-                         over failures
C-
C-    Created :  5/92  D. Hedin
C-    Modified : 
C-     DH 12/92 donot use missing module cut for 3 layer tracks
C-     DH 1-93  ifw1=0,10 for lower angles
C-     TD 1-93  use quad instead of angle for missing mod cuts
C-              do not make cut if mixed orientation track
C-     TD 5-93  Flag SASBWC as IFW4 = 999 for now.
C-     TD 6-93  Flag AStubs as IFW4 = 0   for now.
C-     TD 11-93 Flag A-stubs which include hits on ABC track IFW4=4.
C-     MF 6/94  Tag single tracks
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LMUOT,GZMUOT,I1,I2,I3,I4,ITRAK,I
      INTEGER QUAD
      REAL THETD
C
      LMUOT=GZMUOT(ITRAK)
      IF (LMUOT.EQ.0) RETURN
C
      I1 = IQ(LMUOT+4)
      I2 = IQ(LMUOT+5)
      I3 = IBITS(IQ(LMUOT+6),0,16)
      I4 = 0
C
C   Test for SSW tracks
C
      IF (BTEST(I2,10)) THEN
          I4 = 999
          IQ(LMUOT+7) = I4
          RETURN
      ENDIF
C
C   Test for A stub tracks
C
      IF (I1.EQ.5) THEN
          IF (BTEST(I2,12)) THEN
              I4 = 4   ! This stub has hits from another ABC track. 
          ELSE
              I4 = 0
          ENDIF
          IQ(LMUOT+7) = I4
          RETURN
      ENDIF
C
C   Count all bad bend and non bend bits in IFW2
C
      DO I=0,3
          IF (BTEST(I2,I)) I4=I4+1
      ENDDO
C
C   Count missing modules in ends if 2 layer
C
      THETD = ACOS(Q(LMUOT+16))*180./3.14159
      IF (THETD.GT.90.) THETD=180.-THETD
      QUAD = IQ(LMUOT+3)
      IF (QUAD.GE.5.AND.I1.GE.11) THEN
          IF (I3.EQ.0) THEN
              IF (BTEST(I2,4)) I4=I4+1
              IF (BTEST(I2,5)) I4=I4+1
          ENDIF
      ENDIF
C
C   Require 3 layers in overlap region
C
      IF (QUAD.GE.5.AND.THETD.LT.20.) THEN
          IF(I1.NE.0.AND.I1.NE.10) I4=I4+1
      ENDIF
C
      IQ(LMUOT+7)=I4
C
  999 RETURN
      END
