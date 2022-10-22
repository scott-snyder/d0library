      SUBROUTINE PTRD_GET_TRACK(WIRE,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get TRD track (TRDT and TPRL) from wire and layer
C-
C-   Inputs  :
C-   Outputs :      IWS(1+(j-1)*10)       =nb. of hit cells in layer j
C-                  IWS(2,3,4,5 +(j-1)*10)= hit cells layer j
C-                  IWS(6,7,8,9 +(j-1)*10)=cell energies  layer j
C-                   WS(100)              =  total energy
C-                   WS(101)              = truncated mean

C-   Controls:
C-
C-   Created  17-FEB-1994   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:TRD_NWORD.INC'
c      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:worksp.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ICH,IER,IUCOMP,J,k
      INTEGER LAYER,LTRDT,GZTRDT,LTPRL,NHITA,WIRE
      REAL VNTPRL
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C=====================================================================
  900 CONTINUE
      IF(FIRST)THEN
        FIRST=.FALSE.
      END IF
      IWS(1) =0  ! nb. of hit cells in layer 1
      IWS(11)=0 ! nb. of hit cells in layer 2
      IWS(21)=0 ! nb. of hit cells in layer 3
      WS(100)=0.! total energy
      WS(101)=0.!truncated mean
      call vzero(real_word,nword)
      call vzero_i(integer_word,nword)
      LTRDT=GZTRDT()
      DO WHILE (LTRDT.NE.0)
        LTPRL=LQ(LTRDT-LAYER)
        IF(LTPRL.LE.0)GO TO 50
C unpack wire energies
        CALL UNPACK_TPRL(LTPRL,VNTPRL,REAL_WORD,INTEGER_WORD,IER)
        IF(IER.NE.0)GO TO 50
C check if there are any anode hits in this layer
        NHITA=INTEGER_WORD(4)
        IF(NHITA.LE.0)GO TO 50
C  look if hit belongs to a track
        IF(IUCOMP(WIRE,INTEGER_WORD(51),NHITA).LE.0)GO TO 50
          J=(layer-1)*10
          NHITA=MIN0(4,NHITA)
          IWS(J+1)=NHITA
          CALL UCOPY_i(INTEGER_WORD(51),IWS(2+J),NHITA)! cell numbers
          CALL UCOPY(REAL_WORD(51),IWS(6+J),NHITA)   ! energies
        DO 40 ICH=1,3
          IF(ICH.EQ .LAYER)GO TO 40
          LTPRL=LQ(LTRDT-ICH)
          IF(LTPRL.LE.0)GO TO 40
C unpack wire energies
          CALL UNPACK_TPRL(LTPRL,VNTPRL,REAL_WORD,INTEGER_WORD,IER)
          IF(IER.NE.0)GO TO 40
C check if there are any anode hits in this layer
          NHITA=INTEGER_WORD(4)
          IF(NHITA.LE.0)GO TO 40
          J=(ICH-1)*10
          NHITA=MIN0(4,NHITA)
          IWS(J+1)=NHITA
          CALL UCOPY_i(INTEGER_WORD(51),IWS(2+J),NHITA)! cell numbers
          CALL UCOPY(REAL_WORD(51),IWS(6+J),NHITA)   ! energies
   40   CONTINUE
        WS(100)=Q(LTRDT+4)
        WS(101)=Q(LTRDT+5)
        GO TO 999
   50   CONTINUE
        LTRDT=LQ(LTRDT)
      END DO
  995 CONTINUE
  999 RETURN
      END
