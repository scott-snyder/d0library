      SUBROUTINE PTGTRD(TMAX,NTFADC,WIRE,TLAY,TWIR,TFADC,NHITWI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GIVEN A WIRE NUMBER UNPACKED ZEBRA BANK 
C-                  LOOK FOR TRD RAW DATA OF THIS WIRE AND SEND IT 
C-                  BACK IN TFADC.
C-
C-
C-  CREATED:  12/20/88.
C-
C-  BASED ON GTRDFA
C-
C-  AUTHOR:
C-     LUPE ROSAS
C-   Updated  23-MAR-2004   compile with g77.
C-    
C---------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FADCCN.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC/LIST'
      INCLUDE 'D0$LINKS:IZCDD4.LINK/LIST'
C      INCLUDE '[LUPE.TRD]TRDBLK.FOR/LIST'
      INTEGER WIRE,NTFADC,TMAX
      INTEGER TLAY(*),TWIR(*)
      REAL TFADC(128,*)
      INTEGER CHA,CHL, I,IADR,ICR,ILAY,ISH,ISUB,ITRIP,IWIRE,J, K,LL
      INTEGER IPR,IS,JSH,MASK16, NCHAN,ND,IEND,NBBYT,NUP,JJ
      DATA MASK16/65535/
      LOGICAL NHITWI ! FLAG FOR NO HITS
C----------------------------------------------------------------------
      DO 10 J= 1, 128     ! CLEANING ARRAYS 
        DO 10 I=1, TMAX   
         TLAY(I)=0.
         TWIR(I)=0.
         TFADC(J,I)=0.0
  10  CONTINUE
      
      NHITWI = .TRUE.
      NTFADC=0
      IF ( LHEAD .EQ. 0 ) THEN
        WRITE ( LOUT, *)  '  **** Error in GTFTRD ',
     $                    ': Header bank LHEAD not booked'
        GO TO 999
      ENDIF
      IPR=0
      LL = LQ ( LHEAD - IZCDD4 )
      IF ( LL .EQ. 0 ) THEN
      WRITE ( LOUT, *)  '  **** Error in PRCDD4 ',
     $                    ': bank CDD4 not booked'
        GO TO 999
      END IF
      ND=IQ(LL-1)
      IEND=LL+ND
      ISH=32/NDATW
   20 IF(LL.LE.0)GO TO 999
      CHL=IQ(IEND).AND.MASK16              !CHANNEL LENGTH
      CHA=ISHFT(IQ(IEND),-16).AND.MASK16  !CHANNEL NUMBER
      NCHAN=IQ(IEND-1).AND.MASK16          !LENGTH
      NCHAN=(NCHAN-2)/4                  !NUMBER OF DATA WORDS
      IF(NCHAN.LE.0)GO TO 999
C  DECORTICATE THE CHANNEL ADDRESS
      ISUB=CHA.AND.15               !SUB-ADDRESS IN THE FADC MODULE
      ICR=CHA/256                   !CRATE NUMBER
      IADR=ISHFT(CHA,-4).AND.15   !ADDRESS IN THE CRATE
      ITRIP=IADR/3                  !TRIPLET NUMBER
      ILAY =IADR-ITRIP*3+1          !LAYER NUMBER
      IWIRE=ISUB+ITRIP*16+MOD(ICR,4)*64+1
      K=0
      NBBYT=CHL-3
      NUP=NBBYT/4
      IS=IEND-2-NUP
      IEND=IS
C  SEARCHING FOR RIGHT WIRE
      IF (IWIRE.NE.WIRE) THEN
        IF (IS.GT.LL) THEN
          GO TO 20       ! GO BACK KEEP SEARCHING
        ELSE
          GO TO 999       ! RETURN
        ENDIF
      ENDIF
C--SET DATA IN ARRAY AND GO BACK IF MORE.
      NTFADC=NTFADC+1
      IF(NTFADC.GT.TMAX) THEN
        GO TO 999
      ENDIF
      TLAY(NTFADC)=ILAY
      TWIR(NTFADC)=IWIRE
      NHITWI = .FALSE.
      DO 40 I=1,NCHAN      !  SETTING DATA
        JJ=IS+I
C Store FADC contents in COMMON PTRD
        DO 38 J=1,4
          K=K+1
          JSH=ISH*(4-J)
          TFADC(K,NTFADC)=ISHFT(IQ(JJ),-JSH).AND.255
   38   CONTINUE
   40 CONTINUE
      IEND=IS
      IF(IS.GT.LL)GO TO 20
      
  999 RETURN
      END
