      SUBROUTINE MOTRFL(VERSION,JREG,JDIP,KDATA,JMOTR)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill MOTR bank with KTABLE info 
C-   for one OTC trigger
C-
C-   Inputs  :  VERSION = <10 run 1A, >10 run 1B
C-              JREG    = OTC region
C-              JDIP    = OTC DIP switch
C-              KDATA(2)= Ktable words
C-              JMOTR   = -1 to compress bank
C-
C-   Outputs :  JMOTR   = pointer to data stored
C-
C-   Created :  1/28/94  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C                              
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C                                                          
      INTEGER LMOTR,GZMOTR
      EXTERNAL GZMOTR
C
      INTEGER VERSION,JREG,JDIP,KDATA(2)
      INTEGER I,ILOC,ITEMP,ICENT,JMOD,IMOD
      INTEGER IPT,IPTBIT,ICCT,JOTC
      INTEGER IMOTR,JMOTR,KMOTR,NWD
      SAVE KMOTR
C
C       Book bank or compress bank if requested
C
      LMOTR = GZMOTR(0)
      IF (LMOTR .EQ. 0) THEN
          CALL BKMOTR(0,0,LMOTR)
          KMOTR = 0
      END IF
      IF (JMOTR.EQ.-1) THEN
          NWD = KMOTR*8 - IQ(LMOTR-1)
          CALL MZPUSH(IXCOM,LMOTR,0,NWD,'I')
          JMOTR = KMOTR
          RETURN
      END IF
C
C       Initialize
C
      IMOTR = LMOTR + KMOTR*8
      DO I = 1,8
          IQ(IMOTR+I) = 0
      END DO
      KMOTR = KMOTR + 1
      JMOTR = KMOTR
      CALL MOTNUM(JREG,JDIP,JOTC,ITEMP)
C
C       Get table match bits
C
      IPT = 1
      ICCT = 0
      IF (VERSION.GT.10) THEN
          IPT = IBITS(KDATA(1),28,1)
          ICCT = IBITS(KDATA(1),29,1)
      END IF
      IPTBIT = IPT + 2
      IQ(IMOTR+4) = IPT
      IQ(IMOTR+5) = ICCT
C
C       Load ktable data
C
      DO I = 1,3
          ILOC = 8*(I-1)
C
C       Get module information
C
          JMOD = IBITS(KDATA(1),ILOC,3)
          IF (JREG.GE.4) THEN
              IF (JDIP.LE.3) THEN                      ! SAMUS A,B
                  IF (I.EQ.1) THEN
                      JMOD = IBITS(KDATA(1),ILOC+7,1)
                  ELSE IF (I.EQ.2) THEN
                      JMOD = IBITS(KDATA(1),ILOC+5,1)
                  END IF
              ELSE IF (JDIP.EQ.10.OR.JDIP.EQ.11) THEN  ! SA Pair
                  JMOD = IBITS(KDATA(2),ILOC,1)
              ELSE                                     ! SA Road / OV Pair
                  JMOD = IBITS(KDATA(2),ILOC+7,1)
              END IF
          END IF
          CALL MOTDAT(JREG,JDIP,I,JMOD,IMOD)
C
C       Mark modules with centroids
C
          IF (JOTC.GE.700) THEN
              CALL MOTMOD(4,IMOD,IPTBIT)
              IF (IPTBIT.EQ.3) CALL MOTMOD(4,IMOD,2)
          END IF
C
C       Get centroid information
C
          ICENT = IBITS(KDATA(2),ILOC,8)
          ICENT = ICENT + 64*IBITS(KDATA(1),ILOC+6,1)
          IF (VERSION.GT.10.AND.(JREG.EQ.1.OR.
     &        (JREG.LE.3.AND.I.EQ.1))) THEN            ! Run 1B encoding
              ICENT = ICENT - (JMOD-1)*48
          END IF
          IF (JREG.GE.4) THEN
              IF (JDIP.LE.3.AND.I.EQ.1) THEN           ! SAMUS A
                  ITEMP = IBITS(KDATA(1),ILOC,7)
                  ICENT = IAND(ITEMP,7) + 8*(ITEMP/16)
              ELSE IF (JDIP.EQ.10.OR.JDIP.EQ.11) THEN  ! SA Pair
                  ITEMP = IBITS(KDATA(1),ILOC,8)
                  ICENT = IAND(ITEMP,63)*2 + ITEMP/128
              ELSE                                     ! SA Road / OV Pair
                  ICENT = IBITS(KDATA(2),ILOC,7)
              END IF
          END IF
          IQ(IMOTR+I) = IMOD*256 + ICENT
      END DO
      IF (JREG.GE.4) THEN
          IF (JDIP.GE.12) THEN                        ! SA Road
              ITEMP = IBITS(KDATA(1),16,8)
              IQ(IMOTR+6) = IAND(ITEMP,64)/64
              IQ(IMOTR+7) = IAND(ITEMP,63)*2 + IAND(ITEMP,128)/128
          ELSE IF (JDIP.GE.4.AND.JDIP.LE.9) THEN      ! OV Pair
              ITEMP = IBITS(KDATA(1),8,8)
              IQ(IMOTR+6) = ITEMP/32
              IQ(IMOTR+7) = IBITS(KDATA(1),16,6) + IAND(ITEMP,16)*4
          END IF
      END IF
C
      RETURN
      END
