C DEC/CMS REPLACEMENT HISTORY, Element MUVERN.FOR
C *5    10-APR-1989 13:22:02 TAMI "Add upper limit"
C *4     4-MAR-1989 15:23:31 HEDIN "fix divide by zero"
C *3    16-OCT-1988 14:12:45 TAMI "Try lookup table"
C *2    30-AUG-1988 16:44:28 TAMI "Add IORENT and WLEN to argument list"
C *1    19-DEC-1986 18:27:15 HEDIN "Muon Utilities initial versions"
C DEC/CMS REPLACEMENT HISTORY, Element MUVERN.FOR
      SUBROUTINE MUVERN(IORENT,PAD1,PAD2,VOFF,WLEN,X1,X2)
CC    ==========================================================
C     CALCULATES THE VERNIER POSITIONS GIVEN THE PULSE HEIGHT ON
C     EACH PAD. THE 2 POSSIBLE SOLUTIONS ARE IN CM FROM THE
C     CENTER OF THE WIRE FOR THE FIRST SOLUTION (I.E. ALWAYS
C     NEGATIVE NUMBERS)
C     INPUT -- PAD1,PAD2  pads corrected for peds and gains
C              VOFF,WLEN  vernier offset and wire length
C              IORENT -- module orientation; if negative then electronics
C                        are at maximum end
C     OUTPUT-- X1,X2   two solutions
C     HEDIN 12-6-85, 8/88
C     DH 10/88 TRY LOOKUP TABLE
C     DH 2/89 ADD UPPER LIMIT
C       sew 10/90 modify for monte carlo
C     DH 4/91 WORKS EITHER FOR MC OR DATA
C     SI 11/91 FOR MC digitization version 2
C     TM better response
C     ===========================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IORENT,N,MC,IFIRST,L,GZMUD1,NUMVER
      REAL PAD1,PAD2,X1,X2,QQ,DX,VOFF,WLEN,VERNIR,DELQ,ERROR,RIT
      REAL MRPADR
      DATA NUMVER/536875808/              ! '20001320'X
      DATA MC,IFIRST/0,0/
      DATA ERROR/5./     ! THIS ALLOWS THE FIRST VERNIER POINT TO BE
CC  SLIGHTLY OUTSIDE THE PHYSICAL LIMITS OF THE CHAMBER
      DATA VERNIR/60.96/             ! SIZE OF PADS
      IF(IFIRST.EQ.0) THEN
        IFIRST=1
        L=GZMUD1(0)
        IF(IQ(L+4).EQ.1) MC=1      ! MONTE CARLO
        IF(IQ(L+4).EQ.NUMVER) MC=2 ! NEW MONTE CARLO
      ENDIF
      X1=-99999.
      X2=-99999.
Cc ******************bump up minimum pad pulse*******************
Cc ********** but in the interest of keeping all values and flagging
Cc***********with large errors later pad limits remain loose
      IF(PAD1.GT.10..AND.PAD2.GT.10..AND.PAD1.LT.4095..AND.PAD2.
     A   LT.4095.) THEN
        IF(MC.EQ.1) THEN                      ! MONTE CARLO
          QQ=(PAD1-PAD2)/(PAD1+PAD2)
          DX=(1.+QQ)*VERNIR/4.
        ELSEIF(MC.EQ.2) THEN                  ! NEW MONTE CARLO
          QQ=(PAD2-PAD1)/(PAD1+PAD2)
          DX=MRPADR(QQ)
        ELSE                                  ! DATA
          QQ=(PAD1-PAD2)/(PAD1+PAD2)
          IF(QQ.LT.-.68) QQ = -.68
          IF(QQ.GT..62) QQ = .62
C          IF(QQ.LT.-.8) QQ=-.8
          RIT = 202.4 + 220.0*QQ
          IF (RIT .LT.351.93) THEN
            DX =12.76 -SQRT(RIT)
            DX = 2.54*DX
            DX = VERNIR/4. + DX
          ELSE
            DX =0.
          ENDIF
Cccccccccccccc******
          IF(QQ.LT.-.1.AND.QQ.GE.-.3) DX=DX - 2.25*(.1+QQ)
          IF(QQ.LT.-.3.AND.QQ.GE.-.5) DX=DX+.45 + .25*(.3+QQ)
          IF(QQ.LT.-.5.AND.QQ.GE.-.66) DX=DX+.4+21000.*(-.5-QQ)**5.
          IF(QQ.GT..1.AND.QQ.LE..35) DX=DX-.08
          IF(QQ.GT..38.AND.QQ.LE..5) DX=DX+.05
          IF(QQ.GT..5.AND.QQ.LE..66) DX=DX-41000.*(QQ-.5)**5.
          IF(DX.LT.0.) DX=0.
          IF(DX.GT.30.48) DX=30.48
        ENDIF
CCC   GET FIRST 2 SOLUTIONS
        X1=-WLEN/2.+VOFF+DX-VERNIR
        X2=-WLEN/2.+VOFF-DX-VERNIR
        IF(X1.LT.-WLEN/2.-ERROR) X1=X1+VERNIR
        IF(X2.LT.-WLEN/2.-ERROR) X2=X2+VERNIR
        IF(IORENT.LT.0) THEN       ! WANT FROM OTHER END
          N=WLEN/VERNIR
          X1=-(X1+N*VERNIR)
          X2=-(X2+N*VERNIR)
          IF(X1.LT.-WLEN/2.-ERROR) X1=X1+VERNIR
          IF(X2.LT.-WLEN/2.-ERROR) X2=X2+VERNIR
        ENDIF
      ENDIF
      RETURN
      END
