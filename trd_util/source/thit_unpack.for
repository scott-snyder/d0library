      SUBROUTINE THIT_UNPACK(WIRE,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode THIT bank
C-
C-   Inputs  :WIRE, LAYER (starting from 1)
C-   Outputs :Decoded content of bank THIT: total energy on WIRE (in ws(2001))
C-                                          clusters
C-   Controls:
C-
C-   Created   1-NOV-1993   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:clurec.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:THIT_INFO.INC'
      INCLUDE 'D0$INC:ZEBWRK.INC'
C      INCLUDE 'D0$INC:WORKSP.INC'
C   -------COMMON,WORKSP :DUMMY COMMON TO USE ARRAYS NOT TRANSMITTED FROM
C                         ONE SUBROUTINE TO ANOTHER.MAY BE REPLACED BY
C                         'ZEBRA' WORKING SPACE.
      INTEGER LENGWS
      PARAMETER (LENGWS=5000)
C      COMMON/WORKSP/WS(LENGWS)
      REAL WS(LENGWS)
      INTEGER IWS(LENGWS)
      EQUIVALENCE(WS,IWS,W(1001))
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IR,IER,NBMOT,IMOT,JR,NW,LAYER,TCHNB
      INTEGER EXTD,WIRE,WI,LA
      INTEGER LOUT,TRUNIT,VERSION
      INTEGER I,II,GZTHIT,LSH,NCL,JBYT,JJ,LTHIT,NTOT
C      REAL WG,PEDES,CORE(3)
C
C      REAL TRHITS(256,6)
      REAL VASUM
      LOGICAL FIRST
      INTEGER J,K,DEPTH
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        LOUT=TRUNIT()
      ENDIF
      LTHIT = GZTHIT( )
      IF(LTHIT.LE.0)THEN
        CALL ERRMSG('Thit bank does not exist','THIT_UNPACK',' ','W')
        GO TO 999
      END IF
      VERSION=IQ(LTHIT+1)
      LSH=FIRST_INFO(TCHNB(WIRE,LAYER))+LTHIT
      II=IQ(LSH+1)
      K=JBYT(II,1,1)+1 ! k=1 anode,k=2 cathode
      WS(2003)=0
      IF(VERSION.GE.2)THEN
        WI= JBYT(II,2,9)+1
        LA=JBYT(II,11,2)+1+(K-1)*3 ! layer
        NCL=JBYT(II,13,4)! number of clusters
        IF(VERSION.GE.3)WS(2003)=JBYT(II,18,7)
      ELSE
        WI= JBYT(II,2,8)+1
        LA=JBYT(II,10,2)+1+(K-1)*3 ! layer
        NCL=JBYT(II,12,3)! number of clusters
        NCL=MIN0(NCL,4)
      END IF
      CALL SBYT(IWS(2002),IQ(LSH+1),25,8)
      EXTD=JBYT(II,17,1)
      IF(WI.NE.WIRE .OR. LA.NE.LAYER)THEN
          CALL ERRMSG(' Problem unpacking TTHIT',
     &      'THIT_UNPACK',' ','W')
c        PRINT*,' erreur dans thit_unpack,wire,layer',WIRE,LAYER,
c     &      'wi,la',WI,LA,' tchnb',tchnb(wire,layer),'first_info',
c     &      first_info(tchnb(wire,layer))
      END IF
      WS(2001)=FLOAT(IQ(LSH+2))*.1
      IF(NCL.NE.0)THEN
        NTOT=2
        IF(VERSION.GE.2)THEN
          DO IR =  1,NCL
C          JR=JR+1
            ECLR(IR) = JBYT(IQ(LSH+NTOT+1),1,13)!get clusters energy
            IBCEN(IR)= JBYT(IQ(LSH+NTOT+1),14,7)!get clusters position
            IF(EXTD.NE.0)THEN
              IBLFT(IR)=JBYT(IQ(LSH+NTOT+2),1,8)! left position
              IBRGHT(IR)=JBYT(IQ(LSH+NTOT+2),9,8)! right position
              YSUP(IR)=JBYT(IQ(LSH+NTOT+2),17,8)! peak
              NTOT=NTOT+1
            END IF
            NTOT=NTOT+1
          END DO
        END IF
      ELSE !version 1
        DO IR =  1,NCL
          ECLR(IR) =FLOAT(JBYT(IQ(LSH+4),(IR-1)*8+1,8))
          IBCEN(IR)=JBYT(IQ(LSH+3),(IR-1)*8+1,8)
        END DO
        NTOT=NTOT+2
      END IF
      NCREC=NCL
  999 RETURN
      END
