      SUBROUTINE PRTHIT ( PRUNIT, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'THIT'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  22-JUL-1991 16:29:49.47  A. Zylberstejn
C-   Updated  21-APR-1994   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
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
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IR,IER,NBMOT,IMOT,JR,NW,LAYER,TCHNB
      INTEGER EXTD,WIRE,WI,LA,IFL
      INTEGER PRUNIT,TRUNIT,VERSION
      INTEGER I,II,GZTHIT,LSH,NCL,JBYT,JJ,LDUMMY,NTOT,LTHIT
C      REAL WG,PEDES,CORE(3)
C
C      REAL TRHITS(256,6)
      REAL VASUM
      LOGICAL FIRST
      INTEGER J,K,DEPTH
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
      ENDIF
      LTHIT = GZTHIT( )
      IF(LTHIT.LE.0)THEN
        CALL ERRMSG('Thit bank does not exist','THIT_UNPACK',' ','W')
        GO TO 999
      END IF
      VERSION=MOD(IQ(LTHIT+1),10)
      IF(IFL.LE.0)THEN
        WRITE(PRUNIT,1001)IQ(LTHIT+2),VERSION
      END IF
      NTOT=2
      DO I=1,IQ(LTHIT+2)  ! Loop on the hits
C        write(prunit,*)' hit',i,' ntot',ntot
        LSH=LTHIT+NTOT
        II=IQ(LSH+1)
        K=JBYT(II,1,1)+1 ! k=1 anode,k=2 cathode
        IF(VERSION .GE.2)THEN
          WIRE= JBYT(II,2,9)+1
          LAYER=JBYT(II,11,2)+1+(K-1)*3 ! layer
          NCL=JBYT(II,13,4)! number of clusters
        ELSE
          WIRE = JBYT(II,2,8)+1
          LAYER=JBYT(II,10,2)+1+(K-1)*3 ! layer
          NCL=JBYT(II,12,3)! number of clusters
        END IF
        IF(IFL.LE.0)THEN
C        WRITE(PRUNIT,1006)
 1006     FORMAT(' layer  wire nb. of clust E tot  Tmin')
          WRITE(PRUNIT,1008)LAYER,WIRE,NCL,
     &      FLOAT(IQ(LSH+2))*.1,JBYT(II,18,8)
        END IF
 1008   FORMAT('layer',I2,' wire',I4,'nb. of clust:',I3,' energy:',
     &    F8.1,' tmin',I4)
        EXTD=JBYT(II,17,1)
        NTOT=NTOT+2
        IF(IFL.LE.1)THEN
          WRITE(PRUNIT,1012)
        END IF
 1012   FORMAT(' cluster energy center left right peak')
        JJ=2
        IF(NCL.NE.0)THEN
          DO IR =  1,NCL
            JJ=JJ+1
            ECLR(IR) = JBYT(IQ(LSH+JJ),1,13)!get clusters energy
            IBCEN(IR)= JBYT(IQ(LSH+JJ),14,7)!get clusters position
            IF(EXTD.NE.0)THEN
              IBLFT(IR)=JBYT(IQ(LSH+JJ+1),1,8)! left position
              IBRGHT(IR)=JBYT(IQ(LSH+JJ+1),9,8)! right position
              YSUP(IR)=JBYT(IQ(LSH+JJ+1),17,8)! peak
              JJ=JJ+1
            END IF
            IF(IFL.LE.1)THEN
              WRITE(PRUNIT,1014)ECLR(IR),IBCEN(IR),IBLFT(IR),IBRGHT(IR),
     &          YSUP(IR)
 1014         FORMAT(10X,F5.0,2X,I4,2X,I4,2X,I4,F5.0)
            END IF
          END DO ! end of loop on clusters
          NTOT=NTOT+NCL*(1+EXTD)
        END IF
      END DO
      IF(IQ(LTHIT+1).GT.3)THEN
        I=IQ(LTHIT+1)/10
        LSH=LTHIT+I-1
        WRITE(PRUNIT,*)' canary gain',Q(LSH+5),'tcan',Q(LSH+56),
     &    ' ttrd',Q(LSH+57)
      END IF
 1001 FORMAT(' TRD hit Bank TRHIT for',I5,' hits; version',
     &  I5)
  999 RETURN
      END
