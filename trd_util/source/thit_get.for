      SUBROUTINE THIT_GET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode THIT bank globally
C-
C-   Inputs  : THIT bank
C-   Outputs :FIRST_INFO(k)=pointer to first information for wire k
C-                          (from 1 to 1792) in bank THIT
C-   Controls:
C-
C-   Created   1-NOV-1993   A. Zylberstejn
C-   Updated  21-MAR-1994   A. Zylberstejn : read gas and hv constants form THIT
C-   Updated   3-FEB-1995   A. Zylberstejn  read uranium information
C-   Updated   1-MAR-1995   L. T. Goss      fixed RUN1A compatibility problem
C-   Updated  24-MAR-1995   A. Zylberstejn   Divide by 1000 the quantities in the thit bank
C-   to get the correct HV (in kV)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CLUREC.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:THIT_INFO.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
C      INCLUDE 'D0$INC:TRHITW.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LAYER,TCHNB
      INTEGER EXTD,WIRE
C      integer fir(ntot_wire_trd)
      INTEGER LOUT,TRUNIT,VERSION
      INTEGER I,II,GZTHIT,LSH,NCL,JBYT,LTHIT,NTOT
C
      LOGICAL FIRST,DOPRINT, TRD_DO_PRINT
      INTEGER J,K
      INCLUDE 'D0$LINKS:IZTROP.LINK'
      INTEGER LTROP,NDEV_GAS,NDEV_HV
      PARAMETER (NDEV_GAS = 15)
      PARAMETER (NDEV_HV = 48)
      INTEGER NDEV_URAN
      PARAMETER (NDEV_URAN = 14)
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
      END IF
      DOPRINT=TRD_DO_PRINT()
      IF(NWIRE_PER_LAYER(1).LE.0)CALL TRD_NWIRE_PER_LAYER
      DO I=1,6
        DO J=1,NWIRE_PER_LAYER(I)
          TWCOD(TCHNB(J,I))=.FALSE.
C        FIR(tchnb(j,I))=0
          FIRST_INFO(TCHNB(J,I))=0
        END DO
      END DO
      LTHIT = GZTHIT( )
      IF(LTHIT.LE.0)THEN
        CALL ERRMSG('Thit bank does not exist','THIT_GET',' ','W')
        GO TO 999
      END IF
      VERSION=MOD(IQ(LTHIT+1),10)
      IF (VERSION.LT.2) THEN
        CALL ERRMSG('Insufficient info. for full TRD RECO','THIT_GET',
     &    ' ','W')
      ENDIF
      NTOT=2
      DO I=1,IQ(LTHIT+2)  ! Loop on the hits
        LSH=LTHIT+NTOT
        II=IQ(LSH+1)
        K=JBYT(II,1,1)+1 ! k=1 anode,k=2 cathode
        IF(VERSION .GE.2)THEN
          WIRE= JBYT(II,2,9)+1
          LAYER=JBYT(II,11,2)+1+(K-1)*3 ! layer
          NCL=JBYT(II,13,4)! number of clusters
        ELSE
          WIRE= JBYT(II,2,8)+1
          LAYER=JBYT(II,10,2)+1+(K-1)*3 ! layer
          NCL=JBYT(II,12,3)! number of clusters
        END IF
C        IF(FIR(tchnb(wire,LAYER)).LE.0)then
C          FIR(tchnb(wire,LAYER))=NTOT
C          print*,' in thit_get, wire,layer',wire,layer,' fir',
C     &      fir(tchnb(wire,layer)),
C     &      ' first_infost_info',first_info(tchnb(wire,layer))
C          end if
        IF(FIRST_INFO(TCHNB(WIRE,LAYER)).LE.0)
     &              FIRST_INFO(TCHNB(WIRE,LAYER))=NTOT
        EXTD=JBYT(II,17,1)
        TWCOD(TCHNB(WIRE,LAYER))=.TRUE.
        NTOT=NTOT+2
        IF(VERSION .GE.2)THEN
          IF(NCL.NE.0) NTOT=NTOT+NCL*(1+EXTD)
        ELSE
          IF(NCL.NE.0) NTOT=NTOT+2
        END IF
      END DO
  100 CONTINUE
C  Retrieve information forn THIT for gas and HV information
      IF(IQ(LTHIT+1).GT.3)THEN
        I=IQ(LTHIT+1)/10
        LSH=LTHIT+I-1
        LTROP=LC(LTGEN-IZTROP)
        IF(DOPRINT)
     +    WRITE(LOUT,*)' in tredep,ntot',NTOT,' lthit',LTHIT,
     &    ' ltrop',LTROP,' mot 1',I
        IF (LTROP.GT.0) THEN
          IC(LTROP+1)=IQ(LSH+1)! = DATF
          IC(LTROP+2)=IQ(LSH+2)! = TIMF
          C(LTROP+3) =IQ(LSH+3)!= VAL_CANARY(1) ! gain with 50% half max
          C(LTROP+4) =IQ(LSH+4)!= VAL_CANARY(3) ! slope (%)
          C(LTROP+5) =IQ(LSH+5)!= VAL_CANARY(5) ! gain with 80% half max
          DO  I = 1,NDEV_HV
            C(LTROP+5+I)=IQ(LSH+5+I)/1000.   ! = VAL_HV(I)
          END DO
          DO  I = 1,NDEV_GAS
            C(LTROP+5+NDEV_HV+I)=FLOAT(IQ(LSH+5+NDEV_HV+I))*.1 ! = VAL_GAS(I)
          END DO
C    to keep compatibility with previous LTROP structure (JFG)
          C(LTROP+6+NDEV_HV+NDEV_GAS)=IQ(LSH+6+NDEV_HV+NDEV_GAS)!
          C(LTROP+7+NDEV_HV+NDEV_GAS)=IQ(LSH+7+NDEV_HV+NDEV_GAS)!
          C(LTROP+8+NDEV_HV+NDEV_GAS)=IQ(LSH+8+NDEV_HV+NDEV_GAS)!
          IF(DOPRINT)
     +      WRITE(LOUT,*)' in thit_get,gain',C(LTROP+5),
     +      ' val_hv', C(LTROP+8+NDEV_HV+NDEV_GAS)
C  get uranium values from bank(3/feb/1995)
          IF(VERSION.GE.4)THEN
            C(LTROP+NDEV_HV+NDEV_GAS+ 9)=IQ(LSH+9+NDEV_HV+NDEV_GAS)! = VAL_URAN1(1)
            C(LTROP+NDEV_HV+NDEV_GAS+10)=FLOAT(IQ(LSH+10+NDEV_HV+
     &        NDEV_GAS))/1000.! = VAL_URAN1(3)
            DO I= 1,NDEV_URAN-3
              C(LTROP+NDEV_HV+NDEV_GAS+10+I)=FLOAT(IQ(LSH+10+I+NDEV_HV+
     &          NDEV_GAS))/10.
            ENDDO
            C(LTROP+NDEV_HV+NDEV_GAS+22)=IQ(LSH+22+NDEV_HV+NDEV_GAS)! VAL_URAN2(1)
            C(LTROP+NDEV_HV+NDEV_GAS+23)=FLOAT(IQ(LSH+23+NDEV_HV+
     &        NDEV_GAS))/1000. ! VAL_URAN2(3)
            DO I = 1,NDEV_URAN-3
              C(LTROP+NDEV_HV+NDEV_GAS+23+I)=FLOAT(IQ(LSH+23+I+NDEV_HV+
     &          NDEV_GAS))/10.
            ENDDO
          END IF
        END IF
      END IF
  999 RETURN
      END
