      SUBROUTINE TCELL_IN_ROAD(VIN, VOUT, NAN, EAN, NCA, ECA, ISTAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods  : Compute hit TRD anode cells in a road within
C-   2 straight lines defined by VIN and VOUT.  The number of hits
C-   is returned as well as the energies of the anode wires and
C-   cathode strips in the road.
C-
C-   Inputs  : Vin : X0,Y0,Z0,Cx0,Cy0,Cz0
C-             Vout: X1,y1,Z1,Cx1,Cx1,Cz1
C-
C-   Outputs : Nan(3)   number of coded (hit) anode cells in each layer
C-             Ean(6,3) energy of each hit anode cell in each layer
C-             Nca(3)   number of coded (hit) cathode cells in each layer
C-             Eca(6,3) energy of each hit cathode cells in each layer
C-             istat = 0 if no errors occurred
C-   Controls: none
C-
C-   Created  25-OCT-1994   A. ZYLBERSTEJN
C-   Updated   1-DEC-1994   Steven M. Glenn
C-   Updated  22-DEC-1994   Steven M. Glenn  Added Cathode variables
C-   Updated   3-MAY-1995   A. ZYLBERSTEJN  Initialize number of wires if not
C-                                          done before
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMCEL ! max nb. of cells in a layer
      PARAMETER( NMCEL =  6)
      REAL VIN(6),VOUT(6),WOUT(6), EAN(NMCEL,3), ECA(NMCEL,3)
      INTEGER NAN(3), NCA(3), TCHNB, ISTAT
      INTEGER I,IAIN,IAOUT,ICIN,ICOUT,IGTRAK,IMI,IMS,J
      INTEGER ILAY, ICELL,RUNI,RUNNO
      REAL DPHI(3),PHITR,RIN
      REAL DPHIST, PHICAT
      REAL SPHOF(3),CPHOF(3),X,Y
      INTEGER GZTHIT,LOUT,TRUNIT
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:GEOMTC.INC/LIST'
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:THIT_INFO.INC'
      LOGICAL FIRST,RUN1A,TYP_RUN
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:ZEBWRK.INC'
C      include 'D0$INC:worksp.INC'
C   -------COMMON,WORKSP :DUMMY COMMON TO USE ARRAYS NOT TRANSMITTED FROM
C                         ONE SUBROUTINE TO ANOTHER.MAY BE REPLACED BY
C                         'ZEBRA' WORKING SPACE.
      INTEGER LENGWS
      PARAMETER (LENGWS=5000)
C      COMMON/WORKSP/WS(LENGWS)
      REAL WS(LENGWS)
      INTEGER IWS(LENGWS)
      EQUIVALENCE(WS,IWS,W(1001))
      INCLUDE 'D0$INC:zebcom.INC'
C
      DATA FIRST/.TRUE./
      DATA RUNI/0/
C----------------------------------------------------------------------
C
      IF(FIRST)THEN
        LOUT=TRUNIT()
        FIRST=.FALSE.
        IF(NWIRE_PER_LAYER(1).LE.0)CALL TRD_NWIRE_PER_LAYER
        RUNI=RUNNO()
        TYP_RUN=RUN1A()
C  typ_run= ture for run1b data
C
C  NOT ELEGANT:SHOULD BE DONE IN INITRD AND TRANSMITTED
C              THROUGH COMMON BLOCK
C
        DO ILAY = 1, 3
          CPHOF(ILAY)=COS(OFSDPH(ILAY))
          SPHOF(ILAY)=SIN(OFSDPH(ILAY))
          DPHI(ILAY)=2.0*PI/FLOAT(NWIRE_PER_LAYER(ILAY))
        ENDDO
      ENDIF
C
C
C  Zero cell energies
C
      IF(RUNNO().NE.RUNI)THEN !current run not equal to previous run
        RUNI=RUNNO()
        IF(TYP_RUN.NE.RUN1A())THEN ! switch from run 1a to 1b or vice versa
          TYP_RUN=RUN1A()
          CALL TRD_NWIRE_PER_LAYER
          DPHI(3)=2.0*PI/FLOAT(NWIRE_PER_LAYER(3))
        END IF
      END IF
      DO ILAY=1,3
        NAN(ILAY)=0
        NCA(ILAY)=0
        DO ICELL=1,NMCEL
          EAN(ICELL,ILAY) = 0.0
          ECA(ICELL,ILAY) = 0.0
        ENDDO
      ENDDO
      IF(GZTHIT().LE.0)THEN                   ! Check if THIT bank is present
        IF(LQ(LHEAD-IZCDD4).LE.0)THEN        ! If no THIT bank check for CDD4
          CALL ERRMSG(' not enough info',' TCELL_IN_ROAD',
     &                ' ','w')
          GO TO 999
        ELSE
          CALL TSETWC
        ENDIF
      ELSE                                     ! use THIT bank
        CALL THIT_GET
      ENDIF
      DO ILAY=1,3                              ! loop on the 3 layers
        RIN=RADAN(ILAY)
        NAN(ILAY)=0
        NCA(ILAY)=0
C
C  check if edges of the road cross the TRD layer
C
        CALL EXTCYL(VIN,WOUT,RIN,IGTRAK)
        IF(IGTRAK.NE.0)GO TO 100
        CALL UCOPY(WOUT,WS,6)
        CALL EXTCYL(VOUT,WOUT,RIN,IGTRAK)
        IF(IGTRAK.NE.0)GO TO 100
        CALL UCOPY(WOUT,WS(11),6)
        IF(ABS(WS(3)).GT.83.5 .AND. ABS(WS(13)).GT.83.5)THEN
          ISTAT = 1
          GO TO 100
        ENDIF
        ISTAT = 0
C
C  Look at one side of the road.
C
        X=CPHOF(ILAY)*WS(1)+SPHOF(ILAY)*WS(2)
        Y=-SPHOF(ILAY)*WS(1)+CPHOF(ILAY)*WS(2)
        PHITR=ATAN2(-Y,-X)+PI
        IAIN=PHITR/DPHI(ILAY)+1             ! anode cell crossed by the track
        PHICAT = PHITR - OFSDPH(ILAY)-DPHIDZ(ILAY)*VIN(3)+TWOPI
        DPHIST = AMOD(PHICAT-OFSCAT(ILAY),TWOPI)
        ICIN = DPHIST/DPHICA(ILAY) + 1     ! cathode cell crossed by the track
C
C  Look at the other side of the road.
C
        X=CPHOF(ILAY)*WS(11)+SPHOF(ILAY)*WS(12)
        Y=-SPHOF(ILAY)*WS(11)+CPHOF(ILAY)*WS(12)
        PHITR=ATAN2(-Y,-X)+PI
        IAOUT=PHITR/DPHI(ILAY)+1            ! anode cell crossed by the track
        PHICAT = PHITR - OFSDPH(ILAY)-DPHIDZ(ILAY)*VOUT(3)+TWOPI
        DPHIST = AMOD(PHICAT-OFSCAT(ILAY),TWOPI)
        ICOUT = DPHIST/DPHICA(ILAY) + 1    ! cathode cell crossed by the track
C
C  Get energies for anode cells in road.
C
        IMI=MIN0(IAIN,IAOUT)
        IMS=MAX0(IAIN,IAOUT)
        IF(IMS-IMI.GT.20)THEN               ! region around phi=0
          I=IMI
          IMI=IMS
          IMS=NWIRE_PER_LAYER(ILAY)+I
        ENDIF
        DO I=IMI,IMS
          J=I
          IF(J.GT.NWIRE_PER_LAYER(ILAY))
     &         J=I-NWIRE_PER_LAYER(ILAY)
          IF(TWCOD(TCHNB(J,ILAY)))THEN
            IF (NAN(ILAY).LT.NMCEL)
     &            NAN(ILAY)=NAN(ILAY)+1
            CALL THIT_UNPACK(J,ILAY)
            EAN(NAN(ILAY),ILAY)=WS(2001)
          ENDIF
        ENDDO                                  ! end of anode cell loop
C
C  Get energies for cathode cells in road.
C
        IMI=MIN0(ICIN,ICOUT)
        IMS=MAX0(ICIN,ICOUT)
        IF(IMS-IMI.GT.20)THEN                  ! region around phi=0
          I=IMI
          IMI=IMS
          IMS = NWIRE_PER_LAYER(ILAY+3)+I     ! Cathodes are layers 4-6
        ENDIF
        DO I=IMI,IMS
          J=I
          IF(J.GT.NWIRE_PER_LAYER(ILAY+3))
     &         J=I-NWIRE_PER_LAYER(ILAY+3)
          IF(TWCOD(TCHNB(J,ILAY+3)))THEN
            IF (NCA(ILAY).LT.NMCEL)
     &            NCA(ILAY)=NCA(ILAY)+1
            CALL THIT_UNPACK(J,ILAY+3)
            ECA(NCA(ILAY),ILAY)=WS(2001)
          ENDIF
        ENDDO                                 ! end of cathode strip loop
  100   CONTINUE
      ENDDO                                    ! end of layer loop
  999 RETURN
      END
