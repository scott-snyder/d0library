      SUBROUTINE VTXPUL(TDRIFT,AREA,ALENG,COORD,PULSE,DIGFLG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : simulate pulse shape by randomly taking
C-             pulse from the pulse library (there are sets of pulses
C-             for different drift distances and different polar angles
C-             of tracks
C-
C-   Inputs  : INHITS (in VTXPSH.INC):  I_th hit on this wire_end        
C-             TDRIFT:  T0 + Z-coordin./velocity
C-               AREA:  pulse area
C-              ALENG:  track length in cell
C-              COORD:  drift distance
C-   Outputs :  PULSE:  simulated pulse data
C-             DIGFLG:  =1 digitized, =0 not digitized
C-
C-   Created   1-MAR-1992   Alexandre Zinchenko
C-   Updated  20-MAY-1992   Ed Oltman - work with parametrized pulse shapes
C-   Updated  23-May-1992   L.Chen, update pulse area and time in VWDA
C-                          and VSEC with those of the pulse loaded to CDD1 
C-   Updated   9-AUG-1992   Alexandre Zinchenko - some cleaning
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXPSH.INC/LIST'
      INCLUDE 'D0$INC:VTLOCA.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
C
      INTEGER DIGFLG, IBEG, IEND, IDX, NXP, IDT, IDAB, NMIN, J
      INTEGER IDRNDM, IREC, ITPROP, ID, NREHIT(100),IBPULS(100)
      INTEGER LKVSEC, LKVWDA, LVTMW, GZVTMW, NHTS,NEL, NWDSHT
      INTEGER IPTR, IPTRHT
C
      REAL    TIME0(15), AIONIZ(100), DT, DX, TTT, TSIN
      REAL    ALPHA, DANODE, COORD, SHAPE(2500), DXSTEP, DTSIN
      REAL    TDRIFT, AREA, ALENG, PULSE(2500), RNDM, AMPL
      REAL AREAPUS, TZERO, AREASAV
C
      LOGICAL FIRST
      SAVE SHAPE, DT, IBPULS, FIRST, TZERO, AIONIZ, AREASAV
C
      DATA  DANODE/0.4572/, DXSTEP/0.1/
      DATA TIME0/0.,10.8,51.2,134.0,250.5,382.0,518.7,657.3,796.6,
     &           936.2,1076.1,1216.1,1356.7,1502.1,1647.5/
      DATA FIRST/.TRUE./,DTSIN/2./,NXP/14/,NMIN/13/
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
cc        OPEN(UNIT=88,ACCESS='direct',STATUS='old',RECL=2500,
cc     &       READONLY,SHARED)
        DT=100./106.
C
C     **** find the TZERO used in BLVWDA
C
        LVTMW = GZVTMW( 0 )
        IF ( LVTMW .LE. 0 ) THEN
          WRITE (*,*) ' **** VTXPUL: bank VTMW not defined'
          CALL EXIT(1)
        ENDIF
        TZERO  = C( LVTMW + 6 )
      ENDIF
C
      DIGFLG=0
      IF( MOD(IFADCC,2).NE.0 ) THEN  ! wire END=1
         GO TO 51              ! use the pulse shape of the wire END=0
      ENDIF 
      IDX=COORD/DXSTEP+1
      IF(IDX.GE.NXP) IDX=NXP-1
      DX=(COORD-(IDX-1)*DXSTEP)/DXSTEP
      TTT=DX*ABS((TIME0(IDX+1)-TIME0(IDX)))
      IDT=TTT/DT
      IBPULS(INHITS)=IDT
C
C***     inclined tracks
C
      TSIN=ALENG/DANODE
      IDAB=TSIN/DTSIN+1.5
      IF(TSIN.LT.1.5) IDAB=1
      IDAB=MIN0(IDAB,7) ! number of theta angles
      AIONIZ(INHITS)=TSIN*NMEAN(INHITS)/NMIN/IDAB
      ID=(IDAB-1)*NXP+(IDX-1)
      IDRNDM=RNDM(0.)*50.+1.
      IREC=ID*50+IDRNDM
      NREHIT(INHITS)=IREC
cc   51 READ(88,REC=NREHIT(INHITS)) (SHAPE(J),J=1,2500)
   51 CALL GET_PULSE(NREHIT(INHITS), SHAPE)
C
C  fill the raw data array
C
      IF(AREA1(INHITS).LE.0.00000035) GO TO 999
      ALPHA=AREA/AREA1(INHITS)*AIONIZ(INHITS)*1.6*20.
      ITPROP=TDRIFT/DT
      AMPL=ALPHA
      IBEG=ITPROP+IBPULS(INHITS)+0.5
      IF(IBEG.GE.0) CALL VLINE(PULSE(1+IBEG),1.,SHAPE(1),AMPL,
     &          PULSE(1+IBEG),2500-IBEG)
      IF(IBEG.LT.0) CALL VLINE(PULSE(1),1.,SHAPE(1+IBEG),AMPL,
     &          PULSE(1),2500-IBEG)
      DIGFLG = 1
C
C  Update VWDA and VSEC with the area and drift time of the pulse
C  which is going to be loaded to CDD1
C
C **** Get link LKVWDA for sector
C	
      LKVWDA = LVWDA( SECTOR, LAYER )
      IF (LKVWDA .LE. 0) THEN
        WRITE (*,*) '**** VTXPUL: error; VWDA bank no longer exist'
        GO TO 999
      ENDIF
      NEL    = IQ( LKVWDA + 2)
      NWDSHT = IQ( LKVWDA + 3)
      IPTR = 4 + IFADCC
C
C ****  Check if data should be present 
C
      NHTS = IQ( LKVWDA + IPTR )
      IF ( NHTS .LE. 0 ) THEN
        WRITE (*,*) '**** VTXPUL: error; 
     &                no hit should be for this IFADCC'
        GO TO 999
      ENDIF
C
C *** Find the pointer for this hit on this wire end
C
      IPTRHT = IQ( LKVWDA + NEL + IPTR ) + ( INHITS - 1) * NWDSHT 
C
C ***  sum the pulse bins for the pulse area
C
      AREAPUS=0.
      DO J=1,2500
        AREAPUS=AREAPUS+AMPL*SHAPE(J)
      ENDDO
      AREAPUS=AREAPUS/10.   
C     since only 1 out of 10 SHAPE() bins is used as a FADC data point  
C
      Q( LKVWDA + IPTRHT + 1 ) = IBEG*DT+TIME0(IDX)-TZERO
      Q( LKVWDA + IPTRHT + 2 ) = AREAPUS  
C
      IF ( SVTX(2) .EQ. 1) THEN
C 
C **** Fill VSEC when wire END=1
C
        IF( MOD(IFADCC,2).NE.0 ) THEN
          LKVSEC = LVSEC( SECTOR, LAYER )
          IF (LKVSEC .LE. 0) THEN 
            WRITE (*,*) '**** VTXPUL: error;VSEC bank no longer exist'
            GO TO 999
          ENDIF 
          NHTS = IQ(LKVSEC+4+WIRE)
          IF (NHTS .LT. 1) THEN
            WRITE (*,*) '**** VTXPUL: error;LDVSEC did not fill any hit'
            GO TO 999
          ENDIF
          NEL  = IQ(LKVSEC+2)
          IF (NEL .NE. 8) THEN 
            WRITE (*,*) '**** VTXPUL: error;VSEC address wrong,NEL><8'
            GO TO 999
          ENDIF
          NWDSHT=IQ(LKVSEC+3)
           Q( LKVSEC+IQ(LKVSEC+4+NEL+WIRE)+(INHITS-1)*NWDSHT+6)
     &    =AREAPUS+AREASAV
           Q( LKVSEC+IQ(LKVSEC+4+NEL+WIRE)+(INHITS-1)*NWDSHT+8)
     &    =IBEG*DT+TIME0(IDX)-TZERO
        ELSE
          AREASAV=AREAPUS
        ENDIF
      ENDIF 
C
  999 CONTINUE
C  
      RETURN
      END

