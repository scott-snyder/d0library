      SUBROUTINE TSETWC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set TWCOD(TCHNB(I,J) to .TRUE. if wire nb. I is coded
C-                          in layer J
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: TRD.RCP
C-
C-   Created  10-MAY-1991   A. Zylberstejn
C-   Updated  22-NOV-1991   A. Zylberstejn  Activate the cathodes
C-   Updated  28-JAN-1993   Alain PLUQUET   Change cathode_analysis switch
C-   Updated  20-SEP-1993   A. Zylberstejn  updated for 512 channels in layer 3
C-   Updated  15-FEB-1994   A. Zylberstejn  :Do not overwrite Bank THIT if it
C-                                            already exists
C-   Updated  10-OCT-1994   A. Zylberstejn  : overwrite bank THIT for reco
C-                                             versions<12.13
C-   Updated  27-APR-1995   A. Zylberstejn  :clean up and change arguments of
C-                                            GET_TRD_COR_PED
C-   Updated  17-JUL-1995   A. ZYLBERSTEJN  Drop THIT bank if it exists once per
C-   event
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER CHA1,DIFF,ICH,I,ICHM,IER,IMI,REAL_WIRE,WIRE,UBIT,NW,NEVOLD
      INTEGER NEED,N_IN_THIT,NWDS_USED, NWDS_MAX,IFOIS
      LOGICAL FIRST,TWIRHOT,DOPRINT,FILL_HITS,SWAP,CATHODE_ANALYSIS
      LOGICAL FILL_HITS_IN,TRD_DO_PRINT
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:THIT_INFO.INC'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:VARTRD.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INTEGER ISUPFLAT,LOUT,TRUNIT,LTHIT,GZTHIT,TDATA(260)
      INTEGER I0,I1,LT,SOM,IMS,GZTRDH,LHITS,GZHITS,TWIRCOR,TCHNB
      INTEGER RECOVERSION,PASS
      REAL ETOT,VMIN,VMAX,BINMIN,VSUM,VERS,YFADC(NMFADC+10)
      REAL PEDES,PED,VS
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        IFOIS=0
        FIRST=.FALSE.
        CALL EZPICK('TRD_RCP')
        LOUT=TRUNIT()
        DOPRINT=TRD_DO_PRINT()
        SWAP=.TRUE.
        IF(MCDATA)SWAP=.FALSE.
        FILL_HITS_IN=.FALSE.
        IF(GZTHIT().LE.0)THEN ! no Thit bank, check if CDD4 there.
          IF(LQ(LHEAD-IZCDD4).LE.0)THEN!No THIT nor CDD4; should not happen
            CALL ERRMSG('not THIT,no CDD4 banks','TSETWC',' ','W')
            GO TO 999
          END IF
          CALL EZGET_l('BUILD_THIT',FILL_HITS_IN,IER)
        ELSE                  ! THIT bank present. Check version nb.
          CALL RECO_VERSION(RECOVERSION,PASS)
          VERS=RECOVERSION+FLOAT(PASS)/100.
          IF(VERS.LT.12.13 .AND. LQ(LHEAD-IZCDD4).NE.0)
     &                                        FILL_HITS_IN=.TRUE.
        END IF
        CALL EZGET('CATHODE_ANALYSIS',CATHODE_ANALYSIS,IER)
        IF(CATHODE_ANALYSIS.AND.IER.EQ.0) THEN
          ICHM=6
        ELSE
          ICHM=3
        ENDIF
        BINMIN=-10.
        CALL EZGET_i('SUPPRESS_FLAT_CHAN',ISUPFLAT,IER)
        IF(IER.NE.0)BINMIN=FLOAT(ISUPFLAT)
        IF(DOPRINT)
     +      WRITE(LOUT,*)' In Tsetwc,fill_hits = ',FILL_HITS_IN,
     +      ' ISUPFLAT',ISUPFLAT
        IF(DOPRINT .AND. SWAP)WRITE(LOUT,*)
     &      ' We swap cables in TRD layer 3'
        CALL EZRSET
C        DO 16 ICH =  1,  6
C          DO 12 REAL_WIRE =  1,  NWIRE_PER_LAYER(ICH)
C            WIRE=REAL_WIRE
C            IF(.NOT.MCDATA .AND. ICH.EQ.3 .AND.
C     &        NWIRE_PER_LAYER(3).EQ.256) WIRE=TWIRCOR(REAL_WIRE)
C            CALL TRGPED('BID',WIRE,ICH,PEDES,IER) ! Get pedestals
C            IF(IER.NE.0)THEN
C              CALL ERRMSG('CANT FIND TRD PEDESTALS','TSETWC',' ','W')
C              PED_TRD(TCHNB(REAL_WIRE,ICH))=0.
C            ELSE
C              PED_TRD(TCHNB(REAL_WIRE,ICH))=PEDES
C            END IF
C            end if
C          if(ich.eq.3 .and.wire.le.16)print*,' real_wire,wire',
C     &      real_wire,wire,'tchnb',tchnb(real_wire,Ich),' ped',
C     &                            PED_TRD(TCHNB(real_wire,Ich))
   12   CONTINUE
   16   CONTINUE
        LOUT=TRUNIT()
        NEVOLD=0
      END IF
      IF(IQ(LHEAD+9).EQ.NEVOLD)GO TO 999 ! do not call several times for
C                                        ! same event
      FILL_HITS=FILL_HITS_IN
      LTHIT=GZTHIT()
      IF(LTHIT.NE.0 .AND. FILL_HITS_IN)CALL MZDROP(IXMAIN,LTHIT,' ')
      NEVOLD=IQ(LHEAD+9)
      DOPRINT=TRD_DO_PRINT().AND.LOUT.NE.0
      IFOIS=IFOIS+1
      IF(DOPRINT)WRITE(LOUT,'(a30,/,a30)')' enter TSETWC ',
     &    ' ----------------'
      DO  100 ICH =  1,  ICHM
        IMI=0
        NW=0
        NCODED(ICH)=0
        DO 10 REAL_WIRE=1,NWIRE_PER_LAYER(ICH)
          TWCOD(TCHNB(REAL_WIRE,ICH))=.FALSE.
          WIRE=REAL_WIRE
          IF(COSMIC1 .AND.ICH.LE.3 .AND. TWIRHOT(REAL_WIRE,ICH))GO TO 10
          IF(.NOT.MCDATA .AND. SWAP. AND. ICH.EQ.3 .AND.
     &      NWIRE_PER_LAYER(3).EQ.256) WIRE=TWIRCOR(REAL_WIRE)
          CALL TCODER(CHA1,ICH-1,WIRE-1,UBIT,2)
          CALL ZDEXPD(4,CHA1,TDATA)
          IF (TDATA(1).LE.0)GO TO 10
C  real_wire= real chamber wire
C  wire=wire coded in the FADC
          IF(TDATA(1).GT.NMFADC)THEN
            CALL ERRMSG('ERROR reading CDD4 ','TSETWC',' ','W')
            GO TO 10
          END IF
          TWCOD(TCHNB(REAL_WIRE,ICH))=.TRUE.
          NCODED(ICH)=NCODED(ICH)+1
          IF(.NOT.FILL_HITS) GO TO 10
C  Check if enough room in ZEBCOM
          NEED = 1
          CALL MZNEED(IXMAIN, NEED, 'G')
          NWDS_USED = IQUEST(12)
          NWDS_MAX = IQUEST(13)
          IF ((NWDS_MAX - NWDS_USED) .LT. 10000) THEN
            FILL_HITS=.FALSE.
            CALL ERRMSG('Not enough room for TRDHIT','TSETWC',
     &        'Skip THIT filling ' ,'W')
            GO TO 10
          END IF
          CALL VFLOAT(TDATA(3),YFADC,TDATA(1))
          IF(ISUPFLAT.NE.0)THEN
            DIFF=VMAX(YFADC,TDATA(1))-VMIN(YFADC,TDATA(1))
            IF(DIFF .LT. ISUPFLAT) THEN
C              PRINT*,' in tsetwc,real_wire,ich',REAL_WIRE,ICH,
C     &          'diff,isupflat',DIFF,ISUPFLAT,' twcod set to false'
              TWCOD(TCHNB(REAL_WIRE,ICH))=.FALSE.
              GO TO 10
            END IF
          END IF
          CALL GET_TRD_COR_PED(ICH,WIRE,YFADC,PED,IER)
          VS=VSUM(YFADC,TDATA(1))
          CALL VBIAS(YFADC,-PED,YFADC, TDATA(1))
          ETOT=          VSUM(YFADC,TDATA(1))
          IF(ETOT.LE.0.)THEN
            TWCOD(TCHNB(REAL_WIRE,ICH))=.FALSE.
          ELSE
            IMI=IMI+1
            IWS(IMI)=REAL_WIRE
            WS(1000+IMI)=ETOT
            CALL TREDEP(REAL_WIRE,ICH,YFADC,TDATA(1))
          END IF
   10   CONTINUE
        IF(.NOT.DOPRINT)GO TO 100
        WRITE(LOUT,*)' layer',ICH,' nb. of coded wire',IMI
        IF(IMI.NE.0)THEN
          WRITE(LOUT,*)' coded wires'
          WRITE(LOUT,'(10i4)')(IWS(I),I=1,IMI)
          WRITE(LOUT,*)' energies'
          WRITE(LOUT,'(10f7.1)')(WS(1000+I),I=1,IMI)
        END IF
  100 CONTINUE
      LTHIT=GZTHIT()
      IF(LTHIT.NE.0 .AND. DOPRINT)WRITE(LOUT,*)
     &    ' total number of TRD hits',IQ(LTHIT+2)
      IF(FILL_HITS)CALL REDUCE_THIT
      IF(DOPRINT)WRITE(LOUT,'(a30,/,a30)')' exit TSETWC ',
     &    ' ----------------'
  999 RETURN
      END
