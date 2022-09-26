      SUBROUTINE DMONPL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make plots for CDC performance monitoring.
C-                         Histograms are made for:
C-                         1. All hits,  2. Track hits, 3. Pulse-heights
C-                         ( based on all hits) and 4. RMS pedestals
C-                         for SWs and DLs.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  30-MAR-1992   Dhiman Chakraborty
C-   Updated  27-MAY-1992   Dhiman Chakraborty : Added histos for DL hits
C-                            not on track, rms pedestals for SWs and DLs
C-   Updated  19-JUN-1992   Dhiman Chakraborty : Taken out track-related
C-                            histos as tracking is dropped from CD_EXAMINE.
C-   Updated  14-SEP-1992   Dhiman Chakraborty : 1. Changed pulse-height
C-                            histos from "all hits" to "segment hits".
C-                            2. Separated ISWs and OSWs in each SW plot.
C-                            3. Added histos to show avg pulseheight on
C-                            ISWs and OSWs grouped on the basis of HV
C-                            power supplies (made from segment hits only).
C-   Updated  25-SEP-1992   Susan K. Blessing   Include a maximum peak height
C-    for hits included in histograms.  Fix IF (FIRST) logic.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
C
      INTEGER IER,ERR,GZDTRH,PTDTSG,KPDCDA,KPDSEC,KPDTSG
      INTEGER ILAYER,WIRE,SECTOR,IWIRE,IOSW,IEND,ISEG,I,N
      INTEGER SECSEG,CDINFM,HITNUM,HITLBL
      INTEGER NHIT,NSEG,IDLYR
      INTEGER GZDPDH,LDPDL
      INTEGER IDPSI,IDPSO,IDWTI,IDWTO
      INTEGER IHIT,IPTR,JPTR,KPTR,DPTR,IPTRP
      INTEGER L_BADCELL,S_BADCELL
      REAL    FBIN,FBINSW,FBINDL,FBINPS,PULHITE,PED_SIG
      REAL MAX_PEAK
      CHARACTER*20 SWALL,DLALL
      CHARACTER*24 SWSEG,DLSEG
      CHARACTER*24 SWPUL,DLPUL
      CHARACTER*19 SWPED,DLPED
      CHARACTER*32 TITSWA,TITDLA,TITSWS,TITDLS,TITSWP,TITDLP,
     +  TITSWR,TITDLR
      CHARACTER*32 TITPSI,TITPSO,TITWTI,TITWTO
      LOGICAL FIRST,EZERROR
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA SWALL/'CDC all hits, SWs, L'/
      DATA DLALL/'CDC all hits, DLs, L'/
      DATA SWSEG/'CDC segment hits, SWs, L'/
      DATA DLSEG/'CDC segment hits, DLs, L'/
      DATA SWPUL/'CDC pulseheights, SWs, L'/
      DATA DLPUL/'CDC pulseheights, DLs, L'/
      DATA SWPED/'CDC RMS ped, SWs, L'/
      DATA DLPED/'CDC RMS ped, DLs, L'/
      DATA TITPSI/'CDC HV PS, ISWs'/
      DATA TITPSO/'CDC HV PS, OSWs'/
      DATA TITWTI/'CDC HV PS weights, ISWs'/
      DATA TITWTO/'CDC HV PS weights, OSWs'/
      DATA IDPSI/395/,IDPSO/396/,IDWTI/398/IDWTO/399/
      PARAMETER (L_BADCELL=1)
      PARAMETER (S_BADCELL=23)
      DATA MAX_PEAK/350./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
C******Booking histograms here************
        DO 300 I = 0,3
          CALL STRINT(SWALL,I,TITSWA,N)
          CALL STRINT(DLALL,I,TITDLA,N)
          CALL STRINT(SWSEG,I,TITSWS,N)
          CALL STRINT(DLSEG,I,TITDLS,N)
          CALL STRINT(SWPUL,I,TITSWP,N)
          CALL STRINT(DLPUL,I,TITDLP,N)
          CALL STRINT(SWPED,I,TITSWR,N)
          CALL STRINT(DLPED,I,TITDLR,N)
          CALL DHDIR('DTRAKS_RCP','HITS_DIRECTORY',IER,' ')
          IF (IER .NE. 0) CALL ERRMSG('DTRAKS','DMONPL',
     &              ' ERROR SETTING HBOOK DIRECTORY ','W')
          CALL HBOOK1(300+I,TITSWA,224,-0.5,223.5,0.)
          CALL HBOOK1(310+I,TITDLA,128,-0.5,127.5,0.)
          CALL HBOOK1(305+I,TITSWS,224,-0.5,223.5,0.)
          CALL HBOOK1(315+I,TITDLS,128,-0.5,127.5,0.)
          CALL DHDIR('DTRAKS_RCP','PULHITE_DIRECTORY',IER,' ')
          IF (IER .NE. 0) CALL ERRMSG('DTRAKS','DMONPL',
     &              ' ERROR SETTING HBOOK DIRECTORY ','W')
          CALL HBOOK1(320+I,TITSWP,224,-0.5,223.5,0.)
          CALL HBOOK1(330+I,TITDLP,128,-0.5,127.5,0.)
          CALL DHDIR('DTRAKS_RCP','RMSPED_DIRECTORY',IER,' ')
          IF (IER .NE. 0) CALL ERRMSG('DTRAKS','DMONPL',
     &              ' ERROR SETTING HBOOK DIRECTORY ','W')
          CALL HBOOK1(340+I,TITSWR,224,-0.5,223.5,0.)
          CALL HBOOK1(350+I,TITDLR,128,-0.5,127.5,0.)
  300   CONTINUE
        CALL DHDIR('DTRAKS_RCP','PULHITE_DIRECTORY',IER,' ')
        IF (IER .NE. 0) CALL ERRMSG('DTRAKS','DMONPL',
     &              ' ERROR SETTING HBOOK DIRECTORY ','W')
        CALL HBOOK1(IDPSI,TITPSI,16,-0.5,15.5,0.)
        CALL HBOOK1(IDPSO,TITPSO,16,-0.5,15.5,0.)
        CALL HBOOK1(IDWTI,TITWTI,16,-0.5,15.5,0.)
        CALL HBOOK1(IDWTO,TITWTO,16,-0.5,15.5,0.)

C**************************************
        CALL DHDIR('DTRAKS_RCP','RMSPED_DIRECTORY',IER,' ')
        IF (IER .NE. 0) CALL ERRMSG('DTRAKS','DMONPL',
     &                ' ERROR SETTING HBOOK DIRECTORY ','W')
        LDPDH = GZDPDH()
        IF(LDPDH.LE.0) CALL ERRMSG('ZEBSTP','DMONPL',
     &      'No DPDH bank found','W')
        DO 222 ILAYER = 0,3
          LDPDL = LC(LDPDH-ILAYER-1)
          IF(LDPDL.LE.0)CALL ERRMSG('ZEBSTP','DMONPL',
     &        'No DPDL bank found','W')
          DO 333 SECTOR = 0,31
            DO 424 WIRE = 0,10
              IPTRP = LDPDL + (SECTOR*11+WIRE)*IC(LDPDL+3)+4
              PED_SIG = C(IPTRP+2)
              IF (WIRE.LE.6)THEN
                IDLYR = 340+ILAYER
                FBINSW = FLOAT(SECTOR*7+WIRE)
                CALL HF1(IDLYR,FBINSW,PED_SIG)
              ELSE
                IDLYR = 350+ILAYER
                FBINDL = FLOAT(SECTOR*4+(WIRE-7))
                CALL HF1(IDLYR,FBINDL,PED_SIG)
              ENDIF
  424       CONTINUE
  333     CONTINUE
  222   CONTINUE
        CALL EZGET_i('CDINFM',CDINFM,ERR)
        CALL EZGET('MAX_PEAK',MAX_PEAK,ERR)
        CALL EZRSET
      ENDIF
C
      LDTRH = GZDTRH()
      IF (LDTRH.LE.0) THEN
        CALL ERRMSG('ZEBCOM','DMONPL','No DTRH bank found','W')
        GO TO 999
      END IF
C
      DO 100 ILAYER = 0, 3
C
C    First, only those hits which are on segments
C
        NSEG = IQ(LDTRH+3+ILAYER)
        IF (NSEG.LE.0) GOTO 666
        KPDTSG = LQ(LDTRH-2-ILAYER)
        IF(KPDTSG.LE.0) CALL ERRMSG('ZEBCOM','DMONPL',
     &      'No DTSG bank found','W')
        DO 110 ISEG = 1,NSEG
          PTDTSG = KPDTSG + 2 + (ISEG-1) * IQ(KPDTSG+2)
          DO 120 IWIRE = 0,6
            IOSW = -1
            IF (IWIRE.EQ.0) IOSW = 1
            IF (IWIRE.EQ.6) IOSW = 3
            HITLBL = IQ(PTDTSG+9+IWIRE)
            IF (HITLBL.GT.0) THEN
              SECSEG = IBITS(HITLBL, 11, 5)
              HITNUM = IBITS(HITLBL,  1, 7)
              KPDCDA = LDCDA(SECSEG,ILAYER)      ! CDC data (hits)
              JPTR = IQ(KPDCDA+11+IWIRE+4) + IQ(KPDCDA+3)*(HITNUM-1)
              PULHITE = Q(KPDCDA+JPTR+5)
C
C Only look at pulses under 350 counts
              IF (PULHITE.LE.MAX_PEAK) THEN
C
                IF (IWIRE.EQ.0) THEN          ! it is an inner OSW
                  FBINSW = FLOAT(160+SECSEG*2)
                ELSEIF (IWIRE.EQ.6) THEN      ! it is an outer OSW
                  FBINSW = FLOAT(160+SECSEG*2+1)
                ELSE                          ! it is an ISW
                  FBINSW = FLOAT(SECSEG*5+IWIRE-1)
                ENDIF
                IDLYR = 320+ILAYER
                CALL DHDIR('DTRAKS_RCP','HITS_DIRECTORY',IER,' ')
                IF (IER .NE. 0) CALL ERRMSG('DTRAKS','DMONPL',
     &              ' ERROR SETTING HBOOK DIRECTORY ','W')
                CALL HF1(IDLYR-15,FBINSW,1.) ! Histo: segment hits on SW
                CALL DHDIR('DTRAKS_RCP','PULHITE_DIRECTORY',IER,' ')
                IF (IER .NE. 0) CALL ERRMSG('DTRAKS','DMONPL',
     &              ' ERROR SETTING HBOOK DIRECTORY ','W')
                CALL HF1(IDLYR,FBINSW,PULHITE)
C                           ! Histo: p.h. of segment hits on SW
                FBINPS = FLOAT(ILAYER*4 + INT((SECSEG+0.5)/8))
C
C*** THIS LINE AND THOSE OF THE FOLLOWING FOLLOWED BY !*** SHOULD BE
C  DELETED AS SOON AS THE BAD CELL IS FIXED (ALSO COR INT & PARAM DECS)***
C
                IF((ILAYER.EQ.L_BADCELL).AND.(SECSEG.EQ.S_BADCELL))
     &            GOTO 321 !***
                IF (IOSW.LT.0) THEN
                  CALL HF1(IDWTI,FBINPS,1.) ! cumulative hits on
C                           ! ISWs on an HV supply
                  CALL HF1(IDPSI,FBINPS,PULHITE) ! cumulative p.h. on
C                           ! ISWs on an HV supply
                ELSE
                  CALL HF1(IDWTO,FBINPS,1.) ! cumulative hits on
C                           ! ISWs on an HV supply
                  CALL HF1(IDPSO,FBINPS,PULHITE) ! cumulative p.h. on
C                           ! OSWs on an HV supply
                ENDIF
  321           CONTINUE
              END IF
              IF (IOSW.GT.0) THEN
                KPDSEC = LDSEC(SECSEG,ILAYER ) ! Look for DL hits
                IF (KPDSEC .EQ. 0) GO TO 45
                KPTR = IQ(KPDSEC+7+4+IWIRE) + IQ(KPDSEC+3)*(HITNUM-1)
                IF(KPTR.GT.0) THEN
                  DO 40 IEND = 0,1
                    DPTR = IQ(KPDSEC+KPTR+11+IEND)
                    IF(DPTR.GT.0) THEN
                      IDLYR = 330+ILAYER
                      FBINDL = FLOAT(SECSEG*4+IOSW+IEND-1)
                      PULHITE = Q(KPDCDA+DPTR+5)
C
C Only look at pulses under 350 counts
                      IF (PULHITE.LE.MAX_PEAK) THEN
C
                        CALL DHDIR('DTRAKS_RCP','HITS_DIRECTORY',IER,
     &                    ' ')
                        IF (IER .NE. 0) CALL ERRMSG('DTRAKS','DMONPL',
     &                    ' ERROR SETTING HBOOK DIRECTORY ','W')
                        CALL HF1(IDLYR-15,FBINDL,1.)
C                           ! Histo: segment hits on DL
                        CALL DHDIR('DTRAKS_RCP','PULHITE_DIRECTORY',IER,
     &                    ' ')
                        IF (IER .NE. 0) CALL ERRMSG('DTRAKS','DMONPL',
     &                    ' ERROR SETTING HBOOK DIRECTORY ','W')
                        CALL HF1(IDLYR,FBINDL,PULHITE)
C                           ! Histo: P.H. of segment hits on DL

                      ENDIF
                    END IF
   40             CONTINUE
                ENDIF
   45           CONTINUE
              ENDIF
            ENDIF
  120     CONTINUE
  110   CONTINUE
C
C     Next, all hits
C
  666   DO 130 SECTOR = 0,31
          KPDSEC = LDSEC( SECTOR, ILAYER ) !CDC SW hits
          IF (KPDSEC .EQ. 0) GO TO 130
          KPDCDA = LDCDA( SECTOR, ILAYER ) !CDC data (hits)
          DO 140 WIRE = 0, 10
            NHIT = IQ( KPDCDA+4+WIRE )
            IF(NHIT.GT.0)THEN
              IF(WIRE.LE.6) THEN  ! it is an SW
                IF ((WIRE.GE.1).AND.(WIRE.LE.5)) THEN ! ISW
                  IWIRE = SECTOR*5 + WIRE - 1
                ELSEIF (WIRE.EQ.0) THEN   ! Inner OSW
                  IWIRE = 160 + SECTOR * 2
                ELSE                      ! Outer OSW
                  IWIRE = 160 + SECTOR * 2 + 1
                ENDIF
                FBIN = FLOAT(IWIRE)
                IDLYR = 300+ILAYER
                CALL DHDIR('DTRAKS_RCP','HITS_DIRECTORY',IER,' ')
                IF (IER .NE. 0) CALL ERRMSG('DTRAKS','DMONPL',
     &              ' ERROR SETTING HBOOK DIRECTORY ','W')
                CALL HF1(IDLYR,FBIN,FLOAT(NHIT))
              ELSE  ! it is a DL
                IWIRE = SECTOR*4 + (WIRE-6) - 1
                FBIN = FLOAT(IWIRE)
                IDLYR = 310+ILAYER
                CALL HF1(IDLYR,FBIN,FLOAT(NHIT))
              ENDIF
            ENDIF
  140     CONTINUE
  130   CONTINUE
  100 CONTINUE
C
  999 RETURN
      END
