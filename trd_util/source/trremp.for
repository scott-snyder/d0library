      SUBROUTINE TRREMP(TRIGG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FILL EXAMINE_ON HISTOGRAMS
C-
C----------------------------------------------------------------------
C
C            +-------------------+
C            |FILLS THE HISTOGRAMS|
C            +-------------------+
C       ITRIG = 0-9 , ILAYR = 0-2, ISECTR = 0-6
C ITRIG   = 0  no data selection
C ITRIG   = 1  tracks
C ITRIG   = 2  electrons
C ITRIG   = 3  pions
C ILAYR   = I-1  layer I
C ISECTR  = 0  layer  (Anodes)
C ISECTR  = 2  sector
C ISECTR  = 4  wire
C ISECTR  = 6  all trd
C ISECTR  = 1  layer  (Cathodes)
C ISECTR  = 3  sector
C ISECTR  = 5  wire
C ISECTR  = 7  all trd
C  Histo. indexed by HTFIRS+ 100*ILAYR + 1000*ITRIG+ 10*ISECTR
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-JUN-1990  Y.Ducros & J.Fr.Glicenstein
C-   Updated  11-JAN-1994   A. Zylberstejn   : Simplify. Remove test on
C-                                               individual sectors
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:FADCCN.INC'
      COMMON/NORMA/SECTOT(3,2),FILCHA(2),NEVSEC(16,6),NUEV
      REAL FILCHA,NUEV,NEVSEC,SECTOT
      INTEGER I,ICH,K,IK,IER,I1,I2,ISECTR,I3,ILAY,ILAYR,KK,KKF
      INTEGER ISECT,NB_OF_SECTORS(6),WIRE,tchnb
      INTEGER CHA1,IERR,UBIT,TDATA(NMFADC+10),NH1,IMIN,IMCOS,MX,MFIR
      INTEGER IFOIS,NX16,NSPED,IHISTO(6)
      REAL ENTRY,ENERGY,SECTOR,ALAY,ENECHA,WIRCHA,PEDES,PED(16,2,3)
      REAL EN,ETOT,TRDW,VSUM,FIS
      INTEGER ITRIG,NHIT,NSECT,NUMSEC,KSEC,MUM,NFISEC
      INTEGER NUM,NUMTRIG,IS,IM,KKK
C     COMMON/DQDT/NX,NY,NWT
      INTEGER NX,NMAX
      REAL XMI,XMA,YMI,YMA
      CHARACTER*80,CHT
      LOGICAL FIRST,IDEBUG
      CHARACTER*3 SWPHYS(10),SWLAYR(4),SWUNIT(7),SWHIST(10),SWSECT(16),
     #            SWANOD(2),SWPEDES
      INTEGER     ISWPHY(10),ISWLAY(4),ISWUNI(7),ISWHIS(10),ISWSEC(16),
     #            ISWANO(2),ISWPEDE,ISUPFLAT
      INTEGER     TRIGG,NUMSECT,NMOT
      INTEGER IMOT,TDMIN,TDMAX,IJK
      REAL SECENE,SECWIR,SECON,CHAENE,CHAWIR,CHAON,SCH
      REAL TRGTIM
      EQUIVALENCE (IHISTO(1),NX)
C      EQUIVALENCE (ISWPHY,SWPHYS),(ISWLAY,SWLAYR),(ISWANO,SWANOD),
C     +(ISWUNI,SWUNIT),(ISWHIS,SWHIST),(ISWSEC,SWSECT),(ISWPEDE,SWPEDES)
      INCLUDE 'D0$INC:HTFIRS.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:trwcod_512.INC'
      INCLUDE 'D0$INC:TRHITW.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      integer ntot_sector_trd
      parameter (Ntot_sector_trd=NTOT_WIRE_TRD/16)
c      REAL WS2(128,Ntot_sector_trd)
      DATA FIRST/.TRUE./,IDEBUG/.TRUE./
      DATA IFOIS/0/,IMIN/0/
      IF(FIRST)THEN
C****  Reads in TRDHIT_RCP file
        CALL EZPICK('TRDHIT_RCP')
        CALL EZGET('NUM_PHYS_SWIT',NUMTRIG,IER)
        CALL EZGET('PHYSICS_SWITCHES',ISWPHY(1),IER)
        CALL EZGET('LAYER_SWITCHES',ISWLAY(1),IER)
        CALL EZGET('UNIT_SWITCHES',ISWUNI(1),IER)
        CALL EZGET('ANODE_SWITCHES',ISWANO(1),IER)
        CALL EZGET('HISTOON_SWITCHES',ISWHIS(1),IER)
        CALL EZGET('NUM_SEC_SWIT',NUMSECT,IER)
        CALL EZGET('SEC_NUM_SWIT',ISWSEC(1),IER)
        CALL EZGET('HISTO_LIMITS',IHISTO(1),IER)
        CALL EZGET('PED_SOUS_SW',ISWPEDE,IER)
        CALL EZGET('SUPPRESS_FLAT_CHAN',ISUPFLAT,IER)
        CALL EZGET('NBINS_PHYSICS',MX,IER)
C  Transform integer in characters
        DO I=1,10
          CALL UHTOC(ISWPHY(I),3,SWPHYS(I),3)
        END DO
        DO I=1,4
          CALL UHTOC(ISWLAY(I),3,SWLAYR(I),3)
        END DO
        DO I=1,7
          CALL UHTOC(ISWUNI(I),3,SWUNIT(I),3)
        END DO
        DO I=1,2
          CALL UHTOC(ISWANO(I),3,SWANOD(I),3)
        END DO
        DO I=1,10
          CALL UHTOC(ISWHIS(I),3,SWHIST(I),3)
        END DO
        DO I=1,16
          CALL UHTOC(ISWSEC(I),3,SWSECT(I),3)
        END DO
        CALL UHTOC(ISWPEDE,3,SWPEDES,3)
        CALL EZRSET
        DO ICH=1,6
          NB_OF_SECTORS(ICH)=NWIRE_PER_LAYER(ICH)/16
        END DO
        IM=1
c        CALL VZERO(NEVSEC,96)
        CALL VZERO(SECTOT,6)
        CALL VZERO(FILCHA,2)
        NUEV=0.
        NMOT = 0
        FIRST=.FALSE.
      END IF
C   ****
      IFOIS=IFOIS+1
      CALL HCDIR('//PAWC/TRH',' ')  !GO TO TRD_ON DIRECTORY
C   ****
      NUEV=NUEV+1.
C   **** COSMIC TIMING ( IMCOS)
C      CALL DTRGTM(TRGTIM)
C      IMIN = TRGTIM/10.
      IMIN = 0
C   ****
      CALL TSETWC
      DO 200 KK=1,2  !k=1  anodes,k=2 cathodes
        IF(SWANOD(KK).NE.'Y') GO TO 200
        ETOT=0.
        TRDW=0.
        DO 151 ILAY=1,3  ! Loop on the Chambers  *****
          IF(SWLAYR(ILAY).NE.'Y') GO TO 151
          ILAYR=ILAY-1
          CHAON=0.
          CHAENE=0.
          CHAWIR=0.
          ICH=ILAY+(KK-1)*3 ! chamber from 1 to 6
          ITRIG=TRIGG
          NUM    = HTFIRS + 1000*ITRIG + 100*ILAYR
        ISECTR=KK+5
          I3=NUM+10*(KK+3)
          NX16=NX+16*NX
c          CALL VZERO(WS2(1001),NX16)
          DO 138 ISECT=1,NB_OF_SECTORS(ICH)
            SECENE=0.
            SECWIR=0.
            SECON=0.
c            IF(SWSECT(ISECT).NE.'Y') GO TO 138
            DO 125 NHIT=1,16! loop on the channels
              WIRE=(ISECT-1)*16+NHIT
              if(.not.twcod(tchnb(wire,ich)))go to 125
              NH1 = WIRE - 1
              CALL TCODER(CHA1,ICH-1,NH1,UBIT,2)
              CALL ZDEXPD(4,CHA1,TDATA)
              IF (TDATA(1).EQ.0)GO TO 125
              NMOT = TDATA(1)             ! Should be the same for all wires
              CALL VFLOAT(TDATA(3+IMIN),WS,NMOT)
C ******    pedestal substraction   *******
              IF (SWPEDES.EQ.'Y') THEN
                NSPED=1
C              PEDES=PED(NFISEC,NSPED,ILAY)
                CALL TRGPED('BID',WIRE,ICH,PEDES,IERR)
                CALL VBIAS(WS,-PEDES,WS,MX)
              ENDIF
              EN=VSUM(WS,NMOT)
C   ****   DQ/DT  *****************************
C             IF(SWHIST(4).EQ.'Y')THEN! dQ/dT
C              KKF=0
C               KSEC=(NSECT-1)*NX+1000+NX
C              DO 35 K = 1,NX
C                IF(KKF.GE.NX)GO TO 37
C                KKF=KKF+1
C                KSEC=KSEC+1
C                SCH=WS2(K)
C                WS2(KSEC)=WS2(KSEC)+SCH  !dQ/dT per individual sector for one
C                                      !event
C   35         CONTINUE
C   37         CONTINUE
C            END IF
C   ########  ACCUMULATION PER WIRE  #########
              IF(SWUNIT(3).EQ.'Y') THEN
                i3=num+40+(kk-1)*10
                IF(SWHIST(1).EQ.'Y') THEN ! WIRE MAP
c              if(wire.eq.256)
C     +         print*,' in trremp,layer,sector',ich,isect,'wire',wire,
c     + ' accum in histo',i3+1
                  CALL HF1(I3+1,FLOAT(WIRE),1.)
                  IF(SWHIST(2).EQ.'Y')
     +               CALL HF1(I3+2,FLOAT(WIRE),EN)  ! MEAN ENERGY PER WIRE
                END IF
              END IF
C
              NSECT = ISECT
              NFISEC=NHIT! wire number in sector "NSECT"
C  ****   total number of wires histogrammed with sector "NSECT" and layer
C  ****   "ICH"
c              NEVSEC(NSECT,ICH)=NEVSEC(NSECT,ICH)+1.
              FILCHA(KK)=FILCHA(KK)+1.
C  ******     ENERGY AND NB. OF CHANNELS PER SECTOR AND PER LAYER  *****
              SECTOR = FLOAT(NSECT)
              I1 = ISECTR+1
              SECON=1.
              SECENE=SECENE+EN  !tot. energy per sector
              SECWIR=SECWIR+1.  ! Nb. of wires per sector
  125       CONTINUE
            IF(SWLAYR(ILAY).NE.'Y') GO TO 151
C
    5       CONTINUE
C  *********  ACCUMULATION  SECTOR LEVEL  ************
C
            IS=ISECT
            IF(SECON.EQ.1.)THEN ! Make sure the sector has been hit
              CHAENE=CHAENE+SECENE  !tot. energy per layer
              CHAWIR=CHAWIR+1.  ! Nb. of wires per layer
              CHAON=1.
C ****   TOTAL ENERGY DEPOSITED IN THE 3 LAYERS ( ANODE OR CATHODE)
              ETOT=ETOT+SECENE
C ****    Energy scale depends on histogram booking.
              TRDW=TRDW+SECENE
            END IF
            IF(SWUNIT(2).NE.'Y') GO TO 138 ! Check if we want sector plots
            I3=NUM+20+(kk-1)*10
            IF(SWHIST(5).EQ.'Y')
     +         CALL HF1(I3+5,SECENE,1.)  !Energy distribution per sector
            MUM=HTFIRS+1000*ITRIG+100*ILAYR+300*KK+IS-1
            IF(SWHIST(3).EQ.'Y')
     +         CALL HF1(MUM,SECWIR,1.) !Nb of hit channels for
C                                           ! sector IS
            IF(SWHIST(5).EQ.'Y')
     +         CALL HF1(MUM+40,SECENE,1.) ! Energy deposited in
C                                              ! sector IS
            IF(SECON.NE.1.)GO TO 138 ! Make sure the sector has been hit
            SECTOT(ILAY,KK)=SECTOT(ILAY,KK)+1.
            FIS=FLOAT(IS)
            IF(SWHIST(1).EQ.'Y')
     +         CALL HF1(I3+1,FIS,1.)!Sector map
            IF(SWHIST(2).EQ.'Y')
     +         CALL HF1(I3+2,FIS,SECENE) ! Mean Energy per sector
            IF(SWHIST(3).EQ.'Y')
     +         CALL HF1(I3+3,SECWIR,1.) !Mean Nb. of wires per hit
C                                            ! sector
c            IF (SWHIST(4).EQ.'Y') THEN
c              MUM=HTFIRS+1000*ITRIG+100*ILAYR+300*KK+IS-1
c              KSEC=(IS-1)*NX+1000+NX
c              CALL HPAK(IS+1,WS2(KSEC+1))
c              CALL HOPERA(MUM+20,'+',IS+1,MUM+20,1.,1.)
C         Fourier transforms
c              IF (SWHIST(6).EQ.'Y') THEN
c                IJK = MUM + 60
c                IF (NMOT.GE.0) THEN
c                  CALL TFOURIER(IJK,WS2(1),NMOT)
c                ENDIF
c              ENDIF
c            ENDIF
  138     CONTINUE
C
C  ****   ACCUMULATION LAYER LEVEL  ****************
C
          IF(SWUNIT(1).EQ.'Y') THEN
            ISECTR=KK-1
            I3 = HTFIRS + 1000*ITRIG +10*ISECTR
            I3=NUM+(kk-1)*10
            ALAY=ILAY
            IF(SWHIST(1).EQ.'Y') THEN  ! layer map
              CALL HF1(I3+1,ALAY,CHAON)
            END IF
            IF(SWHIST(2).EQ.'Y') THEN ! Mean energy per layer
              CALL HF1(I3+2,ALAY,CHAENE)
            END IF
            ISECTR=KK-1
            I3 = HTFIRS + 1000*ITRIG +100*ILAYR +10*ISECTR
            IF(SWHIST(3).EQ.'Y') THEN ! Nb. of hit per layer
              CALL HF1(I3+3,CHAWIR,1.)
            END IF
            IF(SWHIST(5).EQ.'Y') THEN !Total energy per layer
              CALL HF1(I3+5,CHAENE,1.)
            END IF
          END IF
C  *******    DQ/DT   **************
          I3=10*(KK-1) +NUM
c          IF(SWUNIT(1).EQ.'Y'.AND.SWHIST(4).EQ.'Y')THEN
c            CALL HPAK(1,WS2(1))
c            CALL HOPERA(I3+4,'+',1,I3+4,1.,1.)
C         Fourier transforms
c            IF (SWHIST(6).EQ.'Y') THEN
c              IJK = I3 + 7
c              IF (NMOT.GE.0) THEN
c                CALL TFOURIER(IJK,WS2(1),NMOT)
c              ENDIF
c            ENDIF
c          END IF
  151   CONTINUE
C  ****   Accumulation at the TRD level
        IF(SWUNIT(4).NE.'Y') GO TO 200
        I3 = num+60+(kk-1)*10
        IF(SWHIST(3).EQ.'Y') THEN
          CALL HF1(I3+3,TRDW,1.) !Nb. of wires
        END IF
        IF(SWHIST(5).EQ.'Y') THEN
          CALL HF1(I3+5,ETOT,1.) !Total energy
        END IF
    6   CONTINUE
  200 CONTINUE
  999 RETURN
      END
