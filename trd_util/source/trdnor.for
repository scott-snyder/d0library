      LOGICAL FUNCTION TRDNOR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : NORMALISE EXAMINE_ON HISTOGRAMS
C-
C----------------------------------------------------------------------
C
C            +-------------------------+
C            |NORMALISES THE HISTOGRAMS|
C            +-------------------------+
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
C  Histo. indexed by HTFIRS+ 100*ILAYR + 1000*ITRIG+ 10*ISECTR    :anode
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-AUG-1990  Y.Ducros 
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER I,ICH,K,IER,I2,IK,IH,IS,ISECTR,I3,ILAY,ILAYR,KK,IT
      INTEGER ITRIG,NHIT,NUMSEC,KSEC,MUM,NFISEC
      INTEGER NUM,NUMERO,NUMTRIG,NORMFREQ,XUEV,IC,ICC
      INTEGER NX,NY,NWT,IFOIS,ID,IDD,IDU,IDDU,IFO
      INTEGER HIDENT,KIDENT
      REAL XMI,XMA,YMI,YMA,CONT1(257),CONT2(257),RACINE
     &,CONT3(257)
      CHARACTER*80,CHT
      CHARACTER*4,HIST
      LOGICAL FIRST
      CHARACTER*4 SWPHYS(10),SWLAYR(4),SWUNIT(7),SWHIST(10),SWSECT(16),
     #            SWANOD(2),SWPEDES
      INTEGER     ISWPHY(10),ISWLAY(4),ISWUNI(7),ISWHIS(10),ISWSEC(16),
     #            ISWANO(2),ISWPEDE
      INTEGER     NUMSECT
      EQUIVALENCE (ISWPHY,SWPHYS),(ISWLAY,SWLAYR),(ISWANO,SWANOD),
     +(ISWUNI,SWUNIT),(ISWHIS,SWHIST),(ISWSEC,SWSECT),(ISWPEDE,SWPEDES)
      INCLUDE 'D0$INC:HTFIRS.INC'
      COMMON/NORMA/SECTOT(3,2),FILCHA(3,2),NEVSEC(16,6),NUEV
      REAL FILCHA,NUEV,NEVSEC,SECTOT,ANORM
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRHITW.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      DATA FIRST/.TRUE./
      DATA IFOIS/0/
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
        CALL EZGET('PED_SOUS_SW',ISWPEDE,IER)
        CALL EZGET('NORM_FREQUENCY',NORMFREQ,IER)
        CALL EZRSET
        FIRST=.FALSE.
        END IF
C   **** The following line upsets the common sense
      XUEV = NUEV
      IF (MOD(XUEV,NORMFREQ).NE.0) GOTO 999
      IFOIS=IFOIS+1
      CALL HCDIR('//PAWC/TRH',' ')  !GO TO TRD_ON DIRECTORY
C   ****
      DO 19 IT=1,NUMTRIG
        IF(SWPHYS(IT).NE.'Y') GO TO 19
        ITRIG=IT-1
        ANORM=NUEV
      DO 1 KK=1,2  !k=1  anodes,k=2 cathodes
        IF(SWANOD(KK).NE.'Y') GO TO 1
        IFO=0
        DO 2 ILAY=1,3  ! Loop on the Chambers  *****
          IF(SWLAYR(ILAY).NE.'Y') GO TO 2
          ILAYR=ILAY-1
          IFO=IFO+1
          ICH=ILAY+(KK-1)*3
          ISECTR=KK+5
          NUM    = HTFIRS + 1000*ITRIG + 100*ILAYR + 10*(KK-1)
C  Type 2 histogram normalized relatively to type 1 histogram
          DO 3 I2=1,3
            CALL VZERO(CONT1,257)
            CALL VZERO(CONT2,257)
            CALL VZERO(CONT3,257)
            IF(SWUNIT(I2).NE.'Y') GO TO 3
            ID=NUM+10*(I2-1)*2
            IDD=ID+1000
            IDU=ID
            IDDU=IDD
            IF(I2.EQ.1.AND.IFO.EQ.1) THEN
              IDU=ID-100*ILAYR
              IDDU=IDU+1000
            END IF
            IF(I2.EQ.1.AND.IFO.GT.1) GO TO 20
            IF(SWHIST(1).EQ.'Y') THEN
            CALL HUNPAK(IDU+1,CONT1,HIST,1)
            CALL HPAK(IDDU+1,CONT1)
            END IF
            IF(SWHIST(2).EQ.'Y') THEN
            CALL HUNPAK(IDU+2,CONT2,HIST,1)
            CALL HPAK(IDDU+2,CONT2)
            END IF
            IF(SWHIST(1).EQ.'Y'.AND.SWHIST(2).EQ.'Y') THEN
            IF(I2.EQ.3)  CALL HUNPAK(IDU+6,CONT3,HIST,1)
            CALL HGIVE(IDU+1,CHT,NX,XMI,XMA,NY,YMI,YMA,NWT,K)
            DO 4 IK=1,NX
              IF(CONT1(IK).NE.0.) THEN
              CONT2(IK)=CONT2(IK)/CONT1(IK)
              IF(I2.EQ.3) THEN
               RACINE = (CONT3(IK)/CONT1(IK)-CONT2(IK)**2)
               IF (RACINE.GE.0.) THEN
                  CONT3(IK)=SQRT(RACINE)
               ELSE
                  CONT3(IK) = 0.
               ENDIF
              ENDIF
                ELSE
               IF(I2.EQ.3)  CONT3(IK)=0.
                CONT2(IK)=0.
              END IF
    4       CONTINUE
              CALL HPAK(IDDU+2,CONT2)
              IF (I2.EQ.3) CALL HPAK(IDDU+6,CONT3)
            END IF
   20       CONTINUE
    3       CONTINUE
    2       CONTINUE
C
            DO 6 I2=1,2
C Layer and sector level
C Number of channels, dQ/dt, energy histograms normalization
              IF(I2.EQ.3) GO TO 6
              IF(SWUNIT(I2).NE.'Y') GO TO 6
              IFO = 0
              DO 8 ILAY=1,3  ! Loop on the Chambers  *****
              IF(SWLAYR(ILAY).NE.'Y') GO TO 8
              IFO = IFO + 1
              ILAYR=ILAY-1
              ICH=ILAY+(KK-1)*3
              MUM=HTFIRS+1000*ITRIG+100*ILAYR+300*KK
              ID=HTFIRS+1000*ITRIG+100*ILAYR+10*(KK-1)+20*(I2-1)
              IF ((I2.EQ.4).AND.(IFO.EQ.1)) THEN
               ID = ID - 100*ILAYR
              ENDIF
              IF (I2.EQ.4.AND.IFO.GT.1) THEN
               GOTO 999
              ENDIF
              DO 9 IH=3,6
               IF (SWHIST(IH).NE.'Y') GOTO 9
                  ANORM=NUEV
                  IF(I2.EQ.1.AND.(IH.EQ.4.OR.IH.EQ.6)) 
     &                                    ANORM=FILCHA(ILAY,KK)
                  IF(I2.EQ.2)             ANORM=SECTOT(ILAY,KK)
                  IF (I2.EQ.2.AND.(IH.EQ.4.OR.IH.EQ.6)) GOTO 11
                  IDD=ID+1000
                  KIDENT = ID + IH
                  HIDENT = IDD + IH
                  IF (IH.EQ.6) THEN
                   KIDENT = KIDENT + 1
                   HIDENT = HIDENT + 1
                  ENDIF
                  CALL HUNPAK(KIDENT,CONT1,HIST,1)
                  CALL HGIVE(KIDENT,CHT,NX,XMI,XMA,NY,YMI,YMA,NWT,K)
                  DO 7 IK=1,NX
                    IF(ANORM.NE.0.) THEN
                    CONT1(IK)=CONT1(IK)/ANORM
                    ELSE
                    CONT1(IK)=0.
                    END IF
    7             CONTINUE
                  CALL HPAK(HIDENT,CONT1)
   11             CONTINUE
                IF(I2.EQ.2) THEN
                  DO 12 IS=1,16
                    IF(SWSECT(IS).NE.'Y') GO TO 12
                    IC=MUM+IS-1+20*(IH-3)
                    ICC=IC+1000
                    CALL HUNPAK(IC,CONT1,HIST,1)
                    CALL HGIVE(IC,CHT,NX,XMI,XMA,NY,YMI,YMA,NWT,K)
                    ANORM=NUEV
                    IF(IH.EQ.4) ANORM=NEVSEC(IS,ICH)
                    DO 13 IK=1,NX
                      IF(ANORM.NE.0.) THEN
                      CONT1(IK)=CONT1(IK)/ANORM
                      ELSE
                      CONT1(IK)=0.
                      END IF
   13               CONTINUE
                    CALL HPAK(ICC,CONT1)
   12               CONTINUE
                END IF
    9           CONTINUE
    8           CONTINUE
    6           CONTINUE
C TRD level
C Number of channels, energy histograms normalization
              IF(SWUNIT(4).EQ.'Y') THEN
              ID=HTFIRS+1000*ITRIG+10*(KK-1)+60
              IDD=ID+1000
              ANORM = NUEV
                DO 30 IH = 3,5,2
              CALL HUNPAK(ID+IH,CONT1,HIST,1)
              CALL HGIVE(ID+IH,CHT,NX,XMI,XMA,NY,YMI,YMA,NWT,K)
                 DO 31 IK=1,NX
                    IF(ANORM.NE.0.) THEN
                    CONT1(IK)=CONT1(IK)/ANORM
                    ELSE
                    CONT1(IK)=0.
                    END IF
  31             CONTINUE
                  CALL HPAK(IDD+IH,CONT1)
  30             CONTINUE
              ENDIF
    1           CONTINUE
   19           CONTINUE
 999            CONTINUE
                TRDNOR = .TRUE.
                RETURN
                END
