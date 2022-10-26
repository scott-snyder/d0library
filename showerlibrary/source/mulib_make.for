      SUBROUTINE MULIB_MAKE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store MUON  library entry for each track,
C-
C-   Called once per event.
C-
C-   Inputs  : GMUH banks
C-   Outputs : MUON library entries stored .
C-
C-   Created 10-MAY-1993   Jasbir Singh and Chip Stewart
C-   Updated 24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:MULCON.INC'
      INCLUDE 'D0$INC:MULDAT.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'
      INTEGER LISAE,LISV1,LISP1,LJET
      EQUIVALENCE (CSTLNK(31),LISAE),(CSTLNK(32),LISV1)
      EQUIVALENCE (CSTLNK(33),LISP1),(CSTLNK(34),LJET)
C
      INTEGER GZISAE,GZISV1
      INTEGER IPART
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZGHIT.LINK'
      INCLUDE 'D0$LINKS:IZGMUH.LINK'
C
      INTEGER GZGHIT,GZGMUH,LGMUH
      INTEGER ITRA,ITAG,ICYCLE
      INTEGER IE,IP,IL
      INTEGER I,J,JQ,II,IJ,IX,NPLIVE,PRD_KEY
      REAL XT,PT,EM,P4(4)
      INTEGER NT
      INTEGER VERSION,NSKIP
      INTEGER IREJ_MOM,ARGSOK
      REAL    PRIMARY_ENERGY,GMUH_ENERGY,TOT_GMUH_ENERGY
      REAL    DEL_ENERGY,DEL_ENERGY_MAX,PRIMARY_MOM
      REAL    GMUH_MIN_ENER,KE_DROP,PRIMARY_MASS,PRIMARY_KE
      REAL    GMUH_KE,E_DROP,PAR_MASS
      REAL    EFR_KEEP,DIR_COSINE(3),HIT_XYZ(3)
      INTEGER NUM_GMUH
C
      INTEGER LL,LZLAST
      INTEGER IER
      LOGICAL FIRST
      LOGICAL NEW_LIBRARY
      INTEGER PURGE_TRACK
C
      LOGICAL DO_ANAL
C
      DATA FIRST/.TRUE./
C
      INTEGER SSUNIT
      INTEGER NO_GMUH,YES_GMUH
      INTEGER NDATA_TENT
C
      INTEGER CYC,LREC
      INTEGER NSHDATA,ITRAG
C
      LOGICAL WRITE_OUT,FIL_OPEN,MULBEG_RUN
      INTEGER PTPREV,NL
      INTEGER RKEY(NKEY)
C
      INTEGER RUNNO,EVONUM
      INTEGER MUON_KEY
C
      INTEGER NCYC_MAX(NMOM),NCYC_REJ(NMOM),NCYC_ACC(NMOM)
      INTEGER I1,I2,I3,I4,I5,HBANK
      CHARACTER CBANK*4
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MULINI
        FIL_OPEN= MULBEG_RUN()
        CALL EZPICK('MUONLIBRARY_RCP')
        CALL EZGET_l('DO_MAKE_LIB_ANALYSIS',DO_ANAL,IER)
        CALL EZGET_l('NEW_LIBRARY',NEW_LIBRARY,IER)
        CALL EZGET('MINIMUM_GMUH_ENERGY',GMUH_MIN_ENER,IER)
        CALL EZGET_i('NCYC_MAX',NCYC_MAX,IER)
        CALL DHDIR('MUONLIBRARY_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('MUONLIB','MAKE_MUONLIB',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        CALL EZRSET
C
        DO I = 1 ,NMOM
          NCYC_REJ(I) = 0               ! INIT AT BEGINNING
          NCYC_ACC(I) = 0
        ENDDO
C
        YES_GMUH = 0
        NO_GMUH = 0
        IREJ_MOM = 0
      ENDIF
C
C ****  FILL ISP1 REF LINK -5 WITH LGMUH POINTER
C
      CALL GMUH_ISP1_LINK(0)
C
      KEY(1) = 1
      KEY(2) = 1
      KEY(3) = 1
      KEY(4) = 1
      LISAE=GZISAE()                  ! GET LINK
      LISV1=GZISV1()
      NT=0                              ! number of tracks.
      DO WHILE (LISV1 .NE. 0)
        CALL UCOPY(Q(LISV1+7),VERTEX,3)
        LISP1=LQ(LISV1-IZISP1)          ! NO GZISP1!!
        DO WHILE (LISP1 .NE. 0)

        CALL VZERO(MULIB,NMULIB)

          NL = IQ(LISP1-3)              ! NUMBER OF LINKS
          IF(NL.LT.5)THEN
            CALL ERRMSG('MUONLIBRARY','MAKE_MUONLIB',
     &        'ISP1 DOES NOT HAVE GMUH REF LINK. SKIPPING','W')
            GO TO 190
          ENDIF
C
C GET PRIMARY KEY HERE
C
          CALL UCOPY(Q(LISP1+2),P4,4)
          PRIMARY_MASS = Q(LISP1+6)
          PRIMARY_ENERGY = Q(LISP1+5)
          PRIMARY_MOM = SQRT(P4(1)*P4(1)+P4(2)*P4(2)+P4(3)*P4(3))
          IPART = IQ(LISP1+1)         ! PARTICLE ID
          CALL MUON_GETBIN(IPART,VERTEX,P4)     ! GET PRIMARY KEY
          PRD_KEY = 1                  ! INIT
          DO I = 1 , NKEY
            PRD_KEY = PRD_KEY*KEY(I)
          ENDDO
          IF(PRD_KEY.EQ.0) THEN
            GO TO 190                  ! ONE OF THE KEYS = 0
          ENDIF
C
          NT=NT + 1                     !  COUNT TRACKS
          LGMUH = LQ(LISP1-5)           ! GMUH LINK
          IF(LGMUH.GT.0) THEN
            HBANK = IQ(LGMUH-4)
            CALL DHTOC(4,HBANK,CBANK)
            IF(CBANK.NE.'GMUH') LGMUH = 0
          END IF
          IF(LGMUH.NE.0)THEN
            YES_GMUH = YES_GMUH + 1
C
C ****  REJECT TRACK IF NUMBER OF CYCLES EXCEEDS MAXIMUM ALLOWED
C ****  FOR THAT MOMENTUM BIN
            IF ( CYCLES(MUON_KEY(KEY)).GE.NCYC_MAX(KEY(3)) ) THEN
              NCYC_REJ(KEY(3)) = NCYC_REJ(KEY(3)) + 1
              GO TO 190
            ENDIF
C
            MULIB(1) = 1.0               ! VERSION NUMBER
            PTPREV = 2                  ! POINTER AREA IN PREVIOUS BANK
            MULIB(PTPREV) = 0.0                       ! NO NEXT another YET
            PTCUR = 2
            MULIB(PTCUR+1) = RUNNO()                   ! RUN NUMBER
            MULIB(PTCUR+2) = EVONUM()                  ! EVENT NUMBER
            MULIB(PTCUR+3) = IQ(LISP1+1)               ! ID IN FLOATING PT.
            CALL UCOPY(Q(LISP1+2),MULIB(PTCUR+4),4)    ! ISAJET INFO SAVED.
            MULIB(PTCUR+8) = IETAM_PRIMARY
            MULIB(PTCUR+9) = IPHIM_PRIMARY
            MULIB(PTCUR+10) = PRIMARY_MASS  !MASS
            MULIB(PTCUR+11) = VERTEX(1)
            MULIB(PTCUR+12) = VERTEX(2)
            MULIB(PTCUR+13) = VERTEX(3)
            DO I = 1 , 3
              DIR_COSINE(I) = P4(I)/PRIMARY_MOM
            ENDDO
            NDATA = 15
            WRITE_OUT = .FALSE.
C
C ****  NOW DO SECONDARIES
C
            NUM_GMUH = 0
            DO WHILE (LGMUH.NE.0)
              PTCUR = NDATA + 1
              MULIB(PTCUR) = 0          ! NO NEXT POINTER YET FOR THIS BANK
              CALL FILL_MULIB(LGMUH,MULIB(PTCUR+1),NMULIB-NDATA,NSHDATA,
     &          PAR_MASS,ITRAG)
              NUM_GMUH = NUM_GMUH + 1
              NDATA_TENT = NDATA + NSHDATA + 1        ! 1 FOR THE POINTER
C              CALL PRGMUH(13,0,0,'ALL',0)
              IF ( NSHDATA.EQ.0 ) GO TO 180
              IF ( NDATA_TENT.GT.NMULIB ) THEN
                CALL ERRMSG('MUONLIBRARY','MULIB_MAKE',
     &            'DATA TOO LONG, SKIP','W')
                GOTO 190
              ELSE
                WRITE_OUT = .TRUE.
                NDATA = NDATA_TENT      ! ACCEPT THIS GMUH
                MULIB(PTPREV) = PTCUR      ! ACCEPT CURRENT GMUH
                PTPREV = PTCUR            ! FOR NEXT TIME AROUND
              ENDIF
C
  180         CONTINUE
              LGMUH = LQ(LGMUH-1)
            ENDDO
            IF(ISUNIT.NE.0.AND.WRITE_OUT)THEN
              WRITE_OUT = .TRUE.     ! WRITE OUT
              ICYCLE = NCYCLE_MAX
              CYC = CYCLES(MUON_KEY(KEY)) + 1        ! CYCLE NOW KEY
              CALL UCOPY_i(KEY,RKEY,NKEY)
              CALL MUON_RZVOUT(MULIB(1),NDATA,RKEY,ICYCLE,' ')
              IF (IQUEST(1).NE.0.OR.IQUEST(6).NE.ICYCLE)THEN
                WRITE(SSUNIT(),605)RKEY,ICYCLE
  605           FORMAT(' ****  Error during RZVOUT1 write ***',4I8)
                DO 606 JQ=1,10
                  WRITE(SSUNIT(),607)IQUEST(JQ)
  606           CONTINUE
  607           FORMAT(I8)
              ENDIF
              ICYCLE = NCYCLE_MAX
              CYCLES(MUON_KEY(KEY)) = CYCLES(MUON_KEY(KEY))+1
              NCYCLES = NCYCLES + 1
            ENDIF
          ELSE
C
C ****  RECORDING ISP1's that have no GMUH for each KEY --> AUX File
C
            NO_GMUH = NO_GMUH + 1
            NO_CYCLES(MUON_KEY(KEY)) = NO_CYCLES(MUON_KEY(KEY))+1
            NO_NCYCLES = NO_NCYCLES + 1
          ENDIF
C
C ****  Fill histograms for this track
C
          IF ( DO_ANAL ) THEN
            CALL MULIB_MAKE_ANALYZE             ! ANALYZE AWAY
          ENDIF

  190     CONTINUE      ! jump to here if track be skipped...
C        -- going to next linear link (isp1)...
          LISP1=LQ(LISP1)
        ENDDO
C     -- going to next isv1 linear link...
        LISV1=LQ(LISV1)
      ENDDO
  999 RETURN
      ENTRY STAT_MULIB_MAKE
C PRINT OUT STATISTICS OF MAKEING THE LIBRARY
      DO I1 = 1 , NPHI_ID
        DO I2 = 1 ,NETA
          DO I3 = 1 ,NMOM
            DO I4 = 1,NPART_ID
              KEY(1) = I1
              KEY(2) = I2
              KEY(3) = I3
              KEY(4) = I4
              NCYC_ACC(I3) = NCYC_ACC(I3) + CYCLES(MUON_KEY(KEY))
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      WRITE(SSUNIT(),996)
  996 FORMAT(//' ***** MULIB_MAKE STATISTSICS ***** '/)
      WRITE(SSUNIT(),995)NCYC_ACC
C&IF LINUX
C&  995 FORMAT(' NUMBER OF TRACKS ACCEPTED  IN MOMENTUM BINS ',7I7)
C&  998 FORMAT(' NUMBER OF TRACKS REJECTED IN MOMENTUM BINS ',7I7)
C&      WRITE(SSUNIT(),997)IREJ_MOM,YES_GMUH,NO_GMUH
C&  997 FORMAT(' TRACKS REJECTED DUE TO TOO LOW MOMENTUM ',I7,/,
C&     &  ' TRACKS WITH GMUH BANKS ',I7,/,
C&     &  ' TRACKS WITH NO GMUH BANKS ',I7)
C&ELSE
  995 FORMAT(' NUMBER OF TRACKS ACCEPTED  IN MOMENTUM BINS ',7I)
  998 FORMAT(' NUMBER OF TRACKS REJECTED IN MOMENTUM BINS ',7I)
      WRITE(SSUNIT(),997)IREJ_MOM,YES_GMUH,NO_GMUH
  997 FORMAT(' TRACKS REJECTED DUE TO TOO LOW MOMENTUM ',I,/,
     &  ' TRACKS WITH GMUH BANKS ',I,/,
     &  ' TRACKS WITH NO GMUH BANKS ',I)
C&ENDIF
      END
