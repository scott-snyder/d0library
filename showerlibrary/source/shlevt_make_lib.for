      SUBROUTINE SHLEVT_MAKE_LIB
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store shower library entry for each track,
C-   using Alan Jonckheere utilities to unpack hits.
C-
C-   Called once per event.
C-
C-   Inputs  : GCAH banks
C-   Outputs : Shower library entries stored .
C-
C-   Created  20-FEB-1989   John Womersley
C-   Updated  31-JAN-1990   Rajendran Raja  OPTIMIZED
C-   Updated  14-APR-1992   W.G.D.Dharmaratna MODIFIED FOR THE VERSION 2 
C-   Updated  24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:SHLCON.INC'
      INCLUDE 'D0$INC:SHLDAT.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C      INCLUDE 'D0$INC:PAWC.INC'
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
      INTEGER PTCUR,IPART
C
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZGHIT.LINK'
      INCLUDE 'D0$LINKS:IZGCAH.LINK'
C
      INTEGER GZGHIT,GZGCAH
      INTEGER ITRA,ITAG,ICYCLE
      INTEGER IE,IP,IL
      INTEGER I,J,JQ,II,IJ,IX,NPLIVE,PRD_KEY
      REAL XT,PT,EM,P4(4)
      INTEGER NT,PTDROP
      INTEGER VERSION,NSKIP
      INTEGER IREJ_MOM,ARGSOK
      REAL    PRIMARY_ENERGY,GCAH_ENERGY,TOT_GCAH_ENERGY
      REAL    DEL_ENERGY,DEL_ENERGY_MAX,PRIMARY_MOM
      REAL    GCAH_MIN_ENER,KE_DROP,PRIMARY_MASS,PRIMARY_KE
      REAL    GCAH_KE,E_DROP,PAR_MASS
      REAL    EFR_KEEP,DIR_COSINE(3),HIT_XYZ(3)
      INTEGER NUM_GCAH
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
      INTEGER NO_GCAH,YES_GCAH
      INTEGER NDATA_TENT
C
      INTEGER CYC,LREC
      INTEGER NSHDATA,ITRAG
C
      LOGICAL WRITE_OUT
      INTEGER PTPREV,NL
      INTEGER RKEY(NKEY)
C
      INTEGER RUNNO,EVONUM
      INTEGER INDX_KEY
C
      INTEGER NCYC_MAX(NMOM),NCYC_REJ(NMOM),NCYC_ACC(NMOM)
      INTEGER I1,I2,I3,I4,I5
C----------------------------------------------------------------------
C      CALL RZCDIR('//SHOWER_LIBRARY',' ')
C
      CALL DHDIR('SHOWERLIBRARY_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','SHLEVT',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET('DO_MAKE_LIB_ANALYSIS',DO_ANAL,IER)
        CALL EZGET('NEW_LIBRARY',NEW_LIBRARY,IER)
        CALL EZGET('MINIMUM_GCAH_ENERGY',GCAH_MIN_ENER,IER)
        CALL EZGET('MAX_GCAH_ENERGY_MISMATCH',DEL_ENERGY_MAX,IER)
        CALL EZGET('NCYC_MAX',NCYC_MAX,IER)
        CALL EZRSET
C
        DO I = 1 ,NMOM
          NCYC_REJ(I) = 0               ! INIT AT BEGINNING
          NCYC_ACC(I) = 0
        ENDDO
C
        YES_GCAH = 0
        NO_GCAH = 0
        IREJ_MOM = 0
      ENDIF
C
      KEY(1) = 1
      KEY(2) = 1
      KEY(3) = 1
      KEY(4) = 1
      KEY(5) = 1                        ! DEFAULT
C
      NT = 0
C
C
      LISAE=GZISAE()                  ! GET LINK
C
      LISV1=GZISV1()
      NT=0                              ! number of tracks.
      DO WHILE (LISV1 .NE. 0)
        CALL UCOPY(Q(LISV1+7),VERTEX,3)
        LISP1=LQ(LISV1-IZISP1)          ! NO GZISP1!!
        DO WHILE (LISP1 .NE. 0)
          NL = IQ(LISP1-3)              ! NUMBER OF LINKS
          IF(NL.LT.5)THEN
            CALL ERRMSG('SHOWERLIBRARY','SHLEVT_MAKE_LIB',
     &        'ISP1 DOES NOT HAVE GCAH REF LINK. SKIPPING','W')
            GO TO 190
          ENDIF
          NT=NT + 1                 ! COUNT TRACKS
          LGCAH = LQ(LISP1-5)           ! GCAH LINK.
          IF(LGCAH.NE.0)THEN
            YES_GCAH = YES_GCAH + 1
C
C ****  QUICK MOMENTUM ABORT
C
            CALL UCOPY(Q(LISP1+2),P4,4)
            PRIMARY_MOM = SQRT(P4(1)*P4(1)+P4(2)*P4(2)+P4(3)*P4(3))
            PRIMARY_MASS = Q(LISP1+6)
            PRIMARY_KE   = P4(4) - PRIMARY_MASS
            PRIMARY_ENERGY = Q(LISP1+5)
            TOT_GCAH_ENERGY = 0.0
            KE_DROP = 0.0
            E_DROP = 0.0
C
            IF ( PRIMARY_ENERGY.LT.MOMBIN(1)) THEN
              IREJ_MOM = IREJ_MOM + 1
              GO TO 190                        ! NEXT TRACK FAST
            ENDIF
C
C GET PRIMARY KEY HERE
C
            IPART = IQ(LISP1+1)         ! PARTICLE ID
            CALL GETBIN_NOCD(IPART,VERTEX,P4)     ! GET PRIMARY KEY
            PRD_KEY = 1                  ! INIT
            DO I = 1 , NKEY
              PRD_KEY = PRD_KEY*KEY(I)
            ENDDO
            IF(PRD_KEY.EQ.0) THEN 
              GO TO 190   ! ONE OF THE KEYS = 0
            ENDIF
C
C ****  REJECT TRACK IF NUMBER OF CYCLES EXCEEDS MAXIMUM ALLOWED
C ****  FOR THAT MOMENTUM BIN
            IF ( CYCLES(INDX_KEY(KEY)).GE.NCYC_MAX(KEY(3)) ) THEN
              NCYC_REJ(KEY(3)) = NCYC_REJ(KEY(3)) + 1
              GO TO 190
            ENDIF
C
            SHLB(1) = 2.0            ! VERSION NUMBER
            PTPREV = 2                  ! POINTER AREA IN PREVIOUS BANK
            SHLB(PTPREV) = 0.0          ! NO NEXT POINTER YET
            PTCUR = 2
            SHLB(PTCUR+1) = RUNNO()     ! RUN NUMBER
            SHLB(PTCUR+2) = EVONUM()    ! EVENT NUMBER
            SHLB(PTCUR+3) = IQ(LISP1+1) ! ID IN FLOATING PT.
            CALL UCOPY(Q(LISP1+2),SHLB(PTCUR+4),4)    ! ISAJET INFO SAVED.
            SHLB(PTCUR+8) = IETAC_PRIMARY
            SHLB(PTCUR+9) = IPHIC_PRIMARY
            SHLB(PTCUR+10) = PRIMARY_MASS  !MASS 
            SHLB(PTCUR+11) = VERTEX(1) 
            SHLB(PTCUR+12) = VERTEX(2)
            SHLB(PTCUR+13) = VERTEX(3)
            DO I = 1 , 3
              DIR_COSINE(I) = P4(I)/PRIMARY_MOM
            ENDDO
            CALL CLINPH_FAST_XYZ(VERTEX,DIR_COSINE,HIT_XYZ,ARGSOK)
            IF(ARGSOK.NE.0) THEN 
              CALL ERRMSG('SHOWERLIBRARY','SHLEVT_MAKE_LIB',
     &        'CLINPH_FAST_XYZ ARGSOK NE 0. SKIPPING','W')
              GO TO 190  
            ENDIF
            SHLB(PTCUR+14) = HIT_XYZ(1)
            SHLB(PTCUR+15) = HIT_XYZ(2)
            SHLB(PTCUR+16) = HIT_XYZ(3)
            PTDROP = 19
            SHLB(PTDROP) = 0.0 !TO BE SET: SUM OF THE ENERGY FROM DROPPED BANKS
            NDATA = 19
            WRITE_OUT = .FALSE.
C
C ****  NOW DO SECONDARIES
C
            NUM_GCAH = 0
            DO WHILE (LGCAH.NE.0)
              PTCUR = NDATA + 1
              SHLB(PTCUR) = 0          ! NO NEXT POINTER YET FOR THIS BANK
              CALL FILL_GSHLB(SHLB(PTCUR+1),NSHLB-NDATA,NSHDATA,
     &          PAR_MASS,ITRAG)
              IF (SHLB(PTCUR+10).GT. 9990.0) THEN  ! SKIP THE TRACK
                CALL ERRMSG('SHOWERLIBRARY','SHLEVT_MAKE_LIB',
     &            'BAD FLAG P4=9999 REJECT TRACK','W')
                GOTO 190
              ENDIF
              GCAH_ENERGY = SHLB(PTCUR+4)       !  ENERGY
              GCAH_KE = SHLB(PTCUR+4)-PAR_MASS     !K. ENERGY
              IF(GCAH_KE .LT. 0.0) GCAH_KE = 0.0  !
              NUM_GCAH = NUM_GCAH + 1
              TOT_GCAH_ENERGY = TOT_GCAH_ENERGY + GCAH_KE
              IF(GCAH_ENERGY.LT.GCAH_MIN_ENER .OR. 
     &            SHLB(PTCUR+10).LT. -9990.0) THEN
                E_DROP = E_DROP + GCAH_ENERGY
                GO TO 180    ! REJECT
              ENDIF
              NDATA_TENT = NDATA + NSHDATA + 1        ! 1 FOR THE POINTER
C
              IF(NDATA_TENT.GT.NSHLB)THEN
                CALL ERRMSG('SHOWERLIBRARY','SHLEVT_MAKE_LIB',
     &          'DATA TOO LONG, SKIP','W')
                GOTO 190
              ELSE
                WRITE_OUT = .TRUE.
                NDATA = NDATA_TENT      ! ACCEPT THIS GCAH
                SHLB(PTPREV) = PTCUR      ! ACCEPT CURRENT GCAH
                PTPREV = PTCUR            ! FOR NEXT TIME AROUND
              ENDIF
C
  180         CONTINUE
              LGCAH = LQ(LGCAH-1)
            ENDDO
            SHLB(PTDROP) = E_DROP
C
            IF(ISUNIT.NE.0.AND.WRITE_OUT)THEN
              DEL_ENERGY = TOT_GCAH_ENERGY - PRIMARY_KE
              IF(DEL_ENERGY.GT.DEL_ENERGY_MAX)THEN      ! ONLY IF SECONDARY
                ! GREATER THAN PRIMARY
                CALL ERRMSG('SHOWERLIBRARY','SHLEVT_MAKE_LIB',
     &            'PRIMARY ENERGY TOTAL_GCAH_ENERGY
     &            MISMATCH--REJECT TRACK','W')
                WRITE_OUT = .FALSE.     ! DON'T WRITE OUT
                GO TO 190
              ENDIF
C
              ICYCLE = NCYCLE_MAX
C
              CYC = CYCLES(INDX_KEY(KEY)) + 1        ! CYCLE NOW KEY
C
              CALL UCOPY(KEY,RKEY,NKEY)
C
              CALL RZVOUT1(SHLB(1),NDATA,RKEY,ICYCLE,' ')
C
              IF (IQUEST(1).NE.0.OR.IQUEST(6).NE.ICYCLE)THEN
                WRITE(SSUNIT(),605)RKEY,ICYCLE
  605           FORMAT(' ****  Error during RZVOUT1 write ***',4I8)
                DO 606 JQ=1,10
                  WRITE(SSUNIT(),607)IQUEST(JQ)
  606           CONTINUE
  607           FORMAT(I8)
              ENDIF
C
C ****  Fill histograms for this track
C
              IF ( DO_ANAL ) THEN
                CALL ANALYZE_MAKE_LIB             ! ANALYZE AWAY
              ENDIF
C
              ICYCLE = NCYCLE_MAX
C
              CYCLES(INDX_KEY(KEY)) =
     &        CYCLES(INDX_KEY(KEY))+1
C
              NCYCLES = NCYCLES + 1
C
            ENDIF
          ELSE
            NO_GCAH = NO_GCAH + 1
          ENDIF
  190     CONTINUE      ! jump to here if track be skipped...
C        -- going to next linear link (isp1)...
          LISP1=LQ(LISP1)
        ENDDO
C     -- going to next isv1 linear link...
        LISV1=LQ(LISV1)
      ENDDO
  999 RETURN
      ENTRY STAT_MAKE_LIB
C PRINT OUT STATISTICS OF MAKEING THE LIBRARY
      DO I1 = 1 , NVRT
        DO I2 = 1 ,NETA 
          DO I3 = 1 ,NMOM 
            DO I4 = 1,NPART_ID
              DO I5 = 1,NPHI_ID
                KEY(1) = I1
                KEY(2) = I2
                KEY(3) = I3
                KEY(4) = I4
                KEY(5) = I5
                NCYC_ACC(I3) = NCYC_ACC(I3) + CYCLES(INDX_KEY(KEY))
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      WRITE(SSUNIT(),996)
  996 FORMAT(//' ***** SHLEVT_MAKE_LIB STATISTSICS ***** '/)
      WRITE(SSUNIT(),995)NCYC_ACC
C&IF LINUX
C&  995 FORMAT(' NUMBER OF TRACKS ACCEPTED  IN MOMENTUM BINS ',7I7)
C&      WRITE(SSUNIT(),998)NCYC_REJ
C&  998 FORMAT(' NUMBER OF TRACKS REJECTED IN MOMENTUM BINS ',7I7)
C&      WRITE(SSUNIT(),997)IREJ_MOM,YES_GCAH,NO_GCAH
C&  997 FORMAT(' TRACKS REJECTED DUE TO TOO LOW MOMENTUM ',I7,/,
C&     &  ' TRACKS WITH GCAH BANKS ',I7,/,
C&     &  ' TRACKS WITH NO GCAH BANKS ',I7)
C&ELSE
  995 FORMAT(' NUMBER OF TRACKS ACCEPTED  IN MOMENTUM BINS ',7I)
      WRITE(SSUNIT(),998)NCYC_REJ
  998 FORMAT(' NUMBER OF TRACKS REJECTED IN MOMENTUM BINS ',7I)
      WRITE(SSUNIT(),997)IREJ_MOM,YES_GCAH,NO_GCAH
  997 FORMAT(' TRACKS REJECTED DUE TO TOO LOW MOMENTUM ',I,/,
     &  ' TRACKS WITH GCAH BANKS ',I,/,
     &  ' TRACKS WITH NO GCAH BANKS ',I)
C&ENDIF
      END
