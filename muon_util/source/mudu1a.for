      SUBROUTINE MUDU1A(LCRATE)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack one MUD1 crate in run 1A format and
C-                         fill MUHT, MUHM, MUOF, MUHP
C-
C-   Inputs  :  LCRATE - Pointer to crate in MUD1
C-
C-   Created :  26-AUG-93  M. Fortner
C-
C-   Modified:  2/94 M. Fortner - added MUHM bank
C-              12/94 MF  Add latch words to MUHM bank, patch SAMUS MC bug
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCRATE
      INTEGER LTRAIL,ICONT,NHW,ITRIG,NDW,IVERS,NHIT,IFLG(4)
      INTEGER NHEADW,NDATA(2),NMODC,NTHIT
      INTEGER ICEL,IMOD,JMOD,MODI(12),IHIT(12),JHIT(12),MFLG(12)
      INTEGER LMOD(12),LMOD1,LHIT,LHIT1,LDAT,LMUHM
      INTEGER I,J,ICRID,IMONT,ILATCH,NHITA,IBAD
      INTEGER IUS1,IUS2,IUS3,IUS4,IUS5,IUS6,IUS7,IUS8
      INTEGER MLAT(4,12),ILATW,ILATB
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      INTEGER GZMUHM
      EXTERNAL GZMUHM
C-----------------------------------------------------------------
C               Unpack crate header
C-----------------------------------------------------------------
      CALL GTMUD1(1,LCRATE,LTRAIL,ICONT,NHW,ITRIG,NDW,IVERS,
     &            IUS5,IUS6,IUS7,IUS8)
      ICRID = IBITS(ICONT,24,8)
      IMONT = IBITS(ICONT,29,1)
      IVERS = IBITS(IVERS,0,16)
      NMODC = NHW-7         ! Number of modules in crate
C-----------------------------------------------------------------
C		Unpack module headers
C-----------------------------------------------------------------
      NTHIT = 0
      LMOD1 = LCRATE + 6
      IFLG(2) = 0
      IFLG(3) = 0
      IFLG(4) = 0
      DO I=1,NMODC
        JHIT(I) = 0
        LMOD(I) = LMOD1
        CALL GTMUD1(2,LMOD(I),LMOD1,MODI(I),IHIT(I),MFLG(I),
     &              IUS3,IUS4,IUS5,IUS6,IUS7,IUS8)
        MLAT(1,I) = 0
        MLAT(2,I) = 0
        MLAT(3,I) = 0
        IFLG(1) = MFLG(I)
        IF (IHIT(I).GT.0) THEN
          NTHIT = NTHIT + IHIT(I)
          IHIT(I) = IHIT(I)/3
          IF (ICRID.LT.182) IHIT(I) = IHIT(I)/3
          IF (GZMUHM(MODI(I)).EQ.0) CALL BKMUHM(0,MODI(I),LMUHM)
          CALL MUHMFL(2,MODI(I),IHIT(I),LMOD(I),IFLG)
          CALL MUOFFL(2,MODI(I),IHIT(I),LMOD(I))
        ENDIF
      END DO
      IF (NTHIT.NE.NDW) THEN
        MESSID = 'MUDU1A: crate hit count mismatch'
        WRITE(MESSAG,115) ICRID,NTHIT,NDW
  115   FORMAT('Crate = ',I3,', Hits = ',I4,', Data = ',I5,' ')
        CALLER='MUDU1A'
        CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
        RETURN
      ENDIF
C-----------------------------------------------------------------
C               Loop over data ADCs
C-----------------------------------------------------------------
      LHIT = LCRATE + NHW
      IF (ICRID.LT.182) THEN              ! Wamus
        NHITA = NDW/9
        DO J = 1,NHITA
          CALL GTMUD1(3,LHIT,LHIT1,ICEL,IUS1,IUS2,IUS3,
     &                IUS4,IUS5,IUS6,IUS7,IUS8)
          IMOD = IBITS(ICEL,8,9)
          JMOD = 0
          DO I=1,NMODC
            IF (IMOD.EQ.MODI(I)) JMOD=I
          ENDDO
          IF (JMOD.NE.0) THEN
            JHIT(JMOD) = JHIT(JMOD) + 1
            ILATCH = IBITS(ICEL,30,2)
            ICEL = IAND(ICEL,127)
            ILATW = ICEL/32 + 1
            ILATB = IAND(ICEL,31)
            IF (IAND(ILATCH,1).EQ.1) THEN
              MLAT(ILATW,JMOD) = IBSET(MLAT(ILATW,JMOD),ILATB)
            ENDIF
            IF (IAND(ILATCH,2).EQ.2) THEN
              ILATB = ILATB + 4
              MLAT(ILATW,JMOD) = IBSET(MLAT(ILATW,JMOD),ILATB)
            ENDIF
            LDAT = LHIT + 1
            CALL MUHPFL(2,IMOD,ICEL,ILATCH,LDAT)
            LDAT = LHIT + 5
            CALL MUHPFL(3,IMOD,ICEL,ILATCH,LDAT)
          ENDIF
          LHIT = LHIT1
        ENDDO
C-----------------------------------------------------------------
      ELSE                               ! Samus
        IBAD = 0
        IF (IVERS.EQ.10777.AND.NDW.GT.3) THEN
          LHIT = LHIT + 3
          CALL GTMUD1(4,LHIT,LHIT1,ICEL,IUS1,IUS2,IUS3,
     &                IUS4,IUS5,IUS6,IUS7,IUS8)
          LHIT = LHIT - 3
          IF (IBITS(ICEL,30,1).NE.1) IBAD=1   ! WAMUS code loaded
        ENDIF
        IF (IBAD.EQ.0) THEN
          NHITA = NDW/3
          DO J = 1,NHITA
            CALL GTMUD1(4,LHIT,LHIT1,ICEL,IUS1,IUS2,IUS3,
     &                  IUS4,IUS5,IUS6,IUS7,IUS8)
            IMOD = IBITS(ICEL,8,9)
            JMOD = 0
            DO I=1,NMODC
              IF (IMOD.EQ.MODI(I)) JMOD=I
            ENDDO
            IF (JMOD.NE.0) THEN
              JHIT(JMOD) = JHIT(JMOD) + 1
              ILATCH = 1
              ICEL = IAND(ICEL,255)/2
              ILATW = ICEL/32 + 1	       ! Create latch bit
              ILATB = IAND(ICEL,31)
              MLAT(JMOD,ILATW) = IBSET(MLAT(JMOD,ILATW),ILATB)
              LDAT = LHIT + 1
              CALL MUHPFL(2,IMOD,ICEL,ILATCH,LDAT)
              LDAT = LHIT + 2
              CALL MUHPFL(3,IMOD,ICEL,ILATCH,LDAT)
            ENDIF
            LHIT = LHIT1
          ENDDO
C-----------------------------------------------------------------
        ELSE				! SAMUS runs with WAMUS format
          NHITA = NDW/9
          DO J = 1,NHITA
            CALL GTMUD1(3,LHIT,LHIT1,ICEL,IUS1,IUS2,IUS3,
     &                  IUS4,IUS5,IUS6,IUS7,IUS8)
            IMOD = IBITS(ICEL,8,9)
            JMOD = 0
            DO I=1,NMODC
              IF (IMOD.EQ.MODI(I)) JMOD=I
            ENDDO
            IF (JMOD.NE.0) THEN
              JHIT(JMOD) = JHIT(JMOD) + 1
              ILATCH = IBITS(ICEL,30,2)
              ICEL = IAND(ICEL,127)
              ILATW = ICEL/32 + 1
              ILATB = IAND(ICEL,31)
              IF (IAND(ILATCH,1).EQ.1) THEN
                MLAT(ILATW,JMOD) = IBSET(MLAT(ILATW,JMOD),ILATB)
                LDAT = LHIT + 2
                CALL MUHPFL(2,IMOD,ICEL,1,LDAT)
                LDAT = LHIT + 4
                CALL MUHPFL(3,IMOD,ICEL,1,LDAT)
              ENDIF
              IF (IAND(ILATCH,2).EQ.2) THEN
                ILATB = ILATB + 4
                MLAT(ILATW,JMOD) = IBSET(MLAT(ILATW,JMOD),ILATB)
                LDAT = LHIT + 6
                CALL MUHPFL(2,IMOD,ICEL,1,LDAT)
                LDAT = LHIT + 8
                CALL MUHPFL(3,IMOD,ICEL,1,LDAT)
              ENDIF
            ENDIF
            LHIT = LHIT1
          ENDDO
        ENDIF
      ENDIF
C-----------------------------------------------------------------
C               Test for header - ADC mismatch
C-----------------------------------------------------------------
      DO I = 1,NMODC
        IF (IHIT(I).NE.JHIT(I)) THEN
          MESSID = 'MUDU1A: Module hit count mismatch'
  210     WRITE(MESSAG,215) MODI(I),IHIT(I),JHIT(I)
  215     FORMAT('Module = ',I3,', Header = ',I3,', Data = ',I3,' ')
          CALLER='MUDU1A'
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
          NHIT = 0
          LMOD(I) = 0
          IFLG(1) = 256
          CALL MUHMFL(2,MODI(I),NHIT,LMOD(I),IFLG)
          CALL MUOFFL(2,MODI(I),NHIT,LMOD(I))
        ELSE                              ! Store latches
          IFLG(1) = MFLG(I)
          IFLG(2) = MLAT(1,I)
          IFLG(3) = MLAT(2,I)
          IFLG(4) = MLAT(3,I)
          CALL MUHMFL(2,MODI(I),IHIT(I),LMOD(I),IFLG)
        ENDIF
      ENDDO
C
      RETURN
      END
