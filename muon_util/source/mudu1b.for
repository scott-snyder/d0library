      SUBROUTINE MUDU1B(LCRATE)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack one MUD1 crate in run 1B format and
C-                         fill MUHT, MUHM, MUOF, MUHP
C-
C-   Inputs  :  LCRATE - Pointer to crate in MUD1
C-
C-   Created :  26-AUG-93  M. Fortner
C-
C-   Modified:  2/94 M. Fortner  added MUHM bank
C-              6/94 M. Fortner  protection for module count mismatch
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCRATE
      INTEGER LTRAIL,ICONT,NHW,ITRIG,NDW,IVERS,NHIT,IFLG(4)
      INTEGER NHEADW,NDATA(2),NMODC,NTHIT,NTDATA
      INTEGER ICEL,IMOD,JADD,IHIT(12),JHIT(12)
      INTEGER LMOD(12),LMOD1,LHIT,LHIT1,LADC,LDAT,LMUHM
      INTEGER I,J,K,ICRID,IVRUN,ILATB,ILATW,ILATCH,NHITA
      INTEGER MODI(12),MADC(12),MLAT(4,12),MCEL(12),MFLG(12)
      INTEGER IUS1,IUS2,IUS3,IUS4,IUS5,IUS6,IUS7,IUS8
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      INTEGER GZMUHM
      EXTERNAL GZMUHM
C-----------------------------------------------------------------
C               Unpack crate header
C-----------------------------------------------------------------
      CALL GTMUD1(1,LCRATE,LTRAIL,ICONT,NHW,ITRIG,NDW,IVERS,
     &            IUS5,IUS6,IUS7,IUS8)
      IVRUN = IBITS(IVERS,20,1)
      ICRID = IBITS(ICONT,24,8)
C
      NDATA(1) = NDW
      NDATA(2) = LTRAIL - LCRATE - NHW - NDW
      NMODC = (NHW-8)/8         ! Number of modules in crate
C-----------------------------------------------------------------
C		Unpack module headers
C-----------------------------------------------------------------
      NTHIT = 0
      LMOD1 = LCRATE + 8
      DO I=1,NMODC
          LMOD(I) = LMOD1
          CALL GTMUD1(12,LMOD(I),LMOD1,MODI(I),MADC(I),IHIT(I),MFLG(I),
     &                IUS4,MLAT(1,I),MLAT(2,I),MLAT(3,I),MLAT(4,I))
          IFLG(1) = MFLG(I)
          IFLG(2) = MLAT(1,I)
          IFLG(3) = MLAT(2,I)
          IFLG(4) = MLAT(3,I)
          MCEL(I) = -1
          IF (IHIT(I).GT.0) THEN
              NTHIT = NTHIT + IHIT(I)
              IF (GZMUHM(MODI(I)).EQ.0) CALL BKMUHM(0,MODI(I),LMUHM)
              CALL MUHMFL(2,MODI(I),IHIT(I),LMOD(I),IFLG)
              CALL MUOFFL(2,MODI(I),IHIT(I),LMOD(I))
              MCEL(I) = 0
              IF (MODI(I).LT.400) MCEL(I)=1
          ENDIF
      END DO
      NTDATA = NTHIT*4 + 2
      IF (ICRID.LT.180) NTDATA=NTHIT*8+2
      IF (NTDATA.NE.NDATA(1)+NDATA(2)) THEN
          MESSID = 'MUDU1B: crate hit count mismatch'
          WRITE(MESSAG,115) ICRID,NTHIT,NDATA(1)+NDATA(2)
  115     FORMAT('Crate = ',I3,', Hits = ',I4,', Data = ',I5,' ')
          CALLER='MUDU1B'
          CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
          RETURN
      ENDIF
C-----------------------------------------------------------------
C               Loop over data ADCs
C-----------------------------------------------------------------
      DO I = 1,NMODC
          JHIT(I) = 0
      ENDDO
      LADC = LCRATE + NHW
      DO K = 1,2
          LHIT = LADC + 1
          NHITA = NDATA(K)/4
          DO J = 1,NHITA
              CALL GTMUD1(13,LHIT,LHIT1,ICEL,IUS1,IUS2,IUS3,
     &                    IUS4,JADD,IUS6,IUS7,IUS8)
              IMOD = 0
              DO I = 1,NMODC
                  IF (JADD.EQ.MADC(I)) IMOD=I
              ENDDO
C
              IF (MCEL(IMOD).EQ.0) THEN        ! Samus
                  ILATCH = 1
                  JHIT(IMOD) = JHIT(IMOD) + 1
                  ILATW = ICEL/32 + 1	       ! Create latch bit
                  ILATB = IAND(ICEL,31)
                  MLAT(ILATW,IMOD) = IBSET(MLAT(ILATW,IMOD),ILATB)
                  LDAT = LHIT + 1
                  CALL MUHPFL(2,MODI(IMOD),ICEL,ILATCH,LDAT)
                  LDAT = LHIT + 3
                  CALL MUHPFL(3,MODI(IMOD),ICEL,ILATCH,LDAT)
C
              ELSE IF (MCEL(IMOD).EQ.1) THEN   ! Even wamus cell
                  JHIT(IMOD) = JHIT(IMOD) + 1
                  ILATW = ICEL/32 + 1	       ! Get latch bits 
                  ILATB = IAND(ICEL,31)
                  ILATCH = IBITS(MLAT(ILATW,IMOD),ILATB,5)
                  ILATCH = IAND(ILATCH,17)
                  IF (ILATCH.GE.16) ILATCH=ILATCH-14
                  CALL MUHPFL(2,MODI(IMOD),ICEL,ILATCH,LHIT)
                  MCEL(IMOD) = 2
C
              ELSE IF (MCEL(IMOD).EQ.2) THEN    ! Odd wamus cell
                  CALL MUHPFL(3,MODI(IMOD),ICEL,ILATCH,LHIT)
                  MCEL(IMOD) = 1
              ENDIF
              LHIT = LHIT1
          ENDDO
          LADC = LADC + NDATA(K)
      ENDDO
C-----------------------------------------------------------------
C               Test for header - ADC mismatch
C-----------------------------------------------------------------
      DO I = 1,NMODC
          IF (IHIT(I).NE.JHIT(I)) THEN
              MESSID = 'MUDU1B: Module hit count mismatch'
  210         WRITE(MESSAG,215) MODI(I),IHIT(I),JHIT(I)
  215         FORMAT('Module = ',I3,', Header = ',I3,', Data = ',I3,' ')
              CALLER='MUDU1B'
              CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
              NHIT = 0
              LMOD(I) = 0
              IFLG(1) = 256
              CALL MUHMFL(2,MODI(I),NHIT,LMOD(I),IFLG)
              CALL MUOFFL(2,MODI(I),NHIT,LMOD(I))
          ELSE IF (MCEL(I).EQ.0) THEN           ! Store latches for SAMUS
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
