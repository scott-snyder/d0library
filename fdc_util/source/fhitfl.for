      SUBROUTINE FHITFL(HALF,UNIT,QUAD,SECTOR,NEW_EVENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the FHIT bank for one cell using the 
C-      contents of the FXDA and  FXSC.
C-
C-   Inputs  : HALF,UNIT,QUAD,SECTOR,NEW_EVENT
C-   Outputs : none
C-   
C-   Outputs FHITPT entry:      FIRST_HIT  First hit # in sector
C-                              NHITS      Number of hits in sector
C-
C-   Created  19-AUG-1991   Robert E. Avery
C-   Updated  21-OCT-1991   Robert E. Avery  Change definition of status word
C-     (see FHIT.ZEB). Add entry FHITPT, returns pointers to hits in sectors.
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsice functions fix
C-   Updated   7-FEB-1992   Robert E. Avery  Fix bug (refetch LXSC link).
C-   Updated  17-FEB-1992   Robert E. Avery  Use BYTE_ORDER.PARAMS for
C-     UNIX compatibility.
C-   Updated  25-OCT-1993   Robert E. Avery  New version of FHIT bank
C-      (V1), contains separate delay line hits. Multipliers
C-      for some quantities have changed. See FHIT.ZEB.
C-      NOTE: Argument list for FHITPT has changed!
C-   Updated  18-NOV-1993   Robert E. Avery  minor bug fix for ALPHA. 
C-   Updated  19-AUG-1994   Susan K. Blessing  Add NEW_EVENT to call. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
C  Input:
      INTEGER HALF,UNIT,QUAD,SECTOR
C
C  Output FHITPT entry:
      INTEGER FIRST_HIT, NHITS 
      INTEGER FIRST_DL, N_DLS 
C  Local:
      INTEGER LFHIT, NWDS_FHIT, PNT_FHIT 
      INTEGER LFXSC, NWDS_FXSC, PNT_FXSC 
      INTEGER LFDCH, NEW_SIZE, OLD_LENGTH 
      INTEGER MAXWIR, WIRE
      INTEGER NHIT, IHIT, NHIT_FHIT, NHIT_OLD 
      INTEGER FHIT_STATUS 
      INTEGER FXSC_STATUS 
      INTEGER IONIZE 
      INTEGER DRIFTP 
      INTEGER RUNSAV, IDSAV, RUN, ID
      INTEGER DL_WORDS(2,MX_HIT_WIRE) 
      INTEGER DL, N_DL
      INTEGER FHIT_STAT_DL 
      INTEGER VERSION 
C  Pointer arrays
      INTEGER NUM_TH_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER NUM_DL_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER NUM_PHI_HITS(0:MXSECP,0:MXHALF)
      INTEGER PTR_TH_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER PTR_DL_HITS(0:MXSECT,0:MXQUAD,0:MXHALF)
      INTEGER PTR_PHI_HITS(0:MXSECP,0:MXHALF)
C
      LOGICAL ARRAYS_FILLED 
      LOGICAL NEW_EVENT
      REAL    STAGGER
C
      INTEGER ICOMPRESSED 
      PARAMETER( ICOMPRESSED  = 12 )
      INTEGER MASK11
      PARAMETER( MASK11 =  4095 )        ! 2**12 - 1
      INTEGER NPHI_SECTS 
      PARAMETER( NPHI_SECTS  =  (MXSECP+1)*(MXHALF+1) )
      INTEGER NTH_SECTS 
      PARAMETER( NTH_SECTS = (MXSECT+1)*(MXQUAD+1)*(MXHALF+1) )
C
      INTEGER*2 I2DATA(2)
      INTEGER I4DATA
      EQUIVALENCE (I2DATA,I4DATA)
C
C  Functions:
      INTEGER GZFXSC
      INTEGER GZFHIT
      INTEGER GZFDCH
      REAL FSTAGR
C
      DATA ARRAYS_FILLED /.FALSE./
C----------------------------------------------------------------------
C
      IF (NEW_EVENT) THEN
        CALL VZERO(NUM_TH_HITS,NTH_SECTS)
        CALL VZERO(NUM_DL_HITS,NTH_SECTS)
        CALL VZERO(NUM_PHI_HITS,NPHI_SECTS)
        CALL VZERO(PTR_TH_HITS,NTH_SECTS)
        CALL VZERO(PTR_DL_HITS,NTH_SECTS)
        CALL VZERO(PTR_PHI_HITS,NPHI_SECTS)
        ARRAYS_FILLED = .TRUE. 
      ENDIF
      LFXSC = GZFXSC(HALF,UNIT,QUAD,SECTOR)
      IF( LFXSC .LE. 0 ) GOTO 999
      IF ( BTEST( IQ(LFXSC),ICOMPRESSED ) ) THEN
        GOTO 999                        ! Bank has already been compressed
      ENDIF
      IQ(LFXSC) = IBSET( IQ(LFXSC),ICOMPRESSED ) 
      MAXWIR  = IQ( LFXSC+2 )
      NWDS_FXSC  = IQ( LFXSC+3 )
C
      LFHIT = GZFHIT()
      IF (LFHIT .LE. 0) THEN
        CALL BKFHIT(LFHIT)
        LFXSC = GZFXSC(HALF,UNIT,QUAD,SECTOR)   ! refetch link
      ENDIF
      VERSION = IQ(LFHIT+1)
      IF ( VERSION.NE.1 ) THEN          ! Drop any old version banks.
        CALL MZDROP(IXCOM,LFHIT,' ')
        CALL MZGARB(IXMAIN,0)
        CALL BKFHIT(LFHIT)
        LFXSC = GZFXSC(HALF,UNIT,QUAD,SECTOR)  
      ENDIF
      NHIT_FHIT = IQ(LFHIT+2)
      NWDS_FHIT = IQ(LFHIT+3)
      OLD_LENGTH = 3 + NWDS_FHIT*NHIT_FHIT 
C
C  If necessary, increase FHIT size.
C
      LFDCH = GZFDCH()
      NHIT = IQ(LFDCH+1) + IQ(LFDCH+10)         ! Including DL hits
      NEW_SIZE = MAX( (3 + NWDS_FHIT*NHIT),
     &                (OLD_LENGTH + NWDS_FHIT*IQ(LFXSC+1)) )
      IF ( NEW_SIZE .GT. IQ(LFHIT-1) ) THEN
        CALL MZPUSH(IXCOM,LFHIT,0,NEW_SIZE,'I')
        LFXSC = GZFXSC(HALF,UNIT,QUAD,SECTOR)   ! refetch link
      ENDIF
C  
C  Loop through wires in sector
C
      N_DL = 0
      NHIT_OLD = IQ(LFHIT+2)
      PNT_FHIT = LFHIT + OLD_LENGTH 
      DO WIRE = 0, MAXWIR-1
        STAGGER = FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)
        NHIT    = IQ( LFXSC+4+WIRE )
C
        PNT_FXSC = LFXSC + IQ(LFXSC+4+MAXWIR+WIRE) - 1
        DO IHIT = 1, NHIT
          FHIT_STATUS = IAND( MASK11 , IQ(PNT_FXSC+1) ) ! LOGCHA
          FXSC_STATUS = IQ(PNT_FXSC+9)
C
C  Drift distance:
          I4DATA = NINT( ABS( 10000. * ( Q(PNT_FXSC+2)-STAGGER ) ) ) 
          IF ( I4DATA .GT. 64000 ) THEN
            I2DATA(WORD1) = -1
            I2DATA(WORD2) = 0
          ENDIF
C
          IONIZE = NINT( 500 * Q(PNT_FXSC+7) )   
          I2DATA(WORD2) = MIN( MAX(IONIZE,0), 32000)
C                                             
          IF ( IAND(FXSC_STATUS,3) .GT. 0 ) THEN
            IF ( ABS( Q( PNT_FXSC+4 ) ) .LT. 50. ) THEN
              FHIT_STATUS = IBSET(FHIT_STATUS,13)    ! Delay lines used
            ENDIF
          END IF
C
          IQ(PNT_FHIT+1) = FHIT_STATUS
          IQ(PNT_FHIT+2) = I4DATA
C
C  Delay lines:
C
          IF ( BTEST(FHIT_STATUS,13)  ) THEN
            IF (    BTEST(FXSC_STATUS,0) 
     &        .AND. BTEST(FXSC_STATUS,1) ) THEN
              FHIT_STAT_DL = FHIT_STATUS             ! Two DL hits
            ELSE
              FHIT_STAT_DL = IBCLR(FHIT_STATUS,13)   ! One DL hit
            ENDIF
            FHIT_STAT_DL = IBSET(FHIT_STAT_DL,3)     ! DL = wire 8
            I2DATA(WORD2) = NINT( 500. * Q(PNT_FXSC+4) )   ! Z-position
C
            N_DL = N_DL + 1
            DL_WORDS(1,N_DL) = FHIT_STAT_DL 
            DL_WORDS(2,N_DL) = I4DATA
          END IF
C
          PNT_FHIT = PNT_FHIT + NWDS_FHIT 
          PNT_FXSC = PNT_FXSC + NWDS_FXSC 
        ENDDO
        IQ(LFHIT+2) = IQ(LFHIT+2) + NHIT
      ENDDO
      IF ( UNIT.EQ.0 ) THEN
        NUM_TH_HITS(SECTOR,QUAD,HALF) = IQ(LFHIT+2) - NHIT_OLD
        PTR_TH_HITS(SECTOR,QUAD,HALF) = NHIT_OLD + 1
      ELSE
        NUM_PHI_HITS(SECTOR,HALF) = IQ(LFHIT+2) - NHIT_OLD
        PTR_PHI_HITS(SECTOR,HALF) = NHIT_OLD + 1
      ENDIF
C
C Add delay line words:
C
      IF ( N_DL.GT.0 ) THEN
C
        NHIT_OLD = IQ(LFHIT+2)
        OLD_LENGTH = 3 + NWDS_FHIT * NHIT_OLD 
        NEW_SIZE = OLD_LENGTH + NWDS_FHIT * N_DL
        IF ( NEW_SIZE .GT. IQ(LFHIT-1) ) THEN
          CALL ERRMSG('FDC-FHIT-size','FHITFL',
     &      'FHIT bank too small','W')
          GOTO 999
        ENDIF
C
        PNT_FHIT = LFHIT + OLD_LENGTH 
        DO DL =  1, N_DL
          IQ(PNT_FHIT+1) = DL_WORDS(1,DL) 
          IQ(PNT_FHIT+2) = DL_WORDS(2,DL) 
          PNT_FHIT = PNT_FHIT + NWDS_FHIT 
        ENDDO
        IQ(LFHIT+2) = NHIT_OLD + N_DL
C
        NUM_DL_HITS(SECTOR,QUAD,HALF) = N_DL
        PTR_DL_HITS(SECTOR,QUAD,HALF) = NHIT_OLD + 1
      ENDIF
C
      RETURN
C
C----------------------------------------------------------------------
C
      ENTRY FHITPT(HALF,UNIT,QUAD,SECTOR,
     &  FIRST_HIT,NHITS,FIRST_DL,N_DLS)
C
      IF (.NOT.ARRAYS_FILLED) THEN
        CALL EVNTID(RUN,ID)
        IF (RUN.NE.RUNSAV .OR. ID.NE.IDSAV) THEN
          RUNSAV=RUN
          IDSAV=ID
          CALL FHIT_GETMAPS(
     &      NUM_TH_HITS,NUM_PHI_HITS,NUM_DL_HITS,
     &      PTR_TH_HITS,PTR_PHI_HITS,PTR_DL_HITS)
        ENDIF
      ENDIF
      IF ( UNIT.EQ.0 ) THEN
        NHITS = NUM_TH_HITS(SECTOR,QUAD,HALF) 
        FIRST_HIT = PTR_TH_HITS(SECTOR,QUAD,HALF) 
        N_DLS = NUM_DL_HITS(SECTOR,QUAD,HALF) 
        FIRST_DL =  PTR_DL_HITS(SECTOR,QUAD,HALF)     
      ELSE
        NHITS = NUM_PHI_HITS(SECTOR,HALF) 
        FIRST_HIT = PTR_PHI_HITS(SECTOR,HALF) 
      ENDIF
  999 RETURN
      END
