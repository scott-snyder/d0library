      SUBROUTINE GTMTOF( ITRACK, NMTOF,NMSCT,IADD,IFLAG,IMUOT, TOF,
     1                   TXYZ, XYZ, DXYZ, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return MTOF contents with good TOF flag
C-
C-   Inputs  : ITRACK [I] : Track number at MUON bank (1,2,....)
C-             NMTOF  [I] : Associated Scintillator hit number 
C-   Outputs : NMSCT  [I] : total No of associated scint hits per track 
C-             IADD   [I] : scintillator address ( Chamber_id*256 + cell_id)
C-             IFLAG  [I] : quality flag
C-                           bit 0 : 1st P.M. latch missing
C-                           bit 1 : 2nd P.M. latch missing
C-                           bit 2 : 1st P.M. pedestal
C-                           bit 3 : 2nd P.M. pedestal
C-                           bit 4 : 1st P.M. non-physical
C-                           bit 5 : 2nd P.M. non-physical
C-             IMUOT  [I] : mached track number at MUOT
C-             TOF    [R] : time of flight (nsec)
C-             TXYZ   [R] : track position at scintillator
C-             XYZ    [R] : scintillator center postion (global)
C-             DXYZ   [R] : half width of scintillator  (global)
C-   Controls: None
C-
C-   Created   9-MAR-1992   Atsushi Taketani
C-   Modified  27-Feb-1994  B.S.Acharya : for Run Ib MTOF bank Format
C-   Modified  6-Feb-1995   A.S.Ito to correct bug in IER not being reset
C-                             also reset TOF values.
C-   Modified  21-Sep-1995  RE Hall put in patch to ensure alignment of
C-                            MUON and MUOT (don't use track indicies)
C----------------------------------------------------------------------
      IMPLICIT NONE
C-- Arguments
      INTEGER     ITRACK, NMTOF,NMSCT,IADD,IFLAG,IMUOT,IER
      REAL        TOF, TXYZ(3), XYZ(3), DXYZ(3) 
C-- includes
      INCLUDE 'D0$INC:ZEBCOM.INC'
C-- Local
      INTEGER NMUOT,IT,NS,LMUOT,LMUOT_TEST,LMUON
      INTEGER  LMTOF, LMTOF_TOP, GZMTOF, NR, I, GZMUOT
      CHARACTER*4 BANK_NAME
C----------------------------------------------------------------------
      IER=0                      ! Reset IER for each call
      TOF= -8888.                ! Reset TOF
      LMTOF = GZMTOF(ITRACK)     ! MTOF BANK POINTER
      IF(LMTOF.EQ.0)THEN
        IER=1            ! Bank doesn't Exist : No scint hits for this track
        GO TO 999
      ENDIF
C
C Verify bank name 
C
      CALL UHTOC(IQ(LMTOF-4),4,BANK_NAME,4)
      IF (BANK_NAME.NE.'MTOF') THEN   ! Not MTOF Bank
        IER = 1
        GOTO 999
      END IF
      NR    = IQ(LMTOF+2)                 ! No of SCINT data words per hit
      NMSCT= IQ(LMTOF+3)                 ! Total no of associated scint hits
      LMTOF_TOP = LMTOF
      LMTOF = LMTOF + 3 + (NMTOF-1)*NR    ! repeat part pointer
C
C--
C
      IADD = IQ(LMTOF+1)              ! scintillator address
      IFLAG= IQ(LMTOF+2)              ! PMT Hit  status Flag : Quality Flag
C      IMUON = IQ(LMTOF+3)             ! Track number in MUON
C     can't trust this track number since Zeb banks can get rearranged
C     since the MTOF (or MSCT) bank was filled
C     Find the index of the MUOT track associated with this MUON bank
C     follow the links
      LMUON = LQ(LMTOF_TOP+1)
      CALL UHTOC(IQ(LMUON-4),4,BANK_NAME,4)
      IF(LMUON.LE.0) THEN
        IER = 1
        GOTO 999
      ENDIF
      IF (BANK_NAME.NE.'MUON') THEN   ! Not MUON Bank
        IER = 1
        PRINT *, ' no link to muon bank'
        GOTO 999
      END IF
      NS = IQ(LMUON - 2)
      LMUOT = LQ(LMUON - NS - 1)
      IF(LMUOT.LE.0) THEN
        IER = 1
        GOTO 999
      ENDIF
      IMUOT = 0
      CALL GTMTRH(NMUOT)
      DO IT = 1,NMUOT
        LMUOT_TEST = GZMUOT(IT)
        IF(LMUOT_TEST.EQ.LMUOT) IMUOT = IT
      ENDDO
      IF(IMUOT.LE.0) THEN
        IER = 1
        GOTO 999
      ENDIF
C
      TOF  = Q(LMTOF+5)               ! Measured TOF in ns
C
C -- Track extraporated position (X,Y,Z) & Scint Geometry
C
      DO I=1,3
         TXYZ(I) = Q(LMTOF+5+I)       ! track position
          XYZ(I) = Q(LMTOF+8+I)       ! scintillator center
         DXYZ(I) = Q(LMTOF+11+I)      ! Scint Size (Half width)
      ENDDO
C
  999 RETURN
      END

