      SUBROUTINE GEECOH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Sets up End Cap OH geometry
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  9-NOV-1988   Rajendran Raja
C-   Updated 25-JUN-1989   N.A. Graf 
C-                         Put in mother volumes OHM+/-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INTEGER ISEG,NSGEOH,NLN,IZ,IAB
      CHARACTER*32 CSTRNG
C
      CHARACTER*17 MUTRVL(2) ! EC OH Mother Volumes
      DATA MUTRVL/'OH_BIG_MVOLUME+Z',
     &            'OH_BIG_MVOLUME-Z'/
      CHARACTER*7 ECOH(2)
      DATA ECOH/'EC_OCH+',
     &          'EC_OCH-'/  !EC outer hadronic segment
C
      CHARACTER*7 ECOHB(2)
      DATA ECOHB/'EC_OCZ+',
     &           'EC_OCZ-'/  !EC OH BEAM PIPE MODULE
C
      CHARACTER*11 ECOHE(2)
      DATA ECOHE/'EC_OCH+OEP+',
     &            'EC_OCH-OEP-'/  !EC OH endplates
C
      CHARACTER*11 ECOHEZ(2)
      DATA ECOHEZ/'EC_OCZ+ZEP+',
     &            'EC_OCZ-ZEP-'/  !EC OH MR MODULE endplates
C
      CHARACTER*7 ECOHG(2)
      DATA ECOHG/'EC_OHG+',
     &           'EC_OHG-'/       !EC OH massless gaps
C
      CHARACTER*15 ECOHZD(2)
      DATA ECOHZD/'OH_DIVISIONS+Z',
     &            'OH_DIVISIONS-Z'/
C
      CHARACTER*24 ECOHZB(2)
      DATA ECOHZB/'BEAM_PIPE_OH_DIVISIONS+Z',
     &            'BEAM_PIPE_OH_DIVISIONS-Z'/
      CHARACTER*18 ECOHBM(2)
      DATA ECOHBM/'EC_MAIN_RING_PIPE+',
     &            'EC_MAIN_RING_PIPE-'/
C----------------------------------------------------------------------
      CALL GTSRCP('EC_OH_SEGMENTS',NSGEOH,1)
      CALL SETROT('BEAM_PIPE_ROT_MATRIX')    !Set up beam pipe rotation matrix
C
      DO 400 IZ = 1,2    !+/- z
C
C      Do OH Mother volumes first
C
        CALL VOLPOS(MUTRVL(IZ))
C
        DO 200 ISEG=1,NSGEOH
          CALL STRINT(ECOH(IZ),ISEG,CSTRNG,NLN)
          CALL VOLPOS(CSTRNG(1:NLN))      !Coarse hadronic mother volume
  200   CONTINUE
C
        CALL VOLPOS(ECOHE(IZ)) !Hang  endplate
C
        CALL VOLPOS(ECOHEZ(IZ)) !Hang  endplate for special Main ring module
C
        CALL STZDIV(ECOHZD(IZ),ECOH(IZ))  !Z DIVISIONS. One needs
C
        CALL STZDIB(ECOHZB(IZ),ECOHB(IZ),ECOHBM(IZ))  !Z divisions in
C                                        ! the special main ring beam
C                                        ! pipe module
C
        CALL VOLORD(MUTRVL(IZ),6)       ! Order Mother Volume in Phi 
        CALL VOLORD(ECOH(IZ)//'1',2)    ! Order in Local Y
        CALL VOLORD(ECOH(IZ)//'5',2)    ! Order in Local Y the special module
                                        ! containing main ring beam pipe
C
        DO 100 ISEG=1,NSGEOH       !All massless gaps same
          CALL STRINT(ECOHG(IZ),ISEG,CSTRNG,NLN)
          CALL VOLPOS(CSTRNG(1:NLN))      !POSITION OH MASSLESS GAPS
  100   CONTINUE
  400 CONTINUE
C
  999 RETURN
      END
