      SUBROUTINE COSMIC_L2CDC(FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fast determination of existence of track
C-                         for CDC in Level-2
C-
C-   Inputs  : none
C-   Outputs : FLAG = Set False if Event should be rejected.
C-   Controls: none
C-
C-   Created  20-AUG-1990   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ILYR,ISCT,IWIR
      INTEGER NPULSE,TPULSE,NWIR
      INTEGER SECTOR(0:3)
      REAL HITLST(5,0:2,0:15)
C
      LOGICAL STATUS,FLAG
C----------------------------------------------------------------------
C
C  Initialize SECTOR array
C
      FLAG = .FALSE.
      DO ILYR=0,3
        SECTOR(ILYR) = 0
      ENDDO
C
C  Unpack Crate banks
C
      CALL DCRUNP(STATUS)               ! Unpack data
      IF (.NOT.STATUS) GO TO 999
C
C  Loop over all Layers and sectors and determine which sectors have
C  been hit.
C
      DO 10 ILYR = 0,3
        DO 20 ISCT = 0,31
          NPULSE = 0                    ! Number of hits in a wire
          TPULSE = 0                    ! Total number of hits in a sector
          NWIR = 0                      ! Number of wires hit
          DO 30 IWIR = 0,6
            CALL DFSTRK(ILYR,ISCT,IWIR,NPULSE,HITLST)
            IF (NPULSE.GT.0) THEN
              TPULSE = TPULSE + NPULSE
              NWIR = NWIR + 1
            ENDIF
   30     CONTINUE
          IF (TPULSE.GE.5.AND.NWIR.GE.5) THEN
            CALL MVBITS(1,0,1,SECTOR(ILYR),ISCT)
          ENDIF
   20   CONTINUE
   10 CONTINUE
C
C *** Find Eta, Phi and z vertex information. Pass/Fail event and fill
C     Filter result bank DLEP.
C
      CALL FLDLEP(SECTOR,FLAG)
C          
  999 RETURN
      END
