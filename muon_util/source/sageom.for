      SUBROUTINE SAGEOM(IMOD,ITUBE,ITYPE,SPL,WL1,WL2,RTUB,VTUB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get SAMUS geometry for one tube
C-
C-   Inputs  : IMOD  - module ID
C-             ITUBE - tube number
C-
C-   Outputs : ITYPE - Tube type
C-             SPL   - Split tube half distance
C-             WL1   - Wire length
C-             WL2   - Wire length of split tube
C-             RTUB(3) - center of tube
C-             VTUB(3) - tube axis vectors, |VTUB| = 1
C-
C-   Controls:
C-
C-   Created  9-SEP-1994   M. Fortner
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IMOD,ITUBE,ITYPE
      REAL    SPL,WL1,WL2,RTUB(3),VTUB(3)
      INTEGER I,IMODX,ISTA,ISEC
      REAL    RTUBES(3,2),VTUBES(3,2),TLEN(2),VDIST
      EXTERNAL VDIST
C
      CALL MUMDAT(IMOD,IMODX,ISEC,ISTA)
      CALL GTSSTG(ISTA,ISEC,ITUBE,RTUBES,VTUBES,TLEN)
C
      WL1 = TLEN(1)
      WL2 = TLEN(2)
      IF (WL1.EQ.0) GO TO 999
C
      IF (WL2.EQ.0.0) THEN
        ITYPE = 1
        SPL = 0.0
        DO I = 1,3
          RTUB(I) = RTUBES(I,1)
          VTUB(I) = VTUBES(I,1)
        END DO
      ELSE
        ITYPE = 2
        SPL = VDIST(RTUBES(1,1),RTUBES(1,2),3) * 0.5
        DO I = 1,3
          RTUB(I) = (RTUBES(I,1)+RTUBES(I,2)) * 0.5
          VTUB(I) = VTUBES(I,1)
        END DO
      END IF
C
  999 CONTINUE
      RETURN
      END
