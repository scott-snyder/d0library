      SUBROUTINE FGET_DL_INFO(MODULE,ISEG,
     &          DT_SW0,DT_HV,DT_SIG,
     &          PH_SW0,PH_HV,PH_SIG,
     &          DELLEN,DELVEL, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return delay line info from ftda banks, 
C-                          and STP banks.
C-
C-   Inputs  : MODULE,ISEG = Module and segment number for that module
C-   Outputs : DT_SW0,DT_HV,DT_SIG, Drift time of DL's and assos. SW
C-             PH_SW0,PH_HV,PH_SIG, Pulse Height of DL's and assos. SW
C-             DELLEN,DELVEL Delay line length and Velocities from STP bank
C-             OK = TRUE if delay line found
C-
C-   Created  8-JAN-1991    Rob Avery
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C Input:
      INTEGER MODULE,ISEG
C Output:
      REAL    DT_HV,DT_SIG,DT_SW0
      REAL    PH_HV,PH_SIG,PH_SW0
      REAL    DELLEN,DELVEL
      LOGICAL OK
C Local:
      INTEGER IADDS,NHITS,HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER NEL,NWORDS,IHIT
      INTEGER DPTR_HV ,DPTR_SIG, DPTR_SW0
      INTEGER LFXDA,GZFXDA 
      INTEGER MASK 
      PARAMETER( MASK =  3)
C
      REAL    ETZER0,TZSENS,VELMIN,VELPLU
      REAL    TZPLUS,TZMINU,ZMAX
      REAL QHIT(18)
      INTEGER IQHIT(18)
      REAL CONT(62)
      INTEGER ICONT(62)
      EQUIVALENCE(ICONT,CONT)
      EQUIVALENCE(IQHIT,QHIT)
C
C----------------------------------------------------------------------
      OK=.FALSE.
C
      CALL GTFSEG(MODULE,ISEG,CONT)
      NHITS=ICONT(3)
      IADDS=ICONT(2)
      CALL FCODER(IADDS,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
      IF (NHITS.LE.0) GO TO 999
      IF (UNIT.GT.0) GO TO 999
C
C
      IHIT=CONT(12)
      CALL GTFTSC(HALF,QUAD,SECTOR,'HIT',IHIT,NEL,
     X           NWORDS,QHIT)
      IF ( IAND(IQHIT(9),MASK) .NE. 3 ) GOTO 999

      CALL FDLHST(HALF,UNIT,QUAD,SECTOR,WIRE,IHIT)
C
      CALL FGTLTM(HALF,0,QUAD,SECTOR,0,ETZER0,TZSENS,VELMIN,VELPLU)
      CALL FGTLTM(HALF,0,QUAD,SECTOR,8,ETZER0,TZPLUS,VELPLU,ZMAX)
      CALL FGTLTM(HALF,0,QUAD,SECTOR,9,ETZER0,TZMINU,VELMIN,ZMAX)
C
      DPTR_SW0 = IQHIT(10)
      DPTR_HV = IQHIT(11)
      DPTR_SIG = IQHIT(12)
      LFXDA = GZFXDA(HALF,UNIT,QUAD,SECTOR)
C
      DT_SW0 = Q(LFXDA+DPTR_SW0+2)-TZSENS
      DT_HV = Q(LFXDA+DPTR_HV+2)-TZPLUS
      DT_SIG = Q(LFXDA+DPTR_SIG+2)-TZMINU
C
      PH_SW0 = Q(LFXDA+DPTR_SW0+3)
      PH_HV = Q(LFXDA+DPTR_HV+3)
      PH_SIG = Q(LFXDA+DPTR_SIG+3)
C
      DELLEN = ZMAX
      DELVEL = ABS(VELPLU)
C
      OK=.TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
