      FUNCTION QCD_UPK_JUTL_HEAD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack header part of JUTL bank
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  08-JAN-1993  Andrew G. Brandt
C-   Updated  24-SEP-1993  Andrew G. Brandt save MITOOL L0QUAL
C-   Updated  01-MAR-1994  Andrew G. Brandt update for V4,5 of JUTL
C-   Updated  10-MAR-1994  Andrew G. Brandt No EVTQM
C-   Updated  21-MAR-1994  Andrew G. Brandt Add GTZ
C-   Updated  01-NOV-1994  Andrew G. Brandt updated for CW
C-   Updated  07-DEC-1995  Andrew G. Brandt add new MITOOL
C-   Updated  05-MAR-1996  Andrew G. Brandt QNT_DATA to use L0 Z
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      LOGICAL QCD_UPK_JUTL_HEAD, MULTIPLE_INTERACTION_TOOL_RUN1
C
      INTEGER I
      INTEGER IVERS,FILTM,FILTM2,SLOWF,MINTF,FASTF,IER,TOOL
      REAL    SLOWZ,FASTZ,TKVZ(3),TKVDZ(3),L0Q
C
C----------------------------------------------------------------------
      QCD_UPK_JUTL_HEAD = .TRUE.
C
C- Initialize
C
      JUVERS    = -1
      FILT_MASK = -999
      FILT_MASK2= -999
      L0_Z(1)   = -999.
      L0_Z(2)   = -999.
      L0_ZF(1)  = -1
      L0_ZF(2)  = -1
      MUL_INT   = -1
      TRK_NV    = -1
      DO I=1,2
        TRK_Z(I)  = -999.
      ENDDO
      MITOOL   =  -1
      MIRUN1   =   0
      L0QUAL   =  -999.
C
C- Get JUTL header information
C
      CALL GTJUTL_HEAD
     & (IVERS,FILTM,FILTM2,SLOWZ,SLOWF,MINTF,FASTZ,FASTF,
     &  TKVZ,TKVDZ,TOOL,L0Q,IER)
      IF(IER.NE.0) THEN
        CALL ERRMSG('QCD_UPK_JUTL_HEAD','QCD_UPK_JUTL_HEAD',
     +              'No JUTL head info found','F')
        GO TO 999
      END IF
C
C For D0MDST get L0+Z elsewhere
C
      IF(D0MDST) THEN
        CALL GTZ(SLOWZ,SLOWF,MINTF,FASTZ,FASTF,
     &           TKVZ,TKVDZ,TOOL,L0Q,IER)
      END IF

C
C- Fill the common block
C
      JUVERS    = IVERS
      FILT_MASK = FILTM
      FILT_MASK2= FILTM2
      L0_Z(1)   = SLOWZ
      L0_Z(2)   = FASTZ
      L0_ZF(1)  = SLOWF
      L0_ZF(2)  = FASTF
      MUL_INT   = MINTF
      TRK_NV    = 0
C
C Count number of vertices and save leading 2
C
      DO I=1,3
        IF(TKVZ(I).NE.-999.) TRK_NV=TRK_NV+1
        IF(I.LE.2) TRK_Z(I)=TKVZ(I)
      ENDDO
      MITOOL   = TOOL
      L0QUAL   = L0Q
C
C Tracy's MI TOOL
C
      MIRUN1   = MULTIPLE_INTERACTION_TOOL_RUN1()
C
C Use L0 Z for QNT or 0 if no L0 found
C
      IF(QNT_DATA) THEN
        TRK_NV=1
        IF(L0_Z(1).GT.-990) THEN
          TRK_Z(1) = L0_Z(1)
        ELSE
          TRK_Z(1) = 0.
        END IF
      END IF
C
  999 RETURN
      END
