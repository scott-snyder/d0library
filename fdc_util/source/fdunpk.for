      SUBROUTINE FDUNPK( LOGCHA, DATAS, CMATCH )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack the FADC of the selected chanel of
C-              the FDC for standard datas
C-
C-   Inputs  : LOGCHA [I]
C-   Outputs : DATAS  [I] : Unpacked datas, see FDEXPD description
C-             CMATCH [L]
C-
C-   Created   7-NOV-1988   Jeffrey Bantly
C-   Updated  20-MAR-1990   Jeffrey Bantly  general cleanup 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS/LIST'
      INTEGER DATAS(0:LFADC-1), I, ISTART, ICDDN, LOGCHA
      LOGICAL CMATCH
      DATA ICDDN / 0 /
C----------------------------------------------------------------------
C
C ****  Expand the data by unpacking it
C
      CALL ZDEXPD( ICDDN, LOGCHA, DATAS)
      CMATCH = .FALSE.
      IF ( DATAS(0) .LE. 0 ) GO TO 999
      CMATCH = .TRUE.
   20 CONTINUE
C------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
