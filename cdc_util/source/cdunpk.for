      SUBROUTINE CDUNPK( LAYER, SECTOR, WIRE, DATAS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack the FADC of the selected chanel of
C-              the CDC for standard datas
C-
C-   Inputs  : LAYER  [I] : Layer number  [0:3]
C-             SECTOR [I] : Sector number [0:31]
C-             WIRE   [I] : Wire number   [0:11]
C-   Outputs : DATAS  [I] : Unpacked datas, see ZDEXPD description
C-
C-   Created   4-FEB-1988   Olivier Callot
C-   Updated  19-MAR-1989   Qizhong Li-Demarteau  switch to ZDEXPD
C-   Updated  20-JUN-1989   Qizhong Li-Demarteau  unit for error message 
C-                                               pick up from DDEBUG.INC 
C-   Updated   3-OCT-1989   Qizhong Li-Demarteau   recognize MC data
C-   Updated  16-APR-1991   Lupe Howell   A EZRSET call was put in to 
C-                                       correspond to the EZPICK
C-   Modified 29-APR-1991   Guido Finocchiaro Swap cables for one sector
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added a check on EZERROR
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:DDEBUG.INC/LIST'
      INTEGER LAYER, SECTOR, WIRE, DATAS(*), LABEL
      INTEGER ERR, CDCTYP, DCDTYP, BADSEC, LAY, IER
      LOGICAL FIRST, MCDATA
      LOGICAL EZERROR
C
      SAVE  FIRST, MCDATA
      DATA  FIRST/.TRUE./
      DATA  MCDATA/.FALSE./
C----------------------------------------------------------------------
C
C  temporarily use different LABEL for the MC data and 'new' data, because
C  the LABEL in the earlier CDC test data has no subdetector type.
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDUNPK',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('CDCTYP',CDCTYP,ERR)
        IF (ERR .NE. 0) CDCTYP = 1
        CALL EZGET('DCDTYP',DCDTYP,ERR)
        IF (ERR .NE. 0) DCDTYP = 0
        CALL EZGET('BADSEC',BADSEC,ERR)
        IF (ERR .NE. 0) BADSEC = -1
        IF (IQ(LHEAD+1) .GT. 1000) MCDATA = .TRUE.
        CALL EZRSET                     ! Reseting the RCP file PICKed
      ENDIF
C
      LAY=LAYER  
      IF ((SECTOR.EQ.BADSEC).AND.(WIRE.LE.6)) THEN
        LAY=LAY+2
        IF (LAY.GE.4) LAY=LAY-4
      END IF
      IF (MCDATA .OR. (CDCTYP .EQ. 1)) THEN
        LABEL = 8192 + LAY * 512 + SECTOR * 16 + WIRE
      ELSE
        LABEL = LAY * 512 + SECTOR * 16 + WIRE
      ENDIF
      CALL ZDEXPD(DCDTYP, LABEL, DATAS)
  999 RETURN
      END
