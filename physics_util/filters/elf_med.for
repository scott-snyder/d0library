      LOGICAL FUNCTION ELF_MED()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine if the current event has passed the
C-                         ELE_MEDIUM filter.
C-
C-   Returned value  : .TRUE. if event passed ELE_MEDIUM
C-
C-   Created  14-JUL-1993   Peter Grudberg
C-   Updated  16-JUL-1993   Marcel Demarteau  Adapted to omni_filter standards 
C-   Updated  31-JAN-1994   Marcel Demarteau  Updated for run 1b 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,J,NTRIGON, NFILTON, TBIT_ON(32), FBIT_ON(128)
      INTEGER LEN, LEN1, LEN_DESIRED, TRULEN, NCHAR, IER
      REAL    MED_STAT(20),MED_SUMRY(20)
      CHARACTER*32 TNAME_ON(32), FNAME_ON(128)
      CHARACTER*20 FNAME_DESIRED(20)
      LOGICAL FIRST,ELF_MED_EOJ 
      DATA LEN_DESIRED / 10 /
C      DATA FNAME_DESIRED / 'ELE_MEDIUM' /
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      ELF_MED = .FALSE.
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('ELF_MED_RCP',IER)       
        IF(IER.EQ.0) THEN
          CALL EZPICK('ELF_MED_RCP')  
          CALL EZ_GET_CHARS('FILTER_NAME',NCHAR,FNAME_DESIRED,IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('ELF_MED','ELF_MED_RCP',
     &        'ERROR GETTING ELF_MED RCP VALUES','W')
          ENDIF
          CALL EZRSET
          CALL VZERO(MED_STAT,20)
        ELSE
          CALL ERRMSG('ELF_MED','ELF_MED_RCP',
     &      'ERROR READING ELF_MED RCP FILE','F')
        ENDIF
      ENDIF
C
      MED_STAT(1)=MED_STAT(1)+1.
C
      CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
      DO I = 1, NFILTON
        LEN = TRULEN(FNAME_ON(I))
        DO J=1,NCHAR
          LEN1 = TRULEN(FNAME_DESIRED(J)) 
          IF ( FNAME_ON(I)(1:LEN) .EQ. FNAME_DESIRED(J)(1:LEN1)) THEN
            ELF_MED = .TRUE.
            MED_STAT(J+1)=MED_STAT(J+1)+1.
            GO TO 999
          ENDIF
        ENDDO
      ENDDO
  999 RETURN
C
      ENTRY ELF_MED_EOJ(MED_SUMRY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Some end of job statistics for ELF_MED filter
C-
C-   MED_SUMRY  1 = total number of events on which filter was run 
C-              2 = total number of candidates for filter 1
C-              n = total number of candidates for filter n-1
C-
C----------------------------------------------------------------------
      ELF_MED_EOJ=.TRUE.
      CALL UCOPY(MED_STAT,MED_SUMRY,20)
      RETURN
      END
