      FUNCTION WZ_SSM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-SEP-1992   Cecilia E. Gerber
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*6 CHTOP, CHTOP_Z
      INTEGER NTUPLE_ID, NTUPLE_ID_Z, IERR, IER, NSTRIP, NSTRIP_Z
      INTEGER NGOOD_EVENTS,NWEV,NZEE,LENGTH
      LOGICAL DO_ANLZMUMU, DO_ANLWMUNU, FIRST, DO_W_CUTS, DO_Z_CUTS,
     &  FILL_NT_ALL, FILL_NT_CUT,FILL_NT_ISA,DO_ANLWENU,DO_ANLZEE
      COMMON /WEV/ NWEV
      COMMON /ZEE/ NZEE
      COMMON /WMUNU/ NTUPLE_ID, NSTRIP, NGOOD_EVENTS
      COMMON /ZMUMU/ NTUPLE_ID_Z, NSTRIP_Z
      DATA FIRST /.TRUE./
      LOGICAL WZ_SSM
C----------------------------------------------------------------------
      WZ_SSM = .TRUE.
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('WZ_RCP')
        CALL EZGET('ANALYZE_W_MUNU',DO_ANLWMUNU,IER)
        CALL EZGET('ANALYZE_Z_MUMU',DO_ANLZMUMU,IER)
        CALL EZGET('ANALYZE_W_ENU',DO_ANLWENU,IER)
        CALL EZGET('ANALYZE_Z_EE',DO_ANLZEE,IER)
        CALL EZGET('FILL_NT_ALL',FILL_NT_ALL,IER)
        CALL EZGET('FILL_NT_CUT',FILL_NT_CUT,IER)
        CALL EZGET('FILL_NT_ISA',FILL_NT_ISA,IER)
        CALL EZGET('DO_W_CUTS',DO_W_CUTS,IER)
        CALL EZGET('DO_Z_CUTS',DO_Z_CUTS,IER)
        CALL EZGETS('W_CHTOP',1,CHTOP,LENGTH,IER)
        CALL EZGETS('Z_CHTOP',1,CHTOP_Z,LENGTH,IER)
        CALL EZRSET
      ENDIF
C write ntuple to disk and delete
      IF ((FILL_NT_ALL).OR.(FILL_NT_CUT).OR.(FILL_NT_ISA)) THEN
        IF (DO_ANLWMUNU) THEN
          CALL NTUPLE_FLUSH(CHTOP,NTUPLE_ID,IERR)
        ENDIF
        IF (DO_ANLZMUMU) THEN
          CALL NTUPLE_FLUSH(CHTOP_Z,NTUPLE_ID_Z,IERR)
        ENDIF
      ENDIF
      IF (DO_W_CUTS) THEN
        WRITE (6,*) 'number of events that passed W cuts', NSTRIP
        WRITE (6,*) 'number of events that have good muons',NGOOD_EVENTS
      ENDIF
      IF (DO_Z_CUTS) THEN
        WRITE (6,*) 'number of events that passed Z cuts', NSTRIP_Z
      ENDIF
      IF(DO_ANLWENU)THEN
        WRITE (6,*) 'number of W->ev candidates accepted',NWEV
      ENDIF
      IF(DO_ANLZEE)THEN
        WRITE (6,*) 'number of Z->ee candidates accepted',NZEE
      ENDIF
  999 RETURN
      END
