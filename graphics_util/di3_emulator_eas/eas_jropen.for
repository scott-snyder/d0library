      SUBROUTINE JROPEN (N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   This routine opens a retained segment for drawing. The
CD   parameter passed is used to name the retained segment.
CD       N  --  integer value from 1 to 32767
C-
C-   Inputs  :  N
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-SEP-1988   S. ABACHI, A. VIRGO
C-   UPDATED  01-DEC-1988   S. ABACHI     Added modling routine KMODL
C-   UPDATED  27-feb-1989   S. ABACHI     Modified interactive nodes and
C-                                        introduced KIMGTR routine
C-   UPDATED  11-JUN-1989   S. ABACHI     New DI3000 emulator stuff created
C-   UPDATED  10-MAY-1990   S. ABACHI     rendering nodes included
C-   UPDATED  08-JUL-1990   S. ABACHI     3D matrix nodes were added
C-
C----------------------------------------------------------------------
      EXTERNAL ERRHND
      INTEGER N
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:TEXATT.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
C
      INTEGER I, J, K
      LOGICAL DETEC, NDI3
      CHARACTER*80 EMSG
      CHARACTER*3  SSTR
      CHARACTER*4  DET, PKID, TEMP, DISPL2
      REAL TTRV(3), TSCV(3)
      DATA DISPL2 /'DEFG'/
C
      IF (N .LT. 1 .OR. N .GT. 32767) THEN
        WRITE (EMSG,1000) N
 1000   FORMAT('JROPEN: RETAINED SEGMENT NUMBER INVALID',1X,I10)
        CALL ERROR(EMSG(:42))
      ENDIF
C
      NDI3 = NUDI3
      IF(NOROT) NUDI3 = .FALSE.
C
      DO I=1,NSEGS
        IF (SEGINF(1,I) .EQ. N) THEN
          WRITE (EMSG,1010) N
 1010     FORMAT('JROPEN: SEGMENT ALREADY EXISTS',1X,I5)
          CALL ERROR(EMSG(:28))
        ENDIF
      ENDDO
      NSEGS = NSEGS + 1
      NRSEG = NRSEG + 1
      CALL KBLDN(NRSEG, SSTR)
      SEGNAM = 'R' // SSTR
      INST = SEGNAM // '.S'
      DET    = 'D' // SSTR
      PKID   = 'I' // SSTR
      TEMP   = 'C' // SSTR
      SEGNUM = N
      SEGINF(1,NSEGS) = N
      SEGINF(2,NSEGS) = CTTYPE + 4 * DHILIT + 8 * DVISIB +
     +                  65536 * DPIKID
      SEGINF(3,NSEGS) = DDETEC + 65536 * DSGPRI
C
C   SET THE STARTING PRIMITIVE NUMBER
C
      SEGINF(4,NSEGS) = NPRIM + 1
C
C   Save the internal segment name.
C
      SEGINF(6,NSEGS) = NRSEG
      IF (SEGOPN) THEN
        CALL ERROR('JROPEN: A SEGMENT IS ALREADY OPEN')
        GOTO 999
      ENDIF
      CHJUST = DHJUST
      CVJUST = DVJUST
      NVECTS = 1
      VSTAT(NVECTS) = .FALSE.
      CALL KQUEV(CPX, CPY, CPZ, 'MOVE')
      DETEC = (DDETEC .GT. 0)
      CALL PSEPOF(DET, DETEC, TEMP, ERRHND)
      CALL PSEPID(TEMP, PKID, SEGNAM, ERRHND)
      IF (DVISIB .EQ. 1) THEN
        IF(.NOT. NOROT) THEN
          CALL PINCL (DET, EMDISP, ERRHND)
        ELSE
          CALL PINCL (DET, TDISP, ERRHND)
        ENDIF
      ENDIF
C
C--------------------
      IF(.NOT. NUDI3) THEN
        CALL PBEGS(SEGNAM, ERRHND)
        IF(IREND .EQ. 2) CALL PSURRE('SURRN"', '"', ERRHND)  ! TESTING
        CALL KVWTR
        CALL PXFMAT('MXFORM1', '"', ERRHND)
        CALL KIMGTR
        CALL PXFMAT('MXFORM2', '"', ERRHND)
        IF (MODEL) THEN
          CALL KMODL
        ENDIF
        CALL PINST('S','"',ERRHND)
        CALL PENDS(ERRHND)
C--------------------
      ELSE
        IF(.NOT. BATCH) THEN
          CALL KFUN
          CALL KFUNC
          CALL KVW
        ELSEIF(BATSEG) THEN
          CALL KFUN
          CALL KFUNC
          CALL KVW
          BATSEG = .FALSE.
        ENDIF
        CALL PBEGS(SEGNAM, ERRHND)
        IF(IREND .EQ. 2) CALL PSURRE('SURRN"', '"', ERRHND)       ! TESTING
        IF (MODEL) THEN
          CALL KMODL
        ENDIF
        CALL PINST('S','"',ERRHND)
        CALL PENDS(ERRHND)
      ENDIF
C--------------------
C
      IF(IREND .EQ. 2) THEN              ! TESTING
        CALL PFN('CONS"', 'CONSTANT', ERRHND)
        CALL PCONN(SEGNAM//'.SURRN"', 1, 1, 'CONS"', ERRHND)
        CALL PCONN('CONS"', 1, 1, 'TURNONDISPLAY', ERRHND)
        CALL PSNFIX(0, 2, 'CONS"', ERRHND)
      ENDIF
C
      SEGOPN = .TRUE.
      SEGSOP = .TRUE.
      CALL KINIC
C
      IF(NOROT) NUDI3 = NDI3
C
C
C ****  STORE VIEWING PARAMETER
C
      CALL ESTOREVW(N)
C
  999 CONTINUE
      RETURN
      END
