      SUBROUTINE RECALC_MUON(LPMUO,PT_NEW,ETA_NEW,THETA_NEW,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Jacket around MUON_VTX_RECALC.  
C-
C-   Inputs  : LPMUO - Pointer to PMUO
C-   Outputs : PT_NEW - Muon Pt at primary vtx
C-             ETA_NEW - Muon eta at primary vtx
C-             THETA_NEW - Muon theta at primary
C-             IERR - 0 ==> OK
C-                    1 ==> No calculation (muon already assigned to vtx1)
C-                   <0 ==> Error
C-   Controls:
C-
C-   Created  11-OCT-1995   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LPMUO,IERR
      REAL PT_NEW,ETA_NEW,THETA_NEW
C- Local variables
      REAL    PTKICK,PTKICK_RETURN,ZV0,ZV1
      INTEGER IVTX,LVERT
      CHARACTER ERRTXT*80,HERE*(*)
      PARAMETER(HERE='RECALC_MUON')
C- External functions/saves
      INTEGER GZVERT,LENOCC
      REAL    MUON_VTX_RECALC
      SAVE PTKICK
C-----------------------------------------------------------------------

C- Dummy values

      ETA_NEW = 1.0E10
      THETA_NEW = -1.0
      PT_NEW = -1.0

C- Some initial sanity checks (Is there a PMUO.  Is it associated to a
C- secondary vertex...)

      IERR = -1
      IF( LPMUO.LE.0 ) GOTO 999
C
      IVTX = IQ(LPMUO+54)
      IF( IVTX.LE.1 ) THEN
        ETA_NEW = Q(LPMUO+16)
        THETA_NEW = Q(LPMUO+15)
        PT_NEW = Q(LPMUO+14)
        IERR = 1
        GOTO 999
      ENDIF

C- Setup the vertex Z values

      IERR = -2
      LVERT=GZVERT(1)
      IF( LVERT.LE.0 ) GOTO 999
      ZV1=Q(LVERT+5)

      IERR = -3
      LVERT=GZVERT(IVTX)
      IF( LVERT.LE.0 ) GOTO 999
      ZV0=Q(LVERT+5)

C- Safety checks (after ill-defined PMUO events of e+jets analysis)

      IF( SQRT(Q(LPMUO+66)**2+Q(LPMUO+67)**2).EQ.0 ) THEN
        WRITE(ERRTXT,8001) (Q(LPMUO+66+IVTX),IVTX=0,2)
 8001   FORMAT('Point outside calorimeter=(',F6.1,',',F6.1,',',F6.1,')')
        CALL ERRMSG('Bad_PMUO_Point',HERE,ERRTXT(1:LENOCC(ERRTXT)),'W')
        IERR = -4
        GOTO 999
      ENDIF

C- Do the work

      IERR = 0
      PT_NEW = MUON_VTX_RECALC(IQ(LPMUO+1),ZV0,ZV1,PT_NEW,ETA_NEW,
     >  THETA_NEW,PTKICK)
      GOTO 999
C
C-----------------------------------------------------------------------
      ENTRY RELCALC_MUON_PTKICK(PTKICK_RETURN)
C-----------------------------------------------------------------------
      PTKICK_RETURN = PTKICK
C
  999 RETURN
      END
