      SUBROUTINE CRTSEG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build track segments in the CDC.
C-              Use the modified NA3 algorithm, in the R-Phi space.
C-              Limit phi-diff to speed up the search.
C-              Intermediate planes searched from previous plane.
C-              Store results in DTSG banks
C               In this version: use only sectors along lepton roads
C-
C-   Inputs  : DSEC banks
C-   Outputs : DTSG bank
C-
C-   Created  17-SEP-1987   Olivier Callot
C-   Updated   9-MAY-1988   Olivier Callot  Loop by inefficiencies
C-   Updated  30-JUN-1989   Qizhong Li-Demarteau   use SRCP 
C-   Updated  28-DEC-1990   Qizhong Li-Demarteau   use PI from PI.DEF
C-                                   and no DTRH bank if no enough hits
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
C
      INTEGER  IPL, LHIT, I, J, K, NUM, LABL, MXTAG
      INTEGER  INEF, FIRSPL, LASTPL, FIRHIT, LASHIT, NMISS, MEDPLA
      INTEGER  JHIT, JHTMIN, KPDTSG, IP, KPWIRE
      INTEGER  IPHIT(0:MXSENS), MINLAS(0:MXSENS), LISTPT(0:MXSENS)
      INTEGER  MAXLAY, CDINFM, ERR
      INTEGER  NTOTHT, NHTLMT, GZCDCH
      INTEGER IER
      REAL     CDMXPH, CDTRTO
      REAL     PHIFIR, RAYFIR, PHILAS, PHIOVR, DPH, DPHMIN
      REAL     PHIEXP, TOLRES, PHIMIN, PHIMAX
      LOGICAL  FIRST
      LOGICAL EZERROR
C
      SAVE FIRST
      DATA     FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CRTSEG',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('MAXLAY',MAXLAY,ERR)
        CALL EZGET('CDINFM',CDINFM,ERR)
        CALL EZGET('CDMXPH',CDMXPH,ERR)
        CALL EZGET('CDTRTO',CDTRTO,ERR)
        CALL EZRSET
      ENDIF
C
C   check if there are enough hits possiblely to build a segment 
C
      LCDCH = GZCDCH()
      NTOTHT = IQ(LCDCH + 1)
      NHTLMT = NBSENS - CDINFM
      IF (NTOTHT .LE. NHTLMT) GOTO 999   
      CALL BKDTRH
C
      DO 100 LAYER = 0, MAXLAY
C
C ****  build list of hits, ordered in phi, for all the wires
C
        CALL CRHITL
C
C ****  Start loop to find hits
C
        DO 110 INEF = 0, CDINFM
          MXTAG = NBSENS - INEF
          IF ( DBGFLG .AND. LVLDBG(6) .GE. 3 ) THEN
            WRITE( LUNDBG, 1000 ) MXTAG
 1000       FORMAT(/5X,'**** start looking for segments with ',I1,
     &             ' hits'/)
          ENDIF
          DO 120 FIRSPL = 0, INEF
            CALL VFILL( IPHIT(0) , NBSENS, 1 )
            CALL VZERO( LISTPT(0), NBSENS )
            CALL VFILL( MINLAS(0), NBSENS, 1)
            DO 130 FIRHIT = 1, IQ(LHITL(FIRSPL)+1)
              KPWIRE  = LHITL(FIRSPL) + 7* FIRHIT - 6
              IF( IQ( KPWIRE+7 ) .GT. MXTAG) GOTO 130       ! don't use again
              PHIFIR = Q( KPWIRE+2 )
              PHIMIN = PHIFIR - CDMXPH
              PHIMAX = PHIFIR + CDMXPH
              RAYFIR = Q( KPWIRE+1 )
              TOLRES = CDTRTO / RAYFIR
              LISTPT( FIRSPL ) = FIRHIT
              DO 135 LASTPL = MXSENS, MXSENS - ( INEF - FIRSPL ), -1
                IF( LASTPL .NE. MXSENS ) LISTPT( LASTPL+1 ) = 0
                DO 140 LASHIT = MINLAS(LASTPL), IQ(LHITL(LASTPL)+1)
                  KPWIRE = LHITL( LASTPL ) + 7*LASHIT - 6
                  PHILAS = Q( KPWIRE + 2 )
                  IF ( PHILAS .LT. PHIMIN ) THEN
C
C ****  Because hits are Phi ordered, never look again to this last hit
C ****  during the scan of the first hits of this plane. It has too small
C ****  a phi for being used again...
C
                    MINLAS(LASTPL) = MINLAS(LASTPL) + 1
                    GOTO 140
                  ENDIF
                  IF ( PHILAS .GT. PHIMAX ) GOTO 135
                  IF( IQ( KPWIRE+7 ).GT. MXTAG) GOTO 140      ! don"t reuse it
C
C ****  If PHIFIR is over PI, work only with PHILAS below ( already worked
C ****  with duplicated info around -PI )
C
                  IF ( PHIFIR .GE. PI ) THEN
                    IF( PHILAS .GE. PI ) GOTO 135
                  ENDIF
                  PHIOVR = ( PHILAS-PHIFIR ) /
     &                   ( Q( KPWIRE+1 )- RAYFIR )
                  NMISS = INEF - FIRSPL - (MXSENS-LASTPL)
                  LISTPT( LASTPL ) = LASHIT
                  IF ( DBGFLG .AND. LVLDBG(6) .GE. 3 ) THEN
                    WRITE( LUNDBG, 1200 ) FIRSPL, LASTPL, FIRHIT, LASHIT
 1200               FORMAT(10X,'Firspl, Lastpl, Firhit, Lashit =',4I5)
                  ENDIF
C
C ****  Loop on inner wires
C
                  DO 150 MEDPLA = FIRSPL+1, LASTPL-1
                    DPHMIN = 100.
                    IF( IQ(LHITL(MEDPLA)+1) .LE. 0 ) GOTO 155
                    JHIT   = IPHIT( MEDPLA )
                    KPWIRE = LHITL( MEDPLA ) + 7*(JHIT-1) + 1
C
C ****  Loop on hit, start with previous one, direction depends on sign,
C ****  will stop due to non decreasing distance. Store hit pointer
C
  160               CONTINUE
                    DPH = PHIFIR - Q( KPWIRE+2) + PHIOVR *
     &                           ( Q( KPWIRE+1) - RAYFIR )
                    IF( ABS(DPH) .LT. DPHMIN ) THEN
                      DPHMIN = ABS( DPH )
                      JHTMIN = JHIT
                      IF( DPH .GT. 0. ) THEN
                        IF ( JHIT .LT. IQ(LHITL(MEDPLA)+1) ) THEN
                          JHIT = JHIT + 1
                          KPWIRE = KPWIRE + 7
                          GOTO 160
                        ENDIF
                      ELSE
                        IF( JHIT .NE. 1 ) THEN
                          JHIT = JHIT - 1
                          KPWIRE = KPWIRE - 7
                          GOTO 160
                        ENDIF
                      ENDIF
                    ENDIF
                    IPHIT( MEDPLA ) = JHIT
  155               CONTINUE
                    IF ( DPHMIN .LT. TOLRES ) THEN
                      LISTPT( MEDPLA ) = JHTMIN
                    ELSE
                      LISTPT( MEDPLA ) = 0
                      NMISS = NMISS - 1
                      IF( NMISS .LT. 0 ) GOTO 140         ! Try next couple
                    ENDIF
  150             CONTINUE
C
C ****  Fit the track and Tag hits, avoiding reusing them as seed ...
C
                  CALL CDTSTO( LISTPT )
  140           CONTINUE
  135         CONTINUE
  130       CONTINUE
  120     CONTINUE
  110   CONTINUE
C
C ****  Debug final result
C
        IF ( DBGFLG .AND. LVLDBG(6) .GE. 1 ) THEN
          CALL PRDTSG( LUNDBG, KPDTSG, LAYER, 'SINGLE', LVLDBG(6) )
        ENDIF
        CALL MZDROP( IXCOM,  LUSER, 'L')
  100 CONTINUE
  999 RETURN
      END
