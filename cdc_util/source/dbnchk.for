      SUBROUTINE DBNCHK(BIN,LMAX,EXPDAT,IPEV,IFBIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check FADC bins to avoid the bin droping problem
C-
C-   Inputs  : BIN  : differencial pulse data array
C-             LMAX : length of the cluster
C-             EXPDAT: FADC data array
C-             IPEV  : pointer to the first FADC bin of the cluster in
C-                     array EXPDAT
C-             IFBIN : starting FADC adreess of the cluster
C-   Outputs : dump FADC data on debug unit when a bad FADC bin is found
C-   Controls: 
C-
C-   Created  20-JUN-1989   Qizhong Li-Demarteau
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INTEGER BIN(*), EXPDAT(0:*), LMAX, IPEV, IFBIN
      INTEGER BINCHK, BINDIF, BADLMT, BADDAT, ERR
      INTEGER CON1, CON2, CON3, CON4, I, J
      INTEGER IER
      LOGICAL FIRST
      LOGICAL EZERROR
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DBNCHK',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('BINCHK',BINCHK,ERR)
        CALL EZGET('BINDIF',BINDIF,ERR)
        CALL EZGET('BADLMT',BADLMT,ERR)
        CALL EZRSET
      ENDIF
C
      BADDAT = 0
      DO 111 I = 2, LMAX
        CON1 = .FALSE.
        CON2 = .FALSE.
        CON3 = .FALSE.
        CON4 = .FALSE.
        CON1 = (BIN(I) - BINDIF) .GT. 0
        CON2 = (BIN(I-1) + BINDIF) .LT. 0
        CON3 = (BIN(I) + BINDIF) .LT. 0
        CON4 = (BIN(I-1) - BINDIF) .GT. 0
        IF ((CON1 .AND. CON2) .OR. ((CON3 .AND. CON4)) .AND.
     &       (EXPDAT(IPEV+I) .LE. 10))
     &       BADDAT = BADDAT + 1
        IF (BADDAT .GE. BADLMT) THEN
          WRITE(LUNDBG,4101)
 4101     FORMAT(1X,/,1X,'   ***** FADC DATA PROBLEM ******')
          WRITE( LUNDBG, 4000 ) LAYER, SECTOR, WIRE
 4000     FORMAT(//' Raw data for layer',I2,' sector',I3,' wire',I3/)
          WRITE( LUNDBG, 4100 ) IFBIN, LMAX, (EXPDAT(J), J=IPEV+1,
     &      IPEV+LMAX)
 4100     FORMAT('0 ifbin=',I4,' lmax =',I4,2X,20I5/(25X,20I5))
          GOTO 112
        ENDIF
  111 CONTINUE
  112 CONTINUE
  999 RETURN
      END
