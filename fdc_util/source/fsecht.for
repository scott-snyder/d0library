      SUBROUTINE FSECHT(HALF,UNIT,QUAD,SECTOR,NHITS,NHITSDA,N_DL)
C----------------------------------------------------------------------
C
C-   Purpose and Methods : Routine for hitfinding in an FDC sector.
C-
C-   Inputs  : HALF,UNIT,QUAD,SECTOR = determines sector
C-   Outputs :  NHITS = Number of Sense wire hits in sector
C-              N_DL = Number of associated DL hits.
C
C-   Created  17-APR-1989   Jeffrey Bantly
C-   Updated  23-JUL-1990   Jeffrey Bantly  book banks only if hits,
C-                                          add delay line hits
C-   Updated   1-FEB-1991   Jeffrey Bantly  remove FRHITS_END entry point
C-   Updated  29-APR-1991   Jeffrey Bantly  use cleaned up RCP,PARAMS
C-   Updated  12-JUL-1991   Susan K. Blessing  Remove MAXPUL and check on 
C-    MXHTOT, redundant.
C-   Updated  25-MAY-1992   Robert E. Avery  Remove reference to SHIFTT. 
C-   Updated  18-OCT-1993   Robert E. Avery  Simplify, for use of 
C-      compressed hits, either FHIT or FCHT. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
C
      INTEGER HALF,UNIT,QUAD,SECTOR
      INTEGER NHITS,N_DL
C
      INTEGER WIRE,LOGCHA
      INTEGER NPULSE(0:NBPSEN), SUMHIT
      INTEGER NHITSDA
      INTEGER IER, LUNDBG
      INTEGER LKFTSC, LKFTDA, LKFPSC, LKFPDA
      INTEGER GZFTDA,GZFTSC,GZFPDA,GZFPSC,USUNIT
      INTEGER GZCDD3
      INTEGER MAX_WIRE(0:1)
C
      LOGICAL IOK, OKDONE
      LOGICAL DBG_FDHITS,FIRST
      LOGICAL FROM_CDD3 
C
      SAVE DBG_FDHITS
      SAVE LUNDBG
      DATA FIRST /.TRUE./
      DATA MAX_WIRE /MXWIRT, MXWIRP /
C--------------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        LUNDBG=USUNIT()
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('DBG_FDHITS',DBG_FDHITS,IER)
        CALL EZRSET
        FINDTP=.TRUE.
      ENDIF
C
C  Find hits and make banks of hits and data: FTSC,FTDA and FPSC,FPDA
C  Start from raw data; do hitfinding
C
      NHITS  = 0
      NHITSDA  = 0
      N_DL = 0
      CALL VZERO(NPULSE(0), 16)
      FROM_CDD3 = GZCDD3().GT.0
C
      DO WIRE =  0, MAX_WIRE(UNIT)
        IF ( FROM_CDD3 ) THEN
C
C This builds the Pulse list in HITS array, in COMMON block FDEVNT.
          CALL FDPULS(HALF,UNIT,QUAD,SECTOR,WIRE,NPULSE(WIRE) )
        ELSE
C
C Build the Pulse list in HITS array from existing FCHT bank.
          CALL FCHT_EXPAND(HALF,UNIT,QUAD,SECTOR,WIRE,NPULSE(WIRE) )
        ENDIF
        NHITSDA = NHITSDA + NPULSE(WIRE)
        NHITS = NHITSDA 
      ENDDO
C
      IF(NHITSDA.LE.0) GOTO 999
C
      IF(UNIT .LE. 0 ) THEN
        IF (NPULSE(0).GT.0) THEN      ! Also do delay lines
          DO WIRE =  8, 9
            IF ( FROM_CDD3 ) THEN
              CALL FDPULS(HALF,UNIT,QUAD,SECTOR,WIRE,NPULSE(WIRE) )
            ELSE
              CALL FCHT_EXPAND(HALF,UNIT,QUAD,SECTOR,WIRE,NPULSE(WIRE) )
            ENDIF
            NHITSDA = NHITSDA + NPULSE (WIRE)
          ENDDO
        ELSE
          NPULSE(8)=0
          NPULSE(9)=0
        ENDIF
C
        CALL BKFTSC( HALF,QUAD,SECTOR,NHITS,LKFTSC )
        CALL BKFTDA( HALF,QUAD,SECTOR,NHITSDA,LKFTDA )
C
        CALL ZFFTDA( HALF,QUAD,SECTOR,NPULSE )
        CALL ZFFTSC( HALF,QUAD,SECTOR )
C
C ****  delay line information
C
        IF( NPULSE(0) .GT. 0 ) then
          CALL FDGETD(HALF,QUAD,SECTOR,N_DL)
        ENDIF
C
C ****  Debug the resulting banks if requested
C
        IF ( DBG_FDHITS ) THEN
          IF ( LUNDBG .GT. 0 ) THEN
            IF( NHITSDA .GT. 0 ) THEN
              LKFTDA=GZFTDA(HALF,QUAD,SECTOR)
              LKFTSC=GZFTSC(HALF,QUAD,SECTOR)
              CALL FCODER(LOGCHA,HALF,0,QUAD,SECTOR,0,0,2)
              CALL PRFTDA(LUNDBG,LKFTDA,LOGCHA,'ONE',3)
              CALL PRFTSC(LUNDBG,LKFTSC,LOGCHA,'ONE',3)
            ENDIF
          ENDIF
        ENDIF
C
      ELSE
C
        CALL BKFPSC( HALF,SECTOR,NHITS,LKFPSC )
        CALL BKFPDA( HALF,SECTOR,NHITSDA,LKFPDA )
C
        CALL ZFFPDA( HALF,SECTOR,NPULSE )
        CALL ZFFPSC( HALF,SECTOR )
C
C ****  Debug the resulting banks if requested
C
        IF ( DBG_FDHITS ) THEN
          IF ( LUNDBG.GT.0 ) THEN
            IF( NHITSDA .GT. 0 ) THEN
              LKFPDA=GZFPDA(HALF,SECTOR)
              LKFPSC=GZFPSC(HALF,SECTOR)
              CALL FCODER(LOGCHA,HALF,1,0,SECTOR,0,0,2)
              CALL PRFPDA( LUNDBG, LKFPDA,LOGCHA,'ONE',3)
              CALL PRFPSC( LUNDBG, LKFPSC,LOGCHA,'ONE',3)
            ENDIF
          ENDIF
        ENDIF
C
      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
