      LOGICAL FUNCTION DIGCDC
C----------------------------------------------------------------------
C
C   Purpose and Methods :    Converts hits to Digitization in the FADC
C                            of the CDC for sense wires and delay lines.
C
C   Inputs  :     Hits stored by GEANT in JHITS structure
C   Outputs :     Bank  CDD2
C                 Bank  CDCH
C                 Bank  DCDA
C                          - Fills the Raw data ZEBRA bank CDD2 if SCDC(1)=1.
C                          - Fills the Hits     ZEBRA bank CDCH if SCDC(2)=1.
C                          - Fills the "DATA"   ZEBRA bank DCDA if SCDC(3)=1.
C
C    Created   1-Dec-1985   T. Trippe
C              5-Mar-1986   K. Ng
C              1-Apr-1987   G. Rahal-callot
C-   Updated  27-JAN-1988   Ghita Rahal-Callot
C-   Updated  12-FEB-1988   Ghita Rahal-Callot  : Modified the way the
C-                          different banks are created and filled. The
C-                          level of printout depend now on PCDC
C-                          (0=none,>1=more and more detailed)
C-   Updated  22-NOV-1988   Ghita Rahal-Callot: drop DCDA if not asked for
C-   Updated  20-MAR-1989   Qizhong Li-Demarteau  change routine structure
C-   Updated  14-JUL-1989   Harrison B. Prosper  
C-   Made into pbd logical function
C-   Updated  27-JAN-1990   Qizhong Li-Demarteau  initialize link area and
C-                                              diactivate link area added
C-   Updated  14-JUN-1990   Qizhong Li-Demarteau  fix the call to MZDROP 
C-   Updated  11-AUG-1990   Qizhong Li-Demarteau  drop CDCH-DLYR banks when
C-                                                not needed 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZDRFT.LINK/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC/LIST'
C
      INTEGER LAYER, ISECT, ICELL
      INTEGER LDRFT, LAYMAX, SECMAX(0:3)
C
C----------------------------------------------------------------------
      DIGCDC = .TRUE.
      IF ( DCDC .LT. 3 ) GOTO 999
C
C ****  Initialise the LINK area
C
      IF (CDCLNK(1) .EQ. 0) THEN
        CALL MZLINT(IXCOM,  '/CDCLNK/', CDCLNK, LCDD2, CDCLNK)
      ENDIF
C
C ****  Initialize the Main Bank CDCH for the digitization in the CDC
C
      CALL BKCDCH
C
C  ****  Loop over chamber layers, sectors and cells.
C
      LDRFT = LC( LDGEH - IZDRFT )
      IF ( LDRFT .LE. 0 ) THEN
        WRITE(LOUT,*) '**** DIGCDC : bank LDRFT not defined'
        CALL EXIT(1)
      ENDIF
      LAYMAX = IC( LDRFT + 1 )
      CALL UCOPY( IC(LDRFT+2), SECMAX, 4 )
      DO 300 LAYER = 0, LAYMAX-1
        DO 200 ISECT = 0, SECMAX(LAYER)-1
          CALL MKDCDC(LAYER,ISECT)
  200   CONTINUE
  300 CONTINUE
C
C ****  Create the raw FADC bank CDD2 if asked
C
      IF ( SCDC(1) .EQ. 1. ) CALL BLCDD2
C
C ****  Delete the datas banks DCDA if not asked for
C
      IF ( SCDC(3) .NE. 1. ) THEN
        DO 310 LAYER=0,LAYMAX-1
          DO 210 ISECT=0,SECMAX(LAYER)-1
            IF ( LDCDA(ISECT, LAYER) .GT. 0 ) THEN
              CALL MZDROP(IXCOM, LDCDA(ISECT, LAYER), ' ')
            ENDIF
  210     CONTINUE
  310   CONTINUE
      ENDIF
C
C    drop CDCH-DLYR banks, if no DSEC and DCDA banks
C
      IF (SCDC(2) .EQ. 0 .AND. SCDC(3) .EQ. 0) 
     &      CALL MZDROP(IXCOM,LCDCH,' ')
C
C ****  Printing of the banks if asked
C ****  PCDC = level of printout
C ****  (0=none, 1=minimal, 2= more detailed, 3= full content printed)
C ****  'ALL' means that all the banks with the same name will be printed
C
      IF ( (DDIG.EQ.1) .AND. (PCDC.GT.0) ) THEN
        IF ( SCDC(3) .EQ. 1. ) THEN
          CALL PRCDCH(LOUT,0,0,'ALL',PCDC)
          CALL PRDCDA ( LOUT, 0, 0, 'ALL', PCDC )
        ENDIF
        IF ( SCDC(2) .EQ. 1. ) THEN
          CALL PRDSEC ( LOUT, 0, 0, 'ALL', PCDC )
        ENDIF
        IF ( SCDC(1) .EQ. 1.) THEN
          CALL PRCDD2 ( LOUT, 0, 0, 'ALL', PCDC )
        ENDIF
      ENDIF
C
C   to diactivate the temporary link area
C
      CDCLNK(1) = 0
C
  999 CONTINUE
      RETURN
      END
