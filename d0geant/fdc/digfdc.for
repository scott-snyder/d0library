      LOGICAL FUNCTION DIGFDC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C  DIGFDC converts forward detector hits to time ordered
C  digitizations: channel "data" (drift time, pulse hight, pulse width),
C  and wire "hits" (spacial coordinates).
C-
C-   Returned value  : TRUE
C-   Inputs  : none
C-   Outputs :             - Fills the Raw data ZEBRA bank CDD3 if SFDC(1)=1.
C                          - Fills the Hits     ZEBRA bank FTSC if SFDC(2)=1.
C                          - Fills the "DATA"   ZEBRA bank FTDA if SFDC(3)=1.
C                          - Fills the Hits     ZEBRA bank FPSC if SFDC(2)=1.
C                          - Fills the "DATA"   ZEBRA bank FPDA if SFDC(3)=1.
C-
C-   Created  xx-APR-1987   Daria Zieminska
C-   Updated   7-MAR-1988   Ghita Rahal-Callot  : Printout only if requested
C-   Updated  26-SEP-1988   Jeffrey Bantly      : Eliminate middle routines
C-                                                and reduce GFHITS calls
C-   Updated  24-NOV-1988   Ghita Rahal-Callot  : Take the pointers from the
C-                                                link area FDCLNK and drop
C-                                                data banks if SFDC(3)=0
C-   Updated  14-JUL-1989   Harrison B. Prosper :Made into pbd logical function
C-   Updated   6-AUG-1990   Jeffrey Bantly      : Drop all hit banks if SFDC(2)
C-                                                and SFDC(3)=0
C-
C-----------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INTEGER LL, IFL
      INTEGER IDEN,SECTP,SECTT,LKFDCH
      INTEGER HALF,UNIT,QUAD,SECTOR,NSECTR(2)
      DATA NSECTR/6,36/
      LOGICAL NOMORE
C-----------------------------------------------------------------------------
      DIGFDC = .TRUE.
      IF ( DFDC .LT. 3 ) GOTO 999
C
C ****  Initialize link area
C
      IF(FDCLNK(1).EQ.0) THEN
        CALL MZLINT(IXCOM,'/FDCLNK/',FDCLNK,LCDD3,FDCLNK)
      ENDIF
C
C ****  Initialize the top FDC hit information bank, FDCH
C
      CALL BKFDCH(LKFDCH)      ! book Zebra bank 'FDCH'
C
C-----------------------------------------------------------------------------
C  Makes "hits" and "data" for sectors in Theta units
C  of forward detector.  Unused portions of banks are then trimmed.
C  "hits" are stored in Zebra banks 'FTSC'
C  "data" are stored in Zebra banks 'FTDA'
C-----------------------------------------------------------------------------
C
C  Loop through forward detector 'halves',theta units,quadrants and sectors.
      DO 100 HALF=0,1
          DO 100 QUAD=0,7
            DO 100 SECTOR=0,NSECTR(1)-1
              CALL MKFTHE(HALF,QUAD,SECTOR)
  100 CONTINUE
C
C-----------------------------------------------------------------------------
C  Makes "hits" and "data" for sectors in Phi unit of
C  forward detector.  Unused portions of banks are then trimmed.
C  "hits" are stored in Zebra banks 'FPSC'
C  "data" are stored in Zebra banks 'FPDA'
C-----------------------------------------------------------------------------
C
C  Loop through forward detector 'halves' and sectors.
      DO 101 HALF=0,1
        DO 101 SECTOR=0,NSECTR(2)-1
          CALL MKFPHI(HALF,SECTOR)
  101 CONTINUE
C
C-----------------------------------------------------------------------------
C  Makes the "fadc raw data" bank, 'CDD3', for the FDC.
C  Requires all "data" banks to be filled.
C-----------------------------------------------------------------------------
C
      IF ( SFDC(1) .EQ. 1. ) THEN
        CALL BLCDD3
      ENDIF
C----------------------------------------------------------------------------
C  Drop all hit banks if SFDC(2).NE.1. and SFDC(3).NE.1.
C  or at least drop FTDA and FPDA hit data banks if SFDC(3).NE.1.
C----------------------------------------------------------------------------
      IF ( SFDC(2) .NE. 1. .AND. SFDC(3) .NE. 1. ) THEN
        LL = LFDCH
        CALL MZDROP ( IXCOM, LL, 'L' )           ! drops bank and dependents
      ELSEIF ( SFDC(3) .NE. 1. ) THEN
        DO 111 HALF=0,1
          DO 112 SECTOR=0,NSECTR(2)-1
            LL = LFPDA ( SECTOR, HALF )
            IF ( LL .NE. 0 ) CALL MZDROP ( IXCOM, LL, ' ')  ! drop bank only
  112     CONTINUE
          DO 113 UNIT=0,0
            DO 114 QUAD=0,7
              DO 115 SECTOR = 0 , NSECTR(1)-1
                LL = LFTDA ( SECTOR, QUAD, HALF )           ! drop bank only
                IF ( LL .NE. 0 ) CALL MZDROP ( IXCOM, LL, ' ')
  115         CONTINUE
  114       CONTINUE
  113     CONTINUE
  111   CONTINUE
      ENDIF
C
C----------------------------------------------------------------------------
C  Printing of the banks if asked
C  PFDC = level of printout
C  (0=none, 1=minimal, 2= more detailed, 3= full content printed)
C  'ALL' means that all the banks with the same name will be printed
C  De-activate the temporary link area
C----------------------------------------------------------------------------
      IF ( DDIG .EQ. 1 ) THEN
        IF ( SFDC(3) .EQ. 1. .OR. SFDC(2) .EQ. 1.) CALL PRFDC(LOUT,PFDC)
        IF ( SFDC(1) .EQ. 1.) THEN
          CALL PRCDD3 ( LOUT, 0, 0, 'ALL', PFDC )
        ENDIF
      ENDIF
C
      FDCLNK(1) = 0
C-----------------------------------------------------------------------------
C  Done.
C-----------------------------------------------------------------------------
C
  999 CONTINUE
      RETURN
      END
