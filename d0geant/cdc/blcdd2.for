      SUBROUTINE BLCDD2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the bank CDD2 containing the raw FADC data
C-                         using the informations of the banks DCDA
C-
C-
C-   Inputs  : The banks DCDA are needed to build CDD2
C-   Outputs : none; The bank CDD2 is filled
C-
C-   Created  25-JAN-1988   Ghita Rahal-Callot
C-   Updated  10-MAY-1989   Qizhong Li-Demarteau changed arguments to FICDD2
C-   Updated  26-OCT-1989   Jeffrey Bantly  add Crate Trailer words
C-   Updated  29-JAN-1990   Qizhong Li-Demarteau  fix the call to MZPUSH
C-   Updated  14-MAR-1990   Peter Grudberg - avoid overwrites by trailer words
C-   Updated  24-FEB-1991   Qizhong Li-Demarteau  fix array size for TIME
C-                                                and AREA 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$LINKS:IZDRFT.LINK'
      INCLUDE 'D0$INC:CDCLNK.INC'
C
      INTEGER ILYR, ISEC, NBFADC, IPTH(0:20), NBHITS(0:20), LHIT
      INTEGER NWORDS, NWDAT, LDRFT, NCDLYR, NCDSEC
      INTEGER IHIT, IFADC, IPTR, IAD, IADC(3)
      INTEGER IVER, ISKP, IFL, NHMAX
      PARAMETER( NHMAX = 100 )
      REAL TIME(NHMAX), AREA(NHMAX)
C----------------------------------------------------------------------
      LDRFT = LC ( LDGEH - IZDRFT )
      IF ( LDRFT .LE. 0 ) THEN
        WRITE (LOUT, *)
     &     '**** BLCDD2: Geometry bank DRFT doesn''t exist'
        GO TO 999
      ENDIF
      NCDLYR = IC ( LDRFT + 1)
      DO 100 ILYR = 0, NCDLYR - 1
        IF ( LDLYR ( ILYR ) .LE. 0 ) GO TO 100
        NCDSEC = IC ( LDRFT + ILYR + 2 )
        DO 200 ISEC = 0, NCDSEC - 1
C-             NBADC             = number of FADC in this cell
C-             IPTH  (0:NBADC-1) = pointer on first hit on this FADC
C-             NBHITS(0:NBADC-1) = number of hits on this FADC
C-             LHIT              = number of words per hits. 0 if no hits
C-
          CALL ZGDCDA ( ILYR, ISEC, NBFADC, IPTH, NBHITS, LHIT)
          IF ( LHIT .LE. 0 ) GO TO 200
          DO 300 IFADC = 0, NBFADC - 1
            IPTR = IPTH ( IFADC )
            IAD = IQ ( IPTR + 1 )
            IADC ( 1 ) = ILYR
            IADC ( 2 ) = ISEC
            IADC ( 3 ) = IBITS ( IAD, 0, 4 )
            IF (NBHITS(IFADC) .GT. NHMAX) NBHITS(IFADC) = NHMAX
            DO 400 IHIT = 1, NBHITS ( IFADC )
              TIME ( IHIT ) = Q ( IPTR + 2 )
              AREA(IHIT ) = Q ( IPTR + 3 )
              IPTR = IPTR + LHIT
  400       CONTINUE
            CALL FICDD2 (IADC, TIME, AREA, NBHITS(IFADC), NWDAT)
  300     CONTINUE
  200   CONTINUE
  100 CONTINUE
C
C ****  Insert CDD2 Crate Trailer words and release empty space when the bank
C ****  is filled (For version 2, 4 trailer words are added.  It may be
C ****  necessary to extend the bnk to fit these words.)
C
      IF ( LCDD2 .GT. 0 ) THEN
        IVER =2
        ISKP =NWDAT
        IFL  =2
        NWORDS = NWDAT + 4 - IQ( LCDD2 - 1 )
C
C ****  Make the bank just the right size to fit the trailer words
C
        CALL MZPUSH( IXCOM, LCDD2, 0, NWORDS, 'R' )
        CALL CDD2FL(IVER,ISKP,IFL)
      ENDIF
  999 RETURN
      END
