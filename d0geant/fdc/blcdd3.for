      SUBROUTINE BLCDD3
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the bank CDD3 containing the raw FADC data
C-                         using the informations of the banks FTDA,FPDA
C-
C-
C-   Inputs  : The banks FTDA,FPDA are needed to build CDD3
C-   Outputs : none; The bank CDD3 is filled
C-
C-   Created  25-JAN-1988   Ghita Rahal-Callot
C-   Updated  30-SEP-1988   Jeffrey Bantly  adapted to FDC
C-   Updated  26-OCT-1989   Jeffrey Bantly  added Crate Trailer words
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$LINKS:IZFDRT.LINK/LIST'
C
      INTEGER IHALF, ITHE, ITHQUD, ITHSEC, IPHI, IPHSEC
      INTEGER NBFADC, IPTH(0:20), NBHITS(0:20), LHIT
      INTEGER NWORDS, NWDAT, LKFDRT, GZFDRT
      INTEGER IHIT, IFADC, IPTR, IAD, IADC(6),I
      INTEGER PRUNIT,NHITS,NFDCH,IFL,IVER,ISKP
      REAL TIME(100), AREA(100)
C----------------------------------------------------------------------
      LKFDRT = GZFDRT(0)
      IF ( LKFDRT .LE. 0 ) THEN
        CALL ERRMSG('Bank not booked','BLCDD3',
     &     '**** BLCDD3: Geometry bank FDRT does not exist','I')
        GO TO 999
      ENDIF
C
      DO 100 IHALF = 0, 1
        IF ( LFHLF(IHALF) .LE. 0 ) GO TO 100
        ITHE = 0
        IF ( LFDUN(ITHE,IHALF) .LE. 0 ) GO TO 120
        DO 150 ITHQUD = 0, 7
          IF ( LFTQD(ITHQUD,IHALF) .LE. 0 ) GO TO 150
          DO 200 ITHSEC = 0, 5
            IF ( LFTSC(ITHSEC,ITHQUD,IHALF) .LE. 0 ) GO TO 200
C-----------------------------------------------------------------------
C             NBFADC             = number of FADC in this cell
C             IPTH  (0:NBFADC-1) = pointer on first hit on this FADC
C             NBHITS(0:NBFADC-1) = number of hits on this FADC
C             LHIT              = number of words per hits. 0 if no hits
C-----------------------------------------------------------------------
            CALL ZGFTDA ( IHALF, ITHQUD, ITHSEC, NBFADC, IPTH,
     &                 NBHITS, LHIT)
            IF ( LHIT .LE. 0 ) GO TO 200
            DO 300 IFADC = 0, NBFADC - 1
              IPTR = IPTH ( IFADC )
              IAD = IQ ( IPTR + 1 )
              IADC ( 1 ) = IHALF
              IADC ( 2 ) = ITHE
              IADC ( 3 ) = ITHQUD
              IADC ( 4 ) = ITHSEC
              IADC ( 5 ) = IFADC
              IADC ( 6 ) = IAD
              DO 400 IHIT = 1, NBHITS ( IFADC )
                TIME ( IHIT ) = Q ( IPTR + 2 )        ! drift time
                AREA ( IHIT ) = Q ( IPTR + 3 )        ! pulse area
                IPTR = IPTR + LHIT
  400         CONTINUE
              CALL FICDD3 ( IADC, TIME, AREA, NBHITS(IFADC), NWDAT)
  300       CONTINUE
  200     CONTINUE
  150   CONTINUE
  120   CONTINUE
        DO 170 IPHI = 1,1
          IF ( LFDUN(IPHI,IHALF) .LE. 0 ) GO TO 170
          DO 250 IPHSEC = 0, 35
            IF ( LFPSC(IPHSEC,IHALF) .LE. 0 ) GO TO 250
            CALL ZGFPDA ( IHALF, IPHSEC, NBFADC, IPTH, NBHITS,
     &         LHIT)
            IF ( LHIT .LE. 0 ) GO TO 250
            DO 350 IFADC = 0, NBFADC - 1
              IPTR = IPTH ( IFADC )
              IAD = IQ ( IPTR + 1 )
              IADC ( 1 ) = IHALF
              IADC ( 2 ) = IPHI
              IADC ( 3 ) = 0
              IADC ( 4 ) = IPHSEC
              IADC ( 5 ) = IFADC
              IADC ( 6 ) = IAD
              DO 450 IHIT = 1, NBHITS ( IFADC )
                TIME ( IHIT ) = Q ( IPTR + 2 )      ! drfit time
                AREA ( IHIT ) = Q ( IPTR + 3 )      ! pulse area
                IPTR = IPTR + LHIT
  450         CONTINUE
              CALL FICDD3 ( IADC, TIME, AREA, NBHITS(IFADC), NWDAT)
  350       CONTINUE
  250     CONTINUE
  170   CONTINUE
  100 CONTINUE
C
C ****  Insert CDD3 Crate Trailer words and release empty space when the bank
C ****  is filled (For version 2, 4 trailer words are added.  It may be
C ****  necessary to extend the bnk to fit these words.)
C
      IF ( LCDD3 .GT. 0 ) THEN
        IVER  = 2                   ! Version 2 trailer
        ISKP  = NWDAT               ! Offset to end of CDD3 for Trailer
        IFL   = 2                   ! Fill trailer
        NWORDS =  NWDAT + 4 - IQ( LCDD3 - 1 )
C
C ****  Make the bank just the right size to fit the trailer words
C
        CALL MZPUSH( IXCOM, LCDD3, 0, NWORDS, 'R' )
        CALL CDD3FL(IVER,ISKP,IFL)
      ENDIF
C---------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
