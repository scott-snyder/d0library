      SUBROUTINE MTC_GET_CELLEZ(IETA2,IPHI2,ILYR, zlong,ok)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the longitudinal width of the
C-      calorimeter cell in layer ILYR at ieta,iphi.  This routine is
C-      similar to mtc_get_longz which gets zlong for relative 
C-      ieta,iphi cal cell position.
C-
C-   Inputs  : ieta,iphi,ilyr - specify cell location
C-   Outputs : zlong = z width of the input cell
C-              ok   = 0 if cell exists
C-                   = -1 if cell dne
C-   Calls   :  SUB_CELL_WIDTH
C-
C-   Created   6-SEP-1995   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input
      INTEGER IETA2,IPHI2,ILYR
C- output
      REAL ZLONG
      INTEGER OK
C----------------------------------------------------------------------
C- the number of sub-sub-layers is stored here
      INCLUDE 'D0$INC:MTC_ISSUBCAL.INC'
C----------------------------------------------------------------------
C- local
      INTEGER IHERE,ISUB
      INTEGER INUMSUB,IDUM,ISUBSUB, IERR
      REAL    RC,DR,ZC,DZ,AZI,DAZI
C- functns ...
      INTEGER MTC_IWHERE,MTC_IWHLAYER
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      INUMSUB = 0
      IF(ILYR.LE.0 .OR. ILYR.GE.18) GO TO 666
C- where am I in the calorimeter ?
      IHERE = MTC_IWHERE(IETA2,IPHI2,ILYR)
C- EM3 includes EM4-EM6 so reset IHERE if neccessary
      IF(ABS(IETA2).LE.26 .AND. ILYR.EQ.3) THEN
        IF(IHERE.EQ.0 .AND.
     &    ABS(IETA2).NE.12 .AND. ABS(IETA2).NE.14) GO TO 666
        IF(ABS(IETA2).EQ.14) IHERE = 2       ! ECEM
        IF(ABS(IETA2).EQ.12) IHERE = 1       ! CCEM
      END IF
      IF(IHERE.EQ.0) GO TO 666

C- the ICD/MG's have funny values in SUB_CELL_WIDTH
      IF(IHERE.EQ.3 .OR. IHERE.EQ.4 .OR. IHERE.EQ.5) THEN
        ZLONG = 20.
        OK    = 0.
        GO TO 999
      END IF

C- Get the sublayer number within this calorimeter section ...
      ISUB  = MTC_IWHLAYER(IHERE,ILYR)
      IF(ISUB.LE.0.OR.ISUB.GE.6) GO TO 666
C- Get the number of sub-sub-layers (check to see if its an irregular cell
C- and correct the number of sub-sub-layers if necessary)
      INUMSUB = ISSLNUM(IHERE,ISUB)
      IF(INUMSUB.LE.0) GO TO 666
      IF(IHERE.EQ.7) THEN
        DO 70 IDUM=1,4
          IF(ISUB.EQ.ISSLNUM_7(IDUM,1).AND.
     &      ABS(IETA2).EQ.ISSLNUM_7(IDUM,2)) THEN
            INUMSUB = ISSLNUM_7(IDUM,3)
            GO TO 69
          END IF
   70   CONTINUE
      ELSE IF(IHERE.EQ.8) THEN
        DO 80 IDUM=1,5
          IF(ISUB.EQ.ISSLNUM_8(IDUM,1).AND.
     &        ABS(IETA2).EQ.ISSLNUM_8(IDUM,2)) THEN
            INUMSUB = ISSLNUM_8(IDUM,3)
            GO TO 69
          END IF
   80   CONTINUE
      ELSE IF(IHERE.EQ.10) THEN
        DO 100 IDUM=1,5
          IF(ISUB.EQ.ISSLNUM_10(IDUM,1).AND.
     &         ABS(IETA2).EQ.ISSLNUM_10(IDUM,2)) THEN
            INUMSUB = ISSLNUM_10(IDUM,3)
            GO TO 69
          END IF
  100   CONTINUE
      END IF
      IDUM = 0.
   69 CONTINUE
C- Loop over sub-sub layers, summing the longitudinal widths
      ZLONG = 0.
C- my convention for EM3 is to include all info with ILYR=3 regardless
C- of whether the cell exists or not so return some reasonable value
      IF(ILYR.GE.3.AND.ILYR.LE.6 .AND.
     &      (ABS(IETA2).EQ.12 .OR. ABS(IETA2).EQ.14)) THEN
        IF(ABS(IETA2).EQ.12) THEN
          CALL SUB_CELL_WIDTH(11, IPHI2, ILYR, INUMSUB,
     &        RC, DR, ZC, DZ, AZI, DAZI, IERR)
          IF(IERR.NE.0) GO TO 666
          ZLONG = DZ
        ELSE IF(ABS(IETA2).EQ.14) THEN
          CALL SUB_CELL_WIDTH(15, IPHI2, ILYR, INUMSUB,
     &        RC, DR, ZC, DZ, AZI, DAZI, IERR)
          IF(IERR.NE.0) GO TO 666
          ZLONG = DZ
        END IF
C- ECOH3-15 is tied to ECMH5-15.  The z length of the sum is about 77 cm.
      ELSE IF(IHERE.EQ.8 .and. isub.eq.5 .and. abs(ieta2).eq.15) THEN
        ZLONG = 77.
C- ECOH infor is missing from SUB_CELL_WIDTH
      ELSE IF(IHERE.EQ.10) THEN
        ZLONG = 48.39
C- the ICD/MG's have funny values in SUB_CELL_WIDTH
      ELSE IF(IHERE.EQ.3 .OR. IHERE.EQ.4 .OR. IHERE.EQ.5) THEN
        ZLONG = 10.
C- dz for the rest of the calorimeter cells is calculated normally
      ELSE
        DO 10 ISUBSUB=1,INUMSUB
          CALL SUB_CELL_WIDTH(IETA2, IPHI2, ILYR, ISUBSUB,
     &        RC, DR, ZC, DZ, AZI, DAZI, IERR)
          IF(IERR.NE.0) GO TO 666
          ZLONG = ZLONG + DZ
   10   CONTINUE
      END IF
      OK   = 0
      GO TO 999
C----------------------------------------------------------------------
  666 CONTINUE
      ZLONG = -1000.
      OK   = -1
      WRITE(6,89) IETA2,IPHI2,ILYR,INUMSUB
   89 FORMAT(' MTC_FILL_CELLEZ:  error w/dz ie,ip,il,inss = ',6I4)
C----------------------------------------------------------------------
  999 RETURN
      END
