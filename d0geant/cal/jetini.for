      SUBROUTINE JETINI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : JETINI does the initialization for the calorimeter
C-              simulated processed event.  The routine first finds the number
C-              of jets from the Isajet zebra banks.  It associates each parton
C-              with a particular jet.  This information is stored in the
C-              common /SPEVNT/ for later use.  The routine then books the JETS
C-              and associated banks under HEAD--GEAN--PROC.
C-
C-   Inputs  : Isajet zebra banks
C-   Outputs : JETS and associated banks are booked, common /SPEVNT/ is filled
C-              for use at JETS filling time
C-   Controls:
C-
C-   Created  17-JAN-1989   Z. Wolf
C-   Updated  24-MAR-1989   Z. Wolf
C-   Updated  25-APR-1989   Z. Wolf  use modified BKJETS, BKJPTS
C-   Updated  22-DEC-1989   Chip Stewart  use PJET if it exists 
C-   Updated  18-OCT-1990   Alan M. Jonckheere
C-                              Use CAPHFL_INT instead of CAPHFL 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISAQ.LINK/LIST'
C
C--   COMMON USED TO ASSOCIATE PARTONS WITH A JET
      INCLUDE 'D0$INC:SPEVNT.INC/LIST'
C
C--   GEANT UNITS
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LISAE,LPJET,LISAQ
      INTEGER I
      INTEGER NPARTN,IPARTN
      INTEGER NJETS,IJET
      INTEGER LJET0,LJPT0,LJETS,LJPTS
C----------------------------------------------------------------------
C
C--   GET LINKS TO ISAJET BANKS
C--   SEE IF HEADER BANK EXISTS
      IF(LHEAD.LE.0)THEN
        WRITE(LOUT,*)'JETINI--> HEADER DOES NOT EXIST'
        GOTO 999
      END IF
C
C--   GET LINK TO ISAE
      LISAE=LQ(LHEAD-IZISAE)
      IF(LISAE.LE.0)THEN
        WRITE(LOUT,*)'JETINI--> ISAE DOES NOT EXIST'
        GOTO 999
      END IF
C
C--   GET NUMBER OF PARTONS AND NUMBER OF JETS
      IF ( IQ(LISAE-3).GT.7) THEN
C
C ****  PJET VERSION OF ISAZEB
C
        NPARTN=IQ(LISAE+6)
        NJETS=IQ(LISAE+7)
      ELSE
C
C ****  OLD ISAJ VERSION
C
        NPARTN=IQ(LISAE+7)
        NJETS=IQ(LISAE+5)
      END IF
      IF(NPARTN.LT.0.OR.NPARTN.GT.200)GO TO 999  !BAD VALUE
      IF(NJETS.LT.0.OR.NJETS.GT.200)GO TO 999    !BAD VALUE
C
C--   LOOP THROUGH PARTONS, ASSOCIATE PARTONS WITH JET
      DO I=0,200               !INITIALIZE LOOP
        PNTOJT(I)=0
      END DO
      IF(NPARTN.EQ.0)GO TO 101 !SKIP LOOP
      IF(NJETS.EQ.0)GO TO 101  !SKIP LOOP
      LISAQ=LQ(LISAE-IZISAQ)   !GET LINK TO FIRST PARTON BANK
      IF(LISAQ.LE.0)GO TO 101  !IF NO PARTON BANKS, SKIP LOOP
  100 CONTINUE                 !BEGIN LOOP OVER PARTONS
      IPARTN=IQ(LISAQ-5)       !PARTON NUMBER
      IF ( IQ(LISAE-3).GT.7) THEN
        LPJET=LQ(LISAQ-2)        !LINK TO PJET BANK
      ELSE
        LPJET=LQ(LISAQ-1)        !LINK TO ISAJ BANK
      END IF
      IF(LPJET.GT.0)THEN
        IJET=IQ(LPJET-5)       !JET NUMBER
      ELSE
        IJET=0
      END IF
      IF(IPARTN.GE.0.AND.IPARTN.LE.200)THEN
        PNTOJT(IPARTN)=IJET      !STORE IN /SPEVNT/
      END IF
      LISAQ=LQ(LISAQ)          !NEXT PARTON
      IF(LISAQ.GT.0)GO TO 100  !END LOOP
  101 CONTINUE
C
C--   SET PATH TO BOOK JETS AND JPTS UNDER GEAN
      CALL PATHST('GEAN')
C
C--   BOOK JET0 AND JPT0 BANKS
      CALL BKJETS(LJET0)
      CALL BKJPTS(LJET0,5000,LJPT0)
C
C--   BOOK THE REST OF THE JETS AND JPTS BANKS
      DO I=1,NJETS
        CALL BKJETS(LJETS)
        CALL BKJPTS(LJETS,5000,LJPTS)
      END DO
C
C--   PUT THE NUMBER OF JETS IN CAPH
      CALL CAPHFL_INT(3,NJETS+1)
C
  999 RETURN
      END

