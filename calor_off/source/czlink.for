      SUBROUTINE CZLINK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book banks for calorimeter and
C-                         set up some links in /ZLINKC/.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   4-MAY-1989   Rajendran Raja
C-   Updated  10-OCT-1989   Harrison B. Prosper
C-   Made into logical function. Now PART of package CZLINK.
C-   Updated   5-SEP-1990   Harrison B. Prosper
C-      Removed booking of CAPH. This is now done in CAPHEL and CAJETS.
C-   Updated  13-SEP-1990   Harrison B. Prosper
C-      Made in subroutine and use PATHBK
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C----------------------------------------------------------------------
      CHARACTER*4 PATH
      INTEGER LADDR
      INTEGER GZRECO,GZCAD1,GZCAD2
      INTEGER GZHITS,GZPROC,GZHSTR
      INTEGER GZCAHT,GZCAPH,GZPARH
C----------------------------------------------------------------------
C
C ****  Book PATH bank using currently set path
C
      CALL PATHBK(LADDR)
      CALL PATHGT(PATH)                 ! Get PATH
      IF     ( PATH .EQ. 'RECO' ) THEN
        LRECO = LADDR
      ELSEIF ( PATH .EQ. 'FAKE' ) THEN
        LFAKE = LADDR
      ELSEIF ( PATH .EQ. 'GEAN' ) THEN
        LGEAN = LADDR
      ELSEIF ( PATH .EQ. 'FILT' ) THEN
        LFILT = LADDR
      ENDIF
C
C ****  Book/Fill top levels banks under current path
C
      CALL HITSFL
      LHITS = GZHITS()
      CALL PROCFL
      LPROC = GZPROC()
C
C ****  Calling HSTRFL,PARHFL and CAHTFL will cause these linear
C ****  banks to be booked again. That is not the intent here
C
      IF(GZHSTR().EQ.0)CALL HSTRFL      ! These are linear banks.
      LHSTR = GZHSTR()
      IF(GZPARH().EQ.0)CALL PARHFL
      LPARH = GZPARH()
C
      LCAD1 = GZCAD1()
      LCAD2 = GZCAD2()
C
      IF(GZCAHT().EQ.0)CALL CAHTFL
      LCAHT = GZCAHT()
      LCAHTR = LQ(LCAHT-4)              ! REFERENCE LINK
C
  999 RETURN
      END
