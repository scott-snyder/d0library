      SUBROUTINE PRSSTH ( PRUNIT, LSSTH, NSSTH, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the content of SSTH bank
C-
C-   Inputs  : PRUNIT - Unit number for printout
C-             LSSTH - Bank address
C-             NSSTH - Bank number
C-             CFL - Flag to control printout
C-             IFL - Amount of printing
C-   Outputs : on unit PRUNIT
C-   Controls:
C-
C-   Created  02-MAY-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,LSSTH,NSSTH,IFL
      CHARACTER*(*) CFL
      CHARACTER*4 NAME
      INTEGER I
      CHARACTER*35 TITLE(28),TITL1(14),TITL2(14)
      EQUIVALENCE (TITLE(1),TITL1(1)),(TITLE(15),TITL2(1))
      DATA TITL1/'Type                               ',
     &           'Status                             ',
     &           'Quality                            ',
     &           'Lowest run number                  ',
     &           'Highest run number                 ',
     &           'Run when generated                 ',
     &           'Date generated                     ',
     &           'Generated for type of run          ',
     &           'Inner diameter of tube (cm)        ',
     &           'Thickness of tube wall (cm)        ',
     &           'Tracking medium number of tube wall',
     &           'Tracking medium number of tube gas ',
     &           'Wire diameter                      ',
     &           'Tubes end cap length (cm)          '/
      DATA TITL2/'Step of tubes                      ',
     &           'Z coordinate of tube layer 1       ',
     &           'Z coordinate of tube layer 2       ',
     &           'Z coordinate of tube layer 3       ',
     &           'Z coordinate of tube layer 4       ',
     &           'Number of "standard" tube types    ',
     &           'Length of the 1st "standard" tube  ',
     &           'Step for "standard" tubes          ',
     &           'Number of "nonstandard" tube types ',
     &           'Length of 1st "nonstandard" tube   ',
     &           'Length of 2nd "nonstandard" tube   ',
     &           'Length of 3rd "nonstandard" tube   ',
     &           'Length of 4th "nonstandard" tube   ',
     &           'Length of 5th "nonstandard" tube   '/
C----------------------------------------------------------------------
C
      IF (LSSTH.EQ.0) GOTO 990
C
C ****  Print bank
C
      CALL UHTOC (IC(LSSTH-4),4,NAME,4)
      WRITE (PRUNIT,1000) NAME
 1000 FORMAT (1H1/25X,30(1H*)/25X,'*  Contents of Bank :  ',A4,'  *'/
     &25X,30(1H*)//)
      DO 10 I=1,28
        IF (I.LE.8.AND.I.NE.3 .OR.
     &  I.EQ.11.OR.I.EQ.12.OR.I.EQ.20.OR.I.EQ.23) THEN
          WRITE (PRUNIT,1010) I,TITLE(I),IC(LSSTH+I)
 1010     FORMAT (1X,I3,2H. ,A35,2H :,I10)
        ELSE
          WRITE (PRUNIT,1020) I,TITLE(I),C(LSSTH+I)
 1020     FORMAT (1X,I3,2H. ,A35,2H :,F10.5)
        ENDIF
   10 CONTINUE
      GOTO 999
C
C ****  Error: bank is absent
C
  990 WRITE (PRUNIT,1030)
 1030 FORMAT (1H1/1X,'*** PRSSTH: bank SSTH is absent'/)
C
  999 RETURN
      END
