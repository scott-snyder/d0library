      SUBROUTINE ENICDH
C
C******************************************************************
C
C     PURPOSE: ENICDH is called at the end of each event.  Its
C              purpose is to compress the ICDH bank to the number
C              of words actually required.
C              This was not known in advance.
C
C     INPUT: None
C
C     OUTPUT: None
C
C     CREATED: 03-NOV-1988  by Z. Wolf
C
C******************************************************************
C
      IMPLICIT NONE
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC'
C
C--   GEANT UNITS
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LICDH,GZICDH
      INTEGER NBOOK
      INTEGER NTRK,NWTRK,NUSED

      integer ihicdh/4HICDH/
C
C--   GET LINK TO ICDH BANK
      LICDH=GZICDH()
C
C--   CHECK LINK
      IF ( LICDH.LE.0 ) THEN
        IF ( PD0.GE.2 ) WRITE(LOUT,*)'ENICDH-->NO ICDH bank booked'
        RETURN
      ENDIF
      IF ( IQ(LICDH-4).NE.ihICDH ) THEN
        WRITE(LOUT,*)'ENICDH--> PROBLEM WITH ICDH BANK NAME'
        RETURN
      ENDIF
C
C--   GET NUMBER OF WORDS BOOKED AND NUMBER USED
      NBOOK=IQ(LICDH-1)
      NWTRK=IQ(LICDH+2)
      NTRK=IQ(LICDH+3)
      NUSED=NTRK*NWTRK+3
C
C--   CHECK
      IF ( NUSED.GT.NBOOK ) THEN
C       WRITE(LOUT,*)'ENICDH--> NUSED>NBOOK'
      ENDIF
C
C--   COMPRESS BANK
      CALL MZPUSH(IXCOM,LICDH,0,-(NBOOK-NUSED),'R')
C
      RETURN
      END
