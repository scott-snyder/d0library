      SUBROUTINE FLJPTS(LJET0,JETN,IE,IP,IL,ECELL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FLJPTS fills the JPTS banks
C-
C-   Inputs  : LJET0    link to the first JETS bank
C-             JETN     number of the current jet
C-             IE       cell's eta index
C-             IP       cell's phi index
C-             IL       cell's layer index
C-             ECELL    energy deposited in the cell
C-   Outputs : None
C-   Controls:
C-
C-   Created  13-OCT-1988   Z. Wolf
C-   Updated  20-JAN-1989   Z. Wolf
C-   Modified 27-APR-1989   Z. Wolf
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   I/O
      INTEGER LJET0
      INTEGER JETN
      INTEGER IE,IP,IL
      REAL ECELL
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZJPTS.LINK/LIST'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
C--   GEANT UNITS
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LJETS,LJPTS
      INTEGER I
      INTEGER ND,NPTS
      BYTE BYTES(4)
      INTEGER PKADDR
      EQUIVALENCE (PKADDR,BYTES)
C----------------------------------------------------------------------
C
C--   CHECK LJET0
      IF(LJET0.LE.0)THEN
        WRITE(LOUT,*)'FLJPTS--> PROBLEM WITH LJET0'
        GOTO 999
      END IF
C
C--   FIND LINK TO JPTS BANK
      LJETS=LJET0
      IF(JETN.EQ.0)GO TO 5
      DO I=1,JETN
        LJETS=LQ(LJETS)
        IF(LJETS.LE.0)THEN
          WRITE(LOUT,*)'FLJPTS--> PROBLEM WITH LJETS'
          GOTO 999
        END IF
      END DO
    5 LJPTS=LQ(LJETS-IZJPTS)
C
C--   CHECK LINK TO JPTS BANK
      IF(LJPTS.LE.0)THEN
        WRITE(LOUT,*)'FLJPTS--> PROBLEM WITH LJPTS'
        GOTO 999
      END IF
      IF(IQ(LJPTS-4).NE.4hJPTS)THEN
        WRITE(LOUT,*)'FLJPTS--> PROBLEM WITH LJPTS'
        GOTO 999
      END IF
      IF(IQ(LJPTS-5).NE.JETN)THEN
        WRITE(LOUT,*)'FLJPTS--> BANK ID NOT JETN'
        GO TO 999
      END IF
C
C--   ADD SPACE IF NEEDED
      ND=IQ(LJPTS-1)
      NPTS=IQ(LJPTS+2)
      IF(NPTS+10.GE.ND)THEN
        CALL MZPUSH(IXCOM,LJPTS,0,3000,'I')
      END IF
C
C--   PACK ADDRESS OF HIT INTO PKADDR
      BYTES(BYTE4)=IE
      BYTES(BYTE3)=IP
      BYTES(BYTE2)=IL
C
C--   DO NOTHING IF HAVE PREVIOUS HIT
      DO I=NPTS,1,-1
        IF(PKADDR.EQ.IQ(LJPTS+2+I))GO TO 10
      END DO
C
C--   WRITE NEW LINE FOR NEW HIT, INCREMENT # OF HITS
      NPTS=NPTS+1
      IQ(LJPTS+2)=NPTS
      IQ(LJPTS+2+NPTS)=PKADDR
   10 CONTINUE
C
  999 RETURN
      END
