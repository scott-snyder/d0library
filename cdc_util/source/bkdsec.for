      SUBROUTINE BKDSEC ( ILAYD, ISECD, NHITS, LKDSEC )
C======================================================================
C
C   Purpose and Methods :  Books the bank "DSEC" for a specified
C-                         vertex detector layer and sector.
C
C-  Inputs : ILAYD and ISECD are the CDC layer and sector respectively.
C-           NHITS is the number of hits to be booked
C-  Output : LKDSEC is the link to the booked "DSEC" bank
C-
C-   Created   4-JAN-1987  T. Trippe
C-   Updated   4-FEB-1988   Olivier Callot
C-   Updated  13-JUL-1989   Qizhong Li-Demarteau    put in version # 
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:CDCLNK.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZDSEC.LINK/LIST'
      INTEGER ILAYD, ISECD, NHITS, LKDSEC, MPDSEC(5), ISETVN
      INTEGER NWDSEC, NBWIRE, LHDSEC, IXDSEC
      CHARACTER*20 FMDSEC
      LOGICAL FSDSEC
      DATA FSDSEC /.TRUE./
      DATA MPDSEC / 0, 1, 1, 0, 0 /
C======================================================================
C
C  **** Book layer if needed.
C
      IF (LDLYR (ILAYD) .EQ. 0) CALL BKDLYR (ILAYD)
C
C ****  Fetch sizes and defines format if first call
C
      IF ( FSDSEC ) THEN
        NWDSEC = IQ( LCDCH + 2 )
        NBWIRE = IQ( LCDCH + 3 )
        LHDSEC = 3 + 2 * NBWIRE
        WRITE( FMDSEC, 1000 ) LHDSEC, NWDSEC-9
 1000   FORMAT(I2,'I / 1B 7F 1B ',I1,'I ')
        CALL MZFORM ('DSEC', FMDSEC, MPDSEC(5))
        CALL UCTOH(  'DSEC', MPDSEC(1), 4, 4 )
        FSDSEC =.FALSE.
      ENDIF
C
C ****  Book sector if needed
C
      LKDSEC = LDSEC( ISECD, ILAYD)
      IF ( LKDSEC .EQ. 0) THEN
        MPDSEC(4) = LHDSEC + NHITS * NWDSEC
        CALL MZLIFT (IXMAIN, LDSEC(ISECD,ILAYD), LDLYR(ILAYD),
     &                -IZDSEC-ISECD, MPDSEC, 0 )
        LKDSEC = LDSEC( ISECD, ILAYD)
      ENDIF
C
C ****  Set numeric bank ID
C
      IQ( LKDSEC - 5) = ( 32 * ILAYD + ISECD ) * 16
      IQ(LKDSEC) = ISETVN(IQ(LKDSEC),0)
      IQ( LKDSEC + 1) = 0
      IQ( LKDSEC + 2) = NBWIRE
      IQ( LKDSEC + 3) = NWDSEC
C
  999 CONTINUE
      RETURN
      END
