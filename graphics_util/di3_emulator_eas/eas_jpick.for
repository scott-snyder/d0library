      SUBROUTINE JPICK(DSPDV, PHYDV, ECHLV, BUT, SEGNM, PKID)
C
C    Purpose:
CD   This modules purpose is to pick a retained segment. The parameters
CD   passed in are display device, physical input device, and echo level
CD   (DSPDV, PHYDV, ECHLV). The parameters returned are all integers; a
CD   button value, a segment name and an optional pick-id.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 22-MAR-1989
CH   History:
CH      15-NOV-88  ATV  Initial entry.
C
      IMPLICIT NONE
CH      22-MAR-89  ATV  Added pick retrieval code.
C
C    Common blocks:
CB      SEGINF-R
C
C    Calls:
CC      PFN, PSNST, PCONN, PDI, PSNBOO, PGETW, KUPDV, PPURGE, ERROR.
C
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER DSPDV, PHYDV, ECHLV, BUT, SEGNM, PKID
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I, J, K, L, STRLEN, JSEG, LOCAT
      REAL PKAP(2)
      CHARACTER*254 STR, ICH
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      CALL KUPDV
      CALL PPURGE(ERRHND)
      CALL PFN('PRT', 'PRINT', ERRHND)
      CALL PSNBOO(.FALSE., 2, 'PRT', ERRHND)
      CALL PCONN('PRT', 1, 1, 'HOST_MESSAGE', ERRHND)
      PKAP(1) = 0.01
      PKAP(2) = 0.01
      CALL PSNV2D(PKAP, 2, 'PICK_LOCATION', ERRHND)
      CALL PFN('PIN', 'PICKINFO', ERRHND)
      CALL PSNFIX(50, 2, 'PIN', ERRHND)
      CALL PSNBOO(.FALSE., 2, 'PICK', ERRHND)
      CALL PFN('TMP', 'NOP', ERRHND)
      CALL PDIALL('TMP', ERRHND)
      CALL PCONN('TABLETIN', 6, 1, 'PICK', ERRHND)
      CALL PCONN('PICK', 1, 1, 'PIN', ERRHND)
      CALL PCONN('PIN', 2, 1, 'PRT', ERRHND)
      CALL PCONN('PIN', 1, 1, 'TMP', ERRHND)
      CALL PCONN('PIN', 3, 1, 'TMP', ERRHND)
      CALL PCONN('PIN', 4, 1, 'TMP', ERRHND)
      CALL PCONN('PIN', 5, 1, 'TMP', ERRHND)
      CALL PCONN('PIN', 6, 1, 'TMP', ERRHND)
      CALL PCONN('PIN', 7, 1, 'TMP', ERRHND)
      CALL PCONN('PIN', 8, 1, 'TMP', ERRHND)
      CALL PCONN('PIN', 9, 1, 'TMP', ERRHND)
      CALL PSNST('PICK INPUT REQUESTED. TRIGGER BY PRESSING '//
     +           'BUTTON ON PUCK.', 1,
     +           'MESSAGE_DISPLAY', ERRHND)
   10 CONTINUE
      CALL PGETW(STR, STRLEN, ERRHND)
      IF (STRLEN .EQ. 0) GOTO 10
      CALL PDI('PRT', 1, 1, 'HOST_MESSAGE', ERRHND)
      CALL PDI('TABLETIN', 6, 1, 'PICK', ERRHND)
      CALL PDI('PICK', 1, 1, 'PIN', ERRHND)
      CALL PDI('PIN', 2, 1, 'PRT', ERRHND)
      CALL PDI('PIN', 1, 1, 'TMP', ERRHND)
      CALL PDI('PIN', 3, 1, 'TMP', ERRHND)
      CALL PDI('PIN', 4, 1, 'TMP', ERRHND)
      CALL PDI('PIN', 5, 1, 'TMP', ERRHND)
      CALL PDI('PIN', 6, 1, 'TMP', ERRHND)
      CALL PDI('PIN', 7, 1, 'TMP', ERRHND)
      CALL PDI('PIN', 8, 1, 'TMP', ERRHND)
      CALL PDI('PIN', 9, 1, 'TMP', ERRHND)
      CALL KDECN(STR(2:4), JSEG)
      LOCAT = 0
      DO 20 I=1,NSEGS
         IF (SEGINF(6,I) .EQ. JSEG) LOCAT = I
   20 CONTINUE
      IF (LOCAT .EQ. 0)
     +   CALL ERROR('JPICK: INTERNAL ERROR WITH '//STR(1:STRLEN))
      SEGNM = SEGINF(1,LOCAT)
      PKID  = SEGINF(2,LOCAT) / 65536
      BUT = 0
      IF (PHYDV .GT. 1) RETURN
      WRITE (6,120)
      READ (5,110,END=1,ERR=1) ICH
  110 FORMAT(A1)
  120 FORMAT(' HIT ANY CHARACTER FOR BUTTON VALUE'
     +       ' AND RETURN TO CONTINUE')
      BUT = ICHAR(ICH)
      IF ( BUT .GT. 47 .AND. BUT .LT. 58) BUT = BUT - 48
      RETURN
C
C   Error exit.
C
    1 CONTINUE
      BUT = 999
      RETURN
      END
