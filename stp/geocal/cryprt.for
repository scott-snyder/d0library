      SUBROUTINE CRYPRT (LUN,NAME,SHAPE,VNAME,MEDIUM,Z0,Z,RMIN,RMAX,NP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out PCON and TUBE parameters in SRCP
C-                         format for cryostat. Each volume is POSitioned
C-                         by a simple translation along the Z axis.
C-                         The mother volume is MCAL.
C-
C-   Inputs  : LUN         Unit number for output stream
C-             NAME        Name of cryostat section
C-             SHAPE       GEANT shape name
C-             VNAME       GEANT volume name
C-             MEDIUM      GEANT medium number
C-             Z0          Z-displacement of section
C-             Z(*),RMIN(*),RMAX(*)  Parameters
C-             NP          Number of parameters
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-OCT-1988   Harrison B. Prosper
C-   Updated  20-JUL-1989   N.A. Graf
C-                          GEANT assumes that the PCON parameters
C-                          are monotonically increasing in Z. Therefore
C-                          the Z values have been reordered. This could
C-                          perhaps be handled more elegantly, but...
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*4  VNAME,SHAPE,MOTHER,OPTION
      CHARACTER*(*) NAME
      CHARACTER*80 STRING
      INTEGER      I,J,K,L,M,N
      INTEGER      LUN,LNAME,MEDIUM,MATRIX,NP,JJ
      REAL         Z(*),RMIN(*),RMAX(*),X0,Y0,Z0
      INTEGER      NCH, IMAP(500)
      REAL SRTM(500), SRT1(500), SRT2(500)
      PARAMETER( MATRIX = 1 )
      PARAMETER( OPTION = 'POS ' )
      PARAMETER( MOTHER = 'MCAL' )
      PARAMETER( JJ = 1 )
      PARAMETER( X0 =  0.0)
      PARAMETER( Y0 =  0.0)
C----------------------------------------------------------------------
C
C ****  PRINT OUT HEADER info
C
      L = LEN (NAME)
      STRING = ' \ARRAY '//NAME(1:L)
      WRITE(LUN,300) STRING
  300 FORMAT(A80)
C
      WRITE(LUN,310)''''//VNAME//'''',
     &    ''''//SHAPE//'''',MEDIUM, ''''//MOTHER//''''
  310 FORMAT(1X,5X,A6,2X,A6,2X,I6,2X,A6)
C
      WRITE(LUN,320) ''''//OPTION//'''', MATRIX, JJ, X0, Y0, Z0, NP
  320 FORMAT(1X,5X,A6,2X,I6,2X,I6,3F10.4,I6)
C
C ****  Print OUT POINTS
C
C ****  POLYCONE
C
      IF ( SHAPE .EQ. 'PCON' ) THEN
C
C:::  Added sort routine here
C
        NCH = NP/3 - 1
        DO 137 I = 1,NCH
         SRTM(I) = Z(I+1)
         IMAP(I) = (I)
         SRT1(I) = RMIN(I+1)
         SRT2(I) = RMAX(I+1)
137     CONTINUE
        CALL SRTFLT(SRTM,NCH,IMAP)
        WRITE(LUN,340) Z(1),RMIN(1),RMAX(1)
        DO 345 K =  1,NCH
          WRITE(LUN,340) SRTM(K),SRT1(IMAP(K)),SRT2(IMAP(K))
  340     FORMAT(1X,3F10.4)
  345   CONTINUE
C
C ****  TUBE
C
      ELSEIF ( SHAPE .EQ. 'TUBE' ) THEN
        WRITE(LUN,FMT=340) RMIN(1),RMAX(1),Z(1)
      ENDIF
C
      STRING = ' \END'
      WRITE(LUN,300) STRING
C
  999 RETURN
      END
