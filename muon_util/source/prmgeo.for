      SUBROUTINE PRMGEO(PRUNIT,LMGEOI,NMGEO,CFL,IFL)
C----------------------------------------------------------------------
C.
C-    PRMGEO  - PRINT MGEO BANK                 NO  1.00 (03/05/87)
C.
C.    INPUT:
C.          PRUNIT - Unit Number for Printout
C.          LMGEOI - Not used
C.          NMGEO(*)  -  module numbers
C.        * CFL    - Flag to control Printout(ONE/ALL/OLD/NEW/CURRENT)
C.          IFL    - Number of modules (0 all)
C.
C.       (*)... Dummy Argument
C.  modified      22-aug-90    S.T.Repond  new print format (6 decimals)
C.                              for Rot mtx elements
C.
C-   Updated  14-JUN-1991   Silvia Repond
C    DH 3/92 words 14-17
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT,LMGEOI,NMGEO(*),IFL
      CHARACTER   CFL*(*)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      INTEGER MAXCOL, MAXROW, MAXBUF, MAXMOD
      PARAMETER( MAXCOL = 9 )
      PARAMETER( MAXROW = 58)
      PARAMETER( MAXBUF = MAXCOL*MAXROW )
      PARAMETER( MAXMOD = 307)
C
      INTEGER JMOD,GZMGEH
      INTEGER LMGEO,LMGEHI, NUMIDB(MAXCOL),ICOL,IMOD,I,IBIG,IEND,J
C
      REAL     BUFC(MAXROW,MAXCOL)
      INTEGER IBUFC(MAXROW,MAXCOL)
      EQUIVALENCE (IBUFC(1,1),BUFC(1,1))
      LOGICAL ACTIVE
C
      CHARACTER*4 JNAME,BANK
      CHARACTER*8 FMFLO,FMINT
      CHARACTER*8 FMFLOAT,FMFLOAR
      CHARACTER*32 CTITL(MAXROW), FMCHAR
C-
      DATA FMCHAR(1:22) /'(1X,I3,1H.,2X,A32,1H:,'/
      DATA FMINT,FMFLO  /'9I10)','9F10.2)'/
      DATA FMFLOAT,FMFLOAR  /'9F10.3)','9F10.5)'/
      DATA CTITL /'BANK VRSN ','Status','Quality','Low_Run#','High_Run#'
     6, 'Generated Run','Generated Date','Generated Type of Run'
     9, 'Module Number','# of planes per Module','# of cells per Plane'
     2, 'Module Orientation','Stagger Type','Length active area',
     a  'Height active area','Width active area','Z wire 0, plane 0'
     8, 'Average Resolution(drift)','Average Resolution(wire)'
     +, 'X of Local Origin in Global','Y        "','Z        "'
     3, 'cos(x,X)',' " (x,Y)',' " (x,Z)',' " (y,X)',' " (y,Y)'
     8, ' " (y,Z)',' " (z,X)',' " (z,Y)',' " (z,Z)'
     2, 'Wire Spacing in Local y','      "      in Local x'
     4, 'Length of Active Area','Height     "','Width      "'
     7, 'x of Wire 0 in Plane 0','y of Wire 0 in Plane 0'
     9, '      "     in Plane 1','      "     in Plane 2'
     1, '      "     in Plane 3','Gap between Plane 1 and 2'
     3, 'z of Ref. Point in Plane 0','         "      in Plane 1'
     5, '         "      in Plane 2','         "      in Plane 3'
     7, 'Correction to T1(X)','      "         (Y)'
     9, '      "         (Z)','dcos(x,X)','  " (x,Y)','  " (x,Z)'
     3, '  " (y,X)','  " (y,Y)','  " (y,Z)','  " (z,X)'
     7, '  " (z,Y)','  " (z,Z)'/
C----------------------------------------------------------------------
C
C ****  Get support bank (MGEH)
C
      LMGEHI = GZMGEH(0)
      IF ( LMGEHI .LE. 0 ) THEN
        WRITE (PRUNIT,1201)
        GOTO 999
      ENDIF
C
C ****  Check name of bank
C
      CALL DHTOC(4,IC(LMGEHI-4),BANK)
      IF ( BANK .NE. 'MGEH' ) THEN
        WRITE (PRUNIT,1202) BANK
        GOTO 999
      ENDIF
C
C ****  Loop over modules
C
      JNAME = 'MGEO'
      JMOD  = 0
      IMOD  = 0
      ICOL  = 0
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
   90   CONTINUE
C
C ****  Get next valid module bank
C
        IF ( IFL .LE. 0 ) THEN
  100     CONTINUE
          IF ( IMOD .LT. MAXMOD ) THEN
            IMOD = IMOD + 1
            LMGEO= LC(LMGEHI-IMOD)
            IF ( LMGEO .LE. 0 ) GOTO 100
          ENDIF
        ELSE
  110     CONTINUE
          IF ( JMOD .LT. IFL ) THEN
            JMOD = JMOD + 1
            IMOD = NMGEO(JMOD)
            LMGEO= LC(LMGEHI-IMOD)
            IF ( LMGEO .LE. 0 ) THEN
              GOTO 110
            ENDIF
          ENDIF
        ENDIF
C
C ****  We now have a valid bank
C
C ****  ZERO BUFFER
C
        IF ( ICOL .LE. 0 ) THEN
          CALL VZERO(BUFC(1,1),MAXBUF)
        ENDIF
C-
C---  Store BUFC
C
        ICOL = ICOL + 1
        NUMIDB(ICOL) = IC(LMGEO-5)

        DO J=1,MAXROW
          IF (J .LE. 13) THEN
            IBUFC(J,ICOL) = IC(LMGEO+J)
          ELSE
            BUFC(J,ICOL) =  C(LMGEO+J)
          ENDIF
        ENDDO
C
C ****  Check if we should print (continue if this is the last module)
C
        IF ( IFL .LE. 0 ) THEN
          IF ((ICOL .LT. MAXCOL) .AND. (IMOD .LT. MAXMOD) ) GOTO 90
        ELSE
          IF ((ICOL .LT. MAXCOL) .AND. (JMOD .LT. IFL) ) GOTO 90
        ENDIF
C-
C---  Print MGEO Bank
C-
   30   WRITE (PRUNIT,1000)
        WRITE (PRUNIT,1005) JNAME
        WRITE (PRUNIT,1015)
        WRITE (PRUNIT,1010) (NUMIDB(I), I=1,ICOL)
        WRITE (PRUNIT,1015)
        DO J=1,MAXROW
          IF (J .LE. 13) THEN
            FMCHAR(23:28) = FMINT
            WRITE (PRUNIT,FMCHAR) J,CTITL(J),(IBUFC(J,I), I=1,ICOL)
          ELSEIF (J .GT. 13.AND.J.LT.23) THEN
            FMCHAR(23:30) = FMFLOAT
            WRITE (PRUNIT,FMCHAR) J,CTITL(J),(BUFC(J,I), I=1,ICOL)
          ELSEIF (J .GT. 22.AND.J.LT.31) THEN
            FMCHAR(23:30) = FMFLOAR
            WRITE (PRUNIT,FMCHAR) J,CTITL(J),(BUFC(J,I), I=1,ICOL)
          ELSE
            FMCHAR(23:30) = FMFLO
            WRITE (PRUNIT,FMCHAR) J,CTITL(J),( BUFC(J,I), I=1,ICOL)
          ENDIF
        ENDDO
C
C ****  Reset column counter
C
        ICOL = 0
C
        IF ( IFL .LE. 0 ) THEN
          IF (IMOD .GE. MAXMOD ) GOTO 999
        ELSE
          IF (JMOD .GE. IFL )    GOTO 999
        ENDIF
      ENDDO
C
C---  Format Seq.
 1000 FORMAT(1H1)
 1005 FORMAT(1H0,6X,'==============================',/
     +,          7X,'I  Contents of Bank :  ',A4,'  I',/
     +,          7X,'==============================')
 1010 FORMAT(22X,'Bank ID          =',9I10)
 1015 FORMAT(3X,130(1H-))
 1200 FORMAT(' === @PRMGEO. NO MGEO BANK !!! ===')
 1201 FORMAT(' === @PRMGEO. NO MGEH BANK !!! ===')
 1202 FORMAT(' === @PRMGEO. WRONG SUPPORT BANK ',A4,' !!! ===')
C-
  999 RETURN
      END
