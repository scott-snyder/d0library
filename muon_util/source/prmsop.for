      SUBROUTINE PRMSOP(PRUNIT,LMSOPI,NMSOP,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'MSOP'.
C-
C-   Inputs  : PRUNIT   [I] : Unit number for printout
C-             LMSOPI   [I] : Not used
C-             NMSOP(*) [I] : Module Number(s)
C-             CFL      [C*]: Not used
C-             IFL      [I] : Number of modules (0 for all)
C-
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  27-JUNE-90       S.T.Repond
C-   modified 22-aug-90        STR    print format extended to 3 decimals
C-                                    (T1) and 5 decimals (R1)
C-   Updated  14-JUN-1991   Silvia Repond
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT,LMSOPI,NMSOP(*),IFL
      CHARACTER   CFL*(*)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      INTEGER MAXCOL, MAXROW, MAXBUF, MAXMOD
      PARAMETER( MAXCOL = 9 )
      PARAMETER( MAXROW = 33)
      PARAMETER( MAXBUF = MAXCOL*MAXROW )
      PARAMETER( MAXMOD = 307)
C
      INTEGER JMOD,GZMSRH
      INTEGER LMSOP,LMSRHI, NUMIDB(MAXCOL),ICOL,IMOD,I,IBIG,IEND,J
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
      DATA CTITL /'Bank VRSN','Status','Quality','Low_Run#','High_Run#'
     6, 'Generated Run','Generated Date','Generated Type of Run'
     9, 'Module Number','spare','spare'
     2, 'Module Orientation','Stagger Type',3*'Spare'
     +, 'X: Loc. Corner in Glb. from M.C.'
     +, 'Y        "','Z        "'
     +, 'X: Loc. Center in Glb. from SRV'
     +, 'Y        "','Z        "'
     3, 'cos(X,x) R11',' " (X,y) R12',' " (X,z) R13'
     +, ' " (Y,x) R21' ,' " (Y,y) R22'
     8, ' " (Y,z) R23',' " (Z,x) R31',' " (Z,y) R32',' " (Z,z) R33'
     2, 'status','status' /
C----------------------------------------------------------------------
C
C ****  Get support bank (MSRH)
C
      LMSRHI = GZMSRH(0)
      IF ( LMSRHI .LE. 0 ) THEN
        WRITE (PRUNIT,1201)
        GOTO 999
      ENDIF
C
C ****  Check name of bank
C
      CALL DHTOC(4,IC(LMSRHI-4),BANK)
      IF ( BANK .NE. 'MSRH' ) THEN
        WRITE (PRUNIT,1202) BANK
        GOTO 999
      ENDIF
C
C ****  Loop over modules
C
      JNAME = 'MSOP'
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
            LMSOP= LC(LMSRHI-IMOD)
            IF ( LMSOP .LE. 0 ) GOTO 100
          ENDIF
        ELSE
  110     CONTINUE
          IF ( JMOD .LT. IFL ) THEN
            JMOD = JMOD + 1
            IMOD = NMSOP(JMOD)
            LMSOP= LC(LMSRHI-IMOD)
            IF ( LMSOP .LE. 0 ) THEN
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
        NUMIDB(ICOL) = IC(LMSOP-5)

        DO J=1,MAXROW
          IF (J .LE. 16) THEN
            IBUFC(J,ICOL) = IC(LMSOP+J)
          ELSE
            BUFC(J,ICOL) =  C(LMSOP+J)
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
C---  Print MSOP Bank
C-
        WRITE (PRUNIT,1000)
        WRITE (PRUNIT,1005) JNAME
        WRITE (PRUNIT,1015)
        WRITE (PRUNIT,1010) (NUMIDB(I), I=1,ICOL)
        WRITE (PRUNIT,1015)
        DO J=1,33
          IF (J .LE. 16) THEN
            FMCHAR(23:28) = FMINT
            WRITE (PRUNIT,FMCHAR) J,CTITL(J),(IBUFC(J,I), I=1,ICOL)
          ELSEIF(J.GT.16.AND.J.LT.23) THEN
            FMCHAR(23:30) = FMFLOAT
            WRITE (PRUNIT,FMCHAR) J,CTITL(J),(BUFC(J,I), I=1,ICOL)
          ELSEIF(J.GT.22.AND.J.LT.31) THEN
            FMCHAR(23:30) = FMFLOAR
            WRITE (PRUNIT,FMCHAR) J,CTITL(J),(BUFC(J,I), I=1,ICOL)
          ELSE
            FMCHAR(23:30) = FMFLO
            WRITE (PRUNIT,FMCHAR) J,CTITL(J),(BUFC(J,I), I=1,ICOL)
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
C-
 1200 FORMAT(' === @PRMSOP. NO MSOP BANK !!! ===')
 1201 FORMAT(' === @PRMSOP. NO MSRH BANK !!! ===')
 1202 FORMAT(' === @PRMSOP. WRONG SUPPORT BANK ',A4,' !!! ===')
C
  999 RETURN
      END
