      SUBROUTINE CJET_DUMP (PRUNIT,BANK,ICONT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine to dump JETS, PJET or CACL banks in a
C-   uniform format.
C-
C-   Inputs  : PRUNIT   [I]     Unit number for printout
C-             BANK     [C*]    'JETS' or 'PJET' or 'CACL'
C-   Outputs : None
C-   Controls: ICONT    [I]     Control flag (Unused for now)
C-
C-   Created  14-JAN-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER PRUNIT
      CHARACTER*(*) BANK
      INTEGER ICONT
C
      INTEGER MAXBNK
      PARAMETER( MAXBNK = 255 )
      INTEGER LL(MAXBNK),NBANK,I
      REAL    ET(MAXBNK)
C
      INTEGER MAXWRD
      PARAMETER( MAXWRD = 8 )
      REAL    RRR(MAXWRD),P
C
      INTEGER GZJETS,GZPJET,GZCACL
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C----------------------------------------------------------------------
C
C ****  Get address of first bank in linear chain
C
      IF     ( BANK .EQ. 'JETS' ) THEN
        LBANK = GZJETS()
      ELSEIF ( BANK .EQ. 'PJET' ) THEN
        LBANK = GZPJET()
      ELSEIF ( BANK .EQ. 'CACL' ) THEN
        LBANK = GZCACL()
      ELSE
        CALL ERRMSG ('CJET_DUMP','CALORIMETER',
     &    'No coding for Bank '//BANK(1:4),'W')
        GOTO 999
      ENDIF
      IF ( LBANK .LE. 0 ) THEN
        CALL ERRMSG ('CJET_DUMP','CALORIMETER',
     &    'Bank '//BANK(1:4)//' NOT found','W')
        GOTO 999
      ENDIF
C
C ****  Order in descending Et
C
      NBANK = 0
      DO WHILE ( (LBANK .GT. 0) .AND. (NBANK .LT. MAXBNK) )
        NBANK = NBANK + 1
        IF     ( BANK .EQ. 'JETS' ) THEN
          ET(NBANK) = Q(LBANK+6)
C
        ELSEIF ( BANK .EQ. 'PJET' ) THEN
          P = SQRT(Q(LBANK+3)**2+Q(LBANK+4)**2+Q(LBANK+5)**2)
          RRR(1) = Q(LBANK+6)*Q(LBANK+3)/P              ! Ex 
          RRR(2) = Q(LBANK+6)*Q(LBANK+4)/P              ! Ey
          ET(NBANK) = SQRT(RRR(1)**2 + RRR(2)**2)
C
        ELSEIF ( BANK .EQ. 'CACL' ) THEN
          ET(NBANK) = Q(LBANK+8)
        ENDIF
        LL(NBANK) = LBANK
        LBANK = LQ(LBANK)                       ! Get next bank 
      ENDDO
      CALL SRTFLT (ET,NBANK,LL)                 ! Sort Et's
C
C ****  Print title
C
      WRITE(PRUNIT,1000) BANK
C
C ****  Print data
C
      DO I =  NBANK, 1,-1
        LBANK = LL(I)                           ! Bank address
        IF     ( BANK .EQ. 'JETS' ) THEN
          CALL UCOPY (Q(LBANK+2),RRR(1),8)      ! Ex,Ey,Ez,E,Et,Theta,Phi,Eta
C
        ELSEIF ( BANK .EQ. 'PJET' ) THEN
          CALL UCOPY (Q(LBANK+3),RRR(1),4)      ! Px,Py,Pz,E
          P = SQRT(RRR(1)**2+RRR(2)**2+RRR(3)**2)
          RRR(1) = RRR(4)*RRR(1)/P              ! Ex 
          RRR(2) = RRR(4)*RRR(2)/P              ! Ey
          RRR(3) = RRR(4)*RRR(3)/P              ! Ez
          RRR(5) = ET(I)                        ! Et
          RRR(6) = Q(LBANK+9)                   ! Theta
          RRR(7) = Q(LBANK+8)                   ! Phi
          RRR(8) = Q(LBANK+10)                  ! Eta
C
        ELSEIF ( BANK .EQ. 'CACL' ) THEN
          CALL UCOPY (Q(LBANK+4),RRR(1),5)      ! Ex,Ey,Ez,E,Et
          CALL UCOPY (Q(LBANK+11),RRR(6),3)     ! Theta,Phi,Eta
        ENDIF
        RRR(1) = RRR(1)/RRR(4)          ! nx
        RRR(2) = RRR(2)/RRR(4)          ! ny
        RRR(3) = RRR(3)/RRR(4)          ! nz
        WRITE(PRUNIT,1010) NBANK-I+1,RRR
      ENDDO
C
C ****  FORMATS
C
 1000 FORMAT(1X,/,1X,'CJET_DUMP: Partial dump of BANK ',A4,/,
     &            1X,'------------------------------------',/,
     &       1X,7X,7X,'nx',7X,'ny',7X,'nz',8X,'E',7X,'Et',
     &       4X,'Theta',6X,'Phi',6X,'Eta')
 1010 FORMAT(1X,I3,4X,8(F9.3))
C
  999 RETURN
      END
