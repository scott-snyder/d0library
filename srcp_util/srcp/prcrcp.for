      SUBROUTINE PRCRCP ( PRUNIT, LCRCP1, NCRCP, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CRCP'. LCRCP must point to a CRCP bank in either
C-              ZEBCOM or ZEBSTP. If LCRCP=0 then CRCP is found via LZLOC
C-              in ZEBSTP. 
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCRCP  [I] : LCRCP must point to a CRCP bank in either
C-                          ZEBCOM or ZEBSTP. If LCRCP=0 then CRCP is found 
C-                          via LZLOC in ZEBSTP. 
C-                          Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-             NCRCP  [I] : Bank number, used only if CFL='ONE' and LCRCP = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LCRCP point to a bank, or if <0, NCRCP is
C-                                  the bank number.
C-                          'LINEAR' : LCRCP points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: IFL.ge.0 means full
C-                          printout. -1 Supresses the first /START line
C-                          
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  13-JUN-1992   Chip Stewart   
C-   Updated   4-Sep-1993   Herbert Greenlee
C-    Added memory-to-memory option (EZ_PUT_FIFO).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER PRUNIT, LCRCP, NCRCP, IFL,IFL1
      CHARACTER*(*) CFL
      CHARACTER LINE*132,STRING*4,CR*1
      INTEGER N,HCRCP,ISTORE,LENL
      INTEGER LCRCP1, GZCRCP, LZLOC,LZFIDH, I,J,K,L,ND
C----------------------------------------------------------------------
      IFL1 = IFL
      LCRCP = LCRCP1
      CR = CHAR(13)
      CALL UCTOH('CRCP',HCRCP,4,4)
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LCRCP1 .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LCRCP1 .LE. 0 ) THEN
          IF( NCRCP .EQ. 0 ) GOTO 980          ! Error exit
          LCRCP = LZLOC( IDVSTP, 'CRCP', NCRCP )
          IF (LCRCP.EQ.0) LCRCP =LZFIDH(IXMAIN,HCRCP,0)
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
        IF( LCRCP1 .LE. 0 ) THEN
          LCRCP =LZFIDH(IDVSTP,HCRCP,0)
          IF (LCRCP.EQ.0) LCRCP =LZFIDH(IXMAIN,HCRCP,0)
        ENDIF
      ELSE
        IF(PRUNIT.GE.0)THEN
          WRITE( PRUNIT, 1000 ) CFL
        ELSE
          WRITE( 6, 1000 ) CFL
        ENDIF
1000    FORMAT(/' ** PRCRCP ** Illegal value of CFL = ',a/)
       GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Check LCRCP for CRCP bank
C
      IF(IC(LCRCP-4).EQ.HCRCP) THEN
        ISTORE = 0
        ND = IC( LCRCP-1) 
      ELSE IF(IQ(LCRCP-4).EQ.HCRCP) THEN
        ISTORE = 1
        ND = IQ( LCRCP-1) 
      ELSE
        IF(PRUNIT.GE.0)THEN
          WRITE( PRUNIT, 1010 ) LCRCP
        ELSE
          WRITE( 6, 1010 ) LCRCP
        ENDIF
1010    FORMAT(/' ** NO CRCP BANK AT ',I5/)
        GOTO 999
      END IF
      LINE = ' '
      N = 0
      DO 1100 J = 1, ND
        IF(ISTORE.EQ.1) THEN
          CALL UHTOC ( IQ( LCRCP + J ),4,STRING,4)
        ELSE 
          CALL UHTOC ( IC( LCRCP + J ),4,STRING,4)
        END IF
        IF (N.GT.0) THEN
          LINE(1:N+4)  = LINE(1:N)//STRING(1:4)  
        ELSE
          LINE(1:4)  = STRING(1:4)  
        END IF
        LENL = INDEX(LINE(1:N+4),CR)-1
        IF (LENL .GE. 0 ) THEN
          IF(IFL1.EQ.-1) THEN
            CALL WORD (LINE(1:LENL),I,K,L)
            CALL UPCASE (LINE(I:K),LINE(I:K))
            IF ( LINE(I:K) .EQ. '\START' ) THEN
              LINE = ' '
              N = 0
              IFL1 = 1
              GOTO 1100
            END IF
          END IF
          IF(PRUNIT.GE.0)THEN
            WRITE( PRUNIT, '(A)') LINE (1:LENL)
          ELSE
            CALL EZ_PUT_FIFO(LINE(1:LENL))
          ENDIF
          LINE = ' '
          N = 0
        ELSE
          N = N + 4
        END IF
 1100 END DO
C 1200 FORMAT(/1X,'! PRCRCP - RCP FILE ** ')
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF((CFL(1:3).EQ.'LIN').OR.(CFL(1:3).EQ.'ALL')) THEN
        IF(ISTORE.EQ.0) THEN
          LCRCP = LC( LCRCP )
        ELSE
          LCRCP = LQ( LCRCP )
        END IF
        IF( LCRCP .NE. 0 ) GOTO 1
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 CONTINUE
      IF(PRUNIT.GE.0)THEN
        WRITE( PRUNIT, 2000 ) LCRCP
      ELSE
        WRITE( 6, 2000 ) LCRCP
      ENDIF
 2000 FORMAT(/' ** PRCRCP ** called for LINEAR without valid bank '
     & ,       'pointer, LCRCP =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 CONTINUE
      IF(PRUNIT.GE.0)THEN
        WRITE( PRUNIT, 2100 ) LCRCP, NCRCP
      ELSE
        WRITE( 6, 2100 ) LCRCP, NCRCP
      ENDIF
 2100 FORMAT(/'  ** PRCRCP ** called for ONE without bank pointer and '
     &  ,      'bank number, LCRCP =',I10,' NCRCP =', I10/)
      GOTO 999
      END
