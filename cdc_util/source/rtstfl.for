      SUBROUTINE RTSTFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank RTST.
C-
C-   Inputs  :  
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  23-OCT-1995 09:20:30.95  Jadwiga Warchol
C-
C-   Updated  20-NOV-1995 Jadwiga Warchol Added checks for CDD1 bank, copy
C-                                        RTST from FILT to RECO if CDD1
C-                                        absent and fitfinding done in L2
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZRTST.LINK'
      CHARACTER*4 PATH
      INTEGER LCDD1, LUPGD, GZUPGD
      INTEGER LRTST
      INTEGER GZRTST
      INTEGER IADDR, RADDR
      DATA RADDR /'1C0D'X/
C      DATA RADDR /'1C1C'X/
      INTEGER MaxData
      PARAMETER   ( MaxData = 1536 )
      INTEGER       DATA(MaxData)
      INTEGER IF, NTD, IS, I, IC, WIDTH, ID, NTF, K, NTP
C----------------------------------------------------------------------
C
C***  Check CDD1 bank
C
      LCDD1 = LQ(LHEAD-IZCDD1)
      IF ( LCDD1 .LE. 0 ) THEN
C
C***    If no CDD1, check under FILT for RTST banks created in Level 2
C     
        CALL PATHGT(PATH)
C
        IF ( PATH .NE. 'FILT' ) CALL PATHST('FILT')
        LRTST = GZRTST()
        IF ( LRTST .LE. 0 ) GOTO 20
C       
        IF ( PATH .NE. 'RECO' ) CALL PATHST('RECO')
        LUPGD = GZUPGD()
        IF ( LUPGD .LE. 0 ) CALL BKUPGD(LUPGD)
        IF ( LUPGD .LE. 0 ) GOTO 20
        CALL MZCOPY(IXMAIN,LRTST,IXMAIN,LUPGD,-IZRTST,'S')
C
C***    Reset conditions and return
C
   20   CONTINUE
        CALL PATHST(PATH)
        RETURN
      ENDIF
C
C***  Check CDD1 for data
C
      NTD = 0
      IF = 1
      DO IADDR = RADDR, RADDR + 2
        IS = 1 + (IF - 1)*512
        CALL ZDEXPD( 1, IADDR,  DATA(IS) )
        IF( DATA(IS) .EQ. 0) GOTO 5
        IC = IS
    1   CONTINUE
        WIDTH =  DATA(IC)
        I = WIDTH/4
        NTD = NTD + 1 + I
        IC = IC + 2 + WIDTH
        IF(  DATA(IC) .NE. 0) GOTO 1
    5   CONTINUE
        IF = IF + 1
      ENDDO       
C
      IF( NTD .EQ. 0) GOTO 999
C
C***  Book the bank of NTDATA words
C
      NTD = NTD + 3
      CALL BKRTST(NTD, LRTST)
      IF( LRTST .LE. 0) GOTO 999
C
C***  fill in the bank here.
C
      ID = 4
      DO IF = 1,3
        NTF = 0
        IS = 1 + (IF - 1)*512
        IF( DATA(IS) .EQ. 0) GOTO 10
        IC = IS
    6   CONTINUE
        NTP = 0
        WIDTH =  DATA(IC)
        IQ( LRTST + ID ) = IOR(ISHFT(DATA(IC+1),16),WIDTH)
        I = WIDTH/4
        DO K = 1, I
          IQ( LRTST+ID+K ) = IOR(IOR(ISHFT(DATA(IC+4*K+1),24),
     &                               ISHFT(DATA(IC+4*K  ),16)),
     &                               IOR(ISHFT(DATA(IC+4*K-1),8),
     &                               DATA(IC+4*K-2)))
        ENDDO
        NTF = NTF + 1 + I
        NTP = NTP + 1 + I
        IC = IC + 2 + WIDTH
        ID = ID + NTP
        IF(  DATA(IC) .NE. 0) GOTO 6
   10   CONTINUE
        IQ( LRTST + IF) = NTF
      ENDDO       
C
  999 RETURN
      END
