      SUBROUTINE SNAPSHOT(DOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Take a snapshot of the GEANT stacks (JVERT,
C-                         JKINE,JSTAK) and dump it to an output file
C-
C-   Inputs  : DOUT  Unit number to which output is written
C-   Outputs : 
C-   Controls: 
C-
C-   Created  12-DEC-1991   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:GCKING.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
      INCLUDE 'D0$INC:GCNUM.INC'
      INCLUDE 'D0$INC:GCSTAK.INC'
      INCLUDE 'D0$INC:GCTRAK.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
C
      INTEGER DOUT
      INTEGER I,J,N,K,L,NU
      INTEGER JV,JT,JUV,JUT,JST

C
C----------------------------------------------------------------------
C
C *** CURRENT TRACK PARAMETERS FROM /GCKINE/, /GCTRAK/, AND /GCKING/
C
      WRITE (DOUT,1000) ITRA,ISTAK,IVERT
      WRITE (DOUT,1001) IPART,NAPART,AMASS,CHARGE,TLIFE,ITRTYP,IPAOLD
      WRITE (DOUT,1002) VERT,PVERT
      WRITE (DOUT,1003) NGKINE,KCASE
      IF ( NGKINE.GT.0 ) THEN
        DO I = 1 , NGKINE
          WRITE (DOUT,1004) (GKIN(J,I),J=1,5),TOFD(I),IFLGK(I)
        ENDDO
      ENDIF
 1000 FORMAT(//,' **** STACK SNAPSHOT ****',//,
     &  ' ITRA= ',I3,' ISTAK= ',I3,' IVERT= ',I3)
 1001 FORMAT(' IPART ',I3,1X,5(A4),' M ',1PG10.2,' Q ',1PF5.0,' T0 ',
     &  1PG10.2,/,' ITRTYP ',I2,' IPAOLD',I4)
 1002 FORMAT(' VERT = ',3(1X,1PG10.2),/,' PVERT = ',4(1X,1PG10.2),//)
 1003 FORMAT(//,' NGKINE = ',I5,1X,A4)
 1004 FORMAT(6(1X,F10.2),1X,I3)
C
C *** VERTEX STACK
C
C      WRITE (DOUT,2000) NVERTX,JVERTX
C      IF ( JVERTX.NE.0 ) THEN
C        N = IQ(JVERTX+1)
C        WRITE (DOUT,2001) N
C        IF (N .GT. 0) THEN
C          DO I = 1 , N
C            JV = LQ(JVERTX - I)
C            WRITE (DOUT,2002) I,(Q(K),K=JV+1,JV+6)
C            JUV = LQ(JV - 1)
C            NU = IQ(JUV - 1)
C            IF (NU.GT.0) WRITE (DOUT,2003) NU,(Q(JUV+K),K=1,NU)
C          ENDDO
C        ENDIF
C      ENDIF
C 2000 FORMAT(' NVERTX = ',I4,' JVERTX = ',I10)
C 2001 FORMAT(' IQ(JVERTX + 1) = ', I5)
C 2002 FORMAT(' VTX # ',I3,' X,Y,Z ',3(1X,F6.1),' TOFG ',F6.1,
C     &  ' NTBEAM ',F5.0,' NTTARG ', F5.0)
C 2003 FORMAT(' # USER WORDS ',I3,' USER WORDS: ',5(1X,F10.2))
C
C *** JKINE stack
C
      WRITE (DOUT,3000) NTRACK,JKINE
      IF ( JKINE.NE.0 ) THEN
        N = IQ(JKINE+1)
        WRITE (DOUT,3001) N
        IF (N .GT. 0) THEN
          DO I = 1 , N
            JT = LQ(JKINE - I)
            JUT = LQ(JT - 1)
            NU = IQ(JUT - 1)
            WRITE (DOUT,3002) I,(Q(K),K=JT+1,JT+6),NU,(Q(JUT+K),K=1,NU)
          ENDDO
        ENDIF
      ENDIF
 3000 FORMAT(///,' NTRACK = ',I4,' JKINE = ',I10)
 3001 FORMAT(' IQ(JKINE + 1) = ', I5)
 3002 FORMAT(1X,I3,': ',4(1X,F6.1),2(1X,F5.0),1X,I1,2(1X,F3.0),
     &  1X,F4.0,4(1X,F3.0))
C
C *** JSTAK stack
C
      WRITE (DOUT,4000) JSTAK
      IF ( JSTAK.NE.0 ) THEN
        N = IQ(JSTAK + 1)
        WRITE (DOUT,4001) N,IQ(JSTAK+2),IQ(JSTAK+3),NSTMAX
        IF ( N.GT.0 ) THEN
          DO I = 1 , N
            JST = JSTAK + (I-1)*NWSTAK + 3
            WRITE (DOUT,4002) I,(IQ(JST+K),K=1,3),(Q(JST+L),
     &        L=4,12)
          ENDDO
        ENDIF
      ENDIF
 4000 FORMAT(///,' JSTAK = ',I10)
 4001 FORMAT('  IQ(JSTAK+1,2,3) ',3(1X,I5),' NSTMAX ',I5)
 4002 FORMAT(1X,I4,1X,I4,1X,I3,1X,I3,6(1X,F7.1),F6.1,1X,F8.2,1X,F3.0)
C
  999 RETURN
      END
