      SUBROUTINE GTPWCT(YT,DXT,UXT,NTY,NTXD,NTXU,NHY,NHXD,CHY,CHXD,
     &                  NP,MOM,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch PWC track parameters from PWCT bank.
C-                         A indicates intercept, B the slope.
C-
C-   Inputs  : None
C-   Outputs : NTY   = Number of y-tracks
C-             YT(5) = Track parameters: (Ay,By,sigma Ay,sigma By,chi2y)
C-             NHY   = number of hits on Y track 
C-             CHY   = binary: which chambers were hit for y-track
C-             NTXD  = Number of down stream x-tracks 
C-             DXT(5)= Track parameters: (Axd,Bxd,sigma Axd,sigma Bxd,chi2xd) 
C-             NHXD  = number of hits on downstream x-track 
C-             CHXD  = binary: which chambers were hit for downstream x-track
C-             NTXU  = Number of upstream x-tracks 
C-             UXT(4)= Track parameters: (A1xu,B1xu,A2xu,B2xu)
C-             NP    = Number of momenta
C-             MOM   = Momenta plus errors
C-             IER   = error code   0: ok
C-                                 -4: no TRPT bank
C-
C-   Created  19-APR-1990   Marcel Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER  NTY,NTXD,NTXU,NHY,NHXD,CHY,CHXD 
      INTEGER  NTR, NWTRK,NP,IER
      INTEGER  LPWCT,GZPWCT,LDATA,I
      REAL     YT(5),DXT(5),UXT(4),MOM(4)
C----------------------------------------------------------------------
      IER = 0
      LPWCT = GZPWCT() 
      IF( LPWCT .LE. 0 ) THEN
        IER = - 4
        GOTO 999
      ENDIF
C
C
      NWTRK = IQ(LPWCT+2)
      NTY   = IQ(LPWCT+3) 
      NTXD  = IQ(LPWCT+4) 
      NTXU  = IQ(LPWCT+5) 
      NP    = IQ(LPWCT+6) 
C
      LDATA = LPWCT+6 
      DO I = 1,4
        MOM(I) = Q(LDATA+I)
      ENDDO
C
C
C ****  fill values for Y track
C
      LDATA = LPWCT+10
      IF(NTY.NE.0) THEN 
        NHY = IQ(LDATA+1)
        CHY = IQ(LDATA+2)
        DO I = 1, 5
          YT(I) = Q(LDATA+2+I)
        ENDDO 
        LDATA=LDATA+NWTRK 
      ENDIF
C
C ****  fill values for down stream x-track
C
      IF(NTXD.NE.0) THEN 
        NHXD = IQ(LDATA+1)
        CHXD = IQ(LDATA+2)
        DO I = 1, 5
          DXT(I) = Q(LDATA+2+I)
        ENDDO 
        LDATA=LDATA+NWTRK 
      ENDIF
C
C
C ****  fill bank with Upstream X track(s)
C
      IF(NTXU .GT. 0) THEN
        UXT(1) = Q(LDATA+3)
        UXT(2) = Q(LDATA+4)
        IF (NTXU .GT. 1) THEN
          UXT(3) = Q(LDATA+NWTRK+3)
          UXT(4) = Q(LDATA+NWTRK+4)
        END IF
      END IF
C
  999 RETURN
      END
