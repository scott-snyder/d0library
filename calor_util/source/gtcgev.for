      SUBROUTINE GTCGEV(ILYR,IPHI,IETA,SCALE,NCGEV,CGEV,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns CGEV contents for channel IETA,IPHI,ILYR
C-
C-   Inputs  : IETA  [I]  physics address ETA
C-             IPHI  [I]  physics address PHI
C-             ILYR  [I]  physics address LAYER
C-             SCALE [I]  ADC SCALE 0 (*8) OR 1 (*1)
C-   Outputs : NCGEV [I]  Number of values returned
C-             CGEV  [R]  VALUES (K=AWCG,S=ped SIGMA,P=Pedestal)
C-             IER   [I]  IF IER = 0 OK
C-   Controls: none
C-
C-   Created   8-APR-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,ILYR,SCALE,NCGEV,IER
      REAL    CGEV(*)
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER I,LZFIND,NH,NR,ICGEV,MAX_CGEV,MCGEV,NV,NS,ND
      INTEGER LCGEV1,LCGEV,GZCGEV
      PARAMETER( MCGEV = NLYRL*NPHIL*(NETAL*2+1))
      CHARACTER MSG*80
      LOGICAL FIRST,LSCALE
      SAVE FIRST,NH,NR
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IER = 0
      LCGEV = GZCGEV ()
      IF (LCGEV.LE.0) THEN
        CGEV(1) = 0
        IER = -1
        GOTO 999
      END IF
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL GTCGEV_HEAD(NH,NV,NS,NR,ND,IER)
        MAX_CGEV = NH + NR*MCGEV
        IF(ND.NE.MAX_CGEV) THEN
          WRITE(MSG,100)ND,MAX_CGEV
  100     FORMAT(' ND=',I10,' NEED ',I10)
          CALL ERRMSG('CGEV_MISCOUNT','GTCGEV',MSG,'W')
          IER = -2
        END IF
        IF( (NR.LT.1).OR.(NR.GT.3) ) THEN
          WRITE(MSG,102)NR
  102     FORMAT(' NR=',I10)
          CALL ERRMSG('CGEV_HEADER_PROBLEM','GTCGEV',MSG,'W')
          IER = -2
        END IF
        LCGEV = GZCGEV ()
        IF(LCGEV.GT.0) THEN
          LSCALE = .TRUE.
          LCGEV1=LC(LCGEV)
          IF((LCGEV1.EQ.0)) THEN
            CALL ERRMSG(' CGEV HAS ONLY ONE SCALE ','GTCGEV',
     &        'IGNORE INPUT SCALE','W')
            LSCALE = .FALSE.
          END IF
        END IF
      END IF
      IF((LCGEV.GT.0).AND.(SCALE.NE.NS).and.(LSCALE))
     &  LCGEV=LZFIND(IDVSTP,LCGEV,SCALE,3)
      NCGEV = NR
      ICGEV = NH+NR*(ILYR-1+(IPHI-1)*NLYRL+(IETA+NETAL)*NPHIL*NLYRL)
      IF( (ICGEV.GT.MAX_CGEV) .OR. ICGEV.LT.0) THEN
        CGEV(1) = 0
        IER = -2
        GOTO 999
      END IF
      CALL VZERO(CGEV,NR)
      DO I = 1, NR   !K,Sigma,Pedestal
        CGEV(I) = C(LCGEV+ICGEV+I)             
      END DO
  999 RETURN
      END
