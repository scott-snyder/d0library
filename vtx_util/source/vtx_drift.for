      SUBROUTINE VTX_DRIFT(WIRE,SECTOR,LAYER,
     &                     T,SLOPE,FAST,
     &                     DPLS,DMIN,EPLS,EMIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the two drift distances, perpendicular
C-               to the sector's sense plane, corresponding to the drift
C-               time T.  Isochrones are parametrized and stored in a lookup
C-               table for various values of T.  The interpolating routine 
C-               DIVDIFL is used.
C-      
C-      Isochrones have been parametrized as
C-      
C-                X(Y,T) = A(T) + B(T)*Y + C(T)*Y**2
C-
C-      where X is distance perpendicular to sense plane; Y is parallel
C-      
C-      The drift distance is parametrized as
C-      
C-           D(T;S) = P1(T) + P2(T)*(SLOPE-S0) + P3(T)*(SLOPE-S0)**2
C-                    + 100*(SCALE-1)*P4(T)
C-           
C-      where T is the measured drift time, S0 is the slope of a radial track
C-      relative to the sense plane and SLOPE is an externally supplied slope.
C-      For radial tracks (FAST = .TRUE.) it is not necessary to unpack P2 and
C-      P3 sinse (SLOPE-S0) = 0.
C-      
C-      P4(T) is the correction to the distance time relation for radial tracks
C-      for a +1% increase in E/P.  If E/P --> SCALE*E/P, 
C_      then 100.*(SCALE-1)*P4 is the corresponding correction.
C_      (the E/P change is monitored with CALL VTX_DYNADJ and SCALE is
C_      updated in C(LVDTM+6). Wire dependent fine correction on this 
C_      relation is also parametrized as a multiplicable factor to P4(T),
C_      determined with alignment job, and kept in VTMW bank )
C-
C-   Inputs  : WIRE,SECTOR -- usual, starting at 0
C-             T           -- drift time in nsec
C-             SLOPE       -- angle track makes with sense plane
C-   Outputs : DPLS,DMIN   -- +PHI (positive) and -PHI (negative) drift 
C-                              distances in cm from sense plane
C-             EPLS,EMIN     -- ERRORS IN ABOVE DRIFT DISTANCES
C-   Controls: FAST        -- if true, only use leading term in distance-time 
C-                              map -- eg.. RADIAL TRACKS.
C-
C-   Created  14-JUN-1990   ED OLTMAN
C-   Updated  22-JUN-1990   ED OLTMAN  D0/ZEBRA-ized 
C-   Updated   6-JUL-1990   ED OLTMAN  put SCALE here (modified VDTM bank) 
C-   Updated  18-OCT-1990   ED OLTMAN  ADDED SECTOR DEPENDENCE 
C-   Updated  27-MAY-1992   Ed Oltman  Parametrize tables for radial tracks 
C-   Updated  27-MAY-1992   Ed Oltman  Include scale correction 
C-   Updated   2-JUN-1992   Ed Oltman  INCLUDED ERRORS 
C-   Updated  22-JUL-1992   Ed Oltman  Fix bug for FAST=false 
C-   Updated  29-JAN-1993   Liang-ping Chen  cleanup the comments and use P4  
C-   Updated  11-FEB-1993   Ed Oltman  get SCALE factor from VTMW 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
c I/O:
      INTEGER    WIRE,SECTOR,LAYER
      REAL       T,SLOPE,DPLS,DMIN,EPLS,EMIN
      LOGICAL      FAST
c Locals:
      REAL      P3MIN
      INTEGER LVTMW,LVDTM,SIZE,PT,N,ITEMS,PTS,NS
      INTEGER SIDE,OFFSET,EVOD,ERR,VERS
      REAL    SIGN,P1,P2,P3,P4,D(0:1),DE(0:1),VEL,SCALE,S0
      REAL    JUNK,ANOD_VEL
      LOGICAL FIRST,OLD,RCPROB
      REAL    DELTA_T,DT_RESA,DT_RESB,DT_ENV,TNHALF(0:2),MAX,ERROR_TERM
c Externals:
      INTEGER GZVDTM,GZVTMW
c Data:
      DATA FIRST/.TRUE./
      DATA OLD  /.FALSE./
      DATA VERS/1/
C----------------------------------------------------------------------
      IF (FIRST) THEN
C
C ****  Make sure this is the right version number
C
        FIRST = .FALSE.
        LVDTM =GZVDTM(0,0)
        IF (IBITS(IC(LVDTM),13,5) .NE. VERS) THEN
          CALL ERRMSG('VTRAKS','VTX_DRIFT','
     &      DTM BANK WRONG VERSION!','W')
          OLD = .TRUE.
        ENDIF
        CALL EZPICK('VTRAKS_RCP')
        RCPROB = .FALSE.
        CALL EZGET('DT_RESA',DT_RESA,ERR)
        RCPROB = RCPROB .OR. (ERR .NE. 0)
        CALL EZGET('DT_RESB',DT_RESB,ERR)
        RCPROB = RCPROB .OR. (ERR .NE. 0)
        CALL EZGET('DT_ENV',DT_ENV,ERR)
        RCPROB = RCPROB .OR. (ERR .NE. 0)
        IF (RCPROB) THEN
          CALL INTMSG(
     &        ' VTX_DRIFT: DRIFT VEL ERR PARAMS NOT IN VTRAKS_RCP')
          DT_RESA =   12.5
          DT_RESB = 0.0385
          DT_ENV  = 0.0
        ENDIF
C..COMPUTE ERROR FOR T<T0 -- USE WIRE 0, SIDE 0 LAYER 0 SECTOR 0
        ITEMS =IC(LVDTM+3)
        SIZE = IC(LVDTM+5)
        OFFSET = LVDTM + 22 
        PT = LVDTM + 22 + 32*ITEMS + IC(OFFSET+1)
        N  = IC(OFFSET+2)
        CALL DIVDIFL( C(PT),C(PT+SIZE),N, 0.,JUNK, ANOD_VEL)
        ANOD_VEL = ABS(ANOD_VEL)
        CALL EZRSET
        TNHALF(0) = TAN( ATAN(1.)/4. )
        TNHALF(1) = TAN( ATAN(1.)/8. )
        TNHALF(2) = TNHALF(1)
        ERROR_TERM= 1/SQRT(12.)
      ENDIF
C
C ****  ENTRY POINT
C
      LVTMW = GZVTMW(LAYER)
      ITEMS = IC(LVTMW+3)
      SCALE = C(LVTMW + ITEMS*(8*SECTOR+WIRE) + 10)
      LVDTM = GZVDTM(LAYER,SECTOR)
      ITEMS =IC(LVDTM+3)
      SIZE = IC(LVDTM+5)
      SCALE= SCALE*C(LVDTM+6)                         
C
C  this is an awfully crude parametrization of gas diffusion
C
      DELTA_T = DT_RESA + DT_RESB*AMAX1(0.,T)
c
c   If sector number is odd, then let Pn --> -Pn for n=1,2,3,4 and
c       let phi --> -phi 
c
      SIGN = 1.
      IF (MOD(SECTOR,2) .EQ. 1) SIGN = -1.
c
c  Loop over drift directions
c
      DO SIDE = 0,1
        P4 = 0.
        EVOD = SIDE
        IF (SIGN .LT. 0.) EVOD = 1 - SIDE
        OFFSET = LVDTM + 22 + 2*ITEMS*WIRE + 16*ITEMS*EVOD
C.. POINTER AND SIZE OF DRIFT DIRECTION FOR RADIAL TRACK
        PT = LVDTM + 22 + 32*ITEMS + IC(OFFSET+1)
        N  = IC(OFFSET+2)
C... POINTER AND SIZE OF SCALE CORRECTION (not for old version)
        IF (.NOT. OLD) THEN
          PTS = LVDTM + 22 + 32*ITEMS + IC(OFFSET+7)
          NS  = IC(OFFSET+8)
        ENDIF
        IF (T .LE. 0.) THEN                      ! t<t_0?? set dist = 0
          D(SIDE) = 0.
          DE(SIDE) = ANOD_VEL*SQRT(DELTA_T)
        ELSEIF (T .LE. C(PT+SIZE+N-1)) THEN        ! normal time 
          CALL DIVDIFL(C(PT),C(PT+SIZE),N,T,P1,VEL)
          IF (.NOT. OLD) CALL DIVDIFL( C(PTS),C(PTS+SIZE),NS,T,P4,
     &      JUNK)
          P1 = SIGN*( P1 + 100.*(SCALE-1.)*P4)
          IF (FAST .OR. OLD) THEN
            D (SIDE) = P1
            DE(SIDE) = SQRT( DELTA_T*VEL**2 + (DT_ENV*P4)**2)
          ELSE
            S0 = P1/C(LVDTM+7+WIRE) ! angle w.r.t. sense plane for radial track
C... POINTERS AND SIZES FOR ANGLE CORRECTIONS
            PT = LVDTM + 22 + 32*ITEMS + IC(OFFSET+3)
            N  = IC(OFFSET+4)
            CALL DIVDIFL( C(PT),C(PT+SIZE),N,T,P2,JUNK)
            PT = LVDTM + 22 + 32*ITEMS + IC(OFFSET+5)
            N  = IC(OFFSET+6)
            CALL DIVDIFL( C(PT),C(PT+SIZE),N,T,P3,JUNK)
            D(SIDE) = P1 + SIGN*(P2*(SLOPE-S0) + P3*(SLOPE-S0)**2)
            DE(SIDE) = SQRT( DELTA_T*VEL**2 + (DT_ENV*P4)**2)
          ENDIF                         ! END OF FAST = .FALSE.
        ELSE                            ! late arrival time
C
C ****  P1 is the distance to the last good isochrone; max is the distance to
C ****  the cathode (wire at nominal positon)
C
          P1 = SIGN*(C(PT+N-1) + 100.*(SCALE-1.)*C(PTS+NS-1))
          MAX = (2.*SIDE-1.)*C(LVDTM+7+WIRE)*TNHALF(LAYER) 
     &        - SIGN*C(LVDTM+15+WIRE)
          D(SIDE) = 0.5*( P1 + MAX )
          DE(SIDE)= ERROR_TERM*ABS(P1-MAX)
        ENDIF
      ENDDO
C
C These are the drift distances in a frame that has the sense plane along the 
C +x-axis  -- DPLS >0 and DMIN < 0
C
      DPLS = -D(0)
      DMIN = -D(1)
      EPLS =DE(0)
      EMIN =DE(1)
  999 RETURN
      END
