      SUBROUTINE CRYCEN (LUNOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert raw points to PCON and TUBE
C-                         parameters for CENTRAL cryostat.
C-
C-   Inputs  : LUNOUT      Unit number for output file
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-OCT-1988   Harrison B. Prosper, Elliott A. Treadwell
C-   Modified 14-NOV-1988   Harrison B. Prosper
C-   Modified  8-DEC-1988   Harrison B. Prosper
C-                          Modularize code a bit more
C-   Updated   8-DEC-1989   Harrison B. Prosper   
C-      Made compatible with new RCP
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL         ANGLE1,ANGLE2,THICK,DZ,DY
      INTEGER      I,J,K,L,M,N,NMAX,NSEC,NP,II,III,MEDIUM,NZ,LNAME
      INTEGER      NPT,LUNOUT,MXSEC,IER
C
      PARAMETER( NMAX = 500 )
      PARAMETER( MXSEC = 50 )
C
      CHARACTER*4  VNAME,SHAPE
      CHARACTER*32 STRING,NAME(MXSEC)
      REAL  Z(0:NMAX),Y(0:NMAX)
      REAL  Z1(0:NMAX),Z2(0:NMAX),Y1(0:NMAX),Y2(0:NMAX)
      REAL  RMIN(0:NMAX),RMAX(0:NMAX)
C----------------------------------------------------------------------
C
C ****  Get section names. Names are to be appended with either +Z or -Z
C
      CALL EZ_GET_CHARS ('CENTRAL_CRYOSTAT_VOLUMES',NSEC,NAME,IER)
C
C ****  SET UP DETECTOR GEOMETRY
C
      WRITE(LUNOUT,
     & FMT='(''!  '')')
      WRITE(LUNOUT,
     & FMT='(''!      GEANT GEOMETRY FOR CENTRAL CRYOSTAT '')')
      WRITE(LUNOUT,
     & FMT='(''!  '')')
C
C ****  LOOP OVER CRYOSTAT SECTIONS
C
      DO 300 II =  1,NSEC
C
C ****  Convert raw data to (Z,Y) points
C
        CALL CRYCNT(NAME(II),
     &    SHAPE,VNAME,MEDIUM,ANGLE1,ANGLE2,THICK,Z(1),Y(1),NPT)
C
C ****  Convert (Z,Y) points to PCON and TUBE parameters and write
C       out results
C
        CALL CRYOUT(LUNOUT,NAME(II),
     &    SHAPE,VNAME,MEDIUM,ANGLE1,ANGLE2,THICK,Z(0),Y(0),NPT)
C
  300 CONTINUE
C
  999 RETURN
      END
