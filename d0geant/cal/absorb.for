      SUBROUTINE ABSORB(X,NUMEDX,TOTABS,TOTRAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Routine calculates total
C-                          absorption length along a vector
C-                          beginning at x(1-3) and
C-                          in the direction cosines x(4-6)
C
C-
C-   Inputs  : X(1-3) Beginning co-ordinate of vector with
C-             direction cosines X(4-6)
C-   Outputs : NUMEDX = Medium number of last volume
C-             TOTABS = Total numer of absorption lengths
C-             TOTRAD = Total numer of radiation lengths
C-   Controls: None
C-
C-   Created  1985          Rajendran Raja
C-   Updated  13-SEP-1988   Rajendran Raja
C-   Updated  18-FEB-1991   John Womersley  added radiation lengths, some 
C-                              minor bug fixing 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:GCTMED.INC/LIST'
      INCLUDE 'D0$INC:ABSLN.INC/LIST'
C
C
      REAL X(6),UBUF(100)
      REAL DENS,RADL,ABSL,SNEXT,SAFETY,DELABS,TOTABS,TLEN
      REAL    EPSABS,TOTRAD
C
      INTEGER IPRABS,ITMED,NMAT,I,ISAME,K
      INTEGER NWBUF,NAMATE(5)
      REAL A,Z
      INTEGER NUMEDX
C
C----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST = .FALSE.
        EPSABS = 0.01
        IPRABS = DLAM-1
      ENDIF
C
    4 CONTINUE
      CALL GMEDIA(X,NUMED)
      IF(NUMED.EQ.0)GO TO 5
      IF(IPRABS.NE.0)WRITE(LOUT,1)(X(K),K=1,3),NUMED
    1 FORMAT(' X(1-3),MEDIUM NUMBER ',3F15.5,I5)
      ITMED=NUMED
      NUMEDX=NUMED                      ! for output

      CALL GFTMED(ITMED,NATMED,NMAT,ISVOL,IFIELD,FIELDM,
     +              TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,UBUF,NWBUF)
      IF(STMIN.EQ.0.)THEN
        write(6,*)' Warning: Material',ITMED,
     &    ' has zero minimum step length!'
        STMIN=1.0e-03
      ENDIF
      CALL GFMATE(NMAT,NAMATE,A,Z,DENS,RADL,ABSL,UBUF,NWBUF)
      IF(ABSL.EQ.0.)THEN
        write(6,*)' Warning: Material',ITMED,
     &    ' has zero absorption length!'
        ABSL= 1.0e15
      ENDIF
      IF(RADL.EQ.0.)THEN
        write(6,*)' Warning: Material',ITMED,
     &    ' has zero radiation length!'
        RADL=1.0e15
      ENDIF

      IF(IPRABS.NE.0)WRITE(LOUT,6)NUMED,NATMED,NMAT,ISVOL,IFIELD,
     +               FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN
    6 FORMAT(' MEDIA PARS ',I5,5A4,2X,3I5,/,2X,6E12.3)
      IF(IPRABS.NE.0)WRITE(LOUT,7)NMAT,NAMATE,A,Z,DENS,RADL,ABSL
    7 FORMAT(' MATER. PARS ',I5,5A4,2X,/,2X,5E12.3)
C
   31 CONTINUE
      CALL GNEXT(X,SNEXT,SAFETY)
      IF(SAFETY.EQ.0.)SAFETY=1.0E-09       ! stop underflows
      IF(SNEXT.LT.EPSABS)SNEXT=EPSABS      !GRAZING INCIDENCES AVOIDED
      SNEXT=SNEXT+EPSABS
      DO 3 I=1,3
        X(I)=X(I)+X(I+3)*SNEXT
    3 CONTINUE
      DELABS=SNEXT/ABSL
      TOTABS=TOTABS+DELABS
      TOTRAD=TOTRAD+DELABS*ABSL/RADL
      IF(IPRABS.NE.0)
     +  WRITE(LOUT,2)SNEXT,SAFETY,DELABS,TOTABS,TOTRAD,NUMED
    2 FORMAT(' SNEXT,SAFETY,DELABS,TOTABS,TOTRAD,NUMED ',
     +           5E10.3,I5)
      GO TO 4
    5 CONTINUE
      RETURN
      END
