      SUBROUTINE CRYSET (LUNSET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up cryostat detectors sets in correct
C-                         RCP format (see document SETDET.SCP).
C-
C-   Inputs  : LUNSET      Output unit number for SETS
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-NOV-1988   Harrison B. Prosper
C-   Updated   8-DEC-1989   Harrison B. Prosper  
C-      Made compatible with new RCP 
C-   Updated  17-Feb-1992   Herbert Greenlee
C-      Fixed bug
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER      I,J,K,L,M,N,II,JJ,KK,LL,NN,NUMSET
      INTEGER      LUNSET,IVOL,NBITSD,ND,NUM,IER
      INTEGER      NWHI,NWDI,ITRS,NHMAX,NSET,NV,NBITSV,NH,NBITSH
      REAL         ORIG,FACT
C
      INTEGER NMAX,MXVOL,MXSET
      PARAMETER( NMAX  = 50 )
      PARAMETER( MXVOL = 50 )
      PARAMETER( MXSET = 20 )
C
      CHARACTER*1  SIGN
      CHARACTER*4  VNAME,SNAME,HOLL
      CHARACTER*6  IUSET,IUDET(MXSET)
      CHARACTER*6  NAMESV,NAMESH,NAMESD
      CHARACTER*32 NAME(MXSET),VOLUME(0:MXVOL),STRING
      INTEGER      IDTYPE(MXSET)
      REAL    RAW(NMAX)
      INTEGER IRAW(NMAX)
      EQUIVALENCE ( RAW(1),IRAW(1) )
C----------------------------------------------------------------------
C
C ****  Get general SET data
C
      CALL EZGSET_i ('IUSET_GENERAL(1)',IRAW(1),1)
C
      NWHI  = IRAW(1)
      NWDI  = IRAW(2)
      ITRS  = IRAW(3)
      NHMAX = IRAW(4)
      NV    = IRAW(5)
      NBITSV= IRAW(6)
      NH    = IRAW(7)
      CALL UHTOC (IRAW(8),4,HOLL,4)
      NAMESH= ''''//HOLL//''''
      NBITSH= IRAW(9)
      ORIG  = RAW(10)
      FACT  = RAW(11)
      ND    = IRAW(12)
      CALL UHTOC (IRAW(13),4,HOLL,4)
      NAMESD= ''''//HOLL//''''
      NBITSD= IRAW(14)
C
C ****  Get cryostat set names
C
      CALL EZ_GET_CHARS ('CRYOSTAT_SETS',NUMSET,NAME,IER)
C
C ****  LOOP OVER SETS AND PRINT OUT DATA IN CORRECT RCP FORMAT
C
      WRITE(LUNSET,
     & FMT='(''!  '')')
      WRITE(LUNSET,
     & FMT='(''! GEANT SET DEFINITIONS FOR CRYOSTAT'')')
      WRITE(LUNSET,
     & FMT='(''!  '')')
C
      DO 400 II =  1,NUMSET
C
C ****  Get GEANT set name and volumes/set
C
        CALL EZ_GET_CHARS (NAME(II),NUM,VOLUME(0),IER)
C
        IUSET = ''''//VOLUME(0)(1:4)//''''      ! GEANT SET name
        NSET  = NUM - 1                         ! Number of volumes in set
C
        DO 100 I = 1, NSET
C
C ****  Check for +Z or -Z in VOLUME name
C
          CALL WORD (VOLUME(I),JJ,KK,NN)
          IF ( VOLUME(I)(NN:NN) .EQ. 'Z' ) THEN
            SIGN = VOLUME(I)(NN-1:NN-1)
            LL = NN-2
          ELSE
            SIGN = ' '
            LL = NN
          ENDIF
C
C ****  Get GEANT volume name from volume descriptor.
C ****  This is the 8th word in the array.
C
          CALL EZGETC (VOLUME(I)(1:LL),8,4,VNAME,IER) ! Get GEANT volume name
          IF ( SIGN .NE. ' ' ) THEN
            VNAME = VNAME(1:2)//SIGN//VNAME(4:4)
          ENDIF
          IUDET(I) = ''''//VNAME//''''
C
C ****  Get GEANT volume IDTYPE
C
          CALL EZGSET (VOLUME(I)(1:LL)//'(12:12)',IVOL,1) ! Get one word only
          IDTYPE(I) = IVOL
  100   CONTINUE
C
        WRITE(LUNSET,FMT='('' \ARRAY '',A32)') NAME(II)
        WRITE(LUNSET,310) IUSET,NWHI,NWDI,ITRS,NHMAX,NSET
  310   FORMAT(A7,4(1X,I4),/,1X,I4)
        DO 345 K =  1,NSET
          WRITE(LUNSET,340) IUDET(K),IDTYPE(K),NV,IUDET(K),
     &                      NBITSV,NH,NAMESH,
     &                      NBITSH,ORIG,FACT,ND,NAMESD,NBITSD
  340     FORMAT(A7,1X,I4,1X,I3,A7,2(1X,I3),A7,1X,I3,
     &           2(1X,F7.1),1X,I3,A7,1X,I3)
  345   CONTINUE
        WRITE(LUNSET,FMT='('' \END'')')
  400 CONTINUE
C
  999 RETURN
      END
