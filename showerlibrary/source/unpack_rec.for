      SUBROUTINE UNPACK_REC(RKEY,ICYCLE,LREC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : UNPACKS RKEY AND ICYCLE INTO A RECORD NUMBER
C-
C-   Inputs :  LREC = RECORD NUMBER IN RANDOM ACCESS FILE
C-   Outputs  : RKEY (*) = VECTOR OF KEYS
C-              ICYCLE   = CYCLE NUMBER
C-   Controls: 
C-
C-   Created  11-APR-1990   Rajendran Raja
C-   Updated  24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RKEY(*),ICYCLE,LREC
      INCLUDE 'D0$INC:SHLDAT.INC'
      INCLUDE 'D0$INC:SHLCON.INC'
      INTEGER BIT_BOUNDARY(3),LEN_BYTE(4)
      INTEGER MAX_CYCLE
      INTEGER IER
      INTEGER IDIV(6)
      INTEGER LRECS,LRECO
      INTEGER RKEY1
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET('MAXIMUM_CYCLES',MAX_CYCLE,IER)
        IDIV(1) = (MAX_CYCLE+1)
        IDIV(2) = NPHI_ID
        IDIV(3) = NPART_ID
        IDIV(4) = NMOM
        IDIV(5) = NETA
        IDIV(6) = NVRT
        CALL EZRSET
      ENDIF
C
C ****  NOW UNPACK BITS
C
      LRECS = LREC
C
      LRECO = LRECS/IDIV(1)
      ICYCLE = LRECS - LRECO*IDIV(1)
      LRECS = LRECO
C
      LRECO = LRECS/IDIV(2)
      RKEY(5) = LRECS - LRECO*IDIV(2) + 1
      LRECS = LRECO
C
      LRECO = LRECS/IDIV(3)
      RKEY(4) = LRECS - LRECO*IDIV(3) + 1
      LRECS = LRECO
C
      LRECO = LRECS/IDIV(4)
      RKEY(3) = LRECS - LRECO*IDIV(4) + 1
      LRECS = LRECO
C
      LRECO = LRECS/IDIV(5)
      RKEY(2) = LRECS - LRECO*IDIV(5) + 1
      LRECS = LRECO
C
      LRECO = LRECS/IDIV(6)
      RKEY(1) = LRECS - LRECO*IDIV(6) + 1
      LRECS = LRECO
C
  999 RETURN
      END
