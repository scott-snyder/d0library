      SUBROUTINE PACK_REC(RKEY,ICYCLE,LREC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PACKS RKEY AND ICYCLE INTO A RECORD NUMBER
C-
C-   Inputs  : RKEY (*) = VECTOR OF KEYS
C-             ICYCLE   = CYCLE NUMBER
C-   Outputs : LREC = RECORD NUMBER IN RANDOM ACCESS FILE
C-   Controls: 
C-
C-   Created  11-APR-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RKEY(*),ICYCLE,LREC
      INCLUDE 'D0$INC:SHLDAT.INC'
      INCLUDE 'D0$INC:SHLCON.INC'
      INTEGER MAX_CYCLE
      LOGICAL FIRST
      INTEGER IER
      DATA FIRST/.TRUE./
      INTEGER IMULT(6)
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET('MAXIMUM_CYCLES',MAX_CYCLE,IER)
        CALL EZRSET
        IMULT(1) = 1
        IMULT(2) = (MAX_CYCLE+1)*IMULT(1)
        IMULT(3) = NPHI_ID*IMULT(2)
        IMULT(4) = NPART_ID*IMULT(3)
        IMULT(5) = NMOM*IMULT(4)
        IMULT(6) = NETA*IMULT(5)
      ENDIF
C
C ****  NOW PACK BITS
C
C
      IF ( RKEY(1) . GT. NVRT.OR. RKEY(1).LE.0 ) THEN
        CALL ERRMSG('SHOWERLIBRARY','PACK_REC',
     &    'KEY(1) OUT OF RANGE ','W')
        RKEY(1) = NVRT
      ENDIF
C
      IF ( RKEY(2). GT. NETA.OR. RKEY(2).LE.0 ) THEN
        CALL ERRMSG('SHOWERLIBRARY','PACK_REC',
     &    'KEY(2) OUT OF RANGE ','W')
        RKEY(2) = NETA
      ENDIF
C
      IF ( RKEY(3). GT. NMOM.OR. RKEY(3).LE.0 ) THEN
        CALL ERRMSG('SHOWERLIBRARY','PACK_REC',
     &    'KEY(3) OUT OF RANGE ','W')
        RKEY(3) = NMOM
      ENDIF
C
      IF ( RKEY(4). GT. NPART_ID.OR. RKEY(4).LE.0 ) THEN
        CALL ERRMSG('SHOWERLIBRARY','PACK_REC',
     &    'KEY(4) OUT OF RANGE ','W')
        RKEY(4) = NPART_ID
      ENDIF
C
      IF ( RKEY(5). GT. NPHI_ID.OR. RKEY(5).LE.0 ) THEN
        CALL ERRMSG('SHOWERLIBRARY','PACK_REC',
     &    'KEY(5) OUT OF RANGE ','W')
        RKEY(5) = NPHI_ID
      ENDIF
C
      IF ( ICYCLE.GT.MAX_CYCLE ) THEN
        CALL ERRMSG('SHOWERLIBRARY','PACK_REC',
     &    'CYCLE NUMBER TOO LARGE FOR PACKING ','W')
        ICYCLE = MAX_CYCLE
      ENDIF
C
      LREC = ICYCLE + IMULT(2)*(RKEY(5)-1) + IMULT(3)*(RKEY(4)-1) +
     &  IMULT(4)*(RKEY(3)-1) + IMULT(5)*(RKEY(2)-1) + 
     &  IMULT(6)*(RKEY(1)-1) 
C
  999 RETURN
      END
