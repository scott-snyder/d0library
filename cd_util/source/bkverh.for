      SUBROUTINE BKVERH(LVERH)
C-----------------------------------------------------------------------
C  Subroutine BKVERH books bank VERH  (header for vertex banks)
C
C  Output:
C    LVERH       location of the booked bank in ZEBCOM.
C
C  Daria Zieminska Nov 1988
C-   Updated  02-APR-1990   Qizhong Li-Demarteau   not fix on 'RECO' path
C-   Updated  19-NOV-1993   Srini Rajagopalan   Increase Number of structural
C-                          links from 1 to 2 to include VFIT bank;
C-                          Total number of links increased from 2 to 3.
C-                          (Version = 1)
C-   Updated  30-NOV-1993   Qizhong Li-Demarteau  fill bank version # and
C-                                                ref. link to HSTR bank
C-   Updated   6-FEB-1995   Ed Oltman   Change version to #2 (see VERH.ZEB)
C-                          Fill Beam_pos and Beam_err
C-   Updated   5-SEP-1995   Srini Rajagopalan   Add word 11, 
C-                          Change to version #3
C-   Note: Version#2 was never released resulting in VERH bank being booked as
C-   as integer bank. Therefore words 3-10 in Version=2 banks are garbage.
C-   Updated  26-SEP-1995   Srini Rajagopalan  Add word to store old z. 
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZVERH.LINK/LIST'
      LOGICAL First
      INTEGER LPROC, GZPROC, LVERH, GZHSTR, IXVERH
      DATA First/.TRUE./
C
      IF(First) THEN
        First = .FALSE.
        CALL MZFORM('VERH', '3I 8F 1I', IXVERH)
      ENDIF
      LPROC = GZPROC()
      IF (LPROC .LE. 0) CALL BKPROC(LPROC)
      LVERH = LQ(LPROC - IZVERH)
      IF ( LVERH .EQ. 0 ) THEN
C
C  1 structural link to VERT (linear structure of vertex banks)
C  1 structural link to VFIT (linear structure of fitted vertex banks)
C  1 reference link to HSTR
C  10 words
C
        CALL MZBOOK(IXMAIN,LVERH,LPROC,-IZVERH,'VERH',3,2,12,IXVERH,0)
        LQ(LVERH - 3) = GZHSTR()        ! Reference Link to latest History
        IQ(LVERH + 1) = 3   ! version number
      ENDIF
C
      RETURN
      END
