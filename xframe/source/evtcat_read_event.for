      SUBROUTINE EVTCAT_READ_EVENT(IRUN,IEVENT,IER)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: , not a D0DADF file.   The event catalog 
C-     to use is chosen based on run number using the following 
C-     logical names:
C-
C-          DataSet    Catalog Name
C-          -------    -------------
C-           Run1A     RUN1A$CATALOG     (Unix: $run1a_catalog)
C-           Run1A     RUN1B$CATALOG
C-           Other     PRIVATE$CATALOG
C-
C-
C-   Inputs  : IRUN   - Run number
C-             IEVENT - Event number
C-   Outputs : IER    - 0 ==> No errors
C-   Controls:
C-
C-   Created  21-May-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'd0$xframe$source:d0map.inc'
c
      INTEGER IRUN,IEVENT,IER
      CHARACTER*(*) RUN1A,RUN1B,PERSONAL
      CHARACTER*128 NEWCATALOG
C&IF VAXVMS
      PARAMETER(RUN1A='D0$D0DAD$CATALOGS:RUN1A_MDS.EVTCAT',
     >          RUN1B='D0$D0DAD$CATALOGS:RUN1B_MDS.EVTCAT',
     >          PERSONAL='PERSONAL$CATALOG')
C&ELSE
C&      PARAMETER(RUN1A='$d0dad__catalogs/run1a_mds.evtcat',
C&     >          RUN1B='$d0dad__catalogs/run1b_mds.evtcat',
C&     >          PERSONAL='$personal_catalog')
C&ENDIF
C-----------------------------------------------------------------------
C
C   Use the run number to key the catalog unless otherwise specified
C
      if (d0dadrdc) then
        IF( IRUN.GT.50002 .AND. IRUN.LE.65981 ) THEN
          NEWCATALOG=RUN1A
        ELSEIF( IRUN.GT.65981 ) THEN
          NEWCATALOG=RUN1B
        ELSE
          NEWCATALOG=PERSONAL
        ENDIF
      else
        newcatalog = d0dadcat
      endif
C
      CALL D0DAD_ECREAD(NEWCATALOG,IRUN,IEVENT,IER)
C
  999 RETURN
      END
