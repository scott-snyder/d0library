      SUBROUTINE WRITE_STAT_HDR(PATH, VSN, DEVICE, NODE, PROJECT, 
     +                          FILENAME, IERR)
C  K.Denisenko 05-Aug-1992
C  To be called from Unix outspool process
C  Creates and writes the header part of the STAT.RCP file
C  The file name has a form of STAT_VSN.RCP, e.g. STAT_UU0061.RCP
C
C  > PATH   - points to the area where the stat.rcp file will reside
C             requires the last slash to pe passed
C  > VSN    - tape label to write to
C  > DEVICE - cps device name of the tape drive (truncated, e.g. sts01)
C  > NODE   - node name for the production node
C  < FILENAME STAT file name composed from the input; used to reopen the file
C  < IERR   - non zero Ierr indicates some problem
      Implicit None
      Character*(*) VSN, Device, Node, Project, Filename, Path
      Character*80 String, Date_Time
      Integer Pathlen, Vsnlen, Devlen, Nodlen, Prolen
      Integer Strilen, Datimlen
      Integer STR$UPCASE, LIB$DATE_TIME, Trulen
      Integer Iuser, Iunit, Ierr

C Calculate character lengths
      Ierr = STR$UPCASE(VSN, VSN)
      Ierr = STR$UPCASE(Device, Device)
      Ierr = STR$UPCASE(Node, Node)
      Ierr = STR$UPCASE(Project, Project)
      Pathlen = Trulen (Path)
      Vsnlen  = Trulen (VSN)
      Devlen  = Trulen (Device)
      Nodlen  = Trulen (Node)
      Prolen  = Trulen (Project)

C Construct a file name
      Filename = Path(1:Pathlen)//'STAT_'//VSN(1:VSNlen)
     +           //'.RCP'

C Open a file
      Iuser = 21 ! Random number; 21 is no better than anything else
      Call Gtunit(Iuser, Iunit, Ierr)
      If(Ierr.NE.0) Return
      Open(unit=Iunit,file=filename,status='unknown',iostat=Ierr)      
      If(Ierr.NE.0) then
        Call Rlunit(Iuser, Iunit, Ierr)
        IERR = -1
        Return
      Endif

C Create the header line \START
      String='\\START STAT_'//VSN(1:Vsnlen)//'_RCP'
      Strilen = Trulen(String)
      write(Iunit,*) String(1:Strilen)

C Create the size line
      String = '\\SIZE'
      Strilen = Trulen(String)
      write(Iunit,*) String(1:Strilen)

C Create the VSN lines
      String = " VISUAL_TAPE_LABEL   '"//VSN(1:Vsnlen)//"'"
      Strilen = Trulen(String)
      write(Iunit,*) String(1:Strilen)
      String = " INTERNAL_TAPE_LABEL   '"//VSN(1:Vsnlen)//"'"
      Strilen = Trulen(String)
      write(Iunit,*) String(1:Strilen)

C Create the date
      Ierr = LIB$DATE_TIME(Date_Time)
      Datimlen = Trulen (Date_Time)
      Ierr = STR$UPCASE (Date_Time, Date_Time)
      String = " CREATION_DATE   '"//Date_time(1:Datimlen)//"'"
      Strilen = Trulen(String)
      write(Iunit,*) String(1:Strilen)

C Create the nodename
      String = " NODE_NAME       '"//Node(1:Nodlen)//"'"
      Strilen = Trulen(String)
      write(Iunit,*) String(1:Strilen)

C Create the project name
      String = " PROJECT       '"//Project(1:Prolen)//"'"
      Strilen = Trulen(String)
      write(Iunit,*) String(1:Strilen)

C Create the tape_unit_name
      String = " TAPE_UNIT_NAME    '"//Device(1:Devlen)//"'"
      Strilen = Trulen(String)
      write(Iunit,*) String(1:Strilen)

C Create the auxiliary strings
      String = "DATA_FORMAT     '99'"
      Strilen = Trulen(String)
      write(Iunit,*) String(1:Strilen)
      String = "FILE_FORMAT     'FX'"
      Strilen = Trulen(String)
      write(Iunit,*) String(1:Strilen)
      String = '\\ARRAY FILENAMES'
      Strilen = Trulen(String)
      write(Iunit,*) String(1:Strilen)

C That is about all; close the file and exit gracefully
      Close(unit=Iunit)
      Call Rlunit(Iuser, Iunit, Ierr)

      Return
      End

      SUBROUTINE WRITE_STAT_SET(FILENAME, SETNAME, IERR)
C  K.Denisenko 05-Aug-1992
C  To be called from Unix outspool process
C  Adds a dataset name just written to tape
C  The file name has a form of STAT_VSN.RCP, e.g. STAT_UU0061.RCP
C
C  > Filename - name of the file created previously by WRITE_SET_HDR
C  > Setname  - name of the dataset being written to tape
C  < Ierr     - error status if non-zero

      Implicit None
      Character*(*) Filename, Setname
      Character*130 String
      Integer Ierr, Iuser, Iunit
      Integer STR$UPCASE, Trulen

C Upcase the setname (just in case...)
      Ierr = STR$UPCASE ( Setname, Setname )

C Open the file in append mode (filename includes path already)
      Iuser = 25
      Call Gtunit(Iuser, Iunit, Ierr)
      If(Ierr.NE.0) Return
C&IF IBMAIX
C&      Open(Unit=Iunit, File=Filename, Status='OLD',
C&     +     Iostat=Ierr)
C&ELSE
C&      Open(Unit=Iunit, File=Filename, Access='Append', Status='Old',
C&     +     Iostat=Ierr)
C&ENDIF
      If(ierr.NE.0) Then
        Call Rlunit(Iuser, Iunit, Ierr)
        Ierr = -1
        Return
      Endif

C Append the dataset name and close the file
      String = "   '"//Setname(1:Trulen(Setname))//"'"
      Write(Iunit,*) String (1:Trulen(String))

C Close the file
      Close(Iunit)
      Call Rlunit(Iuser, Iunit, Ierr)

      Return
      End

      SUBROUTINE WRITE_STAT_LSET(FILENAME, SETNAME, INVSN, IERR)
C  K.Denisenko 05-Aug-1992
C  To be called from Unix outspool process
C  Adds a dataset name just written to tape
C  The file name has a form of STAT_VSN.RCP, e.g. STAT_UU0061.RCP
C
C  > Filename - name of the file created previously by WRITE_SET_HDR
C  > Setname  - name of the dataset being written to tape
C  < Ierr     - error status if non-zero
      Implicit None
      Character*(*) Filename, Setname
      Character*(*) Invsn
      Character*130 String
      Integer Ierr, Iuser, Iunit
      Integer STR$UPCASE, Trulen

C Upcase the setname (just in case...)
      Ierr = STR$UPCASE ( Setname, Setname )
      Ierr = STR$UPCASE ( Invsn, Invsn )

C Open the file in  mode (filename includes path already)
      Iuser = 25
      Call Gtunit(Iuser, Iunit, Ierr)
      If(Ierr.NE.0) Return
C&IF IBMAIX
C&      Open(Unit=Iunit, File=Filename, Status='Old',
C&     +     Iostat=Ierr)
C&ELSE
C&      Open(Unit=Iunit, File=Filename, Access='Append', Status='Old',
C&     +     Iostat=Ierr)
C&ENDIF
      If(ierr.NE.0) Then
        Call Rlunit(Iuser, Iunit, Ierr)
        Ierr = -1
        Return
      Endif

C Append the dataset name and close the file
      String = "   '"//Setname(1:Trulen(Setname))//"'   "
     +//Invsn(1:Trulen(Invsn))
      Write(Iunit,*) String (1:Trulen(String))

C Close the file
      Close(Iunit)
      Call Rlunit(Iuser, Iunit, Ierr)

      Return
      End

      SUBROUTINE WRITE_STAT_END(FILENAME, IERR)
C  K.Denisenko 05-Aug-1992
C  To be called from Unix outspool process
C  Adds an \END and \STOP to the STAT RCP file when the tape is finished 
C  The file name has a form of STAT_VSN.RCP, e.g. STAT_UU0061.RCP
C
C  > Filename - name of the file created previously by WRITE_SET_HDR
C  < Ierr     - error status if non-zero

      Implicit None
      Character*(*) Filename
      Character*80  String
      Integer Ierr, Iuser, Iunit
      Integer Trulen

C Open the file in append mode (filename includes path already)
      Iuser = 25
      Call Gtunit(Iuser, Iunit, Ierr)
      If(Ierr.NE.0) Return
C&IF IBMAIX
C&      Open(Unit=Iunit, File=Filename, Status='Old',
C&     +      Iostat=Ierr)
C&ELSE
C&      Open(Unit=Iunit, File=Filename, Access='Append', Status='Old',
C&     +     Iostat=Ierr)
C&ENDIF
      If(Ierr.NE.0) Then
        Call Rlunit(Iuser, Iunit, Ierr)
        Ierr = -1
        Return
      Endif

C Append the strings and close the file
      String = '\\END'
      Write(Iunit,*) String (1:Trulen(String))
      String = '\\STOP'
      Write(Iunit,*) String (1:Trulen(String))


C Close the file
      Close(Iunit)
      Call Rlunit(Iuser, Iunit, Ierr)

      Return
      End
