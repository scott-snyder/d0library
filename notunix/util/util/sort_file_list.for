      PROGRAM SORT_FILE_LIST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sort file list
C-
C-   Inputs  : INLIST - logical for input file
C-   Outputs : OUTLIST - logical for output file
C-
C-   Created   3-OCT-1992   Ulrich Heintz
C-   Updated   3-MAY-1993   Harrison B. Prosper  
C-    Change name from SORT to SORT_FILE_LIST 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL swap/.true./
      INTEGER run,part,i,j,l,n,i1,i2,indx,ifile,nfile,version,nfil
      INTEGER ltmp,list(50000),vlist(50000),ldir,irun,jrun,ipart,jpart
      CHARACTER*80 line,file(50000),outfile,outname,directory
C
C... get user input      
      open(1,name='inlist',type='old',readonly)
      type *,'enter string to precede all file names (e.g. directory) '
      accept 1,directory
      ldir=index(directory,' ')
      type *,'enter name (without extension) of output files '
      accept 1,outfile
      if (outfile.ne.' ') then
        type *,'enter starting index '
        accept *,indx
        open(4,name='sort.log',type='new',carriagecontrol='list')
        type *,' number of files per list'
        accept *,nfil
      else
        type *,'using OUTLIST as output file'
        open(3,name='outlist',type='new')
      endif
C... read input file and make list of files
      do while(.true.)
        read(1,1,end=999)line
        if(line(1:1).eq.'*')goto 100
    1   format(a)
        i1=index(line,'Directory')
        if(i1.eq.1)goto 100
        i1=index(line,'_')
        if(i1.gt.0.and.line(i1+10:i1+10).eq.'.')then
          i=i+1
          decode(9,2,line(i1+1:i1+9))run,part
    2     format(i6,1x,i2)
          i2=index(line,';')
          file(i)=line(:i2-1)
          list(i)=(100*run+part)
          i2=index(line,'REU')
          if(i2.gt.0)then
            vlist(i)=vlist(i)+1
          else
            i2=index(line,'REC')
          endif
          if(i2.le.0)then
            type *,'no RECO version found: ',line
          else
            decode(4,3,line(i2+3:i2+6))version
    3       format(i4)
            vlist(i)=vlist(i)+10*version
          endif
        endif
  100 enddo
  999 continue
      nfile=i
C... now sort the file names so that list(j) increases as j increases
      do while(swap)
        swap=.false.
        do j=1,nfile-1
          if(list(j).gt.list(j+1).or.
     &      (list(j).eq.list(j+1).and.vlist(j).gt.vlist(j+1)))then
            ltmp=list(j)
            list(j)=list(j+1)
            list(j+1)=ltmp
            ltmp=vlist(j)
            vlist(j)=vlist(j+1)
            vlist(j+1)=ltmp
            line=file(j)
            file(j)=file(j+1)
            file(j+1)=line
            swap=.true.
          endif
        enddo
      enddo
C... look for missing partitions
D      open(99,type='new',carriagecontrol='list')
      do j=2,nfile
        jrun=list(j-1)/100
        irun=list(j)/100
        ipart=list(j)-irun*100
        if(irun.ne.jrun)then
D          write(99,*)jrun
          if(ipart.eq.2)then
            type 8,irun,1
    8       format(1x,'missing run',I6,' part ',I2)
          elseif(ipart.ne.1)then
            type 7,irun,1,ipart-1
    7       format(1x,'missing run',I6,' parts ',I2,' - ',I2)
          endif
        else
          jpart=list(j-1)-jrun*100
          if(ipart.eq.jpart+2)then
            type 8,irun,jpart+1
          elseif(ipart.gt.jpart+1)then
            type 7,irun,jpart+1,ipart-1
          endif
        endif
      enddo
D      close(99)
C... eliminate double run/part numbers and split files into groups of 80-180
      ifile=0
      do j=1,nfile-1
          if(list(j).eq.list(j+1))then
            n=n+1
            type *,file(j)
            type *,file(j+1)
            type *
          else ! only write last occurance of double run/parts 
            ifile=ifile+1
            if(ifile.eq.1)then
              if(outfile.ne.' ')then
                i1=index(outfile,' ')
                write(outname,4)outfile(:i1-1),indx
    4           format(a,i4,'.lst')
                indx=indx+1
                do i=1,i1+4
                  if(outname(i:i).eq.' ')outname(i:i)='0'
                enddo
                open(3,name=outname,type='new')
                write(4,1)outname
                write(4,5)list(j)/100,list(j)-(list(j)/100)*100
    5           format(5x,'first run ',I6,' part ',I2)
              endif
            endif
            i1=index(file(j),' ')
            write(3,1)directory(:ldir-1)//file(j)(:i1)
            if(ifile.ge.nfil.and.list(j+1)/100.ne.list(j)/100.and.
     &        ifile.ne.nfile-1)then
              if(outfile.ne.' ')then
                close(3)
                write(4,6)list(j)/100,list(j)-(list(j)/100)*100
    6           format(5x,'last  run ',I6,' part ',I2)
                ifile=0
              endif
            endif
          endif
      enddo
      i1=index(file(j),' ')
      write(3,1)directory(:ldir-1)//file(j)(:i1)
      if(outfile.ne.' ')write(4,5)list(j)/100,list(j)-(list(j)/100)*100
      close(1)
      close(3)
      close(4)
      type *,' sorted',nfile,' entries -',n,' are duplicates'
C----------------------------------------------------------------------
      END
