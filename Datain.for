      SUBROUTINE TITPAG(LUSTIN,LUSTOT)
C
C  Routine to print the title page on the screen
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**09/01/91
C
      INTEGER LUSTIN,LUSTOT,I,J
      CHARACTER*10 A(13),D(13),E(13),F(13),R(13),X(13) 
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTIN  Logical unit for standard input
C  LUSTOT  Logical unit for standard output
C
C**** Local variables
C  I       Loop counter
C  J       Loop counter
C
C**** Local Arrays
C  A       Character array to hold the *'s for the letter A
C  D       Character array to hold the *'s for the letter D
C  E       Character array to hold the *'s for the letter E
C  F       Character array to hold the *'s for the letter F
C  R       Character array to hold the *'s for the letter R
C  X       Character array to hold the *'s for the letter X
C
C**** Functions and Subroutines
C  CLEAR   Clear the screen
C  PRSRTC  Display message 'Press return to continue'
C
C******************************************************************************

      data  a/'   AAAA   ','   AAAA   ',
     1        '  AA  AA  ','  AA  AA  ',
     2        ' AA    AA ',' AA    AA ',
     3        ' AA    AA ',
     4        ' AAAAAAAA ',' AAAAAAAA ',
     5        ' AA    AA ',' AA    AA ',
     6        ' AA    AA ',' AA    AA '/
      data  d/' DDDDDD   ',' DDDDDD   ',
     1        ' DD  DD   ',' DD   DD  ',
     2        ' DD   DD  ',' DD    DD ',
     3        ' DD    DD ',' DD    DD ',
     4        ' DD   DD  ',' DD   DD  ',
     5        ' DD  DD   ',' DDDDDD   ',
     6        ' DDDDDD   '/
      data  e/' EEEEEEE  ',' EEEEEEE  ',
     1        ' EE       ',' EE       ',
     2        ' EE       ',' EEEE     ',
     3        ' EEEE     ',' EEEE     ',
     4        ' EE       ',' EE       ',
     5        ' EE       ',' EEEEEEE  ',
     6        ' EEEEEEE  '/
      data  f/' FFFFFFFF ',' FFFFFFFF ',
     1        ' FF       ',' FF       ',
     2        ' FF       ',' FF       ',
     3        ' FFFFF    ',' FFFFF    ',
     4        ' FF       ',' FF       ',
     5        ' FF       ',' FF       ',
     6        ' FF       '/
      data  r/' RRRRR    ',' RRRRRRR  ',
     1        ' RR   RR  ',' RR    RR ',
     2        ' RR    RR ',' RR   RR  ',
     3        ' RRRRRRR  ',' RRRRR    ',
     4        ' RRRR     ',' RR RR    ',
     5        ' RR  RR   ',' RR   RR  ',
     6        ' RR    RR '/
      data  x/'XX     XX ','XX     XX ',
     1        ' XX   XX  ',' XX   XX  ',
     2        '  XX XX   ','  XXXXX   ',
     3        '   XXX    ',
     4        '  XXXXX   ','  XX XX   ',
     5        ' XX   XX  ',' XX   XX  ',
     6        'XX     XX ','XX     XX '/

C**** Clear the screen then print large RADEXF
      CALL CLEAR(LUSTOT)
      DO 20 I=1,13
         WRITE (LUSTOT,15) R(I),A(I),D(I),E(I),X(I),F(I)
15       FORMAT (10X,6(A10))
20    CONTINUE
      WRITE (LUSTOT,30)
30    FORMAT (//11X,'A program to calculate RADiative EXchange Factors'
     1        //11X,'DUNCAN LAWSON Maths Dept. Coventry Polytechnic',
     2        ' COVENTRY'/
     3        25X,'Tel: 0203 838975 or 0203 631313'//)

C**** Give press return to continue message
      CALL PRSRTC(LUSTIN,LUSTOT)

      RETURN
      END


      SUBROUTINE CLEAR(LUSTOT)
C
C  Routine to clear the screen - At present this is achieved by 
C  print 30 blank lines - it would be better if the correct control
C  sequence to clear the screen and home the cursor was sent, but 
C  this may differ from terminal to terminal
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**29/11/90
C
C
      INTEGER LUSTOT
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTOT  Logical unit for standard output (the screen)
C
C******************************************************************************

      CALL BLANK(LUSTOT,30)

      RETURN
      END

      SUBROUTINE PRSRTC(LUSTIN,LUSTOT)
C
C  Routine to print the message "PRESS RETURN TO CONTINUE" on the
C  screen and suspend further running until return is pressed
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**29/11/90
C
      INTEGER LUSTIN,LUSTOT
      CHARACTER*1 WAIT
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTIN  Logical unit for standard input (the screen)
C  LUSTOT  Logical unit for standard output (the screen)
C
C**** Local variables
C  WAIT    Character variable to receive the return
C
C******************************************************************************

      WRITE (LUSTOT,10)
10    FORMAT (20X,'<< PRESS RETURN TO CONTINUE >> ',$)
      READ (LUSTIN,20) WAIT
20    FORMAT (A1)

      RETURN
      END

      SUBROUTINE BLANK(LU,N)
C
C  Routine to write N blank lines on logical unit LU
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**30/03/90
C
      INTEGER LU,N
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LU      Logical unit number where blanks are to be written
C  N       Number of blank lines to be written
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  NONE
C
C**** Common variables - output
C  NONE
C  
C**** Local variables
C  I       Loop counter on number of lines
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

      DO 10 I=1,N
         WRITE (LU,5)
5        FORMAT (1X)
10    CONTINUE

      RETURN
      END

      SUBROUTINE PARVAL(LUSTIN,LUSTOT,NVMAX,NZMAX,NOBMAX,MXWLCL,MXELEM,
     1                  MXELMV,MXZN,MXCP,MXVZN,MXVZNC,NATMAX)
C
C  Routine to display the value of the parameters set at the beginning
C  of COMVAR.F and used throughout the program
C  
C*****VERSION 1.2********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**04/12/90
C  04/12/90  Inclusion of gas zone parameters
C
C
      INTEGER LUSTIN,LUSTOT,NVMAX,NZMAX,NOBMAX,MXWLCL,MXELEM,MXELMV,
     1        MXZN,MXCP,MXVZN,MXVZNC,NATMAX
      CHARACTER*1 REPLY
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTIN  Logical unit number of standard input channel
C  LUSTOT  Logical unit number of standard output channel
C  MXCP    Maximum number of components a coarse surface zone is allowed to have
C  MXELEM  Maximum number of surface elements (fine grid zones) allowed
C  MXELMV  Maximum number of volume elements (fine grid cells) allowed
C  MXVZN   Maximum number of coarse volume zones allowed
C  MXVZNC  Maximum number of components (fine grid cells) a coarse volume zone
C          is allowed to have 
C  MXWLCL  Maximum number of wall segments allowed in one cell
C  MXZN    Maximum number of coarse surface zones allowed
C  NATMAX  Maximum number of atmospheres allowed
C  NOBMAX  Maximum number of obstacles allowed
C  NVMAX   One more than the maximum number of vertices allowed
C  NZMAX   One more than the maximum number of slices allowed
C
C**** Arguments - output
C  NONE
C
C**** Local Variables
C  REPLY   Character variable indicating whether or not to continue
C
C**** Functions and Subroutines
C  BLANK   Print blank lines on the terminal screen
C  CLEAR   Clear the screen
C  PRSRTC  Display the message 'Press return to continue'
C
C******************************************************************************

      CALL CLEAR(LUSTOT)

      WRITE (LUSTOT,50) 
50    FORMAT (' VALUES OF THE GLOBAL PARAMETERS'/
     1        ' ==============================='/
     2        ' RADEXF requires certain arrays to be',
     3        ' dimensioned at compilation.'/
     4        ' If these arrays are not big enough for the',
     5        ' geometry the run will fail.'/
     6        ' Check the values given below and if any are too',
     7        ' small, then abort the run'/
     8        ' change the appropriate parameters in COMVAR.F and',
     9        ' re-compile RADEXF'/)


      WRITE (LUSTOT,100) NVMAX-1
100   FORMAT (' The maximum number of vertices allowed (NVMAX-1) is '
     1        ,I3)
      WRITE (LUSTOT,200) NZMAX-1
200   FORMAT (' The maximum number of slices allowed (NZMAX-1) is ',
     1        I3)
      WRITE (LUSTOT,300) NOBMAX
300   FORMAT (' The maximum number of obstacles (NOBMAX) allowed is ',
     1        I2)
      WRITE (LUSTOT,400) MXWLCL
400   FORMAT (' The maximum number of walls (MXWLCL) allowed in a cell',
     1        ' is ',I2)
      WRITE (LUSTOT,500) MXELEM,MXELMV,MXZN,MXVZN
500   FORMAT (' The maximum number of fine surface zones (MXELEM)',
     1        ' allowed is ',I3/
     2        ' The maximum number of fine volume zones (MXELMV) ',
     3        ' allowed is ',I3/
     4        ' The maximum number of coarse surface zones (MXZN)',
     5        ' allowed is ',I3/
     6        ' The maximum number of coarse volume zones (MXVZN)',
     7        ' allowed is ',I3)
      WRITE (LUSTOT,600) MXCP,MXVZNC
600   FORMAT (' The maximum number of fine surface zones in a',
     1        ' coarse surface zone (MXCP) is ',I3/
     2        ' The maximum number of fine volume zones in a',
     3        ' coarse volume zone (MXVZNC) is ',I3)
      WRITE (LUSTOT,620) NATMAX
620   FORMAT (' The maximum number of atmopsheres allowed (NATMAX) is',
     1        I3/)
   
650   WRITE (LUSTOT,700)
700   FORMAT (' If all these values are acceptable then enter Y',
     1        ' otherwise enter N'/
     2        ' (After N, RADEXF will stop - you',
     3        ' should change the required parameters'/
     4        ' in comvar.f and recompile RADEXF and run again)'//
     5        ' ARE ALL THE VALUES SATISFACTORY (Y or N) >> ',$)
      READ (LUSTIN,800) REPLY
800   FORMAT (A1)
      IF ((REPLY.EQ.'Y').OR.(REPLY.EQ.'y')) THEN
         CONTINUE
      ELSE IF ((REPLY.EQ.'N').OR.(REPLY.EQ.'n')) THEN
         WRITE (LUSTOT,900)
900      FORMAT (' RUN ABORTED TO ALLOW CHANGE OF PARAMETERS')
         STOP
      ELSE
         WRITE (LUSTOT,1000)
1000     FORMAT (' INVALID RESPONSE')
         GOTO 650
      END IF

      RETURN
      END

      SUBROUTINE OLDRUN(LUSTIN,LUSTOT,TITLE,IEXFCT,INDATA,TOTRAY,RAYDEF)
C
C  Routine to determine the run details (title, exchange factor type
C  and mode of data entry)
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**12/04/91
C
C
      INTEGER LUSTIN,LUSTOT,IEXFCT,INDATA,TOTRAY
      CHARACTER*50 TITLE
      LOGICAL RAYDEF
      CHARACTER*1 YORN
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTIN  Logical unit number for standard input
C  LUSTOT  Logical unit number for standard output
C  TOTRAY  Default value for total number of rays to be fired
C
C**** Arguments - output
C  IEXFCT  Exchange factor type (1-view factor, 2-DEA, 3-TEA, 4-all)
C  INDATA  Input mode (1-Interactive, 2-Quasi-Interactive, 3-Non-Interactive)
C  RAYDEF  Logical flag indicating if default ray density option is to be used
C  TITLE   Title for the run
C
C**** Local variables
C  YORN    Character variable with value Y (yes) or N (no)
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C**** Determine the title
      CALL CLEAR(LUSTOT)
      WRITE (LUSTOT,10)
10    FORMAT (' Enter a title for this run of RADEXF (up to',
     1        ' 50 characters) >> ')
      READ (LUSTIN,20) TITLE
20    FORMAT (A50)
25    WRITE (LUSTOT,30)
30    FORMAT (//' There are three types of exchange factors'/
     1        '      1. View Factors'/
     2        '      2. Direct Exchange Areas'/
     3        '      3. Total Exchange Areas'/
     4        ' RADEXF will calculate any or all of these'/
     5        ' Enter number of those you require (1-4) [4=All] >> ',$)
      READ (LUSTIN,*) IEXFCT
      IF ((IEXFCT.LT.1).OR.(IEXFCT.GT.4)) THEN
         WRITE (LUSTOT,40)
40       FORMAT (/' INVALID RESPONSE '/' Value must be in range 1-4'/)
         GOTO 25
      END IF
45    WRITE (LUSTOT,50)
50    FORMAT (//' RADEXF may be run in one of three modes:'/
     1        '     1.  TOTALLY INTERACTIVE - all data is entered at',
     2        ' the terminal'/
     3        '     2.  QUASI-INTERACTIVE - some data from files,',
     4        ' some from the terminal'/
     5        '         (Geometric data must be from a file)'/
     6        '     3.  NON-INTERACTIVE - all data from files'//
     7        ' Select the mode (1-3) required for this run >> ',$)
      READ (LUSTIN,*) INDATA
      IF ((INDATA.LT.1).OR.(INDATA.GT.3)) THEN
         WRITE (LUSTOT,55)
55	  FORMAT (/' INVALID RESPONSE'/' Value must be in range 1-3'/)
         GOTO 45
      END IF

C**** Determine if default ray density option is required
60    WRITE (LUSTOT,70) TOTRAY
70    FORMAT (//' Exchange factors are calculated using a Monte',
     1        ' Carlo method'/
     2        ' This involves tracing rays fired from each zone'/
     3        ' The number of rays to be fired can be varied'/
     4        ' The default option is to fire ',I5,' rays in total',
     5        ' from all the surfaces'/
     6        ' Do you want the default option (Y/N)? >> ',$)
      READ (LUSTIN,80) YORN
80    FORMAT (A1)
      IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) THEN
            RAYDEF=.TRUE.
      ELSE IF ((YORN.EQ.'N').OR.(YORN.EQ.'n')) THEN
            RAYDEF=.FALSE.
      ELSE
         WRITE (LUSTOT,90)
90	  FORMAT (/' INVALID RESPONSE'/' Answer Y or N'/)
         GOTO 60
      END IF
      
      RETURN
      END


      SUBROUTINE OLDOPE(LUSTIN,LUSTOT,LUNIT,FILE,ITYPE,NEWOLD)
C
C  Routine to open the file FILE on logical unit LUNIT
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**11/04/91
C
      INTEGER LUSTIN,LUSTOT,LUNIT,ITYPE,NEWOLD
      CHARACTER FILE*30,REPLY*1
      LOGICAL PROMPT
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  ITYPE   Flag indicating which type the file is, 1=geometric data,
C          2=absorptivity data, 3=emissivity data, 5=exchange factor data
C          6=run data
C  LUNIT   Logical unit on which file is to be opened
C  LUSTIN  Logical unit for standard input
C  LUSTOT  Logical unit for standard output
C  NEWOLD  Flag showing if the file is new or already exists (0=old,1=new)
C
C**** Arguments - output
C  FILE    The name of the file
C
C**** Local Variables
C  IERROR  The value of the IOSTAT flag returned by the open command
C  PROMPT  Logical flag indicating if a prompt should be given for 
C          filename when ITYPE=5 or ITYPE=6 
C
C******************************************************************************

C**** Initialise the PROMPT flag for ITYPE=5 or 6
      PROMPT=.FALSE.
      
5     CONTINUE
      IF (NEWOLD.EQ.1) THEN
C****    New file
         IF (ITYPE.EQ.1) THEN
            WRITE (LUSTOT,10)
10          FORMAT (/' Enter a name for the GEOMETRIC data file >> ',$)
         ELSE IF (ITYPE.EQ.2) THEN
            WRITE (LUSTOT,20)
20          FORMAT (/' Enter a name for the GAS ABSORPTIVITY data',
     1               ' file >> ',$)
         ELSE IF (ITYPE.EQ.3) THEN
            WRITE (LUSTOT,30)
30          FORMAT (/' Enter a name for the SURFACE EMISSIVITY data',
     1               ' file >> ',$)
         ELSE IF ((ITYPE.EQ.5).AND.(PROMPT)) THEN
            WRITE (LUSTOT,40)
40          FORMAT (/' Enter a name for the EXCHANGE FACTOR output',
     1               ' file >> ',$)
         ELSE IF ((ITYPE.EQ.6).AND.(PROMPT)) THEN
            WRITE (LUSTOT,50)
50          FORMAT (/' Enter a name for the RUN DATA output file >> ',$)
         END IF
         IF ((ITYPE.LT.5).OR.(PROMPT)) READ (LUSTIN,60) FILE
60       FORMAT (A30)
         OPEN (UNIT=LUNIT,FILE=FILE,STATUS='NEW',IOSTAT=IERROR)

      ELSE
C****    The file already exists
         IF (ITYPE.EQ.1) THEN
            WRITE (LUSTOT,70)
70          FORMAT (/' Enter the name of the GEOMETRIC data file >> ',$)
         ELSE IF (ITYPE.EQ.2) THEN
            WRITE (LUSTOT,80)
80          FORMAT (/' Enter the name of the GAS ABSORPTIVITY data',
     1               ' file >> ',$)
         ELSE IF (ITYPE.EQ.3) THEN
            WRITE (LUSTOT,90)
90          FORMAT (/' Enter the name of the SURFACE EMISSIVITY data',
     1               ' file >> ',$)
         END IF
         READ (LUSTIN,100) FILE
100      FORMAT (A30)
         OPEN(UNIT=LUNIT,FILE=FILE,STATUS='OLD',IOSTAT=IERROR)
      END IF

C**** Check that file opened OK
      IF (IERROR.NE.0) THEN
C****    Set PROMPT flag to true for ITYPE=5 or 6
         PROMPT=.TRUE.
         WRITE (LUSTOT,110) IERROR
110      FORMAT (' ERROR opening this file'/' IOSTAT is ',I4)
120      WRITE (LUSTOT,130) 
130      FORMAT (' You can abort the run or try another file'/
     2           ' Do you wish to try another file (Y/N)? >> ',$)
         READ (LUSTIN,140) REPLY
140      FORMAT (A1)
         IF ((REPLY.EQ.'Y').OR.(REPLY.EQ.'y')) THEN
            GOTO 5
         ELSE IF ((REPLY.EQ.'N').OR.(REPLY.EQ.'n')) THEN
            WRITE (LUSTOT,150)
150         FORMAT (/' RUN ABORTED'/)
            STOP
         ELSE
            WRITE (LUSTOT,160)
160         FORMAT (/' INVALID RESPONSE'/' The answer must be Y or N'/)
            GOTO 120
         END IF
      END IF

      RETURN
      END


      SUBROUTINE GEOMIN(INDATA,LUSTIN,LUSTOT,LU,NVMAX,NZMAX,NOBMAX,
     1                  NV,VX,VY,COARSE,NVXFS,NVXFE,NVXRS,NVXRE,NGZ,
     2                  GZ,COARSZ,NOB,TYPE,GLO,GHI,SINK,FILE1,FILE2)
C
C  Routine to determine all the geometric data required
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**17/04/91
C
C
      PARAMETER (NROW=19)
      INTEGER INDATA,LUSTIN,LUSTOT,LU,NVMAX,NZMAX,NOBMAX,NV,NVXFS,
     1        NVXFE,NVXRS,NVXRE,NGZ,NOB
      REAL VX(NVMAX),VY(NVMAX),GZ(NZMAX),GLO(NOBMAX,3),GHI(NOBMAX,3)
      LOGICAL COARSE(NVMAX),COARSZ(NZMAX),SINK(NOBMAX),
     1        CHANGE,CHANG1,RESTRT
      CHARACTER*1 TYPE(NOBMAX),YORN
      CHARACTER*30 FILE1,FILE2
      CHARACTER*61 APIC(NROW)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  INDATA  Integer flag giving mode of data input (1-interactive,
C          2-quasi-interactive, 3-non-interactive)
C  LU      Logical unit for reading from or writing to
C  LUSTIN  Logical unit number for standard input
C  LUSTOT  Logical unit number for standard output
C  NOBMAX  Maximum number of obstacles allowed
C  NVMAX   Maximum number of vertices allowed
C  NZMAX   Maximum number of z grid lines allowed
C
C**** Arguments - output
C  COARSE  Array of logical flags showing which vertices are on coarse grid
C  COARSZ  Array of logical flags showing which z grid lines are on coarse grid
C  FILE1   Name of file read from
C  FILE2   Name of file written to
C  GHI     Array of obstacle high co-ordinates
C  GLO     Array of obstacle low co-ordinates
C  GZ      Array of z grid line co-ordinates
C  NGZ     Number of z grid lines
C  NOB     Number of obstacles
C  NV      Number of vertices
C  NVXFE   Number of vertex at end of floor
C  NVXFS   Number of vertex at start of floor
C  NVXRE   Number of vertex at end of roof
C  NVXRS   Number of vertex at start of roof
C  SINK    Logical array indicating which obstacles are sinks
C  TYPE    Character array giving type of each obstacle (B=box, C=cylinder)
C  VX      Array of vertex x co-ordinates
C  VY      Array of vertex y co-ordinates
C  
C**** Local variables
C  CHANGE  Logical flag indicating if any changes have been made to
C          vertex data read from file
C  CHANG1  Logical flag indicating if any changes have been made to 
C          vertex data by last call to REVVER
C  NROW    Number of rows in the picture array
C  RESTRT  Logical flag indicating if a complete restart of vertex entry
C          has been requested
C  YORN    Character variable with value Y or N (yes or no)
C
C**** Local Arrays
C  APIC    Array to hold the picture of the cross-section
C
C**** Functions and Subroutines
C  CLEAR   Clear the screen
C  FLRFTM  Read the floor and roof start and end vertex numbers from 
C          the terminal
C  GEOMSV  Write a file containing the geometrical data
C  OBSTFL  Read obstacle data from a file
C  OBSTTM  Read obstacle data from the terminal
C  OPENFL  Open a file on a particular unit number
C  PICTUR  Display a picture of the cross-section outline
C  PRSRTC  display message 'Press return to continue'
C  REVVER  Review the vertex data
C  VERTFL  Read vertex data from a file
C  VERTTM  Read vertex data from the terminal
C  ZGRDFL  Read z grid data from a file
C  ZGRDTM  Read z grid data from the terminal
C******************************************************************************

C**** Give introductory message
      CALL CLEAR(LUSTOT)
      WRITE (LUSTOT,2)
2     FORMAT (' GEOMETRY DEFINITION PHASE'/
     1        ' ========================='/)

C**** Initialise file names
5     FILE1='NOT USED'
      FILE2='NOT USED'
      IF (INDATA.EQ.1) THEN
         CALL VERTTM(LUSTIN,LUSTOT,NVMAX,NV,VX,VY,COARSE,NVXFS,NVXFE,
     1               NVXRS,NVXRE,APIC,NROW)
         CALL ZGRDTM(LUSTIN,LUSTOT,NZMAX,NGZ,GZ,COARSZ)
         CALL OBSTTM(LUSTIN,LUSTOT,NOBMAX,NOB,TYPE,GLO,GHI,SINK)
         WRITE (LUSTOT,10)
10       FORMAT (//' All geometric data has now been entered'/
     1           ' This data will be saved in a file')
         CALL OPENFL(LUSTIN,LUSTOT,LU,FILE2,1,1)
         CALL GEOMSV(LU,NVMAX,NZMAX,NOBMAX,NV,VX,VY,COARSE,NVXFE,
     1               NVXRS,NVXRE,NGZ,GZ,COARSZ,NOB,TYPE,GLO,GHI,SINK)
         CLOSE(LU)
      ELSE
         IF (INDATA.EQ.2) WRITE (LUSTOT,20)
20       FORMAT (//' The GEOMETRIC data must come from a file'/)
         CALL OPENFL(LUSTIN,LUSTOT,LU,FILE1,1,0)
         CALL VERTFL(LU,LUSTOT,NVMAX,NV,VX,VY,COARSE,NVXFS,NVXFE,
     1               NVXRS,NVXRE)
         CALL ZGRDFL(LU,LUSTOT,NZMAX,NGZ,GZ,COARSZ)
         CALL OBSTFL(LU,LUSTOT,NOBMAX,NOB,TYPE,GLO,GHI,SINK)
         CLOSE(LU)
      END IF

C**** If data entry is quasi-interactive allow for editing vertices
      IF (INDATA.EQ.2) THEN
         CHANGE=.FALSE.
30	  WRITE (LUSTOT,40)
40       FORMAT (/' Do you want a picture of the cross-section',
     1           ' (Y/N)? >> ',$)
         READ (LUSTIN,50) YORN
50	  FORMAT (A1)
         IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) THEN
	    CALL PICTUR(LUSTIN,LUSTOT,VX,VY,NVMAX,NV,APIC,NROW)
	  ELSE IF ((YORN.EQ.'N').OR.(YORN.EQ.'n')) THEN
	    CONTINUE
	  ELSE
	    WRITE (LUSTOT,60)
60	    FORMAT (/' INVALID RESPONSE'/' Answer Y or N'/)
            GOTO 30
	  END IF
70       WRITE (LUSTOT,80)
80       FORMAT (/' Do you wish to review the vertices',
     1           ' (Y/N)? >> ',$)	 
         READ (LUSTIN,90) YORN
90	  FORMAT (A1)
         IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) THEN
	    CALL REVVER(NV,VX,VY,NVMAX,COARSE,RESTRT,LUSTIN,LUSTOT,
     1                  CHANG1)
C****       If a restart has been requested there are two possible courses
C****       of action - either abort the run or change from quasi-interactive
C****       mode of data entry to totally interactive
            IF (RESTRT) THEN
92	       WRITE (LUSTOT,94)
94             FORMAT (/' A complete restart of vertex entry has been',
     1                 ' requested'/
     2                 ' You may either abort this run or change to',
     3                 ' totally interactive data entry'/
     4                 ' Do you wish to ABORT (Y/N)? >> ',$)
95             READ (LUSTIN,96) YORN
96             FORMAT (A1)
               IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) THEN
	          WRITE (LUSTOT,97)
97		  FORMAT (/' RUN ABORTED AS REQUESTED'/)
                  STOP
	       ELSE IF ((YORN.EQ.'N').OR.(YORN.EQ.'n')) THEN
	          INDATA=1
		  GOTO 5
	       ELSE
	          WRITE (LUSTOT,98)
98		  FORMAT (/' INVALID RESPONSE'/' Answer Y or N'/)
                  GOTO 92
	       END IF
	    END IF
C****       A restart was not requested - so set CHANGE flag	    
	    CHANGE=CHANGE.OR.CHANG1
	  ELSE IF ((YORN.EQ.'N').OR.(YORN.EQ.'n')) THEN
	    CHANG1=.FALSE.
	  ELSE
	    WRITE (LUSTOT,100)
100         FORMAT (/' INVALID RESPONSE'/' Answer Y or N'/)
            GOTO 70
	  END IF
	 
C****    If any changes were made on the last call of REVVER a new
C****    picture might be wanted	 
	  IF (CHANG1) GOTO 30
	 
C****	 No more changes to be made
	  IF (CHANGE) THEN
110	    WRITE (LUSTOT,120)
120         FORMAT (/' Changes have been made to the vertices read',
     1              ' from input data file'/
     2              ' Do you want to read floor & roof start & end',
     3              ' from input data file (Y/N)? >> ',$)
            READ (LUSTIN,130) YORN
130	    FORMAT (A1)
            IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) THEN
               CONTINUE
	    ELSE IF ((YORN.EQ.'N').OR.(YORN.EQ.'n')) THEN
	       CALL FLRFTM(LUSTIN,LUSTOT,NVMAX,NV,VX,VY,NVXFS,NVXFE,
     1                     NVXRS,NVXRE)
	    ELSE
	       WRITE (LUSTOT,140)
140            FORMAT (/' INVALID RESPONSE'/' Answer Y or N'/)
               GOTO 110
	    END IF
145	    WRITE (LUSTOT,150)
150         FORMAT (/' Changes have been made to the vertices read',
     1               ' from input data file'/
     2               ' Do you want to read z-slice data from input',
     3               ' data file (Y/N)? >> ',$)
            READ (LUSTIN,160) YORN
160         FORMAT (A1)
            IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) THEN
	       CONTINUE
	    ELSE IF ((YORN.EQ.'N').OR.(YORN.EQ.'n')) THEN
	       CALL ZGRDTM(LUSTIN,LUSTOT,NZMAX,NGZ,GZ,COARSZ)
	    ELSE
	       WRITE (LUSTOT,170)
170	       FORMAT (/' INVALID RESPONSE'/' Answer Y or N'/)
               GOTO 145
	    END IF
180	    WRITE (LUSTOT,190)
190         FORMAT (/' Changes have been made to the vertices read',
     1               ' from input data file'/
     2               ' Do you want to read obstacle data from input',
     3               ' data file (Y/N)? >> ',$)
            READ (LUSTIN,200) YORN
200	    FORMAT (A1)
            IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) THEN
	       CONTINUE
	    ELSE IF ((YORN.EQ.'N').OR.(YORN.EQ.'n')) THEN
	       CALL OBSTTM(LUSTIN,LUSTOT,NOBMAX,NOB,TYPE,GLO,GHI,SINK)
	    ELSE
	       WRITE (LUSTOT,210)
210	       FORMAT (/' INVALID RESPONSE'/' Answer Y or N'/)
               GOTO 180
	    END IF
         END IF
C****    If changes have been made to the geometric data file a new
C****    input data file will be created
         IF (CHANGE) THEN
	    WRITE (LUSTOT,220)
220         FORMAT (' Changes have been made to the geometric data file'/
     1              ' A new file will be created')
            CALL OPENFL(LUSTIN,LUSTOT,LU,FILE2,1,1)
	    CALL GEOMSV(LU,NVMAX,NZMAX,NOBMAX,NV,VX,VY,COARSE,NVXFE,NVXRS,
     1                  NVXRE,NGZ,GZ,COARSZ,NOB,TYPE,GLO,GHI,SINK)
	    CLOSE (LU)
	  END IF
      END IF

C**** Geometry is defined - give message
      WRITE (LUSTOT,230)
230   FORMAT (/' End of geometry definition phase'//)
      CALL PRSRTC(LUSTIN,LUSTOT)
	 
      RETURN
      END


      SUBROUTINE VERTTM(LUSTIN,LUSTOT,NVMAX,NV,VX,VY,COARSE,NVXFS,
     1                  NVXFE,NVXRS,NVXRE,APIC,NROW)
C
C  Routine to read the vertex data from the terminal
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**01/02/91
C
C
      INTEGER LUSTIN,LUSTOT,NVMAX,NV,NROW,NVXFS,NVXFE,NVXRS,NVXRE
      REAL VX(NVMAX),VY(NVMAX)
      LOGICAL COARSE(NVMAX)
      CHARACTER*61 APIC(NROW)
      LOGICAL CHANGE,RESTRT
      CHARACTER*1 YORN
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  APIC    Array to hold the picture
C  COARSE  Array to hold coarse grid logical flags
C  LUSTIN  Logical unit number for standard input
C  LUSTOT  Logical unit number for standard output
C  NROW    Number of rows in the picture
C  NVMAX   Maximum number of vertices allowed (dimension of some arrays)
C  VX      Array to hold vertex x co-ordinates
C  VY      Array to hold vertex y co-ordinates
C
C**** Arguments - output
C  APIC    The picture array
C  COARSE  Array of logical flags indicating which vertices are on the coarse
C          grid
C  NV      Number of vertices
C  NVXFE   Number of the vertex at the end of the floor
C  NVXFS   Number of the vertex at the start of the floor (always 1)
C  NVXRE   Number of the vertex at the end of the roof
C  NVXRS   Number of the vertex at the start of the roof
C  VX      Array of vertex x co-ordinates
C  VY      Array of vertex y co-ordinates
C
C**** Local variables
C  CHANGE  Logical flag indicating if changes have been made by call to REVVER
C  RESTRT  Logical flag indicating if vertex entry is to be restarted
C  YORN    Character variable taking the value Y or N
C
C**** Functions and Subroutines
C  FLRFTM  Determine the vertex numbers at the start & end of floor & roof
C  PICTUR  Display a picture of the cross-sectional shape of the enclosure
C  REVVER  Review (edit) the vertices
C  VERTRM  Read vertex data from the terminal
C  
C******************************************************************************

C**** Read the vertex data
10    CALL VERTRM(LUSTIN,LUSTOT,NV,VX,VY,COARSE,NVMAX)

C**** Give picture and review options when appropriate
20    WRITE (LUSTOT,30)
30    FORMAT (/' Do you want a picture of the cross-section',
     1         ' (Y/N)? >> ',$)
      READ (LUSTIN,40) YORN
40    FORMAT (A1)
      IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) THEN
         CALL PICTUR(LUSTIN,LUSTOT,VX,VY,NVMAX,NV,APIC,NROW)
      ELSE IF ((YORN.EQ.'N').OR.(YORN.EQ.'n')) THEN
         CONTINUE
      ELSE
         WRITE (LUSTOT,50)
50       FORMAT (/' INVALID RESPONSE - Answer Y or N'/)
         GOTO 20
      END IF
60    WRITE (LUSTOT,70)
70    FORMAT (/' Do you wish to review the vertices (Y/N)? >> ',$)
      READ (LUSTIN,80) YORN
80    FORMAT (A1)
      IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) THEN
         CALL REVVER(NV,VX,VY,NVMAX,COARSE,RESTRT,LUSTIN,LUSTOT,CHANGE)
C****    If a restart is requested go back to start of routine
         IF (RESTRT) GOTO 10
C****    If changes have been made see if a picture is wanted
         IF (CHANGE) GOTO 20
      ELSE IF ((YORN.EQ.'N').OR.(YORN.EQ.'n')) THEN
         CONTINUE
      ELSE
         WRITE (LUSTOT,90)
90       FORMAT (/' INVALID RESPONSE - Answer Y or N'/)
         GOTO 60
      END IF

C**** Now determine the vertex numbers at the start & end of floor & roof
      CALL FLRFTM(LUSTIN,LUSTOT,NVMAX,NV,VX,VY,NVXFS,NVXFE,NVXRS,NVXRE)

      RETURN
      END

      SUBROUTINE VERTRM(LUSTIN,LUSTOT,NV,VX,VY,COARSE,NVMAX)
C
C  Routine to read the vertex information from the terminal
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**12/12/90
C
C
      INTEGER LUSTIN,LUSTOT,NVMAX,NV,IV
      REAL VX(NVMAX),VY(NVMAX)
      LOGICAL COARSE(NVMAX)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  COARSE  Array to hold the coarse grid vertex flags
C  LUSTIN  Logical unit number of standard input channel
C  LUSTOT  Logical unit number of standard output channel
C  NVMAX   Maximum number plus 1 of vertices allowed
C  VX      Array to hold the vertex x co-ordinates
C  VY      Array to hold the vertex y co-ordinates
C
C**** Arguments - output
C  COARSE  Logical array showing which vertices are on the coarse grid
C  NV      Number of vertices
C  VX      Array of x co-ordinates of vertices
C  VY      Array of y co-ordinates of vertices
C
C**** Local variables
C  ITEMP   Temporary integer flag used to set COARSE
C  IV      Loop counter over the vertives
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

      WRITE (LUSTOT,10)
10    FORMAT (' INTERACTIVE GEOMETRY DEFINITION'//
     1        ' The enclosure must have a uniform cross-sectional',
     2        ' shape in the x-y plane.'/
     3        ' This shape is formed from straight line segments,',
     4        ' whose ends are vertices.'/
     5        ' The last vertex is automatically joined to the',
     6        ' first by RADEXF.'/)
      WRITE (LUSTOT,12)
12    FORMAT (' The geometry of the enclosure defines the',
     1        ' finest zoning allowed.  Grid'/
     2        ' lines are drawn in the x and y directions',
     3        ' through each vertex to produce'/
     4        ' the FINE or GEOMETRIC grid.  The actual zones',
     5        ' are taken from a COARSE or'/
     6        ' HEAT TRANSFER grid, defined by some or all the',
     7        ' fine grid lines.'/)
      WRITE (LUSTOT,13)
13    FORMAT (' If a vertex is placed on the coarse grid then the',
     1        ' x and y grid lines through'/
     2        ' the vertex are put in the coarse grid. The',
     3        ' extreme fine grid lines are'/
     4        ' automatically included in the coarse grid.'/)
      WRITE (LUSTOT,15)
15    FORMAT (' The vertices are given by their x,y co-ordinates'/
     1        ' and must be entered in ANTI-CLOCKWISE order'/
     2        ' Begin at the START of the floor (hearth)'//)
20    WRITE (LUSTOT,30)
30    FORMAT (' HOW MANY VERTICES? >> ',$)
      READ (LUSTIN,*) NV
      IF (NV.GT.NVMAX-1) THEN
C****    Too many vertices 
         WRITE (LUSTOT,40) NVMAX-1,NV
40       FORMAT (' Too many vertices requested'/
     1           ' The maximum allowed is ',I3/
     2           ' To have ',I3,' vertices change parameter NVMAX',
     3           ' in comvar.f'/
     4           ' NB Maximum number of vertices allowed is NVMAX-1'/
     5           ' RUN ABORTED')
         STOP
      ELSE IF (NV.LE.2) THEN
C****    Too few vertices
         WRITE (LUSTOT,50)
50       FORMAT (' There must be at least 3 vertices'/)
         GOTO 20
      END IF
C**** Valid number of vertices
      DO 70 IV=1,NV
C****    Get vertex details
         WRITE (LUSTOT,60) IV
60       FORMAT (' Enter x,y co-ordinates of vertex ',I3,' >> ',$)
         READ (LUSTIN,*) VX(IV),VY(IV)
62       WRITE (LUSTOT,65)
65       FORMAT (' To put vertex on coarse grid enter 1',
     1           ' otherwise enter 0: >> ',$) 
         READ (LUSTIN,*) ITEMP
         IF (ITEMP.EQ.0) THEN
            COARSE(IV)=.FALSE.
         ELSE IF (ITEMP.EQ.1) THEN
            COARSE(IV)=.TRUE.
         ELSE
            WRITE (LUSTOT,67)
67	    FORMAT (/' INVALID RESPONSE'/)
            GOTO 62
	  END IF
70    CONTINUE

C**** Close the enclosure
      VX(NV+1)=VX(1)
      VY(NV+1)=VY(1)

      RETURN
      END


      SUBROUTINE PICTUR(LUSTIN,LUSTOT,VX,VY,NVMAX,NV,APIC,NROW)
C
C  Routine to draw the outline of the cross-sectional shape on the screen
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**07/12/90
C
      INTEGER LUSTIN,LUSTOT,NVMAX,NV,NROW
      REAL VX(NVMAX),VY(NVMAX)
      CHARACTER*61 APIC(NROW)
      PARAMETER (NCOL=61)
      INTEGER I,IXE,IXS,IXMAX,IXMIN,IYS,IYE,IYMAX,IYMIN,J,INCY
      REAL GRAD,SCAL,XMAX,XMIN,XSCAL,YMAX,YMIN,YSCAL
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  APIC    Empty picture array
C  LUSTIN  Logical unit number of standard input channel
C  LUSTOT  Logical unit number of standard output channel
C  NROW    Number of rows for picture array
C  NV      Number of vertices
C  NVMAX   Maximum number plus 1 of vertices allowed
C  VX      Array of x co-ordinates of vertices
C  VY      Array of y co-ordinates of vertices
C
C**** Arguments - output
C  APIC    Set picture array
C
C**** Local variables
C  GRAD    Gradient of current wall
C  I       Loop counter
C  INCY    Increment to the y picture co-ordinate
C  ISTEP   Increment to the x picture co-ordinate along a sloping wall
C  IXMAX   Larger of IXS and IXE
C  IXMIN   Smaller of IXS and IXE
C  IXE     x picture co-ordinate at end of current wall
C  IXS     x picture co-ordinate at start of current wall
C  IYMAX   Larger of IYS and IYE
C  IYMIN   Smaller of IYS and IYE
C  IYS     y picture co-ordinate at end of current wall
C  IYE     y picture co-ordinate at start of current wall
C  J       Loop counter
C  NCOL    Number of columns used in the picture
C  SCAL    Actual scaling factor
C  XMAX    Maximum x value
C  XMIN    Minimum x value
C  XSCAL   x scaling factor
C  YMAX    Maximum y value
C  YMIN    Minimum y value
C  YSCAL   y scaling factor
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  CLEAR   Clear the screen
C  PRSRTC  Print message 'Press return to continue'
C
C******************************************************************************

C**** Set the picture array to blanks
      DO 20 I=1,NROW
         DO 10 J=1,NCOL
            APIC(I)(J:J)=' '
10       CONTINUE
20    CONTINUE

C**** Determine x and y limits
      XMIN=VX(1)
      XMAX=VX(1)
      YMIN=VY(1)
      YMAX=VY(1)
      DO 30 I=2,NV
         XMIN=MIN(XMIN,VX(I))
         XMAX=MAX(XMAX,VX(I))
         YMIN=MIN(YMIN,VY(I))
         YMAX=MAX(YMAX,VY(I))
30    CONTINUE

C**** Compute scaling factors
      XSCAL=FLOAT(NCOL-1)/(XMAX-XMIN)
      YSCAL=FLOAT(2*NROW-2)/(YMAX-YMIN)
      SCAL=MIN(XSCAL,YSCAL)

C**** Set the picture array a wall at a time
      DO 70 I=1,NV
C****    Determine picture co-ords of vertices at start and end of wall
         IXS=NINT((VX(I)-XMIN)*SCAL+1.0)
         IXE=NINT((VX(I+1)-XMIN)*SCAL+1.0)
         IYS=NINT((VY(I)-YMIN)*0.5*SCAL+1.0)
         IYE=NINT((VY(I+1)-YMIN)*0.5*SCAL+1.0)
         IXMIN=MIN(IXS,IXE)
	  IXMAX=MAX(IXS,IXE)
	  IYMIN=MIN(IYS,IYE)
	  IYMAX=MAX(IYS,IYE)
C****    Set point at start of wall
         APIC(IYS)(IXS:IXS)='V'
         IF (IXS.EQ.IXE) THEN
C****       Wall is along an x=constant line
            DO 40 J=IYMIN+1,IYMAX-1
               APIC(J)(IXS:IXS)='*'
40          CONTINUE
         ELSE IF (IYS.EQ.IYE) THEN
C****       Wall is along a y=constant line
            DO 50 J=IXMIN+1,IXMAX-1
               APIC(IYS)(J:J)='*'
50          CONTINUE
         ELSE
C****       Wall is sloping
            GRAD=FLOAT(IYE-IYS)/FLOAT(IXE-IXS)
	    ISTEP=(IXE-IXS)/ABS(IXE-IXS)
            DO 60 J=IXS+ISTEP,IXE-ISTEP,ISTEP
               INCY=NINT(GRAD*FLOAT(J-IXS))
               APIC(IYS+INCY)(J:J)='*'
60          CONTINUE
         END IF
70    CONTINUE

C**** Print the picture
      CALL CLEAR(LUSTOT)
      DO 90 I=NROW,1,-1
         WRITE (LUSTOT,80) APIC(I)
80       FORMAT (10X,A)
90    CONTINUE
      WRITE (LUSTOT,100) 
100   FORMAT (/' V=vertex'//)
      CALL PRSRTC(LUSTIN,LUSTOT)

      RETURN
      END

      SUBROUTINE REVVER(NV,VX,VY,NVMAX,COARSE,RESTRT,LUSTIN,LUSTOT,
     1                  CHANGE)
C
C  Routine to review the vertices to allow them to be altered
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**11/12/90
C
C
      INTEGER NV,NVMAX,LUSTIN,LUSTOT
      REAL VX(NVMAX),VY(NVMAX)
      LOGICAL COARSE(NVMAX),RESTRT,CHANGE,CANADD,MORE
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  COARSE  Logical array indicating which vertices are on the coarse grid
C  LUSTIN  Logical unit for standard input
C  LUSTOT  Logical unit for standard output
C  NV      Number of vertices
C  NVMAX   Maximum number of vertices
C  VX      Vertex x co-ordinates
C  VY      Vertex y co-ordinates
C
C**** Arguments - output
C  CHANGE  Logical flag indicating if changes have been made in this review
C  COARSE  Logical array indicating which vertices are on the coarse grid
C  NV      Number of vertices
C  RESTRT  Logical flag indicating if all vertices are to be re-entered
C  VX      Vertex x co-ordinates
C  VY      vertex y co-ordinates
C
C**** Local variables
C  CANADD  Logical flag indiciating if more vertices may be added
C  MORE    Logical flag indicating if further edit options are to be offered
C
C**** Local Arrays
C
C**** Functions and Subroutines
C  CLEAR   Clear the screen
C  
C******************************************************************************

C**** Initialise logical flags
      CHANGE=.FALSE.
      RESTRT=.FALSE.
      IF (NV.LT.NVMAX-1) THEN
         CANADD=.TRUE.
      ELSE
         CANADD=.FALSE.
      END IF

C**** Give explanatory information
      CALL CLEAR(LUSTOT)
      WRITE (LUSTOT,10) 
10    FORMAT (' VERTEX REVIEW'/
     1        ' You may amend the vertices as follows:'/
     2        '     1.  Add new vertices (one at a time)'/
     3        '     2.  Change an existing vertex'/
     4        '     3.  Delete vertices (one at a time)'/
     5        '     4.  Reject all current vertices and start'/
     6        '         vertex entry again'/
     7        '     5.  Finish reviewing vertices'/)

C**** List the vertices
15    WRITE (LUSTOT,20) NVMAX-1,NV
20    FORMAT (//1X,I3,' vertices are allowed'/
     1        1X,I3,' vertices have been defined'/
     2        1X,' Their co-ordinates and coarse grid flags are below')
      DO 40 I=1,NV
         WRITE (LUSTOT,30) I,VX(I),VY(I),COARSE(I)
30       FORMAT (' Vertex ',I3,' Co-ords: ',2(G12.5,2X),
     1           ' On coarse grid ',L2)
40    CONTINUE

C**** Amend the vertices as requested
50    WRITE (LUSTOT,60)
60    FORMAT (/' Select a review option'/
     1        ' 1. Add  2. Change  3. Delete  4. Restart  5. Finish'/
     2        ' Enter option number (1-5) >> ',$)
      READ (LUSTIN,*) IOPT
C**** Process option
      IF ((IOPT.LT.1).OR.(IOPT.GT.5)) THEN
C****    Invalid response
         WRITE (LUSTOT,70)
70       FORMAT (/' INVALID RESPONSE - Option must be in range 1-5'/)
         GOTO 50
      ELSE IF (IOPT.EQ.1) THEN
C****    Add a new vertex
         IF (CANADD) THEN
75          WRITE (LUSTOT,80) NV
80          FORMAT (' Add new vertex after which vertex (0-',
     1              I3,')? >> ',$)
            READ (LUSTIN,*) IVERT
            IF ((IVERT.LT.0).OR.(IVERT.GT.NV)) THEN
               WRITE (LUSTOT,85) NV
85             FORMAT (/' INVALID RESPONSE - Answer in range 0-',I3)
               GOTO 75
            END IF
            WRITE (LUSTOT,90)
90          FORMAT (' Enter x,y co-ordinates of new vertex >> ',$)
            READ (LUSTIN,*) X,Y
95          WRITE (LUSTOT,100)
100         FORMAT (' If vertex is on coarse grid enter 1',
     1              ' otherwise enter 0 >> ',$)
            READ (LUSTIN,*) ICRS
            IF ((ICRS.LT.0).OR.(ICRS.GT.1)) THEN
               WRITE (LUSTOT,105)
105            FORMAT (/' INVALID RESPONSE - Answer 0 or 1')
               GOTO 95
            END IF
            DO 110 I=NV+1,IVERT+1,-1
               VX(I+1)=VX(I)
               VY(I+1)=VY(I)
               COARSE(I+1)=COARSE(I)
110         CONTINUE
            VX(IVERT+1)=X
            VY(IVERT+1)=Y
            IF (ICRS.EQ.0) THEN
               COARSE(IVERT+1)=.FALSE.
            ELSE
               COARSE(IVERT+1)=.TRUE.
            END IF
            NV=NV+1
            CHANGE=.TRUE.
            IF (NV.EQ.NVMAX-1) CANADD=.FALSE.
         ELSE
            WRITE (LUSTOT,120)
120         FORMAT (/' Maximum number of vertices allowed have',
     1               ' been defined'/
     2               ' No more may be added')
         END IF
         MORE=.TRUE.
      ELSE IF (IOPT.EQ.2) THEN
C****    Change an existing vertex
125      WRITE (LUSTOT,130) NV
130      FORMAT (' Enter number of vertex to be changed (1-',
     1           I3,') >> ',$)
         READ (LUSTIN,*) IVERT
         IF ((IVERT.LT.1).OR.(IVERT.GT.NV)) THEN
            WRITE (LUSTOT,135) NV
135         FORMAT (/' INVALID RESPONSE - Answer in range 1-',I3/)
            GOTO 125
         END IF
         WRITE (LUSTOT,140) 
140      FORMAT (' Enter new x and y co-ordinates >> ',$)
         READ (LUSTIN,*) VX(IVERT),VY(IVERT)
145      WRITE (LUSTOT,150)
150      FORMAT (' If vertex is on coarse grid enter 1',
     1           ' otherwise enter 0 >> ',$)
         READ (LUSTOT,*) ICRS
         IF ((ICRS.LT.0).OR.(ICRS.GT.1)) THEN
            WRITE (LUSTOT,160)
160         FORMAT (/' INVALID RESPONSE - Answer must be 0 or 1'/)
            GOTO 145
         ELSE IF (ICRS.EQ.0) THEN
            COARSE(IVERT)=.FALSE.
         ELSE
            COARSE(IVERT)=.TRUE.
         END IF
         CHANGE=.TRUE.
         MORE=.TRUE.
      ELSE IF (IOPT.EQ.3) THEN
C****    Delete a vertex
170      WRITE (LUSTOT,180) NV 
180      FORMAT (' Enter number of vertex to be deleted (1-'
     1           ,I3,') >> '$)
         READ (LUSTIN,*) IVERT
         IF ((IVERT.LT.1).OR.(IVERT.GT.NV)) THEN
            WRITE (LUSTOT,190) NV
190         FORMAT (/' INVALID RESPONSE - Answer in range 1-',I3/)
            GOTO 170
         END IF
         DO 200 I=IVERT,NV
            VX(I)=VX(I+1)
            VY(I)=VY(I+1)
            COARSE(I)=COARSE(I+1)
200      CONTINUE
         NV=NV-1
         CHANGE=.TRUE.
         IF (NV.LT.NVMAX-1) CANADD=.TRUE.
         MORE=.TRUE.
      ELSE IF (IOPT.EQ.4) THEN
C****    Restart vertex entry
         NV=0
         DO 210 I=1,NVMAX
            VX(I)=0.
            VY(I)=0.
            COARSE(I)=.FALSE.
210      CONTINUE
         CHANGE=.TRUE.
         MORE=.FALSE.
         RESTRT=.TRUE.
      ELSE 
C****    Finish vertex review
         MORE=.FALSE.
      END IF

C**** Further review or return?
      IF (MORE) GOTO 15

      RETURN
      END      

      SUBROUTINE FLRFTM(LUSTIN,LUSTOT,NVMAX,NV,VX,VY,NVXFS,NVXFE,
     1                  NVXRS,NVXRE)
C
C  Routine for inputting of the vertex numbers at the end of the floor
C  and the start and end of the roof of the enclosure (NB the floor starts
C  at vertex 1) - input from the terminal
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**09/01/91
C
C
      INTEGER LUSTIN,LUSTOT,NVMAX,NV,NVXFS,NVXFE,NVXRS,NVXRE,IV
      REAL VX(NVMAX),VY(NVMAX)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTIN  Logical unit number for standard input
C  LUSTOT  Logical unit number for standard output
C  NVMAX   Maximum number of vertices allowed
C  NV      Actual number of vertices
C  VX      Array of vertex x co-ordinates
C  VY      Array of vertex y co-ordinates
C
C**** Arguments - output
C  NVXFS  Vertex number at the start of the floor
C  NVXFE  Vertex number at the end of the floor
C  NVXRS  Vertex number at the start of the roof
C  NVXRE  Vertex number at the end of the roof
C

C**** Local variables
C  IV      Loop counter on number of vertices
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  CLEAR   Clear the screen
C
C******************************************************************************

C**** Input is from the terminal so prompts are required
      CALL CLEAR(LUSTOT)
      WRITE (LUSTOT,10)
10    FORMAT (' In order to assemble the surface elements into zones'/
     1        ' It is necessary to know the boundaries of the floor',
     2        ' & roof'/
     3        ' The floor & roof must begin and end at vertices'/
     4        ' Here is a list of the vertices giving their co-ords'/)
      DO 30 IV=1,NV
         WRITE (LUSTOT,20) IV,VX(IV),VY(IV)
20       FORMAT (' Vertex ',I3,' Co-ords: ',2(F12.6,2X))
30    CONTINUE
      NVXFS=1
40    WRITE (LUSTOT,50)
50    FORMAT (/' The floor starts at vertex 1'/
     1        ' Enter the vertex number where it ends >> ',$)
      READ (LUSTIN,*) NVXFE
      IF ((NVXFE.GT.NV-2).OR.(NVXFE.LT.2)) THEN
         WRITE (LUSTOT,60)
60       FORMAT (/' IMPOSSIBLE VALUE'/)
         GOTO 40
      END IF
70    WRITE (LUSTOT,80)
80    FORMAT (' The start of the roof is the end with the smaller',
     1        ' x co-ord'/
     2        ' Enter the vertex number of the start of the roof >> '
     3        ,$)
      READ (LUSTIN,*) NVXRS
      IF ((NVXRS.LT.NVXFE+2).OR.(NVXRS.GT.NV)) THEN
         WRITE (LUSTOT,60)
         GOTO 70
      END IF
90    WRITE (LUSTOT,100)
100   FORMAT (' The roof must end at a vertex between the end of',
     1        ' the floor'/
     2        ' & the start of the roof'/
     3        ' Enter this vertex number >> ',$)
      READ (LUSTIN,*) NVXRE
      IF ((NVXRE.LT.NVXFE+1).OR.(NVXRE.GT.NVXRS-1)) THEN
         WRITE (LUSTOT,60)
         GOTO 90
      END IF

      RETURN
      END

      SUBROUTINE VERTFL(LU,LUSTOT,NVMAX,NV,VX,VY,COARSE,NVXFS,NVXFE,
     1                  NVXRS,NVXRE)
C
C  Routine to read the vertex information from a file 
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LU,LUSTOT,NVMAX,NV,NVXFS,NVXFE,NVXRS,NVXRE
      REAL VX(NVMAX),VY(NVMAX)
      LOGICAL COARSE(NVMAX)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  COARSE  Logical array to hold coarse grid vertex flags
C  LU      Logical unit number of input file
C  LUSTOT  Logical unit number for standard output
C  NVMAX   Maximum number of vertices allowed (dimension of some arrays)
C  VX      Array to hold x co-ordinates of vertices
C  VY      Array to hold y co-ordinates of vertices
C
C**** Arguments - output
C  COARSE  Logical array showing which vertices are on the coarse grid
C  NV      Number of vertices
C  NVXFE   Number of vertex at end of floor
C  NVXFS   Number of vertex at start of floor
C  NVXRE   Number of vertex at end of roof
C  NVXRS   Number of vertex at start of roof
C  VX      Array of x co-ordinates of vertices
C  VY      Array of y co-ordinates of vertices
C
C**** Local variables
C  ITEMP   Temporary integer flag used to set COARSE
C  IV      Loop counter over the vertives
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

      READ (LU,*) NV
C**** Check that number of vertices is allowed
      IF (NV.GT.NVMAX-1) THEN
C****    Too many vertices
         WRITE (LUSTOT,20) NVMAX-1,NV
20       FORMAT (/' TOO MANY VERTICES REQUESTED'/
     1           I3,' VERTICES ARE ALLOWED'/
     2           ' YOU HAVE REQUESTED ',I3/
     3           ' CHANGE PARAMETER NVMAX IN comvar.f AND RECOMPILE'/
     4           ' RUN ABORTED')	 
         STOP
      ELSE IF (NV.LE.2) THEN
C****    Too few vertices
         WRITE(LUSTOT,30)
30	 FORMAT(' TOO FEW VERTICES - THERE MUST BE AT LEAST 3'/
     1          ' RUN ABORTED')
         STOP
      END IF
      DO 70 IV=1,NV
         READ (LU,*) VX(IV),VY(IV)
         READ (LU,*) ITEMP
         IF (ITEMP.EQ.0) THEN
            COARSE(IV)=.FALSE.
         ELSE IF (ITEMP.EQ.1) THEN
            COARSE(IV)=.TRUE.
         ELSE
            WRITE (LUSTOT,60) IV
60          FORMAT (/' INVALID COARSE GRID FLAG FOR VERTEX ',I3/
     1              ' RUN ABORTED')
            STOP
         END IF
70    CONTINUE

C**** Close the enclosure
      VX(NV+1)=VX(1)
      VY(NV+1)=VY(1)

C**** Determine the start & end of the floor & roof
      NVXFS=1
      READ (LU,*) NVXFE
      IF ((NVXFE.LT.2).OR.(NVXFE.GT.NV-2)) THEN
         WRITE (LUSTOT,80)
         STOP
      END IF
      READ (LU,*) NVXRS
      IF ((NVXRS.LT.NVXFE+2).OR.(NVXRS.GT.NV)) THEN
         WRITE (LUSTOT,80)
         STOP
      END IF
      READ (LU,*) NVXRE
      IF ((NVXRE.LT.NVXFE+1).OR.(NVXFE.GT.NVXRS-1)) THEN
         WRITE (LUSTOT,80)
         STOP
      END IF

80    FORMAT (' IMPOSSIBLE VALUE FOR VERTEX AT END OF ROOF'/
     1        ' RUN ABORTED')

      RETURN
      END

      SUBROUTINE ZGRDTM(LUSTIN,LUSTOT,NZMAX,NGZ,GZ,COARSZ)
C
C  Routine to determine the z grid spacing by reading from the terminal
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**10/01/91
C
C
      INTEGER LUSTIN,LUSTOT,NZMAX,NGZ,NSLICE,IZ
      REAL GZ(NZMAX),DZ
      LOGICAL COARSZ(NZMAX)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTIN  Logical unit number of standard input channel
C  LUSTOT  Logical unit number of standard output channel
C  NZMAX   Maximum number of z grid lines allowed
C
C**** Arguments - output
C  COARSZ  Logical array indicating which z grid lines are in coarse grid
C  GZ      Array of z grid line co-ordinates
C  NGZ     Number of z grid lines
C
C**** Local variables
C  DZ      Thickness of current slice
C  ITEMP   Temporary integer flag used to set COARSZ
C  IZ      Loop counter on slices
C  NSLICE  Number of slices
C
C**** Functions and Subroutines
C  CLEAR   Clear the screen
C  PRSRTC  Display the message 'Press return to continue'
C
C******************************************************************************

C**** Clear the screen and give header message
      CALL CLEAR(LUSTOT)
      WRITE (LUSTOT,10)
10    FORMAT (' Z GRID INFORMATION'/
     1        ' In the z direction the enclosure is divided into',
     2        ' a number of slices'/
     3        ' All slices have the cross-section already defined',
     4        ' by the vertices'/
     5        ' But they may each have different thicknesses'///)
20    WRITE (LUSTOT,30)
30    FORMAT (' HOW MANY SLICES? >> ',$)
      READ (LUSTIN,*) NSLICE
C**** Check that the number of slices is OK
      IF (NSLICE.GT.NZMAX-1) THEN
         WRITE (LUSTOT,40) NZMAX-1
40       FORMAT (' Maximum number of slices allowed is ',I2/
     1           ' To incease this change NZMAX in COMVAR.F and'/
     2           ' recompile'/
     3           ' NB Maximum number of slices is NZMAX-1'/
     4           ' RUN ABORTED')
         STOP
      ELSE IF (NSLICE.LT.1) THEN
         WRITE (LUSTOT,50)
50       FORMAT (' There must be at least 1 slice'/)
         GOTO 20
      END IF

C**** Set first co-ordinate to 0 then read the thicknesses of the slices
      GZ(1)=0.
      DO 90 IZ=1,NSLICE
60       WRITE (LUSTOT,70) IZ
70       FORMAT (' Enter thickness of slice ',I2,' >> ',$)
         READ (LUSTIN,*) DZ
C***     Check that the value is permissible
         IF (DZ.LE.0.) THEN
            WRITE (LUSTOT,80)
80          FORMAT (' Thickness must be positive '/)
            GOTO 60
         END IF
C****    Compute next z grid line co-ordinate
         GZ(IZ+1)=GZ(IZ)+DZ
90    CONTINUE
C**** Record the number of grid lines
      NGZ=NSLICE+1

C**** Determine coarse grid information
      IF (NGZ.EQ.2) THEN
         WRITE (LUSTOT,91)
91       FORMAT (' Only 1 slice so both z grid lines are on coarse',
     1           ' grid')
         COARSZ(1)=.TRUE.
         COARSZ(2)=.TRUE.
      ELSE
         WRITE (LUSTOT,92)
92       FORMAT (' Specify which z grid lines are on coarse grid'/
     1           ' The first & last grid lines are automatically'/)
         COARSZ(1)=.TRUE.
         COARSZ(NGZ)=.TRUE.
         DO 96 IZ=2,NGZ-1
93	    WRITE (LUSTOT,94) IZ,GZ(IZ)
94          FORMAT (' Grid line ',I2,' is at z=',F12.6/
     1              ' Is this on the coarse grid?'/
     2              ' Enter 1 for YES or 0 for NO >> ',$)
            READ (LUSTIN,*) ITEMP
            IF (ITEMP.EQ.0) THEN
               COARSZ(IZ)=.FALSE.
            ELSE IF (ITEMP.EQ.1) THEN
               COARSZ(IZ)=.TRUE.
            ELSE
               WRITE (LUSTOT,95)
95             FORMAT (' INVALID RESPONSE'/' Reply 0 or 1'/)
               GOTO 93
            END IF
96       CONTINUE	       		  
      END IF

C**** Give end of z data message
      WRITE (LUSTOT,130)
130   FORMAT (///' All z grid information has now been given'/
     1        ' External shape of enclosure is now defined'//)
      CALL PRSRTC(LUSTIN,LUSTOT)

      RETURN
      END

      SUBROUTINE ZGRDFL(LU,LUSTOT,NZMAX,NGZ,GZ,COARSZ)
C
C  Routine to read the z grid information from a file
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LU,LUSTOT,NZMAX,NGZ,NSLICE,IZ,ITEMP
      REAL GZ(NZMAX),DZ
      LOGICAL COARSZ(NZMAX)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  COARSZ  Array to hold the coarse grid logical flags
C  GZ      Array to hold the z grid line co-ordinates
C  LU      Logical unit number from which data is to be read
C  LUSTOT  Logical unit number for standard output
C  NZMAX   Maximum number of z grid lines allowed
C  
C**** Arguments - output
C  COARSZ  Array of logical flags showing which z grid lines are on the coarse
C          grid
C  GZ      Array of z grid line co-ordinates
C  NGZ     Number of z grid lines
C
C**** Local variables
C  DZ      Thickness of current slice
C  ITEMP   Temporary integer value used in setting COARSZ
C  IZ      Loop counter over the slices
C  NSLICE  Number of z slices
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C**** Read the number of slices and check it is allowed
      READ (LU,*) NSLICE
      IF (NSLICE.GT.NZMAX-1) THEN
         WRITE (LUSTOT,20) NZMAX-1
20       FORMAT (/' Maximum number of slices allowed is ',I2/
     1           ' To increase this change NZMAX in COMVAR.F',
     2           ' and recompile'/
     3           ' NB Maximum number of slices is NZMAX-1'/
     4           ' RUN ABORTED')
         STOP
      ELSE IF (NSLICE.LT.1) THEN
         WRITE (LUSTOT,30)
30       FORMAT (/' There must be at least 1 slice'/
     1            ' RUN ABORTED')
         STOP
      END IF

C**** Number of slices is OK set 1st co-ordinate to zero & read in thicknesses
      GZ(1)=0.
      DO 50 IZ=1,NSLICE
         READ (LU,*) DZ
         GZ(IZ+1)=GZ(IZ)+DZ
50    CONTINUE
      NGZ=NSLICE+1

C**** Determine coarse grid information
      COARSZ(1)=.TRUE.
      COARSZ(NGZ)=.TRUE.
      IF (NGZ.GT.2) THEN
         DO 80 IZ=2,NSLICE
            READ (LU,*) ITEMP
            IF (ITEMP.EQ.0) THEN
               COARSZ(IZ)=.FALSE.
            ELSE IF (ITEMP.EQ.1) THEN
               COARSZ(IZ)=.TRUE.
            ELSE
               WRITE (LUSTOT,70) IZ
70             FORMAT (' ILLEGAL VALUE FOR COARSE GRID FLAG OF',
     1                 ' Z GRID LINE ',I2/
     2                 ' RUN ABORTED')
               STOP
            END IF
80       CONTINUE
      END IF

      RETURN
      END

      SUBROUTINE OBSTTM(LUSTIN,LUSTOT,NOBMAX,NOB,TYPE,GLO,GHI,SINK)
C
C  Routine to determine the number, type and location of the obstacles
C  from the terminal
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**10/01/91
C
      INTEGER LUSTIN,LUSTOT,NOBMAX,NOB,IOB,K
      REAL GLO(NOBMAX,3),GHI(NOBMAX,3),CLO(3),CHI(3)
      LOGICAL SINK(NOBMAX)
      CHARACTER*1 TYPE(NOBMAX)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTIN  Logical unit number of standard input channel
C  LUSTOT  Logical unit number of standard output channel
C  NOBMAX  Maximum number of obstacles allowed
C
C**** Arguments - output
C  GLO     Array of xyz co-ordinates of low corner or low centre of obstacle
C  GHI     Array of xyz co-ordinates of high corner of a box or for a cylinder
C          the radius, direction (x=1,y=2,z=3) and high centre co-ordinate
C  NOB     Number of obstacles
C  SINK    Logical array of flags indicating if obstacle is a sink (true)
C          or a source (false)
C  TYPE    Character array recording the type of each obstacle (B or C)
C  
C**** Local variables
C  IOB     Loop counter on number of obstacles
C  K       Loop counter over the co-ordinate directions
C
C**** Local Arrays
C  CLO     Low co-ordinates returned by OBINTM
C  CHI     Low co-ordinates returned by OBINTM
C
C**** Functions and Subroutines
C  OBINTM  Read details of a single obstacle from the terminal
C  PRSRTC  Display message 'Press return to continue'
C
C******************************************************************************

C**** Clear the screen and give a header
      CALL CLEAR(LUSTOT)
      WRITE (LUSTOT,10)
10    FORMAT (' OBSTACLE DEFINITION PHASE'//
     1        ' All obstacles are either boxes or cylinders'/
     2        ' Boxes are specified by the co-ordinates of the',
     3        ' low and high corners'/
     4        ' Cylinders are specified by the co-ordinates of the',
     5        ' centre of the low plane face'/
     6        ' the radius, co-ordinates direction and co-ordinate of',
     7        ' the high plane face'//)
20    WRITE (LUSTOT,30)
30    FORMAT (' HOW MANY OBSTACLES? >> ',$)
      READ (LUSTIN,*) NOB
C**** Check that the value of NOB is permissible
      IF (NOB.GT.NOBMAX) THEN
         WRITE (LUSTOT,40) NOBMAX
40       FORMAT (/' Too many obstacles - maximum permitted is ',I2/
     1           ' Change the value of NOBMAX in COMVAR.F and',
     2           ' recompile'/
     3           ' RUN ABORTED')
         STOP
      ELSE IF (NOB.LT.0) THEN
         WRITE (LUSTOT,50)
50       FORMAT (/' The number of obstacles must be non-negative'/)
         GOTO 20
      END IF

C**** Determine the obstacle data
      DO 70 IOB=1,NOB
         CALL OBINTM(LUSTIN,LUSTOT,IOB,TYPE(IOB),CLO,CHI,SINK(IOB))
         DO 60 K=1,3
            GLO(IOB,K)=CLO(K)
            GHI(IOB,K)=CHI(K)
60       CONTINUE
70    CONTINUE

C**** Give end of obstacle definition message
      WRITE (LUSTOT,80)
80    FORMAT (//' Obstacle definition phase completed'/)

      RETURN
      END

      SUBROUTINE OBINTM(LUSTIN,LUSTOT,IOB,KIND,CLO,CHI,LSINK)
C
C  Routine to obtain the details of a single obstacle from the terminal
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**10/01/91
C
C
      INTEGER LUSTIN,LUSTOT,IOB,K,IDIR,ITEMP
      REAL CLO(3),CHI(3)
      LOGICAL LSINK
      CHARACTER*1 KIND,AXIS(3)
      DATA AXIS /'X','Y','Z'/
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  IOB     Number of the obstacle
C  LUSTIN  Logical unit number for standard input
C  LUSTOT  Logical unit number for standard output
C
C**** Arguments - output
C  CHI     Array of high co-ordinates
C  CLO     Array of low co-ordinates
C  KIND    Kind of obstacle (B or C)
C  LSINK   Logical sink/source flag (TRUE=sink)
C
C**** Local variables
C  IDIR    Direction of cylinder
C  ITEMP   Integer flag used in setting LSINK
C  K       Loop counter over co-ordinate directions
C
C**** Local Arrays
C  AXIS    Array of names of co-ordinate directions
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C**** Determine the type
10    WRITE (LUSTOT,20) IOB
20    FORMAT (' Is obstacle ',I2,' a box (B) or a cylinder (C)?',
     1        '  >> ',$)
      READ (LUSTIN,30) KIND
30    FORMAT (A1)
      IF (KIND.EQ.'b') KIND='B'
      IF (KIND.EQ.'c') KIND='C'
C**** If the type is OK determine the co-ordinates
      IF ((KIND.NE.'B').AND.(KIND.NE.'C')) THEN
         WRITE (LUSTOT,40)
40       FORMAT (/' INVALID RESPONSE'/' Answer B or C'/)
         GOTO 10
      ELSE IF (KIND.EQ.'B') THEN
C****    Obstacle is a box
         WRITE (LUSTOT,50)
50       FORMAT (' Enter low co-ordinates of the box >> ')
         READ (LUSTIN,*) (CLO(K),K=1,3)
         WRITE (LUSTOT,60)
60       FORMAT (' Enter high co-ordinates of the box >> ')
         READ (LUSTIN,*) (CHI(K),K=1,3)
      ELSE
C****    Obstacle is a cylinder
         WRITE (LUSTOT,70)
70       FORMAT (' Enter co-ordinates of centre of low plane face',
     1           ' of the cylinder >>')
         READ (LUSTIN,*) (CLO(K),K=1,3)
         WRITE (LUSTOT,80)
80       FORMAT (' Enter radius of the cylinder >> ',$)
         READ (LUSTIN,*) CHI(1)
90       WRITE (LUSTOT,100)
100      FORMAT (/' The axis of the cylinder must be in a co-ordinate',
     1           ' direction'/
     2           ' Enter 1 for X, 2 for Y, 3 for Z >> ',$)
         READ (LUSTIN,*) IDIR
         CHI(2)=FLOAT(IDIR)
         IF ((IDIR.LT.1).OR.(IDIR.GT.3)) THEN
            WRITE (LUSTOT,105)
105         FORMAT (/' INVALID RESPONSE'/' Answer 1,2 or 3'/)
            GOTO 90
         END IF
         WRITE (LUSTOT,110) AXIS(IDIR)
110      FORMAT (' Enter ',A1,' co-ordinate of high plane face >> ',$)
         READ (LUSTIN,*) CHI(3)
      END IF

C**** Determine if obstacle is a source or a sink
120   WRITE (LUSTOT,130)
130   FORMAT (' Enter 1 for a SINK or 0 for a SOURCE >> ',$)
      READ (LUSTIN,*) ITEMP
      IF (ITEMP.EQ.0) THEN
         LSINK=.FALSE.
      ELSE IF (ITEMP.EQ.1) THEN
         LSINK=.TRUE.
      ELSE
         WRITE (LUSTOT,140)
140      FORMAT (/' INVALID RESPONSE'/' Answer 0 or 1'/)
         GOTO 120
      END IF

      RETURN
      END

      SUBROUTINE OBSTFL(LU,LUSTOT,NOBMAX,NOB,TYPE,GLO,GHI,SINK)
C
C  Routine to determine the number, type and location of the obstacles
C  by reading from a file
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
      INTEGER LU,LUSTOT,NOBMAX,NOB,IOB,IDIR
      REAL GLO(NOBMAX,3),GHI(NOBMAX,3)
      LOGICAL SINK(NOBMAX)
      CHARACTER*1 TYPE(NOBMAX)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LU      Logical unit number of file from which data is read
C  LUSTOT  Logical unit number of standard output channel
C  NOBMAX  Maximum number of obstacles allowed
C
C**** Arguments - output
C  GLO     Array of xyz co-ordinates of low corner or low centre of obstacle
C  GHI     Array of xyz co-ordinates of high corner of a box or for a cylinder
C          the radius, direction (x=1,y=2,z=3) and high centre co-ordinate
C  NOB     Number of obstacles
C  SINK    Logical array of flags indicating if obstacle is a sink (true)
C          or a source (false)
C  TYPE    Character array recording the type of each obstacle (B or C)
C  
C**** Local variables
C  IDIR    Integer value of direction of current cylinder
C  IOB     Loop counter on number of obstacles
C  ITEMP   Temporary integer flag used in setting array SINK
C  K       Loop counter on second argument of GLO & GHI 
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C**** Read number of obstacles
      READ (LU,*) NOB
C**** Check the number is obstacles is OK
      IF (NOB.GT.NOBMAX) THEN
C****    Too big
         WRITE (LUSTOT,20) NOBMAX
20       FORMAT (/' Too many obstacles required - maximum allowed is',
     1           I2/' Change NOBMAX in COMVAR.F and recompile'/
     2           ' RUN ABORTED')
         STOP
      ELSE IF (NOB.LT.0) THEN
C****    Too small
         WRITE (LUSTOT,30)
30       FORMAT (/' Number of obstacles must be non-negative'/
     1           ' RUN ABORTED')
         STOP
      END IF
C**** Read the obstacle details
      DO 90 IOB=1,NOB
         READ (LU,40) TYPE(IOB)
40       FORMAT (1X,A1)
         IF (TYPE(IOB).EQ.'b') TYPE(IOB)='B'
         IF (TYPE(IOB).EQ.'c') TYPE(IOB)='C'
         IF ((TYPE(IOB).NE.'B').AND.(TYPE(IOB).NE.'C')) THEN
            WRITE (LUSTOT,50) IOB
50          FORMAT (/' Illegal obstcale type for obstacle ',I2/
     1              ' RUN ABORTED')
            STOP
         END IF
         READ (LU,*) (GLO(IOB,K),K=1,3)
         READ (LU,*) (GHI(IOB,K),K=1,3)
         READ (LU,*) ITEMP
         IF (ITEMP.EQ.0) THEN
            SINK(IOB)=.FALSE.
         ELSE IF (ITEMP.EQ.1) THEN
            SINK(IOB)=.TRUE.
         ELSE
            WRITE (LUSTOT,80) IOB
80          FORMAT (/' Illegal sink/source flag for obstacle ',I2/
     1              ' RUN ABORTED')
            STOP
	  END IF
90    CONTINUE

      RETURN
      END


      SUBROUTINE GEOMSV(LU,NVMAX,NZMAX,NOBMAX,NV,VX,VY,COARSE,NVXFE,
     1                  NVXRS,NVXRE,NGZ,GZ,COARSZ,NOB,TYPE,GLO,GHI,SINK)
C
C  Routine to write the vertex information to a file for future use
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LU,NVMAX,NZMAX,NOBMAX,NV,NVXFE,NVXRS,NVXRE,NGZ,NOB,IZ,
     1        IOB,K
      REAL VX(NVMAX),VY(NVMAX),GZ(NZMAX),GLO(NOBMAX,3),GHI(NOBMAX,3)
      LOGICAL COARSE(NVMAX),COARSZ(NZMAX),SINK(NOBMAX)
      CHARACTER*1 TYPE(NOBMAX)
      INTEGER IV,ITEMP
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  COARSE  Logical array showing which vertices are on the coarse grid
C  COARSZ  Logical array showing which z grid lines are on the coarse grid
C  GHI     Array of obstacle high co-ordinates
C  GLO     Array of obstacle low co-ordinates
C  GZ      Array of z grid line co-ordinates
C  LU      Logical unit number of file being written
C  NGZ     Number of z grid lines
C  NOB     Number of obstacles
C  NOBMAX  Maximum number of obstacles allowed
C  NV      Number of vertices
C  NVMAX   Maximum number plus 1 of vertices allowed
C  NVXFE   Number of the vertex at the end of the floor
C  NVXRE   Number of the vertex at the end of the roof
C  NVXRS   Number of the vertex at the start of the roof
C  NZMAX   Maximum number of z grid lines allowed
C  SINK    Logical array indicating which obstacles are sinks
C  TYPE    Array of obstacle types (B=box, C=cylinder)
C  VX      Array of x co-ordinates of vertices
C  VY      Array of y co-ordinates of vertices
C
C**** Arguments - output
C  NONE
C
C**** Local variables
C  IOB     Loop counter over the obstacles
C  ITEMP   Temporary integer flag used to set COARSE
C  IV      Loop counter over the vertices
C  IZ      Loop counter over the z slices
C  K       Loop counter over co-ordinate directions
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C**** Write vertex co-ordinates and coarse grid flags
      WRITE (LU,10) NV
10    FORMAT (1X,I3,36X,'NO. OF VERTICES')
      DO 40 IV=1,NV
         WRITE (LU,20) VX(IV),VY(IV),IV
20       FORMAT (1X,F10.6,2X,F10.6,17X,'CO-ORDS OF VERTEX ',I3)
         ITEMP=0
         IF (COARSE(IV)) ITEMP=1
         WRITE (LU,30) ITEMP
30       FORMAT (1X,I1,38X,'COARSE GRID FLAG 1=Y,0=N')
40    CONTINUE

C**** Write floor & roof start & ends
      WRITE (LU,50) NVXFE
50    FORMAT(1X,I3,36X,'VERTEX AT END OF FLOOR')
      WRITE (LU,60) NVXRS
60    FORMAT (1X,I3,36X,'VERTEX AT START OF ROOF')
      WRITE (LU,70) NVXRE
70    FORMAT (1X,I3,36X,'VERTEX AT END OF ROOF')


C**** Write z grid information
      WRITE (LU,80) NGZ-1
80    FORMAT (1X,I3,36X,'NO. OF SLICES')
      DO 100 IZ=1,NGZ-1
         WRITE (LU,90) GZ(IZ+1)-GZ(IZ),IZ
90	  FORMAT (1X,F10.6,29X,'THICKNESS OF SLICE ',I2)
100   CONTINUE
      DO 120 IZ=2,NGZ-1
         ITEMP=0
	  IF (COARSZ(IZ)) ITEMP=1
	  WRITE (LU,110) ITEMP
110	  FORMAT (1X,I1,38X,'COARSE GRID FLAG 1=Y,0=N')
120   CONTINUE

C**** Write obstacle information
      WRITE (LU,130) NOB
130   FORMAT (1X,I3,36X,'NO. OF OBSTACLES')
      DO 180 IOB=1,NOB
         WRITE (LU,140) TYPE(IOB),IOB
140	  FORMAT (1X,A1,38X,'OBSTACLE ',I2,' TYPE')
         WRITE (LU,150) (GLO(IOB,K),K=1,3),IOB
150	  FORMAT (1X,3(F10.6,2X),3X,'LOW CO-ORDS OF OBSTACLE ',I2)
         WRITE (LU,160) (GHI(IOB,K),K=1,3),IOB
160	  FORMAT (1X,3(F10.6,2X),3X,'HIGH CO-ORDS OF OBSTACLE ',I2)
         ITEMP=0
	  IF (SINK(IOB)) ITEMP=1
	  WRITE (LU,170) ITEMP
170      FORMAT (1X,I1,38X,'SINK/SOURCE FLAG 1=SINK')
180   CONTINUE

      RETURN
      END


      SUBROUTINE RAYDEN(LUSTIN,LUSTOT,RAYDEF,TOTRAY,NZONES,ACTZON,MXZN,
     1                  ZNAREA,NRAYS,TRAYS)
C
C  Routine to determine the ray density for the Monte Carlo calculations
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**17/06/91
C
C
      INTEGER LUSTIN,LUSTOT,TOTRAY,NZONES,ACTZON(MXZN),MXZN,NRAYS,
     1        TRAYS
      REAL ZNAREA(MXZN)
      LOGICAL RAYDEF
      INTEGER IZONE,ISURF,IRYOPT,NRAYTT
      REAL SFAREA
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  ACTZON  Array giving the zones numbers of those surface zones with 
C          non-zero area
C  LUSTIN  Logical unit number for standard input
C  LUSTOT  Logical unit number for standard output
C  MXZN    Maximum number of zones permitted
C  NZONES  Number of surface zones with non-zero area
C  RAYDEF  Logical flag indicating if the default option is selected for
C          the ray density (default option is TOTRAY/(total surface area))
C  TOTRAY  Total number of rays to be traced in default option
C  ZNAREA  Array of surface zones areas
C
C**** Arguments - output
C  NRAYS   Ray density (ie number of rays per unit area)
C  TRAYS   Total number of rays fired from surfaces
C
C**** Local variables
C  IRYOPT  Ray density option (for non-default case) 1-fix total number of rays
C          2-fix the ray density
C  ISURF   Number of current surface zone
C  IZONE   Loop counter on surface zones
C  NRAYTT  User specified total number of rays (in non-default case)
C  SFAREA  Total surface area of all surface zones
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C**** Determine the total surface area
      SFAREA=0.
      DO 10 IZONE=1,NZONES
         ISURF=ACTZON(IZONE)
         SFAREA=SFAREA+ZNAREA(ISURF)
10    CONTINUE

C**** Determine ray density
20    IF (RAYDEF) THEN
C****    Default option
         NRAYS=NINT(REAL(TOTRAY)/SFAREA)
         TRAYS=TOTRAY
      ELSE
C****    Non-default option
30       WRITE(LUSTOT,40) SFAREA
40       FORMAT (//' RAY DENSITY SPECIFICATION'//
     1           ' Total surface area (enclosure plus obstacles) is',
     2           G12.4/' Options to set the ray density'/
     3           '     1. Specify the total number of rays to be fired'/
     4           '     2. Specify the ray density directly'/
     5           ' Enter option number >> ',$)
         READ (LUSTIN,*) IRYOPT
         IF ((IRYOPT.LT.1).OR.(IRYOPT.GT.2)) THEN
            WRITE (LUSTOT,50)
50          FORMAT (/' INVALID RESPONSE'/' Answer must be 1 or 2'/)
            GOTO 30
         ELSE IF (IRYOPT.EQ.1) THEN
            WRITE (LUSTOT,60)
60          FORMAT (/' Enter total number of rays >> ',$)
            READ (LUSTIN,*) NRAYTT
            NRAYS=NINT(REAL(NRAYTT)/SFAREA)
            TRAYS=NRAYTT
         ELSE
            WRITE (LUSTOT,70)
70          FORMAT (/' Enter ray density >> ',$)
            READ (LUSTIN,*) NRAYS
	    TRAYS=NINT(SFAREA*REAL(NRAYS))
         END IF
      END IF

C**** Check that the ray density is not zero
      IF (NRAYS.EQ.0) THEN
         WRITE (LUSTOT,80)
80       FORMAT (/' REQUESTED RAY DENSITY IS ZERO'/
     1           ' This means no rays will be fired anywhere'/
     2           ' Reset the ray density before continuing'/)
         RAYDEF=.FALSE.
         GOTO 20
      END IF

      RETURN
      END

