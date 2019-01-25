      Program prgm_01_02
!
!     This program reads a 3x3 matrix from a user-provided input file. After the
!     file is opened and read, it is closed and then printed.
!
!     H. P. Hratchian, 2019.
!
      implicit none
      integer,parameter::inFileUnitA=10,inFileUnitB=11
      integer::errorFlag,i
      real,dimension(3,3)::matrixInA,matrixInB
      character(len=128)::fileNameA,fileNameB
!
!
!     Start by asking the user for the name of the data file.
!
      write(*,*)' What is the name of the input data file A?'
      read(*,*) fileNameA
      write(*,*)' What is the name of the input data file B?'
      read(*,*) fileNameB
!
!     Open the data file and read matrixInA from that file.
!
      open(unit=inFileUnitA,file=TRIM(fileNameA),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the input file.'
        goto 999
      endIf
      do i = 1,3
        read(inFileUnitA,*) matrixInA(i,1),matrixInA(i,2),matrixInA(i,3)
      endDo
      close(inFileUnitA)
!
!     Copied A above and changed variables to B
!
      open(unit=inFileUnitB,file=TRIM(fileNameB),status='old',  &
        iostat=errorFlag)
      if(errorFlag.ne.0) then
        write(*,*)' There was a problem opening the input file.'
        goto 999
      endIf
      do i = 1,3
        read(inFileUnitB,*) matrixInB(i,1),matrixInB(i,2),matrixInB(i,3)
      endDo
      close(inFileUnitB)
!
!     Call the subroutine PrintMatrix to print matrixInA.
!
      call PrintMatrix3x3(matrixInA)
      call PrintMatrix3x3(matrixInB)
!
  999 continue
      End Program prgm_01_02


      Subroutine PrintMatrix3x3(matrix)
!
!     This subroutine prints a 3x3 real matrix. The output is written to StdOut.
!
      implicit none
      real,dimension(3,3),intent(in)::matrix
      integer::i
!
!     Format statements.
!
! 1000 format(3(2x,f5.1))
!
!     Do the printing job.
!
      write(*,*)' Printing Matrix'
!
! ADD CODE HERE
!
      write(*,'(3(2x,f5.1))') matrix
      return
      End Subroutine PrintMatrix3x3
