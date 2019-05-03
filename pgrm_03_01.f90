      program pgrm_03_01
!
!     This program computes the inverse square-root of a matrix.
!
!     At execution time, the program expects 2 command line arguments: (1) nDim;
!     and (2) the matrix being raised to the (1/2) power.
!
!
      implicit none
      integer,parameter::unitIn=10
      integer::i,iError,nDim,lenSym
      real,dimension(:),allocatable::inputSymMatrix
      real,dimension(:,:),allocatable::inputSqMatrix,invSqrtInputMatrix
      character(len=256)::cmdlineArg
!
!
!     Begin by reading the leading dimension of the matrix and the input file
!     name from the command line. Then, open the file and read the input matrix,
!     inputSymMatrix
!
      call Get_Command_Argument(1,cmdlineArg)
      read(cmdlineArg,'(I1)') nDim
      lenSym = (nDim*(nDim+1))/2
      allocate(inputSymMatrix(lenSym),inputSqMatrix(nDim,nDim),  &
        invSqrtInputMatrix(nDim,nDim))
      call Get_Command_Argument(2,cmdlineArg)
      open(Unit=unitIn,File=TRIM(cmdlineArg),Status='OLD',IOStat=iError)
      if(iError.ne.0) then
        write(*,*)' Error opening input file.'
        STOP
      endIf
      do i = 1,lenSym
        read(unitIn,*) inputSymMatrix(i)
      endDo
      close(Unit=unitIn)
!
!     Form the square-root of inputSymMatrix. The result is loaded into square
!     (full storage) matrix invSqrtInputMatrix.
!
      write(*,*)' The matrix loaded (column) upper-triangle packed:'
      call SymmetricPacked2Matrix_UpperPac(nDim,inputSymMatrix,  &
        inputSqMatrix)
      write(*,*)' Input Matrix:'
      call Print_Matrix_Full_Real(inputSqMatrix,nDim,nDim)
      call InvSQRT_SymMatrix(nDim,inputSymMatrix,invSqrtInputMatrix)
      write(*,*)' Inverse SQRT Matrix:'
      call Print_Matrix_Full_Real(invSqrtInputMatrix,nDim,nDim)
      write(*,*)' Matrix product that should be the identity.'
      call Print_Matrix_Full_Real(MatMul(MatMul(invSqrtInputMatrix,  &
        inputSqMatrix),invSqrtInputMatrix),nDim,nDim)
!
      end program pgrm_03_01

      Subroutine InvSQRT_SymMatrix(nDim,inputSymMatrix,  &
        invSqrtInputMatrix)
!
      implicit none
      integer :: nDim, i 
      real :: Ierror
      real, Dimension((nDim*(nDim+1)/2)) :: inputSymMatrix,input_copy
      real, Dimension(NDim) :: Eval
      real,Dimension(nDim,nDim) :: invSqrtInputMatrix,Evec,EvecT,EvalMat
      real,dimension(nDim,nDim) :: Mat
      real, Dimension(nDim*3) :: temp_vec
!
      input_copy = inputSymMatrix
!
      Call SSPEV('V','U',nDim,inputSymMatrix,Eval,Evec,nDim, &
        temp_vec,Ierror)
!
!      call Print_Matrix_Full_Real(Evec,nDim,nDim)
      EvecT = transpose(Evec)
!      call Print_Matrix_Full_Real(EvecT,nDim,nDim)
!
      do i = 1,nDim
        EvalMat(i,i) = 1/(sqrt(Eval(i)))
      end do
!
!      print*, EvalMat
      Mat = MatMul(Evec,MatMul(EvalMat,EvecT))
!      print*, Mat
      invSqrtInputMatrix = Mat
!
      End Subroutine InvSQRT_SymMatrix

      Subroutine Print_Matrix_Full_Real(AMat,M,N)
!
!     This subroutine prints a real matrix that is fully dimension - i.e.,
!     not stored in packed form. AMat is the matrix, which is dimensioned
!     (M,N).
!
!     The output of this routine is sent to unit number 6 (set by the local
!     parameter integer IOut).
!
!
!     Variable Declarations
!
      implicit none
      integer,intent(in)::M,N
      real,dimension(M,N),intent(in)::AMat
!
!     Local variables
      integer,parameter::IOut=6,NColumns=5
      integer::i,j,IFirst,ILast
!
 1000 Format(1x,A)
 2000 Format(5x,5(7x,I7))
 2010 Format(1x,I7,5F14.6)
!
      Do IFirst = 1,N,NColumns
        ILast = Min(IFirst+NColumns-1,N)
        write(IOut,2000) (i,i=IFirst,ILast)
        Do i = 1,M
          write(IOut,2010) i,(AMat(i,j),j=IFirst,ILast)
        endDo
      endDo
!
      Return
      End Subroutine Print_Matrix_Full_Real

      Subroutine SymmetricPacked2Matrix_UpperPac(N,ArrayIn,AMatOut)
!
!     This subroutine accepts an array, ArrayIn, that is (N*(N+1))/2 long.
!     It then converts that form to the N-by-N matrix AMatOut taking
!     ArrayIn to be in upper-packed storage form. Note: The storage mode
!     also assumes the upper-packed storage is packed by columns.
!
      Implicit None
      Integer,Intent(In)::N
      Real,Dimension((N*(N+1))/2),Intent(In)::ArrayIn
      Real,Dimension(N,N),Intent(Out)::AMatOut
!
      Integer::i,j,k
!
!     Loop through the elements of AMatOut and fill them appropriately from
!     Array_Input.
!
      k = size(ArrayIn)
      do i = N,1,-1 
        do j = i,1,-1
          AMatOut(i,j) = ArrayIn(k)
          if (i.ne.j) then
            AMatOut(j,i) = ArrayIn(k)
          end if
          k = k-1
        end do
      end do
!
!
!
      Return
      End Subroutine SymmetricPacked2Matrix_UpperPac
