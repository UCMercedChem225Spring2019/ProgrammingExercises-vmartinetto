      program pgrm_03_03
!
!     This program computes components of the Hartree-Fock energy and the number
!     of electrons using contraction of various matrices loaded from
!     user-provided files (that presumably come from Gaussian calculations).
!
!     At run time, the program expects 5 command line arguments:
!       (1) the number of electrons;
!       (2) the number of atomic orbital basis functions;
!       (3) an input file containing core-Hamiltonian matrix elements (in
!           symmetric upper/column storage form);
!       (4) an input file containing the overlap matrix elements (in
!           symmetric upper/column storage form); and
!       (5) an input file containing Fock matrix elements (in symmetric
!           upper/column storage form).
!
!     At run time, the program outputs 6 items:
!       (1) the MO energies;
!       (2) the MO coefficients;
!       (3) the density matrix;
!       (4) the one-electron contribution to the total electronic energy;
!       (5) the two-electron contribution to the total electronic energy; and
!       (6) the tr(PS), which should equal the number of electrons.
!
!
!
      implicit none
      integer,parameter::unitIn=10
      integer::i,iError,nElectrons,nOcc,nBasis,lenSym
      real::oneElectronEnergy,twoElectronEnergy,tracePS
      real,dimension(:),allocatable::symFock,symCoreHamiltonian, &
        symOverlap,moEnergies,tempSymMatrix
      real,dimension(:,:),allocatable::sqFock,fockTilde,  &
        sqCoreHamiltonian,InvSqrtOverlap,moCoefficients,densityMatrix, &
        tempSqMatrix,SSPEV_Scratch
      character(len=256)::cmdlineArg
!
!
!     Begin by reading the number of basis functions, allocating array memory,
!     and loading the symmetric Fock and overlap matrices from input files
!     provided on the command line.
!
      call Get_Command_Argument(1,cmdlineArg)
      read(cmdlineArg,'(I)') nElectrons
      nOcc = nElectrons/2
!
      call Get_Command_Argument(2,cmdlineArg)
      read(cmdlineArg,'(I)') nBasis
      lenSym = (nBasis*(nBasis+1))/2
      allocate(symFock(lenSym),symCoreHamiltonian(lenSym),  &
        symOverlap(lenSym),tempSymMatrix(lenSym))
      allocate(moEnergies(nBasis))
      allocate(sqFock(nBasis,nBasis),fockTilde(nBasis,nBasis),  &
        sqCoreHamiltonian(nBasis,nBasis),invSqrtOverlap(nBasis,nBasis),&
        moCoefficients(nBasis,nBasis),densityMatrix(nBasis,nBasis),  &
        tempSqMatrix(nBasis,nBasis),SSPEV_Scratch(nBasis,3))
!      
      call Get_Command_Argument(3,cmdlineArg)
      Open(Unit=unitIn,File=TRIM(cmdlineArg),Status='OLD',IOStat=IError)
      If(IError.ne.0) then
        Write(*,*)' Error opening input file.'
        STOP
      endIf
      do i = 1,((nBasis*(nBasis+1))/2)
        read(unitIn,*) symCoreHamiltonian(i)
      end do
      close(unit=unitIn)
!
      call Get_Command_Argument(4,cmdlineArg)
      Open(Unit=unitIn,File=TRIM(cmdlineArg),Status='OLD',IOStat=IError)
      If(IError.ne.0) then
        Write(*,*)' Error opening input file.'
        STOP
      endIf
      do i = 1,((nBasis*(nBasis+1))/2)
        read(unitIn,*) symOverlap(i)
      end do
      close(unit=unitIn)
!      
      call Get_Command_Argument(5,cmdlineArg)
      Open(Unit=unitIn,File=TRIM(cmdlineArg),Status='OLD',IOStat=IError)
      If(IError.ne.0) then
        Write(*,*)' Error opening input file.'
        STOP
      endIf
      do i = 1,((nBasis*(nBasis+1))/2)
        read(unitIn,*) symFock(i)
      end do
      close(unit=unitIn)
!
!     Form the square-root of the overlap matrix.
!
      tempSymMatrix = symOverlap
      call InvSQRT_SymMatrix(nBasis,symOverlap,invSqrtOverlap)
      symOverlap = tempSymMatrix
!
!     Form fTilde and solve for the MO energies and coefficients.
!
      call SymmetricPacked2Matrix_UpperPac(nBasis,symFock,sqFock)
      tempSqMatrix = MatMul(invSqrtOverlap,sqFock)
      fockTilde = MatMul(tempSqMatrix,invSqrtOverlap)
      call Sq2SymMatrix(nBasis,fockTilde,tempSymMatrix)
      call SSPEV('V','U',nBasis,tempSymMatrix,moEnergies,  &
        moCoefficients,nBasis,SSPEV_Scratch,iError)
      If(iError.ne.0) then
        write(*,*)' Failure in SSPEV.'
        STOP
      endIf
      write(*,*)' MO Energies:'
      call Print_Matrix_Full_Real(Reshape(moEnergies,[nBasis,1]), &
        nBasis,1)
      tempSqMatrix = moCoefficients
      moCoefficients = MatMul(invSqrtOverlap,tempSqMatrix)
      write(*,*)' MO Coefficients:'
      call Print_Matrix_Full_Real(moCoefficients,nBasis,nBasis)

!
!
!
      densityMatrix = Matmul(moCoefficients,transpose(moCoefficients))
      write(*,*) 'Density Matrix:'
      call Print_Matrix_Full_Real(densityMatrix,nBasis,nBasis)
!
      call SymmetricPacked2Matrix_UpperPac(nBasis,symCoreHamiltonian,  &
        sqCoreHamiltonian)
      tempSqMatrix = Matmul(sqCoreHamiltonian,densityMatrix)
      oneElectronEnergy = 0
      do i = 1,nBasis
        oneElectronEnergy = oneElectronEnergy + tempSqMatrix(i,i)
      end do
      write(*,*)' The one electron energy cntribution is:'
      write(*,*) oneElectronEnergy
!
      tempSqMatrix = sqFock - sqCoreHamiltonian
      tempSqMatrix = matmul(tempsqMatrix,densityMatrix)
      twoElectronEnergy = 0 
      do i = 1,nBasis
        twoElectronEnergy = twoElectronEnergy + tempSqMatrix(i,i)
      end do 
      twoElectronEnergy = twoElectronEnergy/2
      write(*,*)' The two electron energy cntribution is:'
      write(*,*) twoElectronEnergy
!
      call SymmetricPacked2Matrix_UpperPac(nBasis,symOverlap,  &
        tempSqMatrix)
      tempSqMatrix = matmul(densityMatrix,tempSqMatrix)
      tracePS = 0
      do i = 1,nElectrons
        tracePS = tracePS + tempSqMatrix(i,i)
      end do
      write(*,*)' The tr(PS) is:'
      write(*,*) tracePS
      end program

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

      Subroutine Sq2SymMatrix(nDim,sqMatrix,symMatrix)
!
      implicit none
      integer :: nDim, i, j, k=1
      real, dimension(nDim,nDim) :: sqMatrix
      real, dimension((nDim*(nDim+1))/2) :: symMatrix
!
      do i = 1,nDim
        do j = 1,i
          symMatrix(k) = sqMatrix(i,j)
          k = k+1
        end do
      end do
!
      Return
      end subroutine

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
