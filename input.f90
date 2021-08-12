!-----------------------------------------------------------------
!                        input.f90  -  description
!                     ------------------------------
!          begin    : Mon 01.08.2016
!          email    : sk1916@ic.ac.uk, skournopoulos@msn.com
!-----------------------------------------------------------------
! This module is used to read and store all input variables used
! by the main program.
!
! Until an input file is prepared and tested this module will only
! contain variables that are user-defined.
!
!-----------------------------------------------------------------
!
! 10.08.2016      Created               Skou
! 15.08.2016      Added input file      Skou
!
!-----------------------------------------------------------------
Module Input
    Use nrtype
    Implicit None

    Integer(I4B) :: IFluid  = 0 ! 1 - SW
                                ! 2 - LJ

    Integer(I4B) :: IMethod = 0 ! 1 - 1st order PT
                                ! 2 - Lambda DFT

    Integer(I4B) :: NTermsCP = 0 !  number if terms to calculate for coupling parameter method
    Real(DP)     :: relaxation   ! multiplied with delta to relax the Newton method (1 is the default value)\
    Real(DP)     :: tolerance    ! tolerance of the numerical method

    ! temperature - initial/final value and increment
    Real(DP)     :: T_init = ZERO ! Τ = 146.4 Τ*
    Real(DP)     :: T_end  = ZERO
    Real(DP)     :: T_inc  = ZERO

    ! initial guesses for packing fractions. they correspond to T_init
    Real(DP)     :: eta_l_init = ZERO   ! eta = rho * 1.3703744
    Real(DP)     :: eta_g_init = ZERO

    ! fluid parameters
    Real(DP)     :: sigma       = ZERO            ! meters
    Real(DP)     :: eps_over_k  = ZERO            ! epsilon over k in Kelvin
    Real(DP)     :: range_attr  = ZERO
    Real(DP)     :: range_rep   = ZERO
    Real(DP)     :: part_mass   = ZERO            ! mass of one particle (eg methane) in kg

    ! output file path and name
    Character    :: CFileOutput*240

contains

    !-----------------------------------------------------------
    !  Read input file. Check input variables for consistency
    !-----------------------------------------------------------
    Subroutine ReadInput
        Implicit None
        Integer(I4B)   :: NLinesRead, IUn

        NLinesRead = 0
        IUn        = NGetFileUnit()
        open(IUn, File='input_phaseEq.txt', status='OLD', ERR=67)

        call ReadCommentLines(IUn, 3)
        NLinesRead  = NLinesRead + 3

        read(IUn, *, ERR=66) CFileOutput
        NLinesRead = NLinesRead + 1

        call ReadCommentLines(IUn, 8)
        NLinesRead  = NLinesRead + 8

        read(IUn, *, ERR=66) IFluid, IMethod, NTermsCP, relaxation, tolerance
        NLinesRead  = NLinesRead + 1

        if ( (IFluid  < 1).OR.(IFluid  > 2) ) STOP "ReadInput: ERROR - IFluid options are 1 (SW) or 2 (LJ)"
        if ( (IMethod < 1).OR.(IMethod > 2) ) STOP "ReadInput: ERROR - IMethod options are 1 (1st order pert theory) &
                                                                       or 2 (lambda method)"
        call ReadCommentLines(IUn, 5)
        NLinesRead  = NLinesRead + 5

        read(IUn, *, ERR=66) T_init, T_end, T_inc, eta_l_init, eta_g_init
        NLinesRead  = NLinesRead + 1

        if (.NOT. (SIGN(ONE,T_end - T_init) == SIGN(ONE,T_inc))) &
            STOP "ReadInput: ERROR - Change the sign of T_inc or the order of T_init, T_end"

        call ReadCommentLines(IUn, 5)
        NLinesRead  = NLinesRead + 5

        read(IUn, *, ERR=66) sigma, eps_over_k, range_rep, range_attr, part_mass
        NLinesRead  = NLinesRead + 1

        ! units transformation to SI
        sigma      = sigma*1.D-10

        close(IUn)

        return

        66 print*, 'ReadInput: Read Error - LinesRead ', NLinesRead ; close(IUn) ; STOP
        67 print*, 'ReadInput: Open  Problem'                       ; close(IUn) ; STOP
    End Subroutine ReadInput


    !-----------------------------------------------------------
    !  Read a number of comment lines from a given file
    !-----------------------------------------------------------
    Subroutine ReadCommentLines(IFileUnit, NLines)
        Implicit None
        Integer(I4B) :: IFileUnit, NLines, i ; Character :: CDummy*1
        Intent(IN)   :: IFileUnit, NLines
        do i=1, NLines  ;  read(IFileUnit,*) CDummy  ;  enddo
    End Subroutine ReadCommentLines

    !-----------------------------------------------------------
    ! Give sequential file unit numbers on each call,
    ! starting from "NFileUnit"
    !-----------------------------------------------------------
    Function NGetFileUnit()
        Implicit None
        Integer(I4B)       :: NGetFileUnit
        Integer(I4B), Save :: NFileUnit = 20
        NGetFileUnit = NFileUnit
        NFileUnit    = NFileUnit + 1
    End Function NGetFileUnit

End Module Input
