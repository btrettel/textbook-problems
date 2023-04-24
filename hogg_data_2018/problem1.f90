! # $File$
! 
! Summary: 
! Standard: Fortran 2003
! Author: Ben Trettel (<http://trettel.us/>)
! Last updated: $Date$
! Revision: $Revision$
! Project: [textbook-problems]()
! License: [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)

program problem1
    ! Table of contents
    ! -----------------
    ! 
    ! 1. Set modules and other boilerplate
    ! 2. Declare parameters
    ! 3. Declare variables
    ! 4. Initialize variables
    ! 5. First part
    ! 6. Second part
    
    ! 1. Set modules and other boilerplate
    ! ------------------------------------
    
    implicit none ! (type, external) ! for Fortran 2018 and later
    
    ! 2. Declare parameters
    ! ---------------------
    
    ! <https://fortranwiki.org/fortran/show/Real+precision>
    ! `wp` stands for "working precision" in case I want to change the
    ! precision later.
    !integer, parameter :: wp = selected_real_kind(6, 37) ! single
    integer, parameter :: wp = selected_real_kind(15, 307) ! double
    !integer, parameter :: wp = selected_real_kind(33, 4931) ! quad
    
    ! 3. Declare variables
    ! --------------------
    
    real(kind=wp) :: x ! description of variable (units)
    
    ! 4. Initialize variables
    ! -----------------------
    
    
    
    ! 5. First part
    ! -------------
    
    
    
    ! 6. Second part
    ! --------------
    
    
contains
    function mean(x) result (array) !
        ! description
        type(real), dimension(:) intent(in) :: array
        type(real) :: mean
        
        ! TODO: use sum and size
    end function mean
end program problem1
