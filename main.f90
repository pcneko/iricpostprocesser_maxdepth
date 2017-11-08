!-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-!
!                   IRIC postprocess                              !
!   max water surface elevation searcher by R.Kaneko              !
!                                (7616607@ed.tus.ac.jp)           !
!-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-!
program main
    implicit none

    integer,parameter                    :: num_c                     = 23,&
                                         &x_c                       =  3,&
                                         &y_c                       =  4,&
                                         &depth_c                   =  5,&
                                         &elevation_c               =  6,&
                                         &water_surface_elavation_c =  7,&
                                         &elevationchange_c         =  9
    integer                              :: i, j, file_num, inum, jnum, totalnum, dfn, dfn_min2max, num_file_max, num_file_min
    real                                 :: depth_threshold
    real, allocatable, dimension(:)      :: max_wse, max_depth, max_elevation, max_elevationchange, hit_num
    integer, allocatable, dimension(:,:) :: hit_pos
    real,dimension(num_c)                :: value
    character(len= 3)                    :: number
    character(len=19)                    :: file_name


! depth, elevation, elevationchange, 
! こいつ求めたい → watersurfaceelavation

! depth <= 0.01 
! depth = ゴミ
    write(*,*)("*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*")
    write(*,*)("*                   IRIC postprocess                              *")
    write(*,*)("*      max water surface elevation searcher by R.Kaneko           *")
    write(*,*)("*                 (^o^)ノ          (7616607@ed.tus.ac.jp)         *")
    write(*,*)("*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*")
    
    open( 20, file = 'input.cfg', status = 'old' )
    read(20,*)
    read(20,*)num_file_min
    read(20,*)
    read(20,*)num_file_max
    read(20,*)
    read(20,*)depth_threshold
    close(20)
    write(*,*)""
    write(*,*)"num_file_min = ", num_file_min
    write(*,*)"num_file_max = ", num_file_max
    write(*,*)"depth_threshold = ", depth_threshold
    write(*,*)""
    write(*,*)"-*-*-*-*-*-*-*- Calc Process Start -*-*-*-*-*-*-*-"

    i = 0
    j = 0
    dfn_min2max = num_file_max - num_file_min

    write(number,'(i3.3)')num_file_min
    file_name = "data/Result_"//number//".csv"
    open(10, file = file_name, status ='old')
    read(10,*)
    read(10,*)inum,jnum
    read(10,*)
    totalnum = inum * jnum
    close(10)
    allocate(max_wse(totalnum), max_depth(totalnum), max_elevation(totalnum), &
            &max_elevationchange(totalnum), hit_num(totalnum), hit_pos(2,totalnum))


    do file_num = num_file_min, num_file_max
        write(number,'(i3.3)')file_num
        file_name = "data/Result_"//number//".csv"
        open(10, file = file_name, status = 'old')
        
        do j = 1, 3
            read(10,*)
        enddo

        j = 1 ! loop init
        do while(.true.)
            read(10, *, end = 900)(value(i),i=1,num_c)
            if(value(depth_c) .gt. depth_threshold)then
                if( value(water_surface_elavation_c) .gt. max_wse(j))then
                    max_wse(j) = value(water_surface_elavation_c)
                    max_depth(j) = value(depth_c)
                    max_elevation(j) = value(elevation_c)
                    max_elevationchange(j) = value(elevationchange_c)
                    hit_num(j) = file_num
                    hit_pos(1,j) = value(x_c)
                    hit_pos(2,j) = value(y_c)
                endif
            endif
            j = j + 1            
        enddo ! J loop
900 continue
    dfn = file_num - num_file_min
    if ( mod(dfn,2) == 0 )then
        call progress_bar(dfn, dfn_min2max)
    endif
    close(10)
    enddo     ! File loop
    write(*,*)"-*-*-*-*-*-*-*- Calc Process End -*-*-*-*-*-*-*-"
    write(*,*)""

    ! output process
    write(*,*)"-*-*-*-*-*-*-*- Output Process Start -*-*-*-*-*-*-*-"
    open( 30, file = "calc_result.csv", status = 'replace' )
    do j = 1, totalnum
        ! write(*,*)hit_num,hit_pos(1,j),hit_pos(2,j),max_depth(j),max_elevation(j),max_wse(j),max_elevationchange(j)
        write( 30, '(3((i2.2),","),(f17.16),",",(f13.2),",",(f13.2),",",(f12.6))')&
                               &hit_num(j),(hit_pos(i,j),i=1,2),max_depth(j),&
                               &max_elevation(j),max_wse(j),max_elevationchange(j)
    enddo
    close(30)
    write(*,*)"-*-*-*-*-*-*-*- Output Process End -*-*-*-*-*-*-*-"
    

contains

    
    subroutine progress_bar(idx, total) ! dfn means "delta file number" (^^)
        integer           :: idx, total, i
        real              :: progress
        progress = real(idx)/real(total)
        write(*,'(a,$)')"["
        do i = 1, 30*progress
            write(*,'(a,$)')"="
        enddo
        write(*,'(a,$)')"ε≡ﾍ( ´Д`)ﾉ"
        do i = 1, 30*(1-progress)
            write(*,'(a,$)')" "
        enddo
        write(*,'(a,$)')"]"
        write(*,'(f5.1,$)')progress*100
        write(*,'(2a,$)')' percent',char(13)
        if(idx.eq.total)write(*,*),''
    end subroutine

end program main