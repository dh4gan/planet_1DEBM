  PROGRAM time_average
    !Program time_averages output from EBM
    integer :: i,j,k,nx,step, dumpcount,dumpcheck,nfiles,nvar
    real :: t_interval,dumpfreq,t_initial

    real,allocatable,dimension (:) :: x, lat,time
    real,allocatable,dimension(:,:) :: mean,stdev
    real,allocatable,dimension(:,:,:) :: data

    character(len=100):: prefix,fileno,filename

    ! Read in inputs

    print*, 'What is the averaging interval (in years)?'
    read*, t_interval

    print*, 'What is the file prefix?'
    read*, prefix

    print*, 'Which file number to begin from?'
    read*, dumpcount

    prefix = TRIM(prefix)

    nvar = 10

    IF(dumpcount<=9) THEN
       WRITE(fileno,'(I1)') dumpcount
    ELSE IF(dumpcount>9.and.dumpcount<=99) THEN
       WRITE(fileno,'(I2)') dumpcount
    ELSE IF(dumpcount>99.and.dumpcount<=999) THEN
       WRITE(fileno,'(I3)') dumpcount
    ELSE IF(dumpcount>999.and.dumpcount<=9999) THEN
       WRITE(fileno,'(I4)') dumpcount
    ELSE IF(dumpcount>9999.and.dumpcount<=99999) THEN
       WRITE(fileno,'(I5)') dumpcount
    ENDIF

    fileno = TRIM(fileno)
    print*, fileno
    filename = TRIM(prefix)//'.'//fileno
    print*, filename

    ! Open initial file

    OPEN(10,file=filename,status='old')
    READ(10,*) nx,t_initial, step, dumpcheck, dumpfreq

    CLOSE(10)

    ! Calculate how many file reads are required based on time_interval and dump frequency

    nfiles = NINT(t_interval/dumpfreq)+1

    print*, nfiles, ' files to be read'

    allocate(x(nx))
    allocate(lat(nx))
    allocate(time(nfiles))
    
    ! Data ordering as in output file
    !    1: T
    !    2: C
    !    3: Q
    !    4: I
    !    5: A
    !    6: S
    !    7: tau
    !    8: f_ice

    allocate(data(nvar,nx,nfiles))

    ! Now read all the files in order

    dumpcount = dumpcount-1
    DO j=1,nfiles
       dumpcount = dumpcount + 1

       IF(dumpcount<=9) THEN
          WRITE(fileno,'(I1)') dumpcount
       ELSE IF(dumpcount>9.and.dumpcount<=99) THEN
          WRITE(fileno,'(I2)') dumpcount
       ELSE IF(dumpcount>99.and.dumpcount<=999) THEN
          WRITE(fileno,'(I3)') dumpcount
       ELSE IF(dumpcount>999.and.dumpcount<=9999) THEN
          WRITE(fileno,'(I4)') dumpcount
       ELSE IF(dumpcount>9999.and.dumpcount<=99999) THEN
          WRITE(fileno,'(I5)') dumpcount
       ENDIF
       
       fileno = TRIM(fileno)
     !  print*, fileno
       filename = TRIM(prefix)//'.'//fileno
       print*, filename
       
    ! Open file
       
       OPEN(10,file=filename,status='old')
       READ(10,*) nx,time(j), step, dumpcheck,dumpfreq
       
       DO i=1,nx
          READ(10,*) x(i), lat(i), (data(k,i,j),k=1,nvar)
       ENDDO
       CLOSE(10) 
       
       

    ENDDO ! End of loop over files
    
    print*, 'Files Read'
    print*, 'Total elapsed simulation time is: ', time(nfiles)-time(1), ' years'
    print*, 'Calculating averages'
    ! Now calculate averages

    allocate(mean(nvar,nx))
    allocate(stdev(nvar,nx))

    mean(:,:) = 0.0
    
    do k=1,nvar
       do i=1,nx
          do j=1,nfiles
             mean(k,i) = mean(k,i) + data(k,i,j)/REAL(nfiles)
          enddo
       enddo
    enddo

    stdev(:,:) = 0.0

    do k=1,nvar
       do i=1,nx
          do j=1,nfiles
             stdev(k,i) = stdev(k,i) + (data(k,i,j)-mean(k,i))**2/REAL(nfiles)
          enddo
       enddo
    enddo

    stdev(:,:) = SQRT(stdev(:,:))

    mean(6,:) = mean(6,:)*(1.0-mean(5,:))

    OPEN(20,file=TRIM(prefix)//'.av', status='unknown')
   WRITE(20,*) nx, time(1), time(nfiles), nfiles, t_interval

    DO i=1,nx
       WRITE(20,*) x(i), lat(i), (mean(k,i),k=1,nvar),(stdev(k,i),k=1,nvar)
    ENDDO

    CLOSE(20)
END PROGRAM time_average
