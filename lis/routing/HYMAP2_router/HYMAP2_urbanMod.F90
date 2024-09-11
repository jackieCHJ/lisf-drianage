module HYMAP2_urbanMod
  ! 模块定义城市排水变量
contains

subroutine calc_urban_drainage(zdt, zdrstomax, rivdph, zdrsto, zrivsto, zdrvel, zdrout, zdrinf, inletarea)
    implicit none
    ! 输入参数
    real, intent(in) :: zdt         ! 时间步长
    real, intent(in) :: zdrstomax   ! 最大排水存储量
    real, intent(in) :: rivdph      ! 河道水深
    real, intent(in) :: inletarea   ! 入口参数

    ! 输入/输出参数
    real, intent(inout) :: zdrsto   ! 排水系统存储量
    real, intent(inout) :: zrivsto  ! 河道水量存储

    ! 输出参数
    real, intent(out) :: zdrvel     ! 排水速度
    real, intent(out) :: zdrout     ! 排水量
    real, intent(out) :: zdrinf     ! 入渗量

    ! 局部变量
    real :: mu, alpha
    real, parameter :: g = 9.81  ! 地球重力加速度，单位 m/s^2

    ! 根据水深和入口参数计算 mu 和 alpha
    mu = 0.385 * sqrt(rivdph)
    alpha = 0.6

    ! 计算流入管道的流量
    zdrvel = alpha * sqrt(2.0 * g * rivdph)

    ! 计算排水量
    zdrout = zdrvel * zdt * inletarea

    ! 计算入渗量
    zdrinf = mu * zdt * inletarea

    ! 更新储水量
    zdrsto = zdrsto - zdrout + zdrinf

    ! 检查是否超过最大储量
    if (zdrsto > zdrstomax) then
        zrivsto = zrivsto + (zdrsto - zdrstomax)  ! 多余的部分回到河流
        zdrsto = zdrstomax                        ! 储水量设为最大值
    elseif (zdrsto < 0) then
        zdrsto = 0  ! 确保储水量不低于 0
    end if
end subroutine calc_urban_drainage

subroutine HYMAP2_calc_urban_drain_stonxt(zdt, zdrstomax, zdrout, zdrinf, rivdph, zdrsto, zrivsto, inletarea, zdrvel)
    implicit none
    ! 输入参数
    real, intent(in) :: zdt         ! 时间步长
    real, intent(in) :: zdrstomax   ! 最大排水存储量
    real, intent(inout) :: zdrinf      ! 排水系统入流量
    real, intent(in) :: rivdph      ! 河道水深
    real, intent(in) :: inletarea   ! 入口参数

    ! 输入/输出参数
    real, intent(inout) :: zdrout   ! 排水系统流出量
    real, intent(inout) :: zdrsto   ! 排水系统存储量
    real, intent(inout) :: zrivsto  ! 河道水量存储

    ! 输出参数A
    real, intent(out) :: zdrvel     ! 排水速度

    ! 局部变量
    real :: zdrcapnow     ! 当前排水能力
    real :: zdrflwpot     ! 潜在流量
    real :: zsrfstoin     ! 表面入流量
    
    ! 更新排水系统水量存储，考虑流出和入流
    zdrsto = max(0., zdrsto - zdrout * zdt)   ! 减去流出量
    zdrsto = zdrsto + zdrinf * zdt            ! 加上入流量
    
    ! 与地表相互作用
    if (zdrsto >= zdrstomax) then
        ! 如果存储量超过最大值
        zrivsto = zrivsto + max(0., zdrsto - zdrstomax)  ! 将多余的水量转移到河道存储
        zdrsto = zdrstomax                                ! 存储量设为最大值
    else
        ! 如果存储量未达到最大值
        zdrcapnow = max(0., zdrstomax - zdrsto)           ! 计算当前排水能力
        call calc_urban_drainage(zdt, zdrstomax, rivdph, zdrsto, zrivsto, zdrvel, zdrout, zdrinf, inletarea)
    end if    

end subroutine HYMAP2_calc_urban_drain_stonxt


     
  ! subroutine HYMAP2_calc_urban_drain_stonxt(zdt,zdrvel,zdrtotwth,zdrstomax,zdrout,zdrinf,rivdph,zdrsto,zrivsto)
  !   implicit none
  !   ! 输入参数
  !   real, intent(in) :: zdt         ! 时间步长
  !   real, intent(in) :: zdrvel      ! 排水速度
  !   real, intent(in) :: zdrtotwth   ! 排水总宽度
  !   real, intent(in) :: zdrstomax   ! 最大排水存储量
  !   real, intent(in) :: zdrout      ! 排水系统流出量
  !   real, intent(in) :: zdrinf      ! 排水系统入流量
  !   real, intent(in) :: rivdph      ! 河道水深
  !   ! 输出参数
  !   real, intent(out) :: zrivsto    ! 河道水量存储
  !   real, intent(out) :: zdrsto     ! 排水系统存储量
    
  !   ! 局部变量
  !   real :: zdrcapnow     ! 当前排水能力
  !   real :: zdrflwpot     ! 潜在流量
  !   real :: zsrfstoin     ! 表面入流量
    
  !   ! 更新排水系统水量存储，考虑流出和入流
  !   zdrsto = max(0., zdrsto - zdrout * zdt)   ! 减去流出量
  !   zdrsto = zdrsto + zdrinf * zdt            ! 加上入流量
    
  !   ! 与地表相互作用
  !   if (zdrsto > zdrstomax) then
  !     ! 如果存储量超过最大值
  !     zrivsto = zrivsto + max(0., zdrsto - zdrstomax)  ! 将多余的水量转移到河道存储
  !     zdrsto = zdrstomax                                ! 存储量设为最大值
  !   else
  !     ! 如果存储量未达到最大值
  !     zdrcapnow = max(0., zdrstomax - zdrsto)           ! 计算当前排水能力
  !     zdrflwpot = min(zrivsto, rivdph * zdrvel * zdrtotwth * zdt)  ! 计算潜在流量
  !     zsrfstoin = min(zdrcapnow, zdrflwpot)             ! 计算表面入流量
  !     zrivsto = zrivsto - zsrfstoin                     ! 更新河道水量存储
  !     zdrsto = zdrsto + zsrfstoin                       ! 更新排水系统存储量
  !   endif    
  ! end subroutine HYMAP2_calc_urban_drain_stonxt
  ! ================================================               
  ! subroutine HYMAP2_calc_urb_drain_out(zdt,zdrrad,zdrman,zdrslp,zdrstomax,zdrtotlgh,zdrnoutlet,zdrsto,zdrout)
  !   implicit none
  !   ! 输入参数
  !   real, intent(in) :: zdt         ! 时间步长
  !   real, intent(in) :: zdrrad      ! 排水半径
  !   real, intent(in) :: zdrman      ! 曼宁系数
  !   real, intent(in) :: zdrslp      ! 坡度
  !   real, intent(in) :: zdrstomax   ! 最大排水存储量
  !   real, intent(in) :: zdrtotlgh   ! 排水总长度
  !   real, intent(in) :: zdrnoutlet  ! 排水出口数量
  !   real, intent(in) :: zdrsto      ! 当前存储量
  !   ! 输出参数
  !   real, intent(out) :: zdrout     ! 流出量

  !   ! 局部变量
  !   real :: zdph  ! 管道水深
  !   real :: zhrad ! 管道水力半径
  !   real :: za    ! 管道湿面积
 
  !   if (zdrsto > 0) then
  !     zdph = zdrsto / zdrrad / zdrtotlgh           ! 计算管道水深
  !     za = zdrsto / zdrtotlgh                      ! 计算管道湿面积
  !     zhrad = za / (2 * zdph + zdrrad)             ! 计算管道水力半径
  !     ! 使用曼宁公式计算单个管道的流出量
  !     zdrout = (1. / zdrman) * za * (zhrad ** (2. / 3)) * sqrt(zdrslp)
  !     ! 约束流出量，确保其不会超过当前存储的水量或出口数量的限制
  !     zdrout = min(zdrsto / zdt, zdrout * zdrnoutlet)
  !   else
  !     zdrout = 0.
  !   endif
    
  ! end subroutine HYMAP2_calc_urb_drain_out
  subroutine HYMAP2_calc_urb_drain_out(zdt, zdrrad, zdrman, zdrslp, zdrstomax, zdrtotlgh, zdrnoutlet, zdrsto, zdrout)
    implicit none
    ! 输入参数
    real, intent(in) :: zdt         ! 时间步长
    real, intent(in) :: zdrrad      ! 管道半径
    real, intent(in) :: zdrman      ! 曼宁系数
    real, intent(in) :: zdrslp      ! 坡度
    real, intent(in) :: zdrstomax   ! 最大排水存储量
    real, intent(in) :: zdrtotlgh   ! 管道总长度
    real, intent(in) :: zdrnoutlet  ! 排水出口数量
    real, intent(in) :: zdrsto      ! 当前存储量

    ! 输出参数
    real, intent(out) :: zdrout     ! 流出量

    ! 局部变量
    real :: zdph  ! 管道水深
    real :: zhrad ! 管道水力半径
    real :: za    ! 管道湿面积
    real :: velocity_in ! 入口速度
    real :: pressure_drop ! 压力降
    real :: pipe_diameter ! 管道直径

    ! 常数定义
    real, parameter :: friction_factor = 0.02    ! 摩擦因子（假设值）
    real, parameter :: fluid_density = 1000.0    ! 流体密度（假设为水，单位：kg/m³）
    real, parameter :: pi = 3.14159265358979323846
    ! 计算管道直径
    pipe_diameter = 2.0 * zdrrad

    if (zdrsto > 0) then
        zdph = zdrsto / (pi * zdrrad**2 * zdrtotlgh)  ! 计算管道水深
        za = zdrsto / zdrtotlgh                      ! 计算管道湿面积
        zhrad = za / (2 * zdph + zdrrad)             ! 计算管道水力半径

        ! 使用曼宁公式计算单个管道的流出量
        zdrout = (1. / zdrman) * za * (zhrad ** (2. / 3)) * sqrt(zdrslp)

        ! 计算入口速度
        velocity_in = sqrt((2 * pressure_drop) / fluid_density)

        ! 计算压力降
        pressure_drop = (friction_factor * zdrtotlgh / pipe_diameter) * (fluid_density * velocity_in**2 / 2.0)

        ! 更新流出量，确保其不会超过当前存储的水量或出口数量的限制
        zdrout = min(zdrsto / zdt, zdrout * zdrnoutlet + pressure_drop / (fluid_density * zdt))
    else
        zdrout = 0.
    endif

end subroutine HYMAP2_calc_urb_drain_out
  !============================================= 
  subroutine HYMAP2_get_urban_parameters(drfile, drwth, drhgt, drden, drvel, drblk, drrad, drlgh, drman, drslp)
    ! 子程序从文件中读取城市排水系统参数
    use LIS_logMod
    implicit none
    ! 输入参数
    character(*), intent(in) :: drfile  ! 参数文件名
    ! 输出参数
    real, intent(out) :: drwth, drhgt, drden, drvel, drblk, drrad, drlgh, drman, drslp
    logical :: file_exists  ! 文件是否存在
    integer :: ftn          ! 文件单元编号

    inquire(file = drfile, exist = file_exists)  ! 检查文件是否存在
   
    if (file_exists) then
      ftn = LIS_getNextUnitNumber()
      open(ftn, file = trim(drfile), status = 'old')
      read(ftn, *)
      read(ftn, *) drwth
      read(ftn, *) drhgt
      read(ftn, *) drden
      read(ftn, *) drvel
      read(ftn, *)
      read(ftn, *)
      read(ftn, *) drblk
      read(ftn, *) drrad
      read(ftn, *) drlgh
      read(ftn, *) drman
      read(ftn, *) drslp
      close(ftn)
      call LIS_releaseUnitNumber(ftn)
    else
      write(LIS_logunit, *) '[ERR] urban drainage parameter file ' // trim(drfile)
      write(LIS_logunit, *) '[ERR] does not exist'
      call LIS_endrun()
    endif 
  end subroutine HYMAP2_get_urban_parameters
  ! ================================================              
  subroutine HYMAP2_gen_urban_drain_maps(nseqall, drrad, drlgh,  drblk, grarea, next, flowmap, drstomax, drnoutlet, drtotlgh,inletarea)
    ! 子程序生成城市排水网络地图
    implicit none
    ! 输入参数
    integer, intent(in) :: nseqall       ! 网格单元数量
    real, intent(in) :: drrad(nseqall)      ! 排水半径和长度密度
    real, intent(in) :: drlgh(nseqall)           ! 排水速度
    ! real, intent(in) :: drden, drwth     ! 排水密度和宽度
    real, intent(in) :: drblk(nseqall)            ! 街区长度
    real, intent(in) :: grarea(nseqall)  ! 网格单元面积
    integer, intent(in) :: next(nseqall) ! 下游网格单元
    real, intent(in) :: flowmap(nseqall) ! 流向图
    ! real, intent(in) :: inletarea(nseqall) ! 排水面积
    ! 输出参数
    real, intent(inout) :: drstomax(nseqall) ! 最大排水存储量
    real, intent(out) :: inletarea(nseqall) ! 总排水宽度
    real, intent(out) :: drnoutlet(nseqall) ! 排水出口数量
    real, intent(out) :: drtotlgh(nseqall)  ! 总排水长度
    
    integer :: i

    do i = 1, nseqall
      if (flowmap(i) == 4) then
        ! 计算最大排水存储量
        drstomax(i) = (drrad(i) ** 2) * drlgh(i) * grarea(i)
        ! 计算网格单元内总排水宽度
        ! inletarea(i) = drden * drwth * grarea(i)

        ! 计算网格单元内排水出口数量
        drnoutlet(i) = 2 * sqrt(grarea(i)) / drblk(i)
        ! 计算网格单元内总排水长度
        drtotlgh(i) = drlgh(i) * grarea(i)
      else
        drstomax(i) = 0.
        ! drtotwth(i) = 0.
        drnoutlet(i) = 0.
        drtotlgh(i) = 0.
      endif
    enddo

  end subroutine HYMAP2_gen_urban_drain_maps
  ! ================================================              
end module HYMAP2_urbanMod