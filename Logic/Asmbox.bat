@echo off
IF EXIST dosbox GOTO dirok
cd ..
:dirok
start "" .\dosbox\DOSBox.exe -c "mount c c_disk" -c "imgmount Y dosbox\asm_dev_disk.iso -t iso" -c "Y:\UTILS\DOSBOX\INIT.BAT Y" -conf dosbox\dosbox.conf
