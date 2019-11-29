..\Assembler\win64\rgbasm.exe -o main.o main.asm

..\Assembler\win64\rgblink.exe -o main.gb main.o

..\Assembler\win64\rgbfix.exe -v -p 0 main.gb

..\Emulator\bgb64.exe main.gb